
module GameEngine.RegisterHandler where

import Text.ParserCombinators.Parsec (
    sepBy,
    oneOf,
    char,
    many,
    GenParser,
    parseFromFile
 )
import GameEngine.Call (Call)
import GameEngine.Item (Item)
import GameEngine.CallRegister (callRegister)
import GameEngine.ItemRegister (itemRegister)

{-

    Idea:
    * Parse Chapter-Files for calls and items.
    * Create a list of tuples of the form: 
        (nameOfCall :: String, nameOfCall :: Call)
        resp.
        (nameOfItem :: String, nameOfItem :: Item)
    * Use that list as a look-up register while loading certain calls or
      items.

    It's like creating a snapshot of all currently available calls/items in
    order to be able to load them the next time a game shall be loaded.
-}

-- ## SOURCE CODE PARSER FOR TOP-LEVEL CALLS AND ITEMS ## 

-- | Load 'Call's or 'Item's from the 'Call'- or 'Item'-register via the 'Call'- or 'Item'-name
loadFromRegister:: String -- ^ 'Call'- or 'Item'-name
                -> [(String, a)] -- ^ The respective register
                -> Maybe a
loadFromRegister _ [] = Nothing
loadFromRegister name ((entryName, entry) : rs)
    | name == entryName = Just entry
    | otherwise         = loadFromRegister name rs

-- | Loads a 'Call' from the recent call-register
loadCall:: String -- ^ 'Call'-name
        -> Maybe Call
loadCall callName =  loadFromRegister callName callRegister

-- | Loads an 'Item' from the recen item-register
loadItem:: String -- ^ 'Item'-name
        -> Maybe Item
loadItem itemName = loadFromRegister itemName itemRegister

{-|
    The files to parse for 'Call's and 'Item's. Update for each new chapter.
    E.g.: callFiles = [ "./src/Chapter/Chapter1.hs"
                      , "./src/Chapter/Chapter2.hs" ]
-}
callFiles:: [FilePath]
callFiles = []

callRegisterPath:: FilePath
callRegisterPath = "./src/GameEngine/CallRegister.hs"

itemRegisterPath:: FilePath
itemRegisterPath = "./src/GameEngine/ItemRegister.hs"

type Config = (String, String, String)

-- | Config for the 'Call'-parser, represents the imports-header in the callRegister.hs-file.
callConfig:: [String] -- ^ The files that are parsed to retrieve 'Call's
          -> Config
callConfig scannedFiles
    = ( "callRegister:: [(String, Call)]\ncallRegister = "
      , "module GameEngine.CallRegister where\n\n"
      , "import GameEngine.Call ( Call )\n\n" ++ fileImports )
    where
        fileImports = concatMap asImport scannedFiles ++ "\n\n"

-- | Call parser API, writes the all top-level 'Call's from the files in 'callFiles' into CallRegister.hs
callParser:: IO ()
callParser = do res <- callParserRec [] callFiles ":: Call"
                let callNames = map (takeWhile (/= ':')) res
                    register = startPreProcess (callConfig callFiles) callNames
                writeFile callRegisterPath register

-- | Config for the 'Item'-parser, represents the imports-header in the itemRegister.hs-file.
itemConfig:: [String] -- ^ The files that are parsed to retrieve 'Item's
          -> Config
itemConfig scannedFiles
    = ( "itemRegister:: [(String, Item)]\nitemRegister = "
      , "module GameEngine.ItemRegister where\n\n"
      , "import GameEngine.Item ( Item )\n\n" ++ fileImports )
    where
        fileImports = concat (map asImport scannedFiles) ++ "\n\n"

-- | Item parser API
itemParser:: IO ()
itemParser = do res <- callParserRec [] callFiles ":: Item"
                let callNames = map (takeWhile (/= ':')) res
                    register = startPreProcess (itemConfig callFiles) callNames
                writeFile itemRegisterPath register

-- | Processes the result of the parser such that it can be written into a file
startPreProcess:: Config -- ^ The configuration, see 'callConfig' or 'itemConfig'
               -> [String] -- ^ The names of the 'Call's or 'Item's
               -> String
startPreProcess _ [] = ""
startPreProcess (valueRegisterName, moduleName, imports) valueNames
     = fileHead ++ valueRegisterName ++ "[\n" ++ preProcess valueNames
    where
        fileHead = moduleName ++ imports

-- | Processes the name of a scanned file to an import
asImport:: String -- ^ The name of the file that shall be imported
        -> String
asImport importName = "import " ++ final ++ "\n"
    where
        withoutPath = reverse $ takeWhile (/= '/')
                              $ reverse importName
        final = "Chapter." ++ takeWhile (/= '.') withoutPath

-- | Traverses over the result of the parser in order to create the register
preProcess:: [String] -- ^ The parsed values
          -> String
preProcess [] = ""
preProcess (valueName : []) = asEntry valueName ++ "\n ]"
preProcess (valueName : cs) = asEntry valueName ++ ",\n" ++ preProcess cs

-- | Creates list elements for the register
asEntry:: String -- ^ The name of the value
       -> String
asEntry valueName = "    (" ++ "\"" ++ valueName ++ "\"" ++ ", " ++ valueName ++ ")"

-- | API for the parser, which scans the chapter files for calls
callParserRec:: [String] -- ^ The scanned values
             -> [FilePath] -- ^ The files to parse
             -> String -- ^ The data type with colons before it (see 'callParser' or 'itemParser' for example)
             -> IO [String]
callParserRec prevCalls [] _ = return prevCalls
callParserRec prevCalls (filePath : fs) lookFor
    = do result <- parseFromFile (callSourceCodeParser lookFor) filePath
         case result of
             Left errMsg -> error $ show errMsg
             Right cs    -> callParserRec (prevCalls ++ cs) fs lookFor

-- | Parser which scans the chapter files for calls
callSourceCodeParser:: String -- ^ The filename
                    -> GenParser Char st [String]
callSourceCodeParser lookFor = do ls <- sepBy (many symbol) (char '\n')
                                  return $ filter (substring lookFor) ls

-- | Parser which accepts any of the listed chars.
symbol:: GenParser Char st Char
symbol = oneOf $ [' '..'}'] ++ ['ä', 'ü', 'ö', 'ß', 'Ä', 'Ü', 'Ö'] ++ "->°~§´"

-- | Checks whether a given string is a substring of another string
substring:: String -- ^ The substring
         -> String -- ^ The entire string
         -> Bool
substring _ [] = False
substring toSearch text@(_ : ts)
    | toSearch == take (length toSearch) text = True
    | otherwise                               = substring toSearch ts
