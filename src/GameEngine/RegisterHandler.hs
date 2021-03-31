
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
    * Create a list of tuples from the form: 
        (nameOfCall :: String, nameOfCall :: Call)
        resp.
        (nameOfItem :: String, nameOfItem :: Item)
    * Use that list as a look-up register while loading certain calls or
      items.

    It's like creating a snapshot of all currently available calls/items in
    order to be able to load them the next time a game shall be loaded.
-}

-- ## SOURCE CODE PARSER FOR TOP-LEVEL CALLS AND ITEMS ## 

{-|
    Files, whose calls will be stored in the call-register in order to be able
    to load them during savegame-parsing.

    Update, if a new chapter has been added.

-}

loadFromRegister:: String -> [(String, a)] -> Maybe a
loadFromRegister _ [] = Nothing
loadFromRegister name ((entryName, entry) : rs)
    | name == entryName = Just entry
    | otherwise         = loadFromRegister name rs

-- | Loads a call from the recent call-register
loadCall:: String -> Maybe Call
loadCall callName =  loadFromRegister callName callRegister

-- | Loads an item from the recen item-register
loadItem:: String -> Maybe Item
loadItem itemName = loadFromRegister itemName itemRegister

-- Update for each new chapter
callFiles:: [FilePath]
callFiles = []

callRegisterPath:: FilePath
callRegisterPath = "./src/GameEngine/CallRegister.hs"

itemRegisterPath:: FilePath
itemRegisterPath = "./src/GameEngine/ItemRegister.hs"

type Config = (String, String, String)

-- | Config for the call-parser
callConfig:: [String] -> Config
callConfig scannedFiles
    = ( "callRegister:: [(String, Call)]\ncallRegister = "
      , "module GameEngine.CallRegister where\n\n"
      , "import GameEngine.Call ( Call )\n\n" ++ fileImports )
    where
        fileImports = concatMap asImport scannedFiles ++ "\n\n"

-- | Call parser API
callParser:: IO ()
callParser = do res <- callParserRec [] callFiles ":: Call"
                let callNames = map (takeWhile (/= ':')) res
                    register = startPreProcess (callConfig callFiles) callNames
                writeFile callRegisterPath register

-- | Config for the item-parser
itemConfig:: [String] -> Config
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
startPreProcess:: Config -> [String] -> String
startPreProcess _ [] = ""
startPreProcess (callRegisterName, moduleName, imports) callNames
     = fileHead ++ callRegisterName ++ "[\n" ++ preProcess callNames
    where
        fileHead = moduleName ++ imports

-- | Processes the name of a scanned file to an import
asImport:: String -> String
asImport importName = "import " ++ final ++ "\n"
    where
        withoutPath = reverse $ takeWhile (/= '/')
                              $ reverse importName
        final = "Chapter." ++ takeWhile (/= '.') withoutPath

-- | Traverses over the result of the parser in order to create the register
preProcess:: [String] -> String
preProcess [] = ""
preProcess (callName : []) = asEntry callName ++ "\n ]"
preProcess (callName : cs) = asEntry callName ++ ",\n" ++ preProcess cs

-- | Creates list elements for the register
asEntry:: String -> String
asEntry callName = "    (" ++ "\"" ++ callName ++ "\"" ++ ", " ++ callName ++ ")"

-- | API for the parser, which scans the chapter files for calls
callParserRec:: [String]
             -> [FilePath]
             -> String
             -> IO [String]
callParserRec prevCalls [] _ = return prevCalls
callParserRec prevCalls (filePath : fs) lookFor
    = do result <- parseFromFile (callSourceCodeParser lookFor) filePath
         case result of
             Left errMsg -> error $ show errMsg
             Right cs    -> callParserRec (prevCalls ++ cs) fs lookFor

-- | Parser which scans the chapter files for calls
callSourceCodeParser:: String -> GenParser Char st [String]
callSourceCodeParser lookFor = do ls <- sepBy (many symbol) (char '\n')
                                  return $ filter (substring lookFor) ls

-- | Parser which accepts any of the listed chars.
symbol:: GenParser Char st Char
symbol = oneOf $ [' '..'}'] ++ ['ä', 'ü', 'ö', 'ß', 'Ä', 'Ü', 'Ö'] ++ "->°~§´"

-- | Checks whether a given string is a substring of another string
substring:: String -> String -> Bool
substring _ [] = False
substring toSearch text@(_ : ts)
    | toSearch == take (length toSearch) text = True
    | otherwise                               = substring toSearch ts

