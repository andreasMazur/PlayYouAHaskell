
module GameEngine.SaveGameParser (
    parseSaveGameFile,
    SaveGame 
 ) where

import Text.ParserCombinators.Parsec
    (lookAhead, manyTill, between, try, parseFromFile, char,
      string,
      anyToken,
      choice,
      GenParser )

import GameEngine.Grid (createGrid,  Grid )
import GameEngine.PinUtilities ( createPinWithGrid )
import GameEngine.Object (Object,  createObject' )
import GameEngine.Pin ( Pin )
import GameEngine.Picture ( Picture(..) )
import GameEngine.Playground ( createPlaygroundWithPin, Playground )
import GameEngine.RegisterHandler (loadItem, loadCall)

-- ## API ##

saveGameFilePath:: FilePath
saveGameFilePath = "./app/saveGame"

type SaveGame = ( IO Playground
                , IO Picture
                , IO Picture
                , IO Picture
                , IO Picture
                , IO Picture )

parseSaveGameFile:: IO SaveGame
parseSaveGameFile 
    = do result <- parseFromFile saveGameFileParser saveGameFilePath
         case result of Left errMsg    -> error $ show errMsg
                        Right saveGame -> return saveGame

saveGameFileParser:: GenParser Char st SaveGame
saveGameFileParser = do pg <- parsePlayground
                        multiUseBox <- parsePicture
                        interBox <- parsePicture
                        textBox <- parsePicture
                        controlsMenu <- parsePicture
                        debugWindow <- parsePicture
                        return ( pg
                               , multiUseBox
                               , interBox
                               , textBox
                               , controlsMenu
                               , debugWindow )

-- ## PARSER ##

-- ## Grid-Parser ##
type GridTuple = (String, (Int, Int))

toGrid:: GridTuple -> IO Grid
toGrid (app, dim) = createGrid dim app

parseGrid:: GenParser Char st (IO Grid)
parseGrid = do --gridTuple <- betweenAlias "Grid" parseGridInnerTuple
               gridTuple <- parseGridInnerTuple
               return $ toGrid gridTuple

parseGridInnerTuple:: GenParser Char st GridTuple
parseGridInnerTuple = do appearence <- parseString "GridStr"
                         dimension <- parseTuple "GridDim"
                         return (appearence, dimension)

-- ## Pin-Parser ##
type PinTuple = (IO Grid, (Int, Int), Int, [String], Bool, Bool, [String])

toPin:: PinTuple -> IO Pin
toPin (ioGrid, sI, ide, callNames, rig, inB, itemNames) 
    = do let finalCalls = unMaybeList $ map loadCall callNames
             finalItems = unMaybeList $ map loadItem itemNames
         createPinWithGrid ioGrid sI ide finalCalls finalItems rig inB

parsePin:: GenParser Char st (IO Pin)
parsePin = do pinTuple <- betweenAlias "Pin" parsePinInnerTuple
              --pinTuple <- parsePinInnerTuple
              return $ toPin pinTuple

parsePinInnerTuple:: GenParser Char st PinTuple
parsePinInnerTuple = do grid <- parseGrid
                        startI <- parseTuple "StartIndex"
                        pinId <- parseInt "IdPin"
                        parseBool "HasChanged"
                        cs <- parseListOfStrings "CallsPin"
                        rig <- parseBool "Rigid"
                        inBg <- parseBool "InBackground"
                        inv <- parseListOfStrings "Inventory"
                        return (grid, startI, pinId, cs, rig, inBg, inv) 

-- ## Picture-Parser ##
type PictureTuple = (IO Pin, IO [Pin], Int, [String], String)

toPicture:: PictureTuple -> IO Picture
toPicture (pb, ps, ide, cs, desc) 
    = do bg <- pb
         ps' <- ps
         return Picture {
             pinboard = bg,
             pins = ps',
             identifier = ide,
             calls = unMaybeList $ map loadCall cs,
             description = desc
         }

parsePicture:: GenParser Char st (IO Picture)
parsePicture = do picTuple <- betweenAlias "Picture" parsePictureInnerTuple
                  --picTuple <- parsePictureInnerTuple
                  return $ toPicture picTuple

parsePictureInnerTuple:: GenParser Char st PictureTuple
parsePictureInnerTuple = do desc <- parseString "Description"
                            pin <- parsePin
                            ps <- manyTill parsePin (lookAhead $ try $ parseInt "Identifier")
                            ide <- parseInt "Identifier"
                            cs <- parseListOfStrings "Calls"
                            return (pin, sequence ps, ide, cs, desc)

-- ## Object-Parser ##
type ObjectTuple = (IO Pin, (Int, Int, Int, Int), Double, Double, Double)

toObject:: ObjectTuple -> IO Object
toObject (p, _, m, vel, res) 
    = do pin <- p
         return $ createObject' pin 
                                (read.show $ m) 
                                (read.show $ vel) 
                                (read.show $ res)

parseObject:: GenParser Char st (IO Object)
parseObject = do objectTuple <- betweenAlias "Object" parseObjectInnerTuple
                 --objectTuple <- parseObjectInnerTuple
                 return $ toObject objectTuple

parseObjectInnerTuple:: GenParser Char st ObjectTuple
parseObjectInnerTuple = do p <- parsePin
                           sh <- parse4Tuple "Shape"
                           mass <- parseDouble "Mass"
                           vel <- parseDouble "Velocity"
                           res <- parseDouble "Restitution"
                           return (p, sh, mass, vel, res)

-- ## Playground-Parser ##
type PlaygroundTuple = (IO Pin, IO [Object], [String], String)

toPlayground:: PlaygroundTuple -> IO Playground
toPlayground (bg, os, cs, desc) 
    = do objects <- os
         createPlaygroundWithPin bg desc objects $ unMaybeList 
                                                 $ map loadCall cs

parsePlayground:: GenParser Char st (IO Playground)
parsePlayground = do pgTuple <- betweenAlias "Playground" parsePlaygroundInnerTuple
                     --pgTuple <- parsePlaygroundInnerTuple
                     return $ toPlayground pgTuple

parsePlaygroundInnerTuple:: GenParser Char st PlaygroundTuple
parsePlaygroundInnerTuple = do bg <- parsePin
                               os <- manyTill parseObject (lookAhead $ try $ parseInt "Identifier")
                               parseInt "Identifier"
                               cs <- parseListOfStrings "Calls"
                               desc <- parseString "Description"
                               return (bg, sequence os, cs, desc)

-- ## HELP PARSER ##

-- | Parses whatever is in between of 'nodeName'-node
parseNode:: String -> GenParser Char st String
parseNode nodeName = do string start
                        continue [] (string end)
  where
    (start, end) = toNode nodeName

toNode:: String -> (String, String)
toNode nodeName = ("<" ++ nodeName ++ ">", "</" ++ nodeName ++ ">")

continue:: String -> GenParser Char st String -> GenParser Char st String
continue consumed end = choice [endNow consumed end, consume consumed end]

endNow:: String -> GenParser Char st String -> GenParser Char st String
endNow consumed end = do try end
                         char '\n'
                         return $ reverse consumed

consume:: String -> GenParser Char st String -> GenParser Char st String
consume consumed end = do c <- anyToken
                          continue (c : consumed) end

-- | Alias for 'between'-parser
betweenAlias:: String -> GenParser Char st a -> GenParser Char st a
betweenAlias nodeName wantedParser
    = between begin stop wantedParser
    where
        begin = do string start
                   char '\n'
        stop = do string end
                  char '\n'
        (start, end) = toNode nodeName

-- | Parses a tuple of Ints
parseTuple:: String -> GenParser Char st (Int, Int)
parseTuple nodeName = do tuple <- parseNode nodeName
                         return (read tuple :: (Int, Int))

-- | Parses a 4-tuple of Ints
parse4Tuple:: String -> GenParser Char st (Int, Int, Int, Int)
parse4Tuple nodeName = do quadruple <- parseNode nodeName
                          return (read quadruple :: (Int, Int, Int, Int))

-- | Parses a boolean
parseBool:: String -> GenParser Char st Bool
parseBool nodeName = do bool <- parseNode nodeName
                        return (read bool :: Bool)

-- | Parses an int
parseInt:: String -> GenParser Char st Int
parseInt nodeName = do int <- parseNode nodeName
                       return (read int :: Int)

-- | Parses a double
parseDouble:: String -> GenParser Char st Double
parseDouble nodeName = do double <- parseNode nodeName
                          return (read double :: Double)

-- | Parses a list of strings
parseListOfStrings:: String -> GenParser Char st [String]
parseListOfStrings nodeName = do list <- parseNode nodeName
                                 return (read (addQuotes list) :: [String])

-- | Adds quotes to a string that appears to be a list of some values
addQuotes:: String -> String
addQuotes [] = []
addQuotes (c : cs)
  | c == '[' = "[\"" ++ addQuotes cs
  | c == ']' = "\"]" ++ addQuotes cs
  | c == ',' = "\",\"" ++ addQuotes cs
  | otherwise = c : addQuotes cs

-- | Parse a string
parseString:: String -> GenParser Char st String
parseString nodeName = do str <- parseNode nodeName
                          return str

-- ## MISC ##

unMaybeList:: [Maybe a] -> [a]
unMaybeList [] = []
unMaybeList (value : xs) = case value of Nothing    -> unMaybeList xs
                                         Just value -> value : unMaybeList xs