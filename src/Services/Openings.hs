{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Services.Openings where

import Data.Attoparsec.Combinator (many', many1')
import Data.Attoparsec.Text
  ( Parser
  , anyChar
  , char
  , digit
  , endOfLine
  , letter
  , manyTill
  , parseOnly
  , skipWhile
  , space
  , string
  )
import Database.Persist (Key, insertBy)
import Database.Persist.Sql (Filter)
import Prelude hiding (lookup)

import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (fold)
import qualified Data.List.Split as LS (splitOn)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text, pack, splitOn, unpack)
import Database.Esqueleto hiding (get)
import Filesystem.Path.CurrentOS (fromText)
import Services.Types
import Turtle (input, strict)

import qualified Chess.Fen as Fen
import qualified Chess.Logic as ChessLogic
import qualified Chess.Pgn.Logic as Pgn
import Debug.Trace (trace)
import Test.Helpers

-- Todo: Remove this duplication
connString :: String -> String
connString dbName = trace name name
  where 
    name = "host=localhost dbname=chess_" ++ dbName ++ " user=postgres"

keyReader :: forall record. Either (Entity record) (Key record) -> Key record
keyReader = either entityKey id

data FullOpeningData = FullOpeningData {
  opMajor :: Entity OpeningLine
, opVariation :: Entity OpeningVariation
, opCode :: Entity OpeningCode
}

type Fen = String
type OpeningMap = Map Fen FullOpeningData


parseOpenings :: Text -> [ListData]
parseOpenings text = catMaybes $ fmap (rightToMaybe . parseOnly parseListData) split
  where split = splitOn "\r\n\r" text

deleteOpenings :: DataAction ()
deleteOpenings = do
  deleteWhere ([] :: [Filter OpeningVariation])
  deleteWhere ([] :: [Filter OpeningLine])
  deleteWhere ([] :: [Filter OpeningCode])

storeOpenings :: String -> IO ()
storeOpenings dbName = do
  text :: Text <- strict $ input $ fromText $ pack "./data/openings.txt"
  let actionRunner = inBackend (connString dbName) 
  let storeIO dat = actionRunner $ tryStoreOpening dat
  actionRunner deleteOpenings
  mapM_ storeIO $ parseOpenings text

-- |Reads the opening data from the database and returns it as a `Map`
-- that makes it easy to obtain the opening corresponding to a game.
getOpeningData :: DataAction OpeningMap
getOpeningData = do
  variations :: [(Entity OpeningLine, Entity OpeningVariation, Entity OpeningCode)] <-
    select $
    from $ \(l, v, c) -> do
      where_ $
        (v ^. OpeningVariationCode ==. c ^. OpeningCodeId) &&.
        (v ^. OpeningVariationLine ==. l ^. OpeningLineId)
      return (l, v, c)
  let list =
        [ (openingVariationFen (entityVal v), FullOpeningData l v c)
        | (l, v, c) <- variations
        ]
  return $ fromList list
  
parseVariation :: String -> Maybe (String, String)
parseVariation variationName = getVariation $ LS.splitOn ":" variationName
        
getVariation :: [String] -> Maybe (String, String)
getVariation [] = Nothing
getVariation [a] = Just (simplifyLine a, "")
getVariation [a,b] = Just (simplifyLine a, b)
getVariation _ = Nothing

-- The original opening names involve duplicates (e.g.
-- both Pirc Defense and Pirc). Removing those duplicates
-- wherever I spot them.
simplifyLine :: String -> String
simplifyLine line = maybe line snd maybeReplaced
  where maybeReplaced = listToMaybe $ filter ((==line) . fst) lineRenames

lineRenames :: [(String, String)]
lineRenames =
  [ ("Polish Opening", "Polish")
  , ("Grob's Attack", "Grob")
  , ("Reti Opening", "Reti")
  , ("Old Indian Defense", "Old Indian")
  , ("Old Benoni Defense", "Old Benoni")
  , ("Czech Benoni Defense", "Czech Benoni")
  , ("Scandinavian Defense", "Scandinavian")
  , ("Pirc Defense", "Pirc")
  , ("English Opening", "English")
  , ("English Opening (e4)", "English")
  , ("Budapest Defense Declined", "Budapest Defense")
  , ("Sicilian Defense", "Sicilian")
  ]

storeOpening :: String -> String -> String -> Pgn.PgnGame -> DataAction ()
storeOpening code variationName standardMoves game = do
  let line = parseVariation variationName
  maybe (return ()) (uncurry (storeWithFullData code standardMoves game)) line

storeWithFullData :: String -> String -> Pgn.PgnGame -> String -> String -> DataAction ()
storeWithFullData code standardMoves game majorLine variation = do
  lineKey :: Key OpeningLine <- keyReader <$> insertBy (OpeningLine majorLine)
  codeKey :: Key OpeningCode <- keyReader <$> insertBy (OpeningCode code)
  let fen = Fen.gameStateToFen $ last $ Pgn.gameStates $ Pgn.parsedPgnGame game
  insertBy $ OpeningVariation variation fen standardMoves codeKey lineKey
  return ()

tryStoreOpening :: ListData -> DataAction ()
tryStoreOpening (ListData code variationName standardMoves) = do
  let game = Pgn.readSingleGame $ pack standardMoves
  either (\_ -> return ()) (storeOpening code variationName standardMoves) game

getOpening :: OpeningMap -> ChessLogic.Game -> Maybe FullOpeningData
getOpening mp game = listToMaybe $ catMaybes sortedMatches
  where sortedMatches = reverse $ flip lookup mp <$> initialFens
        initialFens = take 10 $ Fen.gameStateToFen <$> Pgn.gameStates game

type OpenName = String
type CodeName = String
type OpenMoves = String

data ListData = ListData
  { openCode :: CodeName
  , openName :: OpenName
  , openMoves :: OpenMoves
  } deriving (Eq, Show)

parseListData :: Parser ListData
parseListData = do
  many' endOfLine
  code :: CodeName <- openingCodeParser
  many1' space
  name :: String <- openingNameParser
  many1' endOfLine
  moves <- openMoveParser
  return $ ListData code name moves

openingNameParser :: Parser String
openingNameParser = do
  name <- many1' $ fold $ [letter, digit] ++ fmap char (" /-:()\'" ++ ['.', '/'])
  many' $ char ';'
  skipWhile (\c -> c `notElem` ("\n\r" :: String))
  return name

openingCodeParser :: Parser CodeName
openingCodeParser = many1' $ fold $ digit : fmap char (['A'..'E'] ++ ['/'])

openMoveParser :: Parser OpenMoves
openMoveParser = do
  start :: Text <- string "1."
  rest :: String <- manyTill anyChar (char '/')
  many' endOfLine
  let endPart = " 1" :: String
  let restCleaned = take (length rest - length endPart) rest
  return $ unpack start ++ restCleaned
