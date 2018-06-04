{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}

module Services.Openings where

import Prelude hiding (lookup)
import Data.Attoparsec.Text (Parser, parseOnly, string, digit, char, letter, space, endOfLine, skipWhile, anyChar, manyTill)
import Data.Attoparsec.Combinator (many', many1')
import Database.Persist (insertBy, Key)

import Services.Types
import Data.Text (pack, unpack, Text, splitOn)
import Data.Map (lookup, Map, fromList)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (fold)
import Turtle (input, strict)
import Filesystem.Path.CurrentOS (fromText)
import Database.Esqueleto hiding (get)

import qualified Chess.Logic as ChessLogic
import qualified Chess.Pgn.Logic as Pgn
import qualified Chess.Fen as Fen
import Test.Helpers
import Debug.Trace (trace)

-- Todo: Remove this duplication
connString :: String -> String
connString dbName = trace name name
  where 
    name = "host=localhost dbname=chess_" ++ dbName ++ " user=postgres"

keyReader :: forall record. Either (Entity record) (Key record) -> Key record
keyReader = either entityKey id

data FullOpeningData = FullOpeningData {
  opVariation :: Entity OpeningVariation
, opCode :: Entity OpeningCode
}

type Fen = String
type OpeningMap = Map Fen FullOpeningData


parseOpenings :: Text -> [ListData]
parseOpenings text = catMaybes $ fmap (rightToMaybe . parseOnly parseListData) split
  where split = splitOn "\r\n\r" text

storeOpenings :: String -> IO ()
storeOpenings dbName = do
  text :: Text <- strict $ input $ fromText $ pack "./data/openings.txt"
  let storeIO dat = inBackend (connString dbName) $ tryStoreOpening dat
  mapM_ storeIO $ parseOpenings text

-- |Reads the opening data from the database and returns it as a `Map`
-- that makes it easy to obtain the opening corresponding to a game.
getOpeningData :: DataAction OpeningMap
getOpeningData = do
  variations :: [(Entity OpeningVariation, Entity OpeningCode)] <- select $ 
    from $ \(v, c) -> do
      where_ $ v^.OpeningVariationCode ==. c^.OpeningCodeId
      return (v, c)
  let list = [(openingVariationFen (entityVal v), FullOpeningData v c) | (v,c) <- variations]
  return $ fromList list
  

storeOpening :: String -> String -> String -> Pgn.PgnGame -> DataAction ()
storeOpening code variationName standardMoves game = do
  codeKey :: Key OpeningCode <- keyReader <$> insertBy (OpeningCode code)
  let fen = Fen.gameStateToFen $ last $ Pgn.gameStates $ Pgn.parsedPgnGame game
  insertBy $ OpeningVariation variationName fen standardMoves codeKey
  return ()

tryStoreOpening :: ListData -> DataAction ()
tryStoreOpening (ListData code variationName standardMoves) = do
  let game = Pgn.readSingleGame $ pack standardMoves
  either (\_ -> return ()) (storeOpening code variationName standardMoves) game

getOpening :: OpeningMap -> ChessLogic.Game -> Maybe FullOpeningData
getOpening mp game = listToMaybe $ catMaybes sortedMatches
  where sortedMatches = reverse $ flip lookup mp <$> initialFens :: [Maybe FullOpeningData]
        initialFens = take 10 $ Fen.gameStateToFen <$> Pgn.gameStates game


type OpenName = String
type CodeName = String
type OpenMoves = String

data ListData = ListData {
  openCode :: CodeName
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
  skipWhile (\c -> c `notElem` ("\n\r"::String))
  return name

openingCodeParser :: Parser CodeName
openingCodeParser = many1' $ fold $ digit : fmap char (['A'..'E'] ++ ['/'])

openMoveParser :: Parser OpenMoves
openMoveParser = do
  start :: Text <- string "1."
  -- rest :: String <- many1' $ fold $ [letter, digit] ++ fmap char (" .#x+=O-/ ")
  rest :: String <- manyTill anyChar (char '/')
  many' endOfLine
  let endPart = " 1" :: String
  let restCleaned = take (length rest - length endPart) rest
  return $ unpack start ++ restCleaned
