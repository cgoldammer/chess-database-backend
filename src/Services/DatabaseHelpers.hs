{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Services.DatabaseHelpers where

import Control.Monad.Reader (MonadIO, join, liftIO)
import Data.Attoparsec.Text (Parser, char, digit, many', parseOnly)
import Data.Either (rights)
import Data.Either.Combinators (rightToMaybe)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Foldable (find)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Text.Printf (printf)
import qualified Data.Text as Te (Text, pack, intercalate)
import qualified Data.Text.IO as TeIO (writeFile)
import Data.Time (Day, fromGregorian)
import qualified Filesystem.Path.CurrentOS as FS (fromText)
import qualified Turtle as Tu (input, strict, Text)
import Database.Persist
  ( Entity
  , Key
  , (=.)
  , (==.)
  , entityKey
  , entityVal
  , insert
  , insertBy
  , selectList
  , update
  )
import Database.Persist.Postgresql (transactionSave)
import Debug.Trace (trace)

import qualified Chess.Pgn.Logic as Pgn
import qualified Chess.Helpers as Helpers

import Test.Helpers as Helpers
import Services.Types
import Services.Openings (OpeningMap, opVariation, getOpening, getOpeningData)


connString :: String -> String
connString dbName = trace name name
  where 
    name = "host=localhost dbname=chess_" ++ dbName ++ " user=postgres"

keyReader :: forall record. Either (Entity record) (Key record) -> Key record
keyReader = either entityKey id

-- It can happen that a game doesn't have an opening, and that
-- with improved algorithms, we can now find an opening. Thus we want a function
-- that updates the opening of all games without openings.
addOpeningsToGames :: DataAction ()
addOpeningsToGames = do
  openings <- getOpeningData
  gamesWithoutOpening :: [Entity Game] <- selectList [GameOpeningVariation ==. Nothing] []
  liftIO $ print $ "Games without: " ++ show (length gamesWithoutOpening)
  mapM_ (addOpeningToGame openings) gamesWithoutOpening

addOpeningToGame :: OpeningMap -> Entity Game -> DataAction ()
addOpeningToGame openings entityGame = do
  let key = entityKey entityGame
  let pgnGame = Pgn.readSingleGame $ Te.pack $ gamePgn $ entityVal entityGame
  either (const (return ())) (openingHelper openings key) pgnGame

openingHelper :: OpeningMap -> Key Game -> Pgn.PgnGame -> DataAction ()
openingHelper openings key pgnGame = do
  let game = Pgn.parsedPgnGame pgnGame :: Pgn.Game
  let opening = entityKey . opVariation <$> getOpening openings game
  update key [GameOpeningVariation =. opening]
  return ()

storeGameIntoDB ::
     Key Database -> OpeningMap -> Pgn.PgnGame -> DataAction (Maybe (Key Game))
storeGameIntoDB dbResult openings g = do
  let game = Pgn.parsedPgnGame g
  let opening = entityKey . opVariation <$> getOpening openings game
  let pgn = Pgn.gamePgnFull game
  let tags = Pgn.pgnGameTags g :: [Pgn.PgnTag]
  let requiredTags = trace (show tags) $ parseRequiredTags tags
  if isJust requiredTags
    then do
      let parsedTags = fromJust requiredTags
      (playerWhite, playerBlack) <- storePlayers dbResult parsedTags
      tournament <- storeTournament dbResult parsedTags
      let resultInt = resultDBFormat $ requiredResult parsedTags
      let date = getDate tags -- Maybe Day
      -- Storing the game
      let gm = Game dbResult playerWhite playerBlack resultInt tournament pgn date opening
      gameResult <- keyReader <$> insertBy gm
      -- Storing the tags
      let formattedTags = formatForDB <$> filter (not . isPlayer) tags
      mapM_ (\(name, v) -> insert (GameAttribute gameResult name v)) formattedTags
      return $ Just gameResult
    else do
      liftIO $ print $ show g
      return Nothing


storeTournament :: Key Database -> RequiredTags -> DataAction (Key Tournament)
storeTournament dbResult tags = do
  let (Pgn.PgnEvent eventName) = requiredEvent tags
  result <- insertBy $ Tournament dbResult eventName
  return $ keyReader result


storePlayers :: Key Database -> RequiredTags -> DataAction (Key Player, Key Player)
storePlayers dbResult tags = do
  let (whitePlayer, blackPlayer) = (requiredWhitePlayer tags, requiredBlackPlayer tags)
  let (Pgn.PgnWhite (Pgn.Player firstWhite lastWhite)) = whitePlayer
  let (Pgn.PgnBlack (Pgn.Player firstBlack lastBlack)) = blackPlayer
  whiteResult <- insertBy (Player dbResult firstWhite lastWhite)
  blackResult <- insertBy (Player dbResult firstBlack lastBlack)
  return (keyReader whiteResult, keyReader blackResult)


data RequiredTags = RequiredTags {
    requiredWhitePlayer :: Pgn.PgnTag
  , requiredBlackPlayer :: Pgn.PgnTag
  , requiredResult :: Pgn.PgnTag
  , requiredEvent :: Pgn.PgnTag}

parseRequiredTags :: [Pgn.PgnTag] -> Maybe RequiredTags
parseRequiredTags tags =
  RequiredTags <$> maybeWhite <*> maybeBlack <*> maybeResult <*> maybeEvent
  where
    maybeWhite = Helpers.safeHead $ filter filterWhitePlayer tags
    maybeBlack = Helpers.safeHead $ filter filterBlackPlayer tags
    maybeResult = Helpers.safeHead $ filter filterResult tags
    maybeEvent = Helpers.safeHead $ filter filterEvent tags
  
isPlayer :: Pgn.PgnTag -> Bool
isPlayer (Pgn.PgnWhite _) = True
isPlayer (Pgn.PgnBlack _) = True
isPlayer _ = False

filterWhitePlayer :: Pgn.PgnTag -> Bool
filterWhitePlayer (Pgn.PgnWhite _) = True
filterWhitePlayer _ = False

filterBlackPlayer :: Pgn.PgnTag -> Bool
filterBlackPlayer (Pgn.PgnBlack _) = True
filterBlackPlayer _ = False

filterResult :: Pgn.PgnTag -> Bool
filterResult (Pgn.PgnResult _) = True
filterResult _ = False

filterEvent :: Pgn.PgnTag -> Bool
filterEvent (Pgn.PgnEvent _) = True
filterEvent _ = False

filterDate :: Pgn.PgnTag -> Bool
filterDate (Pgn.PgnDate _) = True
filterDate _ = False


resultDBFormat :: Pgn.PgnTag -> Int
resultDBFormat (Pgn.PgnResult Pgn.WhiteWin) = 1
resultDBFormat (Pgn.PgnResult Pgn.BlackWin) = -1
resultDBFormat (Pgn.PgnResult Pgn.Draw) = 0
resultDBFormat _ = 0

getDate :: [Pgn.PgnTag] -> Maybe Day 
getDate tags = extractParse =<< find filterDate tags
  where extractParse (Pgn.PgnDate d) = rightToMaybe $ parseOnly dateStringParse (Te.pack d)

dateStringParse :: Parser Day
dateStringParse = do
  year <- many' digit
  char '.'
  month <- many' digit
  char '.'
  day <- many' digit
  return $ fromGregorian (read year :: Integer) (read month :: Int) (read day :: Int)

writeChunk :: String -> Int -> [Te.Text] -> IO ()
writeChunk fileName fileNumber texts = do
  let fullName = fileName ++ "_" ++ printf "%05d" fileNumber ++ ".pgn"
  TeIO.writeFile fullName $ Te.intercalate (Te.pack "\n") texts
  

splittingHelper :: String -> Int -> IO ()
splittingHelper fileName number = do
  fileText <- Tu.strict $ Tu.input $ FS.fromText $ Te.pack fileName

  let splits = Pgn.splitIntoGames fileText
  let chunks = zip [0..] $ chunksOf number splits

  mapM_ (uncurry (writeChunk fileName)) chunks


  

readTextIntoDB ::
     MonadIO m
  => String
  -> String
  -> Te.Text
  -> Bool
  -> Maybe String
  -> m (Key Database, [Maybe (Key Game)])
readTextIntoDB dbName chessDBName text isPublic user =
  liftIO $ inBackend (connString dbName) $ readTextWithPersist chessDBName text isPublic user

readTextWithPersist ::
     String -> Tu.Text -> Bool -> Maybe String -> DataAction (Key Database, [Maybe (Key Game)])
readTextWithPersist chessDBName text isPublic user = do
  dbResult <- insertBy (Database chessDBName isPublic user)
  transactionSave
  let dbKey = keyReader dbResult

  -- Lazily parse and enter games into db
  let games = Pgn.getGamesFromText text
  openings <- getOpeningData





  gameResults <- mapM (storeGameIntoDB dbKey openings) $ rights games
  transactionSave
  return (dbKey, gameResults)



listToInClause :: [Int] -> String
listToInClause ints = clause
  where intStrings = fmap show ints :: [String]
        clause = '(' : intercalate ", " intStrings ++ ")"
