{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Services.DatabaseHelpers where

import Database.Persist (selectList, Entity, insertBy, insert, Key, entityVal, entityKey, update, (=.), (==.))
import Database.Persist.Postgresql (transactionSave)
import Data.Time (Day, fromGregorian)
import qualified Data.Text as Te (pack, Text)
import Data.Maybe (isJust, fromJust, listToMaybe)
import Data.Either (rights)
import Control.Monad.Reader (MonadIO, liftIO, join)
import Data.Either.Combinators (rightToMaybe)
import Debug.Trace (trace)
import Data.Attoparsec.Text (parseOnly, many', Parser, digit, char)
import qualified Turtle as Tu (Text)
import Data.List (intercalate)

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

openingHelper :: OpeningMap -> (Key Game) -> Pgn.PgnGame -> DataAction ()
openingHelper openings key pgnGame = do
  let game = (Pgn.parsedPgnGame pgnGame) :: Pgn.Game
  let opening = entityKey . opVariation <$> getOpening openings game
  update key [GameOpeningVariation =. opening]
  return ()

storeGameIntoDB :: Key Database -> OpeningMap -> Pgn.PgnGame -> DataAction (Maybe (Key Game))
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
parseRequiredTags tags = RequiredTags <$> maybeWhite <*> maybeBlack <*> maybeResult <*> maybeEvent
  where maybeWhite = Helpers.safeHead $ filter filterWhitePlayer tags
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
getDate tags = join $ fmap (\(Pgn.PgnDate d) -> rightToMaybe (parseOnly dateStringParse (Te.pack d))) $ listToMaybe $ filter filterDate tags

dateStringParse :: Parser Day
dateStringParse = do
  year <- many' digit
  char '.'
  month <- many' digit
  char '.'
  day <- many' digit
  return $ fromGregorian (read year :: Integer) (read month :: Int) (read day :: Int)

readTextIntoDB :: MonadIO m => String -> String -> Te.Text -> Bool -> m (Key Database, [Maybe (Key Game)])
readTextIntoDB dbName chessDBName text isPublic = liftIO $ inBackend (connString dbName) $ readTextWithPersist chessDBName text isPublic

readTextWithPersist :: String -> Tu.Text -> Bool -> DataAction (Key Database, [Maybe (Key Game)])
readTextWithPersist chessDBName text isPublic = do
  dbResult <- insertBy (Database chessDBName isPublic)
  let dbKey = keyReader dbResult
  let games = Pgn.getGamesFromText text
  openings <- getOpeningData
  gameResults <- mapM (storeGameIntoDB dbKey openings) $ rights games
  transactionSave
  return (dbKey, gameResults)


listToInClause :: [Int] -> String
listToInClause ints = clause
  where intStrings = fmap show ints :: [String]
        clause = '(' : intercalate ", " intStrings ++ ")"

