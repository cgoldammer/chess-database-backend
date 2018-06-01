module Services.Openings where

import Data.Attoparsec.Text (Parser, takeWhile, string, digit, char, letter, space, endOfLine, skipWhile, takeTill, satisfy, inClass)
import Data.Attoparsec.Combinator (many', option, many1')

import Services.Types
import Data.Map (Map)

import qualified Chess.Logic as ChessLogic


data FullOpeningData = FullOpeningData {
  opVariation :: OpeningVariation
, opName :: OpeningName
, opCode :: OpeningCode
}

type Fen = String
type OpeningMap = Map Fen FullOpeningData

getOpeningData :: IO OpeningMap
getOpeningData = undefined

getOpening :: OpeningMap -> ChessLogic.Game -> FullOpeningData
getOpening = undefined

type OpenName = String
type OpenVariationName = String
type OpenMoves = String

data ListData = ListData {
  openName :: OpenName
, openVariationName :: OpenVariationName 
, openMoves :: OpenMoves
}

parseListData :: Parser ListData
parseListData = undefined

openingNameParser :: Parser OpenName
openingNameParser = undefined

openingVariationParser :: Parser OpenVariationName
openingVariationParser = undefined

openMoveParser :: Parser OpenMoves
openMoveParser = undefined




