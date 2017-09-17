-- This mirrors FakeData. But instead of using fake data, it reads in data from 
-- pgn files
-- import Services.Types

-- data ParseInput = ParseInput { 
--     input :: Te.Text
--   , startMove :: Maybe Int
--   , endMove :: Maybe Int
--   , gameStart :: Maybe Int
--   , gameEnd :: Maybe Int }

-- databases :: IO [FilePath]
-- databases = undefined

-- inputCommand :: ParseInput -> String
-- inputCommand = undefined

-- data BestMoveTest = BestMoveTest { moveTestFen :: String, moveTestMove :: String, moveTestBest :: String, moveTestComparison :: Int deriving (Show, Generic)

-- -- read game into database
-- -- use haskell-chess to extract important moves

-- dbs = ["naj"]

-- insertDB :: FilePath -> IO ()
-- insertDB fileName = do
--   gamePgnRaw :: Te.Text <- Tu.strict $ Tu.input fileName
--   let number = 100
--   let needle = "[Event "
--   let elements = Data.List.take (number + 1) $ Te.splitOn needle gamePgnRaw


