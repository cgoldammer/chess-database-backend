-- |Assorted statistical functions needed as part of the app.
module Services.StatsHelpers where

-- Arithmetic mean
mean :: (Floating a) => [a] -> a
mean xs = sum xs / (fromIntegral . length) xs

var :: (Floating a) => [a] -> a
var xs = (sum $ map (\x -> (x - m)^(2::Integer)) xs) / (fromIntegral (length xs)-1)
    where m = mean xs

stdDev :: (Floating a) => [a] -> a
stdDev x = sqrt $ var x

stdError :: (Floating a) => [a] -> a
stdError x = stdDev x / sqrt num
  where num = fromIntegral $ length x

intAverage :: [Int] -> Int
intAverage x = div (sum x) (length x)

