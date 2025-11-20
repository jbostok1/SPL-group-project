data Weather = Weather
  { time :: String
  , temp  :: Double
  , humidity :: Double
  , feel_temp  :: Double
  , precip :: Double
  , rain  :: Double
  , snow :: Double
  , cloudcover  :: Double
  , pressure :: Double
  , windspeed  :: Double
  , gusts  :: Double
  , winddir :: Double
  } deriving (Show)

-- manual splitting so no need to import modulesß
split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
                    "" -> []
                    s' -> w : split p s''
                          where (w, s'') = break p s'


toLine :: [String] -> Weather
toLine [tim, tem, hum, fee, pre, rai, sno, clo, pres, win, gus, dir] = Weather tim (read tem) (read hum) (read fee) (read pre) (read rai) (read sno) (read clo) (read pres) (read win) (read gus) (read dir)
toLine _         = error "Invalid CSV line format"

loadWeather :: FilePath -> IO [Weather]
loadWeather filePath = do
    contents <- readFile filePath
    let rows = lines contents
        csvData = map (split (== ',')) (tail rows)  -- skip first line
        weatherdata = map toLine csvData
    return weatherdata

main :: IO ()
main = do
    weatherdata <- loadWeather "charlotte_weather.csv"
    putStrLn "Weather loaded from CSV:"
    mapM_ print weatherdata
