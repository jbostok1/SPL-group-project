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

--prediction 
data ProcessedWeather = ProcessedWeather
{ targetRain :: Bool --what we want to predict
, isHot      :: Bool
, isHumid    :: Bool
, isCloud    :: Bool
, isWindy    :: Bool
} deriving(Show)

--converter function
processWeather :: Weather -> ProcessedWeather
processWeather w = ProcessedWeather
  { targetRain = rain w > 0.0
  , isHot      = temp w > 25.0        -- e.g., "Hot" is over 25°C
  , isHumid    = humidity w > 60.0    -- e.g., "Humid" is over 60%
  , isCloudy   = cloudcover w > 50.0  -- e.g., "Cloudy" is over 50%
  , isWindy    = windspeed w > 15.0   -- e.g., "Windy" is over 15 km/h
  }
-- these function might break everything :)

toLine :: [String] -> Weather
toLine [tim, tem, hum, fee, pre, rai, sno, clo, pres, win, gus, dir] = Weather tim (read tem) (read hum) (read fee) (read pre) (read rai) (read sno) (read clo) (read pres) (read win) (read gus) (read dir)
toLine _         = error "Invalid CSV line format"

data

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
    putStrLn "Original Weather data (first 5):"
    mapM_ print (take 5 weatherdata)

    -- process the data
    let processedData = map processWeather weatherdata
    
    putStrLn "\nProcessed Boolean data (first 5):"
    mapM_ print (take 5 processedData)