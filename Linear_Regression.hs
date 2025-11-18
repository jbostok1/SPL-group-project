

import Text.Read (readMaybe)
import System.IO
import Data.List.Split (splitOn)
import Control.Exception (try, IOException)

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

linearRegression :: [Double] -> [Double] -> (Double, Double)
linearRegression xs ys = (a, b)
  where
    meanX = mean xs
    meanY = mean ys
    numerator   = sum [(x - meanX) * (y - meanY) | (x, y) <- zip xs ys]
    denominator = sum [(x - meanX)^2 | x <- xs]
    b = numerator / denominator
    a = meanY - b * meanX

predict :: (Double, Double) -> Double -> Double
predict (a, b) x = a + b * x

parseCSVLine :: String -> [String]
parseCSVLine = splitOn ","

-- Parse charlotte_weather.csv format
parseWeatherCSV :: String -> ([Double], [Double])
parseWeatherCSV content = 
    let linesOfFile = lines content
        nonEmptyLines = filter (not . null) linesOfFile
        dataLines = drop 1 nonEmptyLines  -- Skip header
        rows = map parseCSVLine dataLines
        validRows = filter (\row -> length row >= 3) rows
        -- Extract temperature_2m (column 1) and relative_humidity_2m (column 2)
        temperatures = [read temp :: Double | row <- validRows, 
                       let temp = row !! 1, 
                       not (null temp),
                       case readMaybe temp :: Maybe Double of
                         Just _ -> True
                         Nothing -> False]
        humidities = [read hum :: Double | row <- validRows, 
                     let hum = row !! 2, 
                     not (null hum),
                     case readMaybe hum :: Maybe Double of
                       Just _ -> True
                       Nothing -> False]
    in (temperatures, humidities)

main :: IO ()
main = do
    let filename = "charlotte_weather.csv"
    
    -- Read the CSV file
    fileExists <- doesFileExist filename
    if not fileExists
        then putStrLn $ "Error: File '" ++ filename ++ "' not found!"
        else do
            contents <- readFile filename
            let (temps, humidities) = parseWeatherCSV contents
            
            if length temps /= length humidities || null temps
               then putStrLn "Error: Invalid data in CSV file or mismatched values!"
               else do
                   putStrLn $ "Loaded " ++ show (length temps) ++ " data points from " ++ filename
                   putStrLn "Performing Linear Regression: Temperature (X) vs Relative Humidity (Y)"
                   putStrLn ""
                   
                   let (a, b) = linearRegression temps humidities
                   let roundedA = fromIntegral (round (a * 10)) / 10
                   let roundedB = fromIntegral (round (b * 10)) / 10
                   putStrLn $ "Regression Equation: Humidity = " ++ show roundedA ++ " + " ++ show roundedB ++ " * Temperature"
                   putStrLn $ "Intercept: " ++ show roundedA
                   putStrLn $ "Slope: " ++ show roundedB
                   putStrLn ""
                   
                   let minTemp = minimum temps
                   let maxTemp = maximum temps
                   putStrLn $ "Temperature range in training data: " ++ show minTemp ++ "°C to " ++ show maxTemp ++ "°C"
                   putStrLn "Note: Predictions outside this range may be unreliable."
                   putStrLn ""
                   putStrLn "Enter a temperature value to predict relative humidity:"
                   xPredLine <- getLine
                   case readMaybe xPredLine :: Maybe Double of
                     Just newTemp -> do
                         let predHumidity = predict (a, b) newTemp
                         -- Clamp humidity to valid range [0, 100] to prevent invalid values
                         let clampedHumidity = max 0.0 (min 100.0 predHumidity)
                         let roundedHumidity = fromIntegral (round (clampedHumidity * 10)) / 10
                         
                         -- Warn if temperature is outside training data range
                         if newTemp < minTemp || newTemp > maxTemp
                            then do
                                putStrLn $ "⚠ WARNING: Temperature " ++ show newTemp ++ "°C is outside the training data range!"
                                putStrLn "Prediction may be unreliable (extrapolation)."
                            else return ()
                         
                         -- Warn if prediction was clamped due to exceeding 100% or going below 0%
                         if predHumidity > 100.0
                            then do
                                putStrLn $ "⚠ WARNING: Raw prediction was " ++ show (fromIntegral (round (predHumidity * 10)) / 10) ++ "% (clamped to 100%)"
                            else if predHumidity < 0.0
                                then putStrLn $ "⚠ WARNING: Raw prediction was " ++ show (fromIntegral (round (predHumidity * 10)) / 10) ++ "% (clamped to 0%)"
                                else return ()
                         
                         putStrLn $ "Predicted Humidity for temperature = " ++ show newTemp ++ "°C is " ++ show roundedHumidity ++ "%"
                     Nothing -> putStrLn "Invalid number entered!"

-- Helper function to check if file exists
doesFileExist :: FilePath -> IO Bool
doesFileExist path = do
    result <- try (openFile path ReadMode) :: IO (Either IOException Handle)
    case result of
        Left _ -> return False
        Right h -> do
            hClose h
            return True
