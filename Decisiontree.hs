import Data.List (nub, partition, maximumBy)
import Data.Ord (comparing)
import Text.Printf (printf)

-- Weather data types
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

-- Processed weather with boolean features
data ProcessedWeather = ProcessedWeather
  { targetRain :: Bool
  , isHot      :: Bool
  , isHumid    :: Bool
  , isCloud    :: Bool
  , isWindy    :: Bool
  } deriving(Show, Eq)

-- Decision Tree data structure
data DecisionTree = 
    Leaf String                               -- Leaf with classification
  | Node String (Bool -> DecisionTree)       -- Node with attribute and function

-- Attribute type for features we can split on
data Attribute = Hot | Humid | Cloud | Windy | Rain
  deriving (Show, Eq, Ord, Enum, Bounded)

-- Weather classification result
data WeatherClass = 
    HotWeather 
  | HumidWeather 
  | CloudyWeather 
  | WindyWeather 
  | NormalWeather
  | MultiCondition [WeatherClass]
  deriving (Eq)

instance Show WeatherClass where
  show HotWeather = "Hot"
  show HumidWeather = "Humid"
  show CloudyWeather = "Cloudy"
  show WindyWeather = "Windy"
  show NormalWeather = "Normal"
  show (MultiCondition cs) = "Multiple: " ++ show (map show cs)

-- Helper function to split strings
split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
                    "" -> []
                    s' -> w : split p s''
                          where (w, s'') = break p s'

-- Convert Weather to ProcessedWeather
processWeather :: Weather -> ProcessedWeather
processWeather w = ProcessedWeather
  { targetRain = rain w > 0.0
  , isHot      = temp w > 25.0        -- Hot is over 25°C
  , isHumid    = humidity w > 60.0    -- Humid is over 60%
  , isCloud    = cloudcover w > 50.0  -- Cloudy is over 50%
  , isWindy    = windspeed w > 15.0   -- Windy is over 15 km/h
  }

-- Load weather data from CSV
toLine :: [String] -> Weather
toLine [tim, tem, hum, fee, pre, rai, sno, clo, pres, win, gus, dir] = 
    Weather tim (read tem) (read hum) (read fee) (read pre) (read rai) 
            (read sno) (read clo) (read pres) (read win) (read gus) (read dir)
toLine _ = error "Invalid CSV line format"

loadWeather :: FilePath -> IO [Weather]
loadWeather filePath = do
    contents <- readFile filePath
    let rows = lines contents
        csvData = map (split (== ',')) (tail rows)  -- skip header
        weatherdata = map toLine csvData
    return weatherdata

-- Load processed weather from CSV
loadProcessedWeather :: FilePath -> IO [ProcessedWeather]
loadProcessedWeather filePath = do
    contents <- readFile filePath
    let rows = lines contents
        csvData = map (split (== ',')) (tail rows)  -- skip header
        processedData = map parseProcessedLine csvData
    return processedData

parseProcessedLine :: [String] -> ProcessedWeather
parseProcessedLine [rain, hot, humid, cloud, windy] = ProcessedWeather
    { targetRain = parseBool rain
    , isHot = parseBool hot
    , isHumid = parseBool humid
    , isCloud = parseBool cloud
    , isWindy = parseBool windy
    }
  where
    parseBool "True" = True
    parseBool _ = False
parseProcessedLine _ = error "Invalid processed weather CSV format"

-- Get attribute value from ProcessedWeather
getAttributeValue :: Attribute -> ProcessedWeather -> Bool
getAttributeValue Hot w = isHot w
getAttributeValue Humid w = isHumid w
getAttributeValue Cloud w = isCloud w
getAttributeValue Windy w = isWindy w
getAttributeValue Rain w = targetRain w

-- Classify weather based on conditions
classifyWeather :: ProcessedWeather -> WeatherClass
classifyWeather pw = 
    let conditions = []
        conditions' = if isHot pw then HotWeather : conditions else conditions
        conditions'' = if isHumid pw then HumidWeather : conditions' else conditions'
        conditions''' = if isCloud pw then CloudyWeather : conditions'' else conditions''
        conditions'''' = if isWindy pw then WindyWeather : conditions''' else conditions'''
    in case conditions'''' of
        [] -> NormalWeather
        [c] -> c
        cs -> MultiCondition cs

-- Calculate entropy for information gain
entropy :: [ProcessedWeather] -> Attribute -> Double
entropy [] _ = 0.0
entropy samples targetAttr = 
    let total = fromIntegral (length samples)
        positives = fromIntegral $ length $ filter (getAttributeValue targetAttr) samples
        negatives = total - positives
        pPos = positives / total
        pNeg = negatives / total
        log2 x = if x == 0 then 0 else logBase 2 x
    in -(pPos * log2 pPos + pNeg * log2 pNeg)

-- Calculate information gain for an attribute
informationGain :: [ProcessedWeather] -> Attribute -> Attribute -> Double
informationGain samples splitAttr targetAttr = 
    let total = fromIntegral (length samples)
        (trueSamples, falseSamples) = partition (getAttributeValue splitAttr) samples
        trueWeight = fromIntegral (length trueSamples) / total
        falseWeight = fromIntegral (length falseSamples) / total
        originalEntropy = entropy samples targetAttr
        weightedEntropy = trueWeight * entropy trueSamples targetAttr + 
                         falseWeight * entropy falseSamples targetAttr
    in originalEntropy - weightedEntropy

-- Find the best attribute to split on
bestAttribute :: [ProcessedWeather] -> [Attribute] -> Attribute -> Maybe Attribute
bestAttribute _ [] _ = Nothing
bestAttribute samples attributes targetAttr = 
    let gains = [(attr, informationGain samples attr targetAttr) | attr <- attributes]
        (bestAttr, _) = maximumBy (comparing snd) gains
    in Just bestAttr

-- Build decision tree using ID3-like algorithm
buildTree :: [ProcessedWeather] -> [Attribute] -> Attribute -> DecisionTree
buildTree samples [] targetAttr = 
    -- No more attributes to split on
    let targetValues = map (getAttributeValue targetAttr) samples
        trueCount = length (filter id targetValues)
        falseCount = length targetValues - trueCount
    in Leaf $ if trueCount >= falseCount then show targetAttr ++ ": True" else show targetAttr ++ ": False"

buildTree samples attributes targetAttr
    | all (getAttributeValue targetAttr) samples = Leaf $ show targetAttr ++ ": True"
    | all (not . getAttributeValue targetAttr) samples = Leaf $ show targetAttr ++ ": False"
    | otherwise = 
        case bestAttribute samples attributes targetAttr of
            Nothing -> Leaf $ show targetAttr ++ ": Unknown"
            Just attr ->
                let remainingAttrs = filter (/= attr) attributes
                    (trueSamples, falseSamples) = partition (getAttributeValue attr) samples
                    trueSubtree = if null trueSamples 
                                  then Leaf $ show targetAttr ++ ": False"
                                  else buildTree trueSamples remainingAttrs targetAttr
                    falseSubtree = if null falseSamples
                                   then Leaf $ show targetAttr ++ ": False"
                                   else buildTree falseSamples remainingAttrs targetAttr
                in Node (show attr) (\val -> if val then trueSubtree else falseSubtree)

-- Classify using decision tree
classifyWithTree :: DecisionTree -> ProcessedWeather -> String
classifyWithTree (Leaf result) _ = result
classifyWithTree (Node attrName f) weather = 
    let attrVal = case attrName of
            "Hot" -> isHot weather
            "Humid" -> isHumid weather
            "Cloud" -> isCloud weather
            "Windy" -> isWindy weather
            "Rain" -> targetRain weather
            _ -> error $ "Unknown attribute: " ++ attrName
    in classifyWithTree (f attrVal) weather

-- Print decision tree structure
printTree :: DecisionTree -> Int -> IO ()
printTree (Leaf result) indent = 
    putStrLn $ replicate indent ' ' ++ "└─ " ++ result
printTree (Node attr f) indent = do
    putStrLn $ replicate indent ' ' ++ "├─ " ++ attr ++ "?"
    putStrLn $ replicate (indent + 2) ' ' ++ "├─ True:"
    printTree (f True) (indent + 4)
    putStrLn $ replicate (indent + 2) ' ' ++ "└─ False:"
    printTree (f False) (indent + 4)

-- Build multiple trees for different targets
data WeatherTrees = WeatherTrees
    { hotTree :: DecisionTree
    , humidTree :: DecisionTree
    , cloudyTree :: DecisionTree
    , windyTree :: DecisionTree
    }

buildAllTrees :: [ProcessedWeather] -> WeatherTrees
buildAllTrees samples = 
    let availableAttrs = [Hot, Humid, Cloud, Windy]  -- Attributes to use for splitting
    in WeatherTrees
        { hotTree = buildTree samples (filter (/= Hot) availableAttrs) Hot
        , humidTree = buildTree samples (filter (/= Humid) availableAttrs) Humid
        , cloudyTree = buildTree samples (filter (/= Cloud) availableAttrs) Cloud
        , windyTree = buildTree samples (filter (/= Windy) availableAttrs) Windy
        }

-- Comprehensive weather classification
classifyWeatherComprehensive :: WeatherTrees -> ProcessedWeather -> String
classifyWeatherComprehensive trees weather = 
    let hotResult = classifyWithTree (hotTree trees) weather
        humidResult = classifyWithTree (humidTree trees) weather
        cloudyResult = classifyWithTree (cloudyTree trees) weather
        windyResult = classifyWithTree (windyTree trees) weather

        conditions = []
        conditions' = if "True" `elem` words hotResult then "Hot" : conditions else conditions
        conditions'' = if "True" `elem` words humidResult then "Humid" : conditions' else conditions'
        conditions''' = if "True" `elem` words cloudyResult then "Cloudy" : conditions'' else conditions''
        conditions'''' = if "True" `elem` words windyResult then "Windy" : conditions''' else conditions'''
    in case conditions'''' of
        [] -> "Normal weather conditions"
        [c] -> c ++ " weather"
        cs -> "Multiple conditions: " ++ unwords cs

-- Calculate accuracy
accuracy :: DecisionTree -> [ProcessedWeather] -> Attribute -> Double
accuracy tree samples targetAttr = 
    let predictions = map (classifyWithTree tree) samples
        actuals = map (\x -> show targetAttr ++ ": " ++ x) $ map (show . getAttributeValue targetAttr) samples
        correct = length $ filter (uncurry (==)) $ zip predictions actuals
    in fromIntegral correct / fromIntegral (length samples) * 100

-- Main demonstration function
main :: IO ()
main = do
    --  Load from raw weather data
    putStrLn "=== Weather Decision Tree Classifier ==="
    putStrLn "\nOption 1: Loading raw weather data..."
    weatherData <- loadWeather "charlotte_weather.csv"
    let processedFromRaw = map processWeather weatherData

    --  Load from processed CSV
    putStrLn "Option 2: Loading processed weather data..."
    processedData <- loadProcessedWeather "processed_weather.csv"

    -- Use processed data for training
    let trainingData = take 150 processedData  -- Use first 150 for training
        testData = drop 150 processedData      -- Use remaining for testing

    putStrLn $ "\nTraining samples: " ++ show (length trainingData)
    putStrLn $ "Test samples: " ++ show (length testData)

    -- Build decision trees
    putStrLn "\n=== Building Decision Trees ==="
    let trees = buildAllTrees trainingData

    -- Print tree structures
    putStrLn "\n--- Hot Weather Decision Tree ---"
    printTree (hotTree trees) 0

    putStrLn "\n--- Humid Weather Decision Tree ---"
    printTree (humidTree trees) 0

    putStrLn "\n--- Cloudy Weather Decision Tree ---"
    printTree (cloudyTree trees) 0

    putStrLn "\n--- Windy Weather Decision Tree ---"
    printTree (windyTree trees) 0

    -- Test accuracy on training data
    putStrLn "\n=== Training Accuracy ==="
    printf "Hot prediction accuracy: %.2f%%\n" (accuracy (hotTree trees) trainingData Hot)
    printf "Humid prediction accuracy: %.2f%%\n" (accuracy (humidTree trees) trainingData Humid)
    printf "Cloudy prediction accuracy: %.2f%%\n" (accuracy (cloudyTree trees) trainingData Cloud)
    printf "Windy prediction accuracy: %.2f%%\n" (accuracy (windyTree trees) trainingData Windy)

    -- Make predictions on test data
    putStrLn "\n=== Sample Predictions on Test Data ==="
    mapM_ (\(i, weather) -> do
        let prediction = classifyWeatherComprehensive trees weather
        putStrLn $ "Sample " ++ show i ++ ": " ++ prediction
        putStrLn $ "  Actual: Hot=" ++ show (isHot weather) ++ 
                   ", Humid=" ++ show (isHumid weather) ++
                   ", Cloudy=" ++ show (isCloud weather) ++
                   ", Windy=" ++ show (isWindy weather)
        ) (zip [1..5] testData)  -- Show first 5 test samples

    -- Interactive prediction
    putStrLn "\n=== Interactive Weather Classification ==="
    putStrLn "Enter weather values to classify:"
    putStrLn "(Format: temp,humidity,cloudcover,windspeed)"
    putStrLn "Example: 28.5,65.2,60.0,18.5"
    putStr "> "
    input <- getLine

    case split (== ',') input of
        [tempStr, humStr, cloudStr, windStr] -> do
            let testWeather = ProcessedWeather
                    { targetRain = False  -- Not used for classification
                    , isHot = read tempStr > 25.0
                    , isHumid = read humStr > 60.0
                    , isCloud = read cloudStr > 50.0
                    , isWindy = read windStr > 15.0
                    }
            let result = classifyWeatherComprehensive trees testWeather
            putStrLn $ "\nClassification: " ++ result
        _ -> putStrLn "Invalid input format"