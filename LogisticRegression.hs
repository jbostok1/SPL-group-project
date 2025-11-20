import Data.List (foldl', zip)
import Data.Maybe (fromMaybe)
import System.IO (readFile)
import Text.Read (readMaybe)

type Features = [Double]

type Label = Double

type Weights = [Double]

type Bias = Double

type LearningRate = Double

type Iterations = Int

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp (-x))

-- create value to pass into sigmoid function
hypothesis :: Features -> Weights -> Bias -> Double
hypothesis features weights bias =
  let input = sum (zipWith (*) features weights) + bias
   in sigmoid input

-- calculate the gradient of an individaual feature
gradientExample :: Features -> Label -> Weights -> Bias -> (Weights, Bias)
gradientExample features label weights bias =
  let h = hypothesis features weights bias
      error = h - label

      gradWeights = map (* error) features

      gradBias = error
   in (gradWeights, gradBias)

-- update the weights and bias by calculating the gradient for each feature and label.
updateParameters :: [(Features, Label)] -> Weights -> Bias -> LearningRate -> (Weights, Bias)
updateParameters dataset initialWeights initialBias learningRate =
  let numExamples = fromIntegral (length dataset)
      (sumGradientWeights, sumGradientBias) =
        foldl'
          ( \(accumulatedWeight, accumulatedBias) (features, label) ->
              let (gradientWeight, gradientBias) = gradientExample features label initialWeights initialBias
               in (zipWith (+) accumulatedWeight gradientWeight, accumulatedBias + gradientBias)
          )
          (replicate (length (fst (head dataset))) 0.0, 0.0)
          dataset

      avgGradientWeights = map (/ numExamples) sumGradientWeights
      avgGradientBias = sumGradientBias / numExamples
      -- apply the learning rate to the average weights and bias to ensure that too large of steps are not being taken causing our goal to be missed.
      newWeights = zipWith (-) initialWeights (map (* learningRate) avgGradientWeights)
      newBias = initialBias - learningRate * avgGradientBias
   in (newWeights, newBias)

-- train the model on given data to create correct weights and bias valeus.
train :: [(Features, Label)] -> LearningRate -> Iterations -> (Weights, Bias)
train dataset learningRate numOfRuns =
  let numFeatures = length (fst (head dataset))
      initialWeights = replicate numFeatures 0.0
      initialBias = 0.0

      gradientDescentStep (currentWeights, currentBias) _ =
        updateParameters dataset currentWeights currentBias learningRate
      -- itterate the foldl value to accumulate towards the desired values
      (finalWeights, finalBias) = foldl' gradientDescentStep (initialWeights, initialBias) [1 .. numOfRuns]
   in (finalWeights, finalBias)

-- run a test using the trained weights and values to see the prediction on a new set of features.
predict :: Features -> Weights -> Bias -> Label
predict features weights bias =
  let probability = hypothesis features weights bias
   in if probability >= 0.5 then 1.0 else 0.0

-- read a string to a double, make the value 0.0 if it would error out
safeReadDouble :: String -> Double
safeReadDouble s = fromMaybe 0.0 (readMaybe s)

-- split up a string based on a delemiter
splitOn :: Char -> String -> [String]
splitOn delimiter str =
  case break (== delimiter) str of
    (a, delimiter' : b) | delimiter' == delimiter -> a : splitOn delimiter b
    (a, "") -> [a]
    (a, _ : b) -> [a]

-- helper function to map the array of strings back to double values
stringsToDoublesSafe :: [String] -> [Double]
stringsToDoublesSafe = map safeReadDouble

-- read in the CSV data
parseCSVData :: String -> Double -> [(Features, Label)]
parseCSVData fileContent temp =
  let linesOfStrings = lines fileContent
      -- drop the first column of data as it is just time data
      dataLines = drop 1 linesOfStrings
      -- split each row into a list of strings base on ,
      rowsOfStrings = map (splitOn ',') dataLines
      -- only read in the now second row onwards, as we are using the first row to act as the label that we will be testing on
      selectTargetColumns :: [String] -> [Double]
      selectTargetColumns row =
        let col2 = safeReadDouble (row !! 1)
            col3 = safeReadDouble (row !! 2)
            col4 = safeReadDouble (row !! 3)
            col5 = safeReadDouble (row !! 4)
            col6 = safeReadDouble (row !! 5)
            col7 = safeReadDouble (row !! 6)
            col8 = safeReadDouble (row !! 7)
            col9 = safeReadDouble (row !! 8)
            col10 = safeReadDouble (row !! 9)
            col11 = safeReadDouble (row !! 10)
            col12 = safeReadDouble (row !! 11)
         in [col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12]
      -- get arrays to not contain first row to now act as the features value
      selectedNumericRows = map selectTargetColumns rowsOfStrings
      -- create the set of feature and label values
      transformRow :: [Double] -> (Features, Label)
      transformRow selectedRow =
        -- if the first row value is greater than 15 then it is considered true otherwise it is false.
        let label = if head selectedRow > temp then 1.0 else 0.0
            features = tail selectedRow
         in (features, label)

      dataset = map transformRow selectedNumericRows
   in dataset

stringToDouble :: String -> Double
stringToDouble s = read s :: Double

main :: IO ()
main = do
  fileContents <- readFile "charlotte_weather.csv"
  putStrLn "What Temperature would you like to test against?"
  temp <- readLn :: IO Double

  putStrLn "What is the relative humidity of the day you are testing?"
  relHumidity <- readLn :: IO Double

  putStrLn "What is the apparent temperature of the day you are testing?"
  apparentTemp <- readLn :: IO Double

  putStrLn "What is the precipitation of the day you are testing?"
  precipitation <- readLn :: IO Double

  putStrLn "What is the rain of the day you are testing?"
  rain <- readLn :: IO Double

  putStrLn "What is the snowfall of the day you are testing?"
  snowfall <- readLn :: IO Double

  putStrLn "What is the cloud cover of the day you are testing?"
  cloudCover <- readLn :: IO Double

  putStrLn "What is the atmospheric pressure of the day you are testing?"
  pressure <- readLn :: IO Double

  putStrLn "What is the wind speed of the day you are testing?"
  windSpeed <- readLn :: IO Double

  putStrLn "What is the wind gust of the day you are testing?"
  windGust <- readLn :: IO Double

  putStrLn "What is the wind direction of the day you are testing?"
  windDirection <- readLn :: IO Double

  let dataset = parseCSVData fileContents temp
  -- test output to make sure the CSV has run correctly
  putStrLn $ "First data point: " ++ show (head dataset)

  let learningRate = 0.0005
  let numOfRuns = 10000

  let (finalWeights, finalBias) = train dataset learningRate numOfRuns
  putStrLn $ "Trained Weights: " ++ show finalWeights
  putStrLn $ "Trained Bias: " ++ show finalBias

  let prediction = predict [relHumidity, apparentTemp, precipitation, rain, snowfall, cloudCover, pressure, windSpeed, windGust, windDirection] finalWeights finalBias
  putStrLn $ "Prediction for given day" ++ ": " ++ show prediction