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

--  create value to pass into sigmoid function
hypothesis :: Features -> Weights -> Bias -> Double
hypothesis features weights bias =
  let input = sum (zipWith (*) features weights) + bias
   in sigmoid input

-- calculate the gradient of an individual feature
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
      -- iterate the foldl value to accumulate towards the desired values
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

-- split up a string based on a delimiter
splitOn :: Char -> String -> [String]
splitOn delimiter str =
  case break (== delimiter) str of
    (a, delimiter' : b) | delimiter' == delimiter -> a : splitOn delimiter b
    (a, "") -> [a]
    (a, _ : b) -> [a]

-- helper function to map the array of strings back to double values
stringsToDoublesSafe :: [String] -> [Double]
stringsToDoublesSafe = map safeReadDouble