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


hypothesis :: Features -> Weights -> Bias -> Double
hypothesis features weights bias =
  let input = sum (zipWith (*) features weights) + bias
   in sigmoid input


gradientExample :: Features -> Label -> Weights -> Bias -> (Weights, Bias)
gradientExample features label weights bias =
  let h = hypothesis features weights bias
      error = h - label

      gradWeights = map (* error) features

      gradBias = error
   in (gradWeights, gradBias)


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
      
      newWeights = zipWith (-) initialWeights (map (* learningRate) avgGradientWeights)
      newBias = initialBias - learningRate * avgGradientBias
   in (newWeights, newBias)