
# Linear Regression in Haskell

A simple command-line linear regression calculator implemented in Haskell that computes the best-fit line for a given dataset and makes predictions with built-in validation.

After all data is given the code will run and return the result of either a 1.0 or a 0.0.  a 1.0 indicates the model predicts that the data of the day you gave would predict the temperature would be over the given temperature, a 0.0 would predict it will be bellow the given temperature

*****************************************************************************************************************************************************************************

# 1. Basic Concept of Decision tree
A decision tree is a tree-like model of decisions:

Internal nodes represent tests on attributes (e.g., "Is it hot?")
Branches represent the outcome of tests (Yes/No)
Leaf nodes represent class labels (e.g., "Hot Weather")

# 2. Building the Tree
The implementation uses two approaches:
Simple Approach (SimpleDecisionTree.hs)

Manually constructed tree based on logical weather patterns
Easy to understand and modify
Good for learning the concept

ID3 Algorithm Approach (DecisionTree.hs)

Uses entropy to measure impurity in data
Calculates information gain to choose best splitting attributes
Automatically builds optimal trees

# 3. Entropy Calculation 
Entropy = -Σ(p_i * log2(p_i))

# How to run 
ghc DecisionTree.hs -o DecisionTree
./DecisionTree

*****************************************************************************************************************************************************************************

# Linear Regression in Haskell

A simple command-line linear regression calculator implemented in Haskell that computes the best-fit line for a given dataset and makes predictions.

## Overview

This program performs simple linear regression analysis using the least squares method. It calculates the line of best fit in the form `y = a + bx` where:
- `a` is the y-intercept
- `b` is the slope

## Features

- **Interactive Input**: Enter X and Y values via command line
- **Linear Regression Calculation**: Computes intercept and slope using the least squares method
- **Prediction**: Make predictions for new X values based on the computed regression line
- **Input Validation**: Ensures X and Y datasets have the same length

## How It Works

The program uses the following formulas:

**Slope (b)**:
```
b = Σ[(x - mean_x)(y - mean_y)] / Σ[(x - mean_x)²]
```

**Intercept (a)**:
```
a = mean_y - b * mean_x
```

**Prediction**:
```
y_predicted = a + b * x_new
```

## Usage

### Running the Program

```bash
runhaskell Linear_Regression.hs
```

### Example Session

```
Enter X values (space-separated):
1 2 3 4 5

Enter Y values (space-separated):
2 4 5 4 5

Intercept (a): 2.2
Slope (b): 0.6

Enter a new X value to predict Y:
6
Predicted Y for x = 6.0 is 5.8
```

## Functions

### `mean :: [Double] -> Double`
Calculates the arithmetic mean of a list of numbers.

### `linearRegression :: [Double] -> [Double] -> (Double, Double)`
Computes the linear regression coefficients (intercept, slope) from X and Y datasets.

### `predict :: (Double, Double) -> Double -> Double`
Makes a prediction for a new X value using the computed regression model.

### `readDoubles :: String -> [Double]`
Parses a space-separated string of numbers into a list of doubles.

## Requirements

- GHC (Glasgow Haskell Compiler) or any Haskell interpreter
- No external dependencies required

## Error Handling

The program validates that:
- X and Y datasets have the same number of values
- At least one data point is provided
- Prediction input is a valid number

## Mathematical Background

Linear regression finds the line that minimizes the sum of squared residuals (differences between observed and predicted values). This implementation uses the ordinary least squares (OLS) method, which is the most common approach for simple linear regression.

## License

This is an educational implementation for learning purposes.
