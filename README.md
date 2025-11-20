**To Run Each Code File**
1) Ensure ghc is installed to run Haskell.
2) Ensure the charlotte_weather.csv for the sample data is in the same directory as the code files.

**For Decision Tree:**

ghc DecisionTree.hs -o DecisionTree

./DecisionTree

**For Linear Regression:**

ghc LinearRegression.hs -o LinearRegression

./LinearRegression

-> then enter a temperature value to test against

Further details and explanations regarding each code file are listed below...

**For Logisitic Regression:**

ghc LogisticRegression.hs -o LogisticRegression

./LogisticRegression

-> You will be prompted to give data, first will be the temperature you will be comparing agsint.  All other data will be the data of the day you are comparing against. After all data is given the code will run and return the result of either a 1.0 or a 0.0.  a 1.0 indicates the model predicts that the data of the day you gave would predict the temperature would be over the given temperature, a 0.0 would predict it will be bellow the given temperature

*****************************************************************************************************************************************************************************

# 1. Basic Concept of Decision tree
A decision tree is a tree-like model of decisions:

Internal nodes represent tests on attributes (e.g., "Is it hot?")
Branches represent the outcome of tests (Yes/No)
Leaf nodes represent class labels (e.g., "Hot Weather")

# 2. Building the Tree

ID3 Algorithm Approach (DecisionTree.hs)

Uses entropy to measure impurity in data
Calculates information gain to choose best splitting attributes
Automatically builds optimal trees

# 3. Entropy Calculation 
Entropy = -Σ(p_i * log2(p_i))

# How to run Decision Tree
-- ensure that the charlotte_weather.csv is in the same directory as the code file --

ghc DecisionTree.hs -o DecisionTree


./DecisionTree

*****************************************************************************************************************************************************************************
# 1. Basic Concept of Linear Regression

Linear regression finds a straight-line relationship between two variables.
In this program:

Temperature (X) → input

Humidity (Y) → predicted value

The program fits a line: Y = a + bX

# 2. Building the Model
Linear Regression Program (LinearRegression.hs)

This code:

Reads charlotte_weather.csv

Extracts temperature and humidity values

Calculates:

Slope (b)

Intercept (a)

Prints the regression equation

Lets the user enter a temperature to get a predicted humidity

Warns if:

CSV is missing

Temperature is outside the training range

Prediction goes below 0% or above 100% (it clamps it)

# 3. Regression Formula

Y = a + bX

a = intercept

b = slope

# How to run Linear Regression
-- ensure that the charlotte_weather.csv is in the same directory as the code file --

ghc LinearRegression.hs -o LinearRegression

./LinearRegression

-> then enter a temperature value to test against

*****************************************************************************************************************************************************************************

# 1. Basic Concept of Logistic Regression

Logistic regression is a classification method that predicts whether an outcome belongs to one of two classes (0 or 1).

It works by:

Taking numerical feature inputs

Computing a weighted sum of those features

Passing this value through the sigmoid function

Outputting a probability → values ≥ 0.5 are classified as 1, otherwise 0

# 2. Building the Model

Logistic Regression Approach (LogisticRegression.hs)

Uses the sigmoid function to convert values into probabilities
Computes gradients for weights and bias
Uses gradient descent to update parameters over many iterations
Trains weights and bias based on weather data from the CSV file
Predicts whether a given day is “hot” (label 1) or “not hot” (label 0) based on multiple weather features

# How to run Logistic Regression
-- ensure that the charlotte_weather.csv is in the same directory as the code file --

ghc LogisticRegression.hs -o LogisticRegression

./LogisticRegression

-> You will be prompted to give data, first will be the temperature you will be comparing agsint.  All other data will be the data of the day you are comparing against. After all data is given the code will run and return the result of either a 1.0 or a 0.0.  a 1.0 indicates the model predicts that the data of the day you gave would predict the temperature would be over the given temperature, a 0.0 would predict it will be bellow the given temperature
