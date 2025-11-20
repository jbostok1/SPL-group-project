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

-> You will be prompted to give data, first will be the temperature you will be comparing agsint.  All other data will be the data of the day you are comparing against. After all data is given the code will run and return the result of either a 1.0 or a 0.0.  a 1.0 indicates the model predicts that the data of the day you gave would predict the temperature would be over the given temperature, a 0.0 would predict it will be below the given temperature

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

Temperature = X → input

Humidity = Y → predicted value

The program finds a line: Y = a + bX

# 2. Building the Model

This code:

Reads charlotte_weather.csv

Extracts temperature and humidity values

Calculates a slope and intercept (X and Y)

Lets the user enter a temperature to get a predicted humidity

Warns if any of the following:

- CSV is missing

- Temperature is outside the training range

- Prediction goes below 0% or above 100%

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

Logistic regression is used to model the probability of a 'discrete' outcome. 

Outputting a probability → values ≥ 0.5 are classified as 1, otherwise 0

When values are run through the logistic regression model, it can allow the user to know whether or not a value is likely to fall above or below a given value.

# 2. Building the Model

The model takes values from the user and also values from the charlotte_weather.csv. The data from the csv is used as the sample data to compute whether the temperature is more likely to be above or below the given value.

The hypothesis is: h(x) = 1 / (1 + e^(−(w·x + b)))

The sigmoid function is: σ(z) = 1 / (1 + e^(−z))

and the prediction rule is: If h(x) ≥ 0.5 → predict 1, If h(x) < 0.5 → predict 0

# How to run Logistic Regression
-- ensure that the charlotte_weather.csv is in the same directory as the code file --

ghc LogisticRegression.hs -o LogisticRegression

./LogisticRegression

-> You will be prompted to give data, first will be the temperature you will be comparing agsint.  All other data will be the data of the day you are comparing against. After all data is given the code will run and return the result of either a 1.0 or a 0.0.  a 1.0 indicates the model predicts that the data of the day you gave would predict the temperature would be over the given temperature, a 0.0 would predict it will be below the given temperature
