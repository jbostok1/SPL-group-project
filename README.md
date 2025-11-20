
# Linear Regression in Haskell

A simple command-line linear regression calculator implemented in Haskell that computes the best-fit line for a given dataset and makes predictions with built-in validation.

After all data is given the code will run and return the result of either a 1.0 or a 0.0.  a 1.0 indicates the model predicts that the data of the day you gave would predict the temperature would be over the given temperature, a 0.0 would predict it will be bellow the given temperature

# Project Structure - Decision tree

DecisionTree.hs - Complete implementation with ID3 algorithm and multiple trees
SimpleDecisionTree.hs - Simplified educational version with clear explanations
Solution.hs - Your original data processing code
ReadCSV.hs - CSV reading utilities
weather.py - Python script to fetch weather data
charlotte_weather.csv - Raw weather data
processed_weather.csv - Processed boolean features
How Decision Trees Work

# 1. Basic Concept
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
