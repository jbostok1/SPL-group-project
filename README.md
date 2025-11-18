
# Linear Regression in Haskell

A simple command-line linear regression calculator implemented in Haskell that computes the best-fit line for a given dataset and makes predictions with built-in validation.

## Overview

This program performs simple linear regression analysis using the least squares method. It calculates the line of best fit in the form `y = a + bx` where:
- `a` is the y-intercept
- `b` is the slope

The program is configured to analyze weather data, specifically the relationship between temperature and relative humidity.

## Features

- **CSV Input**: Reads temperature and humidity data from `charlotte_weather.csv`.
- **Linear Regression Calculation**: Computes intercept and slope using the least squares method.
- **Rounded Output**: Displays intercept and slope values rounded to one decimal place for clarity.
- **Prediction with Guard Rails**: Make predictions for new temperature values with automatic validation:
  - Humidity values are clamped to the valid range of 0-100%
  - Warnings when predicting outside the training data temperature range
  - Warnings when raw predictions exceed valid humidity bounds
- **Input Validation**: Ensures CSV file format is correct and datasets have the same length.

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

**Guard Rails**:
- Humidity predictions are clamped: `humidity = max(0, min(100, raw_prediction))`
- Temperature range validation against training data bounds

## How to Use

### Prerequisites

Before running the program, ensure you have GHC (Glasgow Haskell Compiler) installed. On Replit, this should already be available in your environment.

### Step 1: Prepare Data File

The program expects a CSV file named `charlotte_weather.csv` with the following structure:

```csv
time,temperature_2m,relative_humidity_2m
2024-01-01T00:00,15.5,65.0
2024-01-01T01:00,14.2,68.5
2024-01-01T02:00,13.8,70.2
```

- **Header row**: Must contain column names (time, temperature_2m, relative_humidity_2m)
- **Data columns**: 
  - Column 0: timestamp (ignored by the program)
  - Column 1: temperature in degrees Celsius
  - Column 2: relative humidity as a percentage
- **Empty lines**: Will be automatically ignored

### Step 2: Run the Program

Execute the program using the `runhaskell` command:

```bash
runhaskell Linear_Regression.hs
```

The program will automatically load data from `charlotte_weather.csv`.

### Step 3: View Results

The program will display:
- Number of data points loaded
- **Intercept (a)**: The y-intercept of the regression line (rounded to 1 decimal)
- **Slope (b)**: The slope of the regression line (rounded to 1 decimal)
- **Regression equation**: In the form `Humidity = a + b * Temperature`
- **Temperature range**: The min and max temperatures in the training data

### Step 4: Make Predictions

After seeing the results, you can make predictions:

1. The program displays the valid temperature range from the training data
2. Enter a temperature value when prompted
3. The program will:
   - Calculate the predicted humidity
   - Clamp the result to 0-100% if needed
   - Warn you if the temperature is outside the training range (extrapolation)
   - Warn you if the raw prediction was clamped
4. Display the final predicted humidity value (rounded to 1 decimal)

### Complete Example Session

Assuming `charlotte_weather.csv` contains weather data with temperatures ranging from 5°C to 30°C:

```
Loaded 168 data points from charlotte_weather.csv
Performing Linear Regression: Temperature (X) vs Relative Humidity (Y)

Regression Equation: Humidity = 85.3 + -0.8 * Temperature
Intercept: 85.3
Slope: -0.8

Temperature range in training data: 5.0°C to 30.0°C
Note: Predictions outside this range may be unreliable.

Enter a temperature value to predict relative humidity:
1.0
⚠ WARNING: Temperature 1.0°C is outside the training data range!
Prediction may be unreliable (extrapolation).
⚠ WARNING: Raw prediction was 127.1% (clamped to 100%)
Predicted Humidity for temperature = 1.0°C is 100.0%
```

### Tips

- Predictions are most reliable within the temperature range of your training data
- The guard rails prevent invalid humidity predictions (below 0% or above 100%)
- Pay attention to warnings about extrapolation when predicting outside the training range
- All numerical outputs (intercept, slope, predictions) are rounded to one decimal place for readability

## Functions

### `mean :: [Double] -> Double`
Calculates the arithmetic mean of a list of numbers.

### `linearRegression :: [Double] -> [Double] -> (Double, Double)`
Computes the linear regression coefficients (intercept, slope) from X and Y datasets.

### `predict :: (Double, Double) -> Double -> Double`
Makes a prediction for a new X value using the computed regression model.

### `parseCSVLine :: String -> [String]`
Splits a CSV line into individual fields.

### `parseWeatherCSV :: String -> ([Double], [Double])`
Parses the weather CSV format, extracting temperature (column 1) and humidity (column 2) data.

### `doesFileExist :: FilePath -> IO Bool`
Helper function to check if a file exists before attempting to read it.

## Requirements

- GHC (Glasgow Haskell Compiler) or any Haskell interpreter
- `split` package for CSV parsing (automatically available in Replit environment)

## Error Handling

The program validates that:
- The CSV file exists before attempting to read it
- The CSV file format is correct (proper header and data structure)
- Temperature and humidity datasets have the same number of values
- At least one data point is provided
- Prediction input is a valid number
- Humidity predictions are within valid bounds (0-100%)

## Guard Rails and Validation

The program includes several safety features:

1. **Humidity Clamping**: Predictions are automatically clamped to 0-100%
2. **Extrapolation Warnings**: Alerts when predicting outside training data range
3. **Clamping Notifications**: Shows the raw prediction value when clamping occurs
4. **Temperature Range Display**: Shows the valid range from training data

## Mathematical Background

Linear regression finds the line that minimizes the sum of squared residuals (differences between observed and predicted values). This implementation uses the ordinary least squares (OLS) method, which is the most common approach for simple linear regression.

## License

This is an educational implementation for learning purposes.
