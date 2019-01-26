# Predictive Modelling Assignment 1 using R

### Key Aim of Project

The Key Aim of the project was to try and create a simple Multi-Variate Regression model which could be used to predict a Worker's Weekly Earnings.

To this end, the following model was created:

Weekly Earnings = 4.17 	+ (0.38 * Weekly Hrs Worked) 	+  (1.07 * Education level) + (8.348 * Standard error)

### Key Findings
Weekly Wages are unsurprisingly dictated by both the Weekly Hours work and the Education Level of the employee. 
Gender and Age also played a minor role, but were considered mostly insignificant.

### Tasks Undertaken


Data Cleaning: 
- Creating Dummy Variables
- Removing Zero Values
- Square Root Transformations

Analysis:
- Creating Correlation Table to see what variables were significant
- Splitting data into Train and Test Samples
- Training Model and Test Validation performance
- Dropping insignificant variables to improve Model Performance
