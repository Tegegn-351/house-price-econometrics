This project analyzes how various housing and regional characteristics influence self-reported house values using Italian household survey data. Employing advanced econometric techniques in R, the study explores the effects of variables such as house age, size, number of bathrooms, and purchase price on property value.
The analysis began with OLS and log-linear models and progressed to more complex estimations, including:
Quadratic and interaction terms to capture nonlinear relationships.
White and RESET tests for model diagnostics and specification checks.
Chow tests to detect structural breaks across regions (North vs. South).
Instrumental Variable (IV) regression using CPI-based instruments to correct for endogeneity in purchase price.
Probit model to estimate the probability of negative returns on housing investment.
