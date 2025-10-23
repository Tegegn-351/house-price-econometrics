# Homework data
setwd("C:\\Users\\hp\\OneDrive - Università degli Studi di Verona\\Desktop\\A-Italy-Works\\A-Verona Courses\\2nd semester_Verona\\Econometrics\\homework I Data")
Dataset = read.csv("shiw_5.csv")
summary(Dataset)
#View(Dataset)

#EXERCISE 1
# A) Run with OLS the regression ( valabit = B1 + B2age + B3bath2 + B4m2)

# Create the necessary variables to run this regressions ( Age, and bath2)
Dataset$age = 2014 - Dataset$ancostr
Dataset$bath2 = ifelse(Dataset$bagni == 2, 1, 0)

Reg1 = lm(valabit ~ age + bath2 + m2, data = Dataset)
summary(Reg1)


#b) Regression with log of valabit (Log(valabit) = B1 + B2age + B3bath2 + B4m2)
Reg2 = lm(log(valabit) ~ age + bath2 + m2, data = Dataset)
summary(Reg2)


#c) Add log of impacq(Log(valabit) = B1 + B2age + B3bath2 + B4m2+ B5log(impacq))

Reg3 = lm(log(valabit) ~ age + bath2 + m2 + log(impacq), data = Dataset)
summary(Reg3)


#d) Test if effects of age and m2 are equal; and if two bathrooms is equal to others

# Load library for linear hypothesis test
library(car)

# H0: Coefficients of age and m2 are equal
linearHypothesis(Reg3, "age = m2")

# H0: Coefficient of bath2 = 0 (no difference from 1 or 0 bathrooms)
linearHypothesis(Reg3, "bath2 = 0")

# E) Add m2 squared and compute effects at 50m² and 200m²
# Log(valabit) = B1 + B2*age + B3*bath2 + B4*m2 + B5*m2^2 + B6*log(impacq)

Dataset$m2_square = Dataset$m2^2 

Reg4 = lm(log(valabit) ~ age + bath2 + m2 + m2_square + log(impacq), data = Dataset)
summary(Reg4)

# Compute marginal effect of House Surface at 50m2 and 200m2
B4 = coef(Reg4)["m2"]
B5 = coef(Reg4)["m2_square"]

effect_When_Surface_50m2 = B4 + 2 * B5 * 50
effect_When_Surface_200m2 = B4 + 2 * B5 * 200

cat("Marginal effect of the house surface at 50 m²:", effect_When_Surface_50m2, "\n")
cat("Marginal effect of the house surface at 200 m²:", effect_When_Surface_200m2, "\n")



# EXERCISE 2

# Load required package
library(lmtest)

# Run Equation (4)
#Dataset$m2_squared <- Dataset$m2^2
#model4 <- lm(log(valabit) ~ age + bath2 + m2 + m2_squared + log(impacq), data = Dataset)

#a) Run a White test (approximate variant). What do you conclude?
#White Test for Heteroskedasticity

# White test, approximate version
resi = residuals(Reg4)    
yhat = predict(Reg4)       
reg_white = lm(I(resi^2) ~ yhat + I(yhat^2), data = Dataset)   
white_stat = nrow(Dataset) * summary(reg_white)$r.squared
white_stat
pchisq(white_stat, df = 2, lower.tail = FALSE)

#2b) RESET Test for Model Specification
library(lmtest)
library(car)

# Ramsey RESET test
resettest(Reg4, power = 2:3, type = "fitted")

# RESET test, by hand
pred = predict(Reg4)
reg_reset = lm(log(valabit) ~ age + bath2 + m2 + m2_squared + log(impacq) + I(pred^2) + I(pred^3), data = Dataset)
linearHypothesis(reg_reset, c("I(pred^2) = 0", "I(pred^3) = 0"))


#c) Run a Chow test comparing two groups of individuals, living in the North, 
#or in the Center/South respectively. What do you conclude?
# Load required packages
library(car)      # for linearHypothesis (lht)
library(lmtest)   # for resettest

# Create a dummy variable for region (1 = North, 0 = Center/South)
Dataset$region_dummy = ifelse(Dataset$area3 == 1, 1, 0)

# Regression with interaction terms for structural break (region)
reg5 = lm(log(valabit) ~ age + bath2 + m2 + m2_square + log(impacq) +
             region_dummy + age:region_dummy + bath2:region_dummy + m2:region_dummy +
             m2_square:region_dummy + log(impacq):region_dummy,
           data = Dataset)
summary(reg5)

# Chow test: test if all interaction terms and region_dummy = 0
Chow_Test = linearHypothesis(reg5, c(
  "region_dummy = 0",
  "age:region_dummy = 0",
  "bath2:region_dummy = 0",
  "m2:region_dummy = 0",
  "m2_square:region_dummy = 0",
  "log(impacq):region_dummy = 0"
), test = "Chisq")
print(Chow_Test)
# RESET test on the full model with interaction terms
reset_test <- resettest(reg5, power=2:3, type="fitted")
print(reset_test)


#d) Add to Equation (4) two dummy variables for North and South, respectively. 
#The equation then becomes:
## Log(valabit) = B1 + B2*age + B3*bath2 + B4*m2 + B5*(m2)^2 + B6*log(impacq) + B7*North + B8*South
#Why is the dummy variable for the Center excluded from the equation?

# Create dummy variables
Dataset$north = ifelse(Dataset$area3 == 1, 1, 0)
Dataset$south = ifelse(Dataset$area3 == 3, 1, 0)


# 2e) Test for Area Effects (North & South Combined)

# Regression with area dummies
Reg5 = lm(log(valabit) ~ age + bath2 + m2 + m2_squared + log(impacq) + north + south, data = Dataset)
summary(Reg5)

# H0: north = south = 0 (no area effect)
linearHypothesis(Reg5, c("north = 0", "south = 0"))



# Exercise 3

#3a) Define return and run the OLS regression

# Create return variable
Dataset$return = (Dataset$valabit / Dataset$impacq) - 1

# OLS regression of return
Reg6 = lm(return ~ age + bath2 + m2 + m2_squared + log(impacq) + north + south, data = Dataset)
summary(Reg6)

#3b) Instrumental Variable (IV) Setup
# Load required package
library(AER)      # for ivreg and diagnostic tests
library(car)      # for linear hypothesis testing (if needed)

# First, run IV regression using ivreg()
Reg7_iv_model = ivreg(return ~ age + bath2 + m2 + m2_squared + north + south + log(impacq) | 
                    age + bath2 + m2 + m2_squared + north + south + cpi1 + cpi2, 
                  data = Dataset)

#3c) Test for Instrument Relevance and Validity

summary(Reg7_iv_model, diagnostics = TRUE)


#3d) Test: OLS vs IV? (Use the Hausman Test)
library(AER)
# Hausman test with ivreg diagnostics summary already run:
summary(Reg7_iv_model, diagnostics = TRUE)$diagnostics

# Run Hausman test in manual way
# Wu-Hausman Test for endogeneity
Dataset$res_first = residuals(reg_first)
reg_hausman = lm(return ~ age + bath2 + m2 + I(m2^2) + log(impacq) + north + south + res_first, data = Dataset)
#summary(reg_hausman)
lht(reg_hausman, c("res_first=0"))

#3e) Using the best model (OLS or IV), report the effect of bath2. Is the coefficient significant?

# Load necessary package
library(car)  # for linear hypothesis testing

# Run the IV regression (2SLS) using ivreg from the AER package
Reg8_iv_model = ivreg(return ~ age + bath2 + m2 + m2_squared + north + south + log(impacq) | 
                        age + bath2 + m2 + m2_squared + north + south + cpi1 + cpi2, 
                      data = Dataset)
summary(Reg8_iv_model)

# Test significance of bath2 coefficient
linearHypothesis(Reg8_iv_model, "bath2 = 0")


###################################EXERCISE 4

# 4a) Create rendneg and run OLS regression

# Create rendneg dummy variable
Dataset$rendneg = ifelse(Dataset$varvalabit == 3, 1, 0)

# Run OLS regression
Reg7 = lm(rendneg ~ age + bath2 + m2 + m2_squared + log(impacq) + north + south, data = Dataset)
summary(Reg7)

#4b) Estimate Equation (7) using Probit

# Load package
library(MASS)

# Run probit model
Reg7_probit = glm(rendneg ~ age + bath2 + m2 + m2_squared + log(impacq) + north + south,
                            family = binomial(link = "probit"), data = Dataset)
summary(Reg7_probit)

#4c) Test the significance of house surface (m2) in probit
# Test joint significance of m2 and m2^2

library(car)

linearHypothesis(Reg7_probit, c("m2 = 0", "m2_squared = 0"))


#4d) Test if North and South effects are identical

# H0: north = south

linearHypothesis(Reg7_probit, "north = south")


#4e) Compute % of correctly predicted observations

# Get predicted probabilities
probs = predict(Reg7_probit, type = "response")
# Predict class (1 if prob > 0.5)
predicted_class = ifelse(probs > 0.5, 1, 0)
# Compare with actual
conf_matrix = table(Predicted = predicted_class, Actual = Dataset$rendneg)
# Compute accuracy
accuracy = sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Proportion correctly predicted:", round(accuracy, 4), "\n")












































