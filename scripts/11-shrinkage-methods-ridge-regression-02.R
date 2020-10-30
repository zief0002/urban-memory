##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(glmnet)
library(tidyverse)



##################################################
### Import and prepare data
##################################################

eeo = read_csv("data/equal-education-opportunity.csv")
head(eeo)


##################################################
### Standardize all variables in the eeo data frame
##################################################

z_eeo = scale(eeo)

# View
head(z_eeo)



##################################################
### Fit ridge regression
##################################################

# Obtain matrix of predictors and outcome
X = z_eeo[ , c("faculty", "peer", "school")]
Y = z_eeo[ , c("achievement")]


# Check that X and Y are matrices
class(X)
class(Y)


ridge_1 = glmnet(
  x = X, # Matrix
  y = Y, # Matrix
  alpha = 0, # Parameter defining penalty; 0 = ridge regression
  lambda = 21.765/70, # modified d value (since function minimizes the MSE rather than SSE)
  intercept = FALSE, # Should intercept be set; default is TRUE
  standardize = FALSE #Should predictors be standardized; default is TRUE
  )

tidy(ridge_1)



# Write a for loop that iterates over the numbers 1 to 10 and prints the square of each number. 
# Use print() to output a value.

for(i in 1:10){
  print(i^2)
}


# OR

x = 1:10

for(i in seq_along(x)){
  print(i^2)
}




# Using a for loop simulate the flip a coin twenty times, keeping track of the individual 
# outcomes (1 = heads, 0 = tails) in a vector called `coin_outcome`.


# Initialize empty vector
coin_outcome = c()

for(i in 1:20){
  coin_outcome[i] = sample(x = c(0, 1), size = 1)
}

# View vector
coin_outcome


# OR


# Initialize empty vector
coin_outcome = c()

for(i in 1:20){
  coin_outcome[i] = rbinom(n = 1, size = 1, prob = 0.5)
}

coin_outcome


