
library(tidyverse)
library(broom)
library(corrr)
library(faux)

##################################################
### Model 2
##################################################



library(lavaan)

technology.model = "
  ease ~ p1*knowledge
  students ~ p2*ease
  teachers ~ p3*ease + p4*students
  attitude ~ p5*students + p6*teachers + ease
  intend ~ knowledge + p8*attitude + teachers + p7*ease
  indirect_knowledge := p1*p3*p6*p8 + p1*p7 + p1*p2*p5*p8 + p1*p2*p4*p6*p8
  indirect_teachers := p6*p8
  indirect_ease := p3*p6*p8 + p2*p5*p8 + p2*p4*p6*p8
  indirect_students := p5*p8 + p4*p6*p8
  total_ease := p7 + p3*p6*p8 + p2*p5*p8 + p2*p4*p6*p8
"

pm.1 = sem(technology.model, data = x)

summary(pm.1, ci = TRUE, rsquare = TRUE)




##################################################
### Input summary measures
##################################################

# Create correlation matrix
corrs = matrix(
  data = c(
     1.00, 0.62,  0.25, 0.16, -0.10,  0.39, 0.29, 0.18,
     0.62, 1.00,  0.09, 0.28,  0.001, 0.24, 0.25, 0.15,
     0.25, 0.09,  1.00, 0.07,  0.03,  0.22, 0.34, 0.19,
     0.16, 0.28,  0.07, 1.00,  0.10,  0.32, 0.37, 0.41,
    -0.10, 0.001, 0.03, 0.10,  1.00,  0.26, 0.13, 0.43,
     0.39, 0.24,  0.22, 0.32,  0.26,  1.00, 0.72, 0.75,
     0.29, 0.25,  0.34, 0.37,  0.13,  0.72, 1.00, 0.55,
     0.18, 0.15,  0.19, 0.41,  0.43,  0.75, 0.55, 1.00
  ),
  nrow = 8
)


means = rep(0, 8) # Create mean vector
n = 162                   # Set sample size



set.seed(1234)


# Simulate the data and convert to data frame
sim_dat <- data.frame(MASS::mvrnorm(n = 162, mu = means, Sigma = corrs, empirical = TRUE)) %>%
  rename(
    ability = X1,
    gpq = X2, 
    preprod = X3,
    qfj = X4,
    sex = X5,
    prod = X6,
    cites = X7,
    pubs = X8
  ) %>%
  mutate(
    male = norm2binom(sex, size = 1, prob = 0.4691358)
  )


