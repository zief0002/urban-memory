##################################################
### Load libraries
##################################################

library(tidyverse)
library(broom)
library(corrr)
library(lavaan)



##################################################
### Import simulated data
##################################################

keith = read_csv("https://raw.githubusercontent.com/zief0002/epsy-8264/master/data/path-model-achievement.csv")



##################################################
### Fit regression models to obtain path coefficients and error path coefficients
##################################################

# Path coefficients
lm.1 = lm(ability ~ 1 + fam_back, data = keith)
tidy(lm.1)

lm.2 = lm(motivation ~ 1 + fam_back + ability, data = keith)
tidy(lm.2)

lm.3 = lm(coursework ~ 1 + fam_back + ability + motivation, data = keith)
tidy(lm.3)

lm.4 = lm(achieve ~ 1 + fam_back + ability + motivation + coursework, data = keith)
tidy(lm.4)


# Paths to Disturbances/Errors
glance(lm.1)
sqrt(1 - 0.174)

glance(lm.2)
sqrt(1 - 0.0552)

glance(lm.3)
sqrt(1 - 0.348)

glance(lm.4)
sqrt(1 - 0.629)




tidy(lm(achieve ~ 1 + ability, data = keith))
tidy(lm(achieve ~ 1 + fam_back + ability, data = keith))








##################################################
### Compute indirect and total effects of ability on achievment
##################################################

# Indirect effects
0.374 * 0.310 + #Ability --> Coursework --> Achievement
0.152 * 0.013 + #Ability --> Motivation --> Achievement
0.152 * 0.267 * 0.310 #Ability --> Motivation --> Coursework --> Achievement

# Total effects
0.551 + 0.374*0.310 + 0.152*0.013 + 0.152*0.267*0.310



##################################################
### Estimate direct effects with lavaan
##################################################

# Define path model
path.model = "
  ability ~ p1*fam_back
  motivation ~ p4*fam_back + p7*ability
  coursework ~ p2*fam_back + p5*ability + p9*motivation
  achieve ~ p3*fam_back + p6*ability + p8*motivation + p10*coursework
"

# Fit model
pm.1 = sem(path.model, data = keith)

# Results
summary(pm.1, ci = TRUE, rsquare = TRUE)





##################################################
### Include indirect effect of ability on achievement (via motivation)
##################################################

# Define path model
path.model.2 = "
  ability ~ p1*fam_back
  motivation ~ p4*fam_back + p7*ability
  coursework ~ p2*fam_back + p5*ability + p9*motivation
  achieve ~ p3*fam_back + p6*ability + p8*motivation + p10*coursework
  indirect_ability_via_motivation := p7*p8
"

# Fit model
pm.2 = sem(path.model.2, data = keith)

# Results
summary(pm.2, rsquare = TRUE)



##################################################
### Estimate direct and indirect effects with lavaan
##################################################

path.model.3 = "
  ability ~ p1*fam_back
  motivation ~ p4*fam_back + p7*ability
  coursework ~ p2*fam_back + p5*ability + p9*motivation
  achieve ~ p3*fam_back + p6*ability + p8*motivation + p10*coursework
  indirect_fam_back := p1*p5*p10 + p1*p6 + p1*p7*p8 + p1*p7*p9*p10 + p4*p8 + p4*p9*p10 + p2*p10
  indirect_ability := p7*p8 + p7*p9*p10 + p5*p10
  indirect_motivation := p9*p10
"

# Fit model
pm.3 = sem(path.model.3, data = keith)

# Results
summary(pm.3, ci = TRUE, rsquare = TRUE)



##################################################
### Estimate direct, indirect, and total effects with lavaan
##################################################

path.model.4 = "
  ability ~ p1*fam_back
  motivation ~ p4*fam_back + p7*ability
  coursework ~ p2*fam_back + p5*ability + p9*motivation
  achieve ~ p3*fam_back + p6*ability + p8*motivation + p10*coursework
  indirect_fam_back := p1*p5*p10 + p1*p6 + p1*p7*p8 + p1*p7*p9*p10 + p4*p8 + p4*p9*p10 + p2*p10
  indirect_ability := p7*p8 + p7*p9*p10 + p5*p10
  indirect_motivation := p9*p10
  total_fam_back := p3 + p1*p5*p10 + p1*p6 + p1*p7*p8 + p1*p7*p9*p10 + p4*p8 + p4*p9*p10 + p2*p10
  total_ability := p6 + p7*p8 + p7*p9*p10 + p5*p10
  total_motivation := p8 + p9*p10
  total_coursework := p10
"

# Fit model
pm.4 = sem(path.model.3, data = keith)

# Results
summary(pm.4, ci = TRUE, rsquare = TRUE)





