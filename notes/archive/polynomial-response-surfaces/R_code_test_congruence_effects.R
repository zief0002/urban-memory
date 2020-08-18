# load data
df <- read.table("example_data.txt", header=TRUE, sep = "\t")

#------------------------------------------------------------------------
# The data frame contains simulated data for the following variables:
# X = self-view (e.g., about a specific personality trait)
# Y = reputation (e.g., about the very same trait)
# outcome_sqd to outcome_bowl = diverse outcome variables
#------------------------------------------------------------------------

# Install and load packages

# package RSA (Schönbrodt, 2016a), required for estimation of the RSA models
# install.packages("RSA")
library(RSA)

# package car (Fox & Weisberg, 2011), required for computation of variance inflation factor 
# as an indicator for potential multicollinearity problems
# install.packages("car")
library(car)


#-------------------
# DATA PREPARATION
#-------------------

# center predictors at the grand mean
grandmean <- mean(c(df$X, df$Y), na.rm=T)
df$X <- df$X-grandmean
df$Y <- df$Y-grandmean


# add squared and interaction terms of the predictors (required to inspect multicollinearity)
df$X2 <- df$X^2
df$XY <- df$X*df$Y
df$Y2 <- df$Y^2

# inspect potential multicollinearity using the variance inflation factor (VIF), 
# for the example of the criterion variable outcome_sqd
# (see Fox, 2016 for a discussion of VIFs and their cutoffs)
lm_sqd <- lm(outcome_sqd ~ X + Y + X2 + XY + Y2, data=df)
vif(lm_sqd)



#------------------------------------------------------------------------
# TEST THE CONGRUENCE HYPOTHESIS FOR THE OUTCOME VARIABLE outcome_sqd
# (Figure 1a in the manuscript)
#------------------------------------------------------------------------

# ESTIMATE THE POLYNOMIAL MODEL
rsa_sqd <- RSA(outcome_sqd ~ X*Y, data=df, model="full")


# PLOT THE ESTIMATED MODEL
plot(rsa_sqd, legend=F, param=F,
     axes=c("LOC", "LOIC","PA1"), 
     project=c("LOC", "LOIC","PA1"), 
     xlab="Self-view X", ylab="Reputation Y", zlab="Happiness Z",
     xlim=c(-3,3), ylim=c(-3,3), zlim=c(0.4,2.0))


# DO THE RSA RESULTS INDICATE A CONGRUENCE EFFECT?

# We can get the required information to test for a congruence effect either from the summary
summary(rsa_sqd)

# ...or from the list of all estimated parameters
getPar(rsa_sqd)

# Here, we use the list of estimated parameters to test for a congruence effect, 
# following the checklist provided in Figure 3. That is, we have to test whether all four 
# (or six, if we want to test the strict version of a congruence effect) 
# necessary conditions for a congruence effect are satisfied:

# 1. Is p10 significant?
getPar(rsa_sqd)[getPar(rsa_sqd)$label == "p10",]
# -> No. -> The first necessary condition for a congruence effect is satisfied.

# 2. Does the confidence interval of p11 exclude 1?
getPar(rsa_sqd)[getPar(rsa_sqd)$label == "p11",]
# -> No. -> The second necessary condition for a congruence effect is satisfied.

# 3. Is a4 significantly negative?
getPar(rsa_sqd)[getPar(rsa_sqd)$label == "a4",]
# -> Yes. -> The third necessary condition for a congruence effect is satisfied.

# 4. Is a3 significant?
getPar(rsa_sqd)[getPar(rsa_sqd)$label == "a3",]
# -> No. -> The fourth necessary condition for a congruence effect is satisfied.

# If our theory justifies to allow a non-constant LOC, we can conclude that data 
# supports the broad version of the congruence hypothesis. 


# If our theory does NOT allow a non-constant LOC, we must test conditions 5 and 6:

# 5. Is a2 significant?
getPar(rsa_sqd)[getPar(rsa_sqd)$label == "a2",]
# -> No. -> The fifth necessary condition for a strict congruence effect is satisfied.

# 6. Is a1 significant?
getPar(rsa_sqd)[getPar(rsa_sqd)$label == "a1",]
# -> No. -> The sixth necessary condition for a strict congruence effect is satisfied.


# -> Data supports the strict congruence hypothesis.





#------------------------------------------------------------------------
# TEST THE CONGRUENCE HYPOTHESIS FOR THE OUTCOME VARIABLE outcome_rr
# (Figure 1b in the manuscript)
#------------------------------------------------------------------------

# ESTIMATE THE POLYNOMIAL MODEL
rsa_rr <- RSA(outcome_rr ~ X*Y, data=df, model="full")


# PLOT THE ESTIMATED MODEL
plot(rsa_rr, legend=F, param=F,
     axes=c("LOC", "LOIC","PA1"), 
     project=c("LOC", "LOIC","PA1"), 
     xlab="Self-view X", ylab="Reputation Y", zlab="Happiness Z",
     xlim=c(-3,3), ylim=c(-3,3), zlim=c(0.4,2.0))


# DO THE RSA RESULTS INDICATE A CONGRUENCE EFFECT?

# 1. Is p10 significant?
getPar(rsa_rr)[getPar(rsa_rr)$label == "p10",]
# -> No. -> The first necessary condition for a congruence effect is satisfied.

# 2. Does the confidence interval of p11 exclude 1?
getPar(rsa_rr)[getPar(rsa_rr)$label == "p11",]
# -> No. -> The second necessary condition for a congruence effect is satisfied.

# 3. Is a4 significantly negative?
getPar(rsa_rr)[getPar(rsa_rr)$label == "a4",]
# -> Yes. -> The third necessary condition for a congruence effect is satisfied.

# 4. Is a3 significant?
getPar(rsa_rr)[getPar(rsa_rr)$label == "a3",]
# -> No. -> The fourth necessary condition for a congruence effect is satisfied.

# If our theory justifies to allow a non-constant LOC, we can conclude that data 
# supports the broad version of the congruence hypothesis. 
# In this case, inspection of a2 and a1 reveals that there is a congruence effect 
# with additional common linear main effect of X and Y. 


# If our theory does NOT allow a non-constant LOC, we must test conditions 5 and 6:

# 5. Is a2 significant?
getPar(rsa_rr)[getPar(rsa_rr)$label == "a2",]
# -> No. -> The fifth necessary condition for a strict congruence effect is satisfied.

# 4. Is a1 significant?
getPar(rsa_rr)[getPar(rsa_rr)$label == "a1",]
# -> Yes. -> The sixth necessary condition for a strict congruence effect is NOT satisfied.

# -> The RSA results contradict a strict congruence effect, we must reject the strict version
# of the congruence hypothesis.




#------------------------------------------------------------------------
# TEST THE CONGRUENCE HYPOTHESIS FOR THE OUTCOME VARIABLE outcome_curviloc
# (Figure 1c in the manuscript)
#------------------------------------------------------------------------

# ESTIMATE THE POLYNOMIAL MODEL
rsa_curviloc <- RSA(outcome_curviloc ~ X*Y, data=df, model="full")


# PLOT THE ESTIMATED MODEL
plot(rsa_curviloc, legend=F, param=F,
     axes=c("LOC", "LOIC","PA1"), 
     project=c("LOC", "LOIC","PA1"), 
     xlab="Self-view X", ylab="Reputation Y", zlab="Happiness Z",
     xlim=c(-3,3), ylim=c(-3,3), zlim=c(0.4,2.0))


# DO THE RSA RESULTS INDICATE A CONGRUENCE EFFECT?

# 1. Is p10 significant?
getPar(rsa_curviloc)[getPar(rsa_curviloc)$label == "p10",]
# -> No. -> The first necessary condition for a congruence effect is satisfied.

# 2. Does the confidence interval of p11 exclude 1?
getPar(rsa_curviloc)[getPar(rsa_curviloc)$label == "p11",]
# -> No. -> The second necessary condition for a congruence effect is satisfied.

# 3. Is a4 significantly negative?
getPar(rsa_curviloc)[getPar(rsa_curviloc)$label == "a4",]
# -> Yes. -> The third necessary condition for a congruence effect is satisfied.

# 4. Is a3 significant?
getPar(rsa_curviloc)[getPar(rsa_curviloc)$label == "a3",]
# -> No. -> The fourth necessary condition for a congruence effect is satisfied.

# If our theory justifies to allow a non-constant LOC, we can conclude that data 
# supports the broad version of the congruence hypothesis.
# In this case, inspection of a2 and a1 reveals that there is a congruence effect 
# with additional common curvilinear main effect of X and Y. 


# If our theory does NOT allow a non-constant LOC, we must test conditions 5 and 6:

# 5. Is a2 significant?
getPar(rsa_curviloc)[getPar(rsa_curviloc)$label == "a2",]
# -> Yes. -> The fifth necessary condition for a strict congruence effect is NOT satisfied.

# -> The RSA results contradict a strict congruence effect, we must reject the strict version
# of the congruence hypothesis.



#------------------------------------------------------------------------
# TEST THE CONGRUENCE HYPOTHESIS FOR THE OUTCOME VARIABLE outcome_ssqd
# (Figure 2a in the manuscript)
#------------------------------------------------------------------------

# ESTIMATE THE POLYNOMIAL MODEL
rsa_ssqd <- RSA(outcome_ssqd ~ X*Y, data=df, model="full")


# PLOT THE ESTIMATED MODEL
plot(rsa_ssqd, legend=F, param=F,
     axes=c("LOC", "LOIC","PA1"), 
     project=c("LOC", "LOIC","PA1"), 
     xlab="Self-view X", ylab="Reputation Y", zlab="Happiness Z",
     xlim=c(-3,3), ylim=c(-3,3), zlim=c(0.4,2.0))


# DO THE RSA RESULTS INDICATE A CONGRUENCE EFFECT?

# 1. Is p10 significant?
getPar(rsa_ssqd)[getPar(rsa_ssqd)$label == "p10",]
# -> Yes. -> The first necessary condition for a congruence effect is NOT satisfied.

# -> The RSA results contradict a congruence effect, we must reject the congruence hypothesis.

# Note: 
# To find out what effect the model indicates instead, one needs to apply further RSA tools 
# (e.g., see Cohen et al., 2010; Edwards, 2002, 2007; Humberg et al., in press; Schönbrodt, 2016b).
# In doing so, one must pay particular attention to interpret only the area of the surface that
# makes predictions for "realistic" predictor combinations (i.e., which actually occurred in the data).
# 
# Here, application of further RSA tools would indicate that there is an "optimal margin" effect: 
# The outcome variable outcome_ssqd is highest for those persons whose reputation Y exceeds
# their self-view X by 0.9 units.
# -> We would now have to test this working hypothesis in a new dataset in a confirmatory way.




#------------------------------------------------------------------------
# TEST THE CONGRUENCE HYPOTHESIS FOR THE OUTCOME VARIABLE outcome_rsqd
# (Figure 2b in the manuscript)
#------------------------------------------------------------------------

# ESTIMATE THE POLYNOMIAL MODEL
rsa_rsqd <- RSA(outcome_rsqd ~ X*Y, data=df, model="full")


# PLOT THE ESTIMATED MODEL
plot(rsa_rsqd, legend=F, param=F,
     axes=c("LOC", "LOIC","PA1"), 
     project=c("LOC", "LOIC","PA1"), 
     xlab="Self-view X", ylab="Reputation Y", zlab="Happiness Z",
     xlim=c(-3,3), ylim=c(-3,3), zlim=c(0.4,2.0))


# DO THE RSA RESULTS INDICATE A CONGRUENCE EFFECT?

# 1. Is p10 significant?
getPar(rsa_rsqd)[getPar(rsa_rsqd)$label == "p10",]
# -> No. -> The first necessary condition for a congruence effect is satisfied.

# 2. Does the confidence interval of p11 exclude 1?
getPar(rsa_rsqd)[getPar(rsa_rsqd)$label == "p11",]
# -> Yes. -> The second necessary condition for a congruence effect is NOT satisfied.

# -> The RSA results contradict a congruence effect, we must reject the congruence hypothesis.

# Note: 
# Here, application of further RSA tools would indicate that the optimal combination of
# self-view and reputation depends on their levels: 
# For example, persons with quite low self-views and reputations have the highest level of 
# outcome_rsqd when their self-view exceeds their reputation, while persons on very high levels 
# have the highest level of outcome_rsqd when their reputation exceeds their self-view. 
# -> We would now have to test this working hypothesis in a new dataset in a confirmatory way.




#------------------------------------------------------------------------
# TEST THE CONGRUENCE HYPOTHESIS FOR THE OUTCOME VARIABLE outcome_bar
# (Figure 2c in the manuscript)
#------------------------------------------------------------------------

# ESTIMATE THE POLYNOMIAL MODEL
rsa_bar <- RSA(outcome_bar ~ X*Y, data=df, model="full")


# PLOT THE ESTIMATED MODEL
plot(rsa_bar, legend=F, param=F,
     axes=c("LOC", "LOIC","PA1"), 
     project=c("LOC", "LOIC","PA1"), 
     xlab="Self-view X", ylab="Reputation Y", zlab="Happiness Z",
     xlim=c(-3,3), ylim=c(-3,3), zlim=c(1,21))


# DO THE RSA RESULTS INDICATE A CONGRUENCE EFFECT?

# 1. Is p10 significant?
getPar(rsa_bar)[getPar(rsa_bar)$label == "p10",]
# -> Yes. -> The first necessary condition for a congruence effect is NOT satisfied.

# -> The RSA results contradict a congruence effect, we must reject the congruence hypothesis.

# Note: 
# Here, application of further RSA tools would show that the surface indicates a combination
# of three effects, namely (a) positive main effects of self-views and reputation, 
# (b) an "optimal margin" effect in the sense that outcome_bar is highest for persons whose  
# reputation exceeds their self-view by some amount, where (c) this "optimal margin" of X and Y 
# is larger on lower levels of self-views and reputations than on higher levels.
# -> We would now have to test this working hypothesis in a new dataset in a confirmatory way.




#------------------------------------------------------------------------
# TEST THE CONGRUENCE HYPOTHESIS FOR THE OUTCOME VARIABLE outcome_onlyx2
# (Figure 2d in the manuscript)
#------------------------------------------------------------------------

# ESTIMATE THE POLYNOMIAL MODEL
rsa_onlyx2 <- RSA(outcome_onlyx2 ~ X*Y, data=df, model="full")


# PLOT THE ESTIMATED MODEL
plot(rsa_onlyx2, legend=F, param=F,
     axes=c("LOC", "LOIC","PA1"), 
     project=c("LOC", "LOIC","PA1"), 
     xlab="Self-view X", ylab="Reputation Y", zlab="Happiness Z",
     xlim=c(-3,3), ylim=c(-3,3), zlim=c(0.65,2.0))


# DO THE RSA RESULTS INDICATE A CONGRUENCE EFFECT?


# 1. Is p10 significant?
getPar(rsa_onlyx2)[getPar(rsa_onlyx2)$label == "p10",]
# -> No. -> The first necessary condition for a congruence effect is satisfied.

# 2. Does the confidence interval of p11 exclude 1?
getPar(rsa_onlyx2)[getPar(rsa_onlyx2)$label == "p11",]
# -> No. -> The second necessary condition for a congruence effect is satisfied.

# 3. Is a4 significantly negative?
getPar(rsa_onlyx2)[getPar(rsa_onlyx2)$label == "a4",]
# -> Yes. -> The third necessary condition for a congruence effect is satisfied.

# 4. Is a3 significant?
getPar(rsa_onlyx2)[getPar(rsa_onlyx2)$label == "a3",]
# -> Yes. -> The fourth necessary condition for a congruence effect is NOT satisfied.

# -> The RSA results contradict a congruence effect, we must reject the congruence hypothesis.

# Note: 
# Here, application of further RSA tools would show that the surface indicates a monotonous
# rising, but diminishing effect of the self-view: Happiness is predicted to be higher 
# the higher a person's self-view, and this association is stronger on lower self-view 
# levels than on higher levels.



#------------------------------------------------------------------------
# TEST THE CONGRUENCE HYPOTHESIS FOR THE OUTCOME VARIABLE outcome_ia
# (Figure 2e in the manuscript)
#------------------------------------------------------------------------

# ESTIMATE THE POLYNOMIAL MODEL
rsa_ia <- RSA(outcome_ia ~ X*Y, data=df, model="full")


# PLOT THE ESTIMATED MODEL
plot(rsa_ia, legend=F, param=F,
     axes=c("LOC", "LOIC","PA1"), 
     project=c("LOC", "LOIC","PA1"), 
     xlab="Self-view X", ylab="Reputation Y", zlab="Happiness Z",
     xlim=c(-3,3), ylim=c(-3,3), zlim=c(0.4,2.0))


# DO THE RSA RESULTS INDICATE A CONGRUENCE EFFECT?

# 1. Is p10 significant?
getPar(rsa_ia)[getPar(rsa_ia)$label == "p10",]
# -> No. -> The first necessary condition for a congruence effect is satisfied.

# 2. Does the confidence interval of p11 exclude 1?
getPar(rsa_ia)[getPar(rsa_ia)$label == "p11",]
# -> Yes. -> The second necessary condition for a congruence effect is NOT satisfied.

# -> The RSA results contradict a congruence effect, we must reject the congruence hypothesis.

# Note: 
# Here, no further RSA tools are necessary, but a consideration of the estimated coefficients b1 to b5 
# suffices to find out that there is an inverted U-shaped association of reputations and 
# outcome_ia, where the optimal reputation level depends on the self-view level.
# -> We would now have to test this working hypothesis in a new dataset in a confirmatory way.




#------------------------------------------------------------------------
# TEST THE CONGRUENCE HYPOTHESIS FOR THE OUTCOME VARIABLE outcome_bowl
# (Figure 2f in the manuscript)
#------------------------------------------------------------------------

# ESTIMATE THE POLYNOMIAL MODEL
rsa_bowl <- RSA(outcome_bowl ~ X*Y, data=df, model="full")


# PLOT THE ESTIMATED MODEL
plot(rsa_bowl, legend=F, param=F,
     axes=c("LOC", "LOIC","PA1"), 
     project=c("LOC", "LOIC","PA1"), 
     xlab="Self-view X", ylab="Reputation Y", zlab="Happiness Z",
     xlim=c(-3,3), ylim=c(-3,3), zlim=c(0.4,2.0))


# DO THE RSA RESULTS INDICATE A CONGRUENCE EFFECT?

# 1. Is p10 significant?
getPar(rsa_bowl)[getPar(rsa_bowl)$label == "p10",]
# -> No. -> The first necessary condition for a congruence effect is satisfied.

# 2. Does the confidence interval of p11 exclude 1?
getPar(rsa_bowl)[getPar(rsa_bowl)$label == "p11",]
# -> Yes. -> The second necessary condition for a congruence effect is NOT satisfied.

# -> The RSA results contradict a congruence effect, we must reject the congruence hypothesis.

# Note: 
# Here, application of further RSA tools would indicate that outcome_bowl is lowest for persons
# whose self-view and reputation agree, and higher the more the two predictors deviate.
# -> We would now have to test this working hypothesis in a new dataset in a confirmatory way.





#-------------------
# REFERENCES
#-------------------

# Cohen, A., Nahum-Shani, I., & Doveh, E. (2010). Further insight and additional inference
# methods for polynomial regression applied to the analysis of congruence. Multivariate
# Behavioral Research, 45 (5), 828-852. doi:10.1080/00273171.2010.519272
#           
# Edwards, J. R. (2002). Alternatives to difference scores: Polynomial regression analysis and
# response surface methodology. In F. Drasgow & N. W. Schmitt (Eds.), Measuring and
# analyzing behavior in organizations: advances in measurement and data analysis
# (pp. 350{400). San Francisco, CA: Jossey-Bass.
# 
# Edwards, J. R. (2007). Polynomial regression and response surface methodology. In C. Ostroff
# & T. A. Judge (Eds.), Perspectives on organizational fit (pp. 361{372). San Francisco:
# ossey-Bass.
# 
# Fox, J. (2016). Applied regression analysis and generalized linear models. Thousand Oaks, 
# USA: SAGE Publications.
#
# Fox, J. and Weisberg, S. (2011) An R Companion to Applied Regression, Second Edition, Sage.
#
# Humberg, S., Dufner, M., Schönbrodt, F. D., Geukes, K., Hutteman, R., van Zalk, M. H. W., 
# Denissen, J. J. A., Nestler, S., & Back, M. D. (in press). Enhanced versus simply positive: 
# A new condition-based regression analysis to disentangle effects of self-enhancement from effects 
# of positivity of self-view. Journal of Personality and Social Psychology. 
# doi:10.1037/pspp0000134. Retrieved from osf.io/smmh7
# 
# Schönbrodt, F. D. (2016a). RSA: An R package for response surface analysis (version 0.9.10).
# Retrieved from https://cran.r-project.org/package=RSA
# 
# Schönbrodt, F. D. (2016b). Testing fit patterns with polynomial regression models. Retrieved
# from osf.io/3889z
