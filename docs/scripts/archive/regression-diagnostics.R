# Load libraries
library(broom)
library(car)
library(corrr)
library(dplyr)
library(ggplot2)
library(gridExtra)


# Load data
data(anscombe)


# Fit linear models
lm.1 = lm(y1 ~ 1 + x1, data = anscombe)
lm.2 = lm(y2 ~ 1 + x2, data = anscombe)
lm.3 = lm(y3 ~ 1 + x3, data = anscombe)
lm.4 = lm(y4 ~ 1 + x4, data = anscombe)

# Model-level output
glance(lm.1)
glance(lm.2)
glance(lm.3)
glance(lm.4)

# Coefficient-level output
tidy(lm.1)
tidy(lm.2)
tidy(lm.3)
tidy(lm.4)

# Scatterplots
p1 = ggplot(data = anscombe, aes(x = x1, y = y1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  scale_x_continuous(name = "x", limits = c(0, 20)) +
  scale_y_continuous(name = "y", limits = c(0, 15)) +
  ggtitle("(a)")

p2 = ggplot(data = anscombe, aes(x = x2, y = y2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  scale_x_continuous(name = "x", limits = c(0, 20)) +
  scale_y_continuous(name = "y", limits = c(0, 15)) +
  ggtitle("(b)")

p3 = ggplot(data = anscombe, aes(x = x3, y = y3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  scale_x_continuous(name = "x", limits = c(0, 20)) +
  scale_y_continuous(name = "y", limits = c(0, 15)) +
  ggtitle("(c)")

p4 = ggplot(data = anscombe, aes(x = x4, y = y4)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  scale_x_continuous(name = "x", limits = c(0, 20)) +
  scale_y_continuous(name = "y", limits = c(0, 15)) +
  ggtitle("(d)")

grid.arrange(p1, p2, p3, p4, nrow = 2)


### Collinearity


data(Ericksen)

Ericksen %>%
  mutate(
    city = if_else(city == "city", 1, 0)
  ) %>%
  select(undercount, minority, crime, poverty, language, highschool, housing, city, conventional) %>%
  correlate()

# Fit linear model
lm.1 = lm(undercount ~ 1 + minority + crime + poverty +
            language + highschool + housing + city + conventional,
          data = Ericksen)

tidy(lm.1)

sqrt(vif(lm.1))

tidy(lm.2)


lm.2 = lm(undercount ~ 1 + crime + poverty +
            language + housing + conventional,
          data = Ericksen)

sqrt(vif(lm.2))


### Outlier scatterplots

data_01 = data.frame(
  x= c(1, 2, 3, 4, 2.6),
  y = c(2, 3, 4, 5, 7)
)

data_02 = data.frame(
  x= c(1, 2, 3, 4),
  y = c(2, 3, 4, 5)
)


p1 = ggplot(data = data_02, aes(x = x, y = y)) +
  geom_abline(intercept = 1.543, slope = 1.054) +
  geom_abline(intercept = 1, slope = 1, linetype = "dotted") +
  geom_point(shape = 21, color = "black", fill = "#17becf", size = 4) +
  geom_point(x = 2.6, y = 7, shape = 22, color = "black", fill = "#bcbd22", size = 4) +
  theme_bw() +
  scale_x_continuous(name = "x", limits = c(0, 6), breaks = NULL) +
  scale_y_continuous(name = "y", limits = c(0, 10), breaks = NULL) +
  ggtitle("(a)")


data_03 = data.frame(
  x= c(1, 2, 3, 4, 9),
  y = c(2, 3, 4, 5, 18)
)


p2 = ggplot(data = data_02, aes(x = x, y = y)) +
  geom_abline(intercept = -1.474 , slope = 2.072) +
  geom_abline(intercept = 1, slope = 1, linetype = "dotted") +
  geom_point(shape = 21, color = "black", fill = "#17becf", size = 4) +
  geom_point(x = 9, y = 18, shape = 22, color = "black", fill = "#bcbd22", size = 4) +
  theme_bw() +
  scale_x_continuous(name = "x", limits = c(0, 10), breaks = NULL) +
  scale_y_continuous(name = "y", limits = c(0, 20), breaks = NULL) +
  ggtitle("(b)")


data_04 = data.frame(
  x = c(1, 2.0, 2.04, 3.0, 3.03, 4),
  y = c(2, 1.8, 2.9, 2.2, 3.6, 3)
)

data_05 = data.frame(
  x = c(1, 2.0, 2.04, 3.0, 3.03, 4, 15),
  y = c(2, 1.8, 2.9, 2.2, 3.6, 3, 7)
)


p3 = ggplot(data = data_04, aes(x = x, y = y)) +
  geom_abline(intercept = 1.6904  , slope = 0.3547  ) +
  geom_abline(intercept = 1.6260, slope = 0.3812, linetype = "dotted") +
  geom_point(shape = 21, color = "black", fill = "#17becf", size = 4) +
  geom_point(x = 15, y = 7, shape = 22, color = "black", fill = "#bcbd22", size = 4) +
  theme_bw() +
  scale_x_continuous(name = "x", limits = c(0, 20), breaks = NULL) +
  scale_y_continuous(name = "y", limits = c(0, 15), breaks = NULL) +
  ggtitle("(c)")


lm(y~x, data_05)

gridExtra::grid.arrange(p1, p2, nrow = 2)


data(Davis)
Davis

ggplot(data = Davis, aes(x = weight, y = repwt)) +
  geom_text(aes(label = sex)) +
  geom_abline(intercept = 41.3228, slope = 0.2645, linetype = "solid") +
  geom_abline(intercept = 1.3586, slope = 0.9898, linetype = "dotted") +
  scale_y_continuous(
    name = "Reported weight (in kg)",
    limits = c(30, 170),
    breaks = seq(from = 30, to = 170, by = 20)
    ) +
  scale_x_continuous(
    name = "Measured weight (in kg)",
    limits = c(30, 170),
    breaks = seq(from = 30, to = 170, by = 20)
  ) +
  theme_bw()


# Female reference group
lm.1 = lm(repwt ~ 1 + weight + sex + weight:sex, data = Davis)
tidy(lm.1)
confint(lm.1)

# Fix data
davis2 = Davis %>%
  arrange(sex, desc(weight))


davis2[1, 2] = 57
davis2[1, 3] = 166


ggplot(data = davis2, aes(x = weight, y = repwt)) +
  geom_text(aes(label = sex)) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = sex)) +
  scale_y_continuous(
    name = "Reported weight (in kg)",
    limits = c(30, 130),
    breaks = seq(from = 30, to = 130, by = 20)
  ) +
  scale_x_continuous(
    name = "Measured weight (in kg)",
    limits = c(30, 130),
    breaks = seq(from = 30, to = 130, by = 20)
  ) +
  theme_bw() +
  scale_linetype_discrete(name = "")

lm.2 = lm(repwt ~ 1 + weight + sex + weight:sex, data = davis2)
tidy(lm.2)
confint(lm.2)



# Analysis 2

Davis %>% filter(sex == "M") %>% lm(weight ~ 1 + repwt, data = .)
Davis %>% filter(sex == "F") %>% lm(weight ~ 1 + repwt, data = .)

ggplot(data = Davis, aes(x = repwt, y = weight)) +
  geom_text(aes(label = sex)) +
  geom_abline(intercept = 3.8685, slope = 0.9594, linetype = "solid") +
  geom_abline(intercept = 1.7943, slope = 0.9689, linetype = "dotted") +
  #geom_smooth(method = "lm", se = FALSE, aes(linetype = sex)) +
  scale_y_continuous(
    name = "Measured weight (in kg)",
    limits = c(30, 170),
    breaks = seq(from = 30, to = 170, by = 20)
  ) +
  scale_x_continuous(
    name = "Reported weight (in kg)",
    limits = c(30, 170),
    breaks = seq(from = 30, to = 170, by = 20)
  ) +
  theme_bw() +
  scale_linetype_discrete(name = "")


Davis = Davis %>%
  mutate(
    female = if_else(sex == "F", 1, 0)
  )

lm.3 = lm(weight ~ 1 + repwt + female + repwt:female, data = Davis)
tidy(lm.3)
glance(lm.3)
confint(lm.3)

davis2 = davis2 %>%
  mutate(
    female = if_else(sex == "F", 1, 0)
  )

lm.4 = lm(weight ~ 1 + repwt + female + repwt:female, data = davis2)
glance(lm.4)
tidy(lm.4)


lm.1 = lm(repwt ~ 1 + weight + female + weight:female, data = Davis)
out1 = augment(lm.1)
head(out1)

mean(out1$.hat)

out1 %>%
  filter(.hat >= 2 * 0.02185792) %>%
  arrange(desc(.hat))


ggplot(data = out1, aes(x = .rownames, y = .hat)) +
  geom_text(aes(label = .rownames), size = 3) +
  theme_bw() +
  scale_x_discrete(name = "", breaks = NULL) +
  geom_hline(yintercept = 0.02185792, color = "red") +
  geom_hline(yintercept = 2 * 0.02185792, linetype = "dotted", color = "red") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
    )

Davis = Davis %>%
  mutate(
    obs12 = if_else(row_number() == 12, 1, 0)
  )

lm.12 = lm(repwt ~ 1 + weight + sex + weight:sex + obs12, data = Davis)
tidy(lm.12)


# Influence

tidy(lm.1)

Davis %>%
  filter(row_number() != 12) %>%
  lm(repwt ~ 1 + weight + female + weight:female, data = .) %>%
  tidy()

(1.3586399 - 1.35863993) #Intercept
(1.3586399 - 1.35863993) #weight
39.9641221 - 1.98928622

dfbeta(lm.1)[12,]

(-0.7253627  + 0.05670878) / 0.03855672

dfbetasPlots(lm.1, intercept = TRUE)



# Cook's D
augment(lm.1)[12, ]

ggplot(data = out1, aes(x = .rownames, y = .cooksd)) +
  geom_text(aes(label = .rownames), size = 3) +
  theme_bw() +
  scale_x_discrete(name = "", breaks = NULL) +
  #geom_hline(yintercept = 0.02185792, color = "red") +
  #geom_hline(yintercept = 2 * 0.02185792, linetype = "dotted", color = "red") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )







out1 %>%
  arrange(desc(.cooksd)) %>%
  head()



# DFFITS
dffits(lm.1)[12]

out1 = out1 %>%
  mutate(
    dffits = dffits(lm.1)
  )


ggplot(data = out1, aes(x = .rownames, y = abs(dffits))) +
  geom_text(aes(label = .rownames), size = 3) +
  theme_bw() +
  scale_x_discrete(name = "", breaks = NULL) +
  ylab("|DFFITS|") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )


out1 %>%
  arrange(desc(abs(dffits))) %>%
  head()


# Plot of leverage vs studentized residual

out1 = out1 %>%
  mutate(
    stud_resid = rstudent(lm.1))
  )


ggplot(data = out1, aes(x = stud_resid, y = .hat)) +
  geom_text(aes(label = .rownames), size = 3) +
  theme_bw() +
  xlab("Studentized residuals") +
  ylab("Leverage values")

# Size obs according to Cook's D
ggplot(data = out1, aes(x = stud_resid, y = .hat)) +
  geom_text(aes(label = .rownames, size = .cooksd)) +
  theme_bw() +
  xlab("Studentized residuals") +
  ylab("Leverage values")


# COVRATIO
covratio(lm.1)


out1 = out1 %>%
  mutate(
    cov_ratio = covratio(lm.1)
  )


ggplot(data = out1, aes(x = .rownames, y = cov_ratio)) +
  geom_text(aes(label = .rownames), size = 3) +
  theme_bw() +
  scale_x_discrete(name = "", breaks = NULL) +
  ylab("COVRATIO") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )


out1 %>%
  arrange(desc(abs(dffits))) %>%
  head()


## Added-variable plots

data("Duncan")
head(Duncan)

# AVP of influence on education
lm.1 = lm(prestige ~ 1 + income, data = Duncan)
lm.2 = lm(education ~ 1 + income, data = Duncan)

data_06 = data.frame(
  e2 = resid(lm.2),
  e1 = resid(lm.1),
  rn = 1:45
)

ggplot(data = data_06, aes(x = e2, y = e1)) +
  geom_text(aes(label = rn), size = 3) +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(name = "X1 | X2 ") +
  scale_y_continuous(name = "Y | X2 ")


tidy(lm(e1 ~ e2, data = data_06))
tidy(lm(prestige ~ income + education, data = Duncan))


#
lm.1 = lm(repwt ~ 1 + weight + female + weight:female, data = Davis)
avPlot(lm.1, variable = "weight")
avPlots(lm.1, intercept = TRUE)
