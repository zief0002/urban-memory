library(dplyr)
library(ggplot2)
library(ggformula)

duff = readr::read_csv("/Users/zief0002/Dropbox/epsy-8264/data/duff.csv")


d2 = duff %>%
  filter(row_number() <= 5) %>%
  mutate(
    dist = quarter - 2,
    scl = dist / (max(dist)),
    w = if_else(scl >= 1, 0, (1 - abs(scl)^3)^3)
  )


fitted(lm(revenue ~ 1 + quarter, data = d2, weights = w))[2]

fitted(loess(revenue ~ quarter, data = duff, span = 0.20, degree = 1))

loess_duff = data.frame(
  x = duff$quarter,
  y = fitted(loess(revenue ~ quarter, data = duff, span = 0.20, degree = 1))
)

ggplot(data = duff, aes(x = quarter, y = revenue)) +
  #geom_smooth(method = "loess", se = FALSE) +
  geom_smooth(color = "red", method = "loess", se = FALSE, span = 0.2, 
              method.args = list(degree = 1)) +
  geom_point(data = loess_duff, aes(x = x, y = y)) +
  geom_path(data = loess_duff, aes(x = x, y = y))






