r_xx = cor(city[c("education", "seniority")])
r_xx

r_xy = cor(city[c("income" , "education", "seniority")])[2:3, 1]
r_xy


solve(r_xx) %*% r_xy


####

lambda_id = 0.1 * diag(2) 
lambda_id

solve(r_xx + lambda_id) %*% r_xy


MASS::lm.ridge(scale(income) ~ scale(education) + scale(seniority), data = city, lambda = 0.1)


neter = read_csv("~/Desktop/neter.csv")



MASS::lm.ridge(bodyfat ~ triceps + thigh + midarm, data = neter, lambda = 0)
MASS::lm.ridge(scale(bodyfat) ~ scale(triceps) + scale(thigh) + scale(midarm) - 1, data = neter, lambda = 0)
?MASS::lm.ridge


neter = neter %>%
  mutate(
    z_bodyfat = as.numeric(scale(bodyfat)),
    z_triceps = as.numeric(scale(triceps)),
    z_thigh   = as.numeric(scale(thigh)),
    z_midarm  = as.numeric(scale(midarm)),
  )

MASS::lm.ridge(z_bodyfat ~ z_triceps + z_thigh + z_midarm - 1, data = neter, lambda = 0)

library(penalized)
my_ridge = penalized(response = scale(neter$bodyfat), penalized = scale(neter[ , c("triceps", "thigh", "midarm")]), lambda2 = 0)
coef(my_ridge)


my_ridge = penalized(
  response = z_bodyfat, 
  penalized = ~ z_triceps + z_thigh + z_midarm + 0, #neter[ , c("z_triceps", "z_thigh", "z_midarm")], 
  data = neter,
  lambda2 = 0
  )
coef(my_ridge)


X = matrix(
  data = c(neter$z_triceps, neter$z_thigh, neter$z_midarm),
  ncol = 3
)


y = matrix(data = neter$z_bodyfat, ncol = 1)
solve(t(X) %*% X + lambda_id)) %*% t(X) %*% y


r_XX = t(X) %*% X / 19

solve(r_XX + lambda_id) %*% (t(X)/19) %*% y


r_xx = cor(neter[c("triceps", "thigh", "midarm")])
r_xy = cor(neter[c("bodyfat", "triceps", "thigh", "midarm")])[2:4, 1]
lambda_id = 0.002 * diag(3) 
solve(r_xx + lambda_id) %*% r_xy




my_ridge = penalized(
  response = z_achievement, 
  penalized = ~ z_faculty + z_peer + z_school + 0, 
  data = eeo,
  lambda2 = 0.1
)

coef(my_ridge)


r_xx = cor(eeo[c("z_faculty", "z_peer", "z_school")])
r_yx = cor(eeo[c("z_achievement", "z_faculty", "z_peer", "z_school")])[2:4, 1]
lambda_I = 0.001 * diag(3)

solve(r_xx + lambda_I) %*% r_yx
MASS::lm.ridge(z_achievement ~ z_faculty + z_peer + z_school - 1, data = eeo, lambda = .001)



eeo = eeo %>% 
  mutate(
    c_achievement = achievement - mean(achievement),
    z_achievement = as.numeric(scale(achievement)),
    z_faculty     = as.numeric(scale(faculty)),
    z_peer        = as.numeric(scale(peer)),
    z_school      = as.numeric(scale(school))
  ) 


x = eeo %>% dplyr::select(z_faculty, z_peer, z_school) %>% data.matrix()
y = eeo$z_achievement

solve( t(x) %*% x + lambda_I ) %*% t(x) %*% y

H = x %*% solve( t(x) %*% x + lambda_I ) %*% t(x)
H

sum(diag(H))


x = matrix(
  data = c(duncan$income, duncan$education),
)


#################

library(lmridge)

ridge3 = lmridge(achievement ~ faculty + peer + school - 1, data = eeo, scaling = "sc", K = 0.1/70)
ridge3

################

X = eeo %>% dplyr::select(faculty, peer, school) %>% data.matrix()
Y = eeo$achievement
tidy(glmnet(x = x, y = y, alpha = 0, lambda = 0.1/70, intercept = FALSE))


X = eeo %>% filter(row_number() < 3) %>% dplyr::select(faculty, peer, school) %>% data.matrix()
Y = eeo$achievement[1:2]

tidy(glmnet(x = x, y = y, alpha = 0, lambda = 0.1/70, intercept = FALSE))

lm(z_achievement ~ z_faculty + z_peer + z_school, data = eeo[1:2, ])
