library(dagitty)
library(ggdag)
library(tidyverse)


dagified <- dagify(x ~ z,
                   y ~ z,
                   exposure = "x",
                   outcome = "y")
tidy_dagitty(dagified)

ggdag(dagified, layout = "circle")


# INPUT MODEL

g <- dagitty('
  dag{
  knowledge -> ease [beta=.669] 
  
  ease -> students [beta=.364]
  
  ease -> teachers [beta=.297]
  students -> teachers [beta=.452]
  
  ease -> attitude [beta=-0.047]
  students -> attitude [beta=.593]
  teachers -> attitude [beta = .220]
  
  ease -> intend [beta=.135]
  teachers -> intend [beta=.059]
  attitude -> intend [beta=.629]
  knowledge -> intend [beta=-.003] 
  }
  ')

ggdag(g)

# SIMULATE FROM TECHNOLOGY MODEL
x <- simulateSEM(g, empirical = TRUE, N = 232) 

# Check correlations
x %>%
  select(intend, knowledge, ease, students, teachers, attitude) %>%
  corrr::correlate()

broom::glance(lm(intend ~ ease + knowledge + attitude + useful_teachers, data = x))



dagify(y ~ x + z, x~ z)

coords <- list(
  x = c(Attitude = 1, B = 2, D = 3, C = 3, F = 3, E = 4, G = 5, H = 5, I = 5),
  y = c(Attitude = 0, B = 0, D = 1, C = 0, F = -1, E = 0, G = 1, H = 0, I = -1)
)

dag <- dagify(G ~~ H,
              G ~~ I,
              I ~~ G,
              H ~~ I,
              D ~ B,
              C ~ B,
              I ~ C + F,
              F ~ B,
              B ~ Attitude,
              H ~ E,
              C ~ E + G,
              G ~ D, coords = coords)

dagitty::is.dagitty(dag)

ggdag(dag)

##################################

coords <- list(
  x = c(useful_students = 1, attitude = 3, intent_use = 5, perceived_knowledge = 2, perceived_ease = 4,  useful_teachers =  2),
  y = c(useful_students = 0, attitude = 0, intent_use = 0, perceived_knowledge = 1, perceived_ease = 1,  useful_teachers = -1)
)

dag2 <- dagify(
  intent_use ~ perceived_ease + perceived_knowledge + attitude + useful_teachers,
  attitude ~ perceived_ease + useful_students + useful_teachers,
  useful_teachers ~ useful_students + perceived_ease,
  useful_students ~ perceived_ease,
  perceived_ease ~ perceived_knowledge,
  coords = coords,
  outcome = "intent_use",
  labels = c(
    "intent_use" = "Intent to\nUse",
    "perceived_ease" = "Perceived\nEase",
    "useful_students" = "Useful for\nStudents",
    "useful_teachers" = "Useful for\nTeachers",
    "perceived_knowledge" = "Knowledge about\nTechnology",
    "attitude" = "Attitude toward\nUsing"
    )
)

ggdag(dag2, use_labels = "label", text = FALSE) +
  geom_dag_point(shape = 10)

dag2 %>%
  tidy_dagitty() %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_label_repel(aes(label = label)) +
  theme_dag()


library(DiagrammeR)

grViz("
digraph path_model {

  graph [layout = dot, rankdir = LR]

  subgraph {
    rank = same; A; C;
  }

  subgraph {
    rank = same; B; D;
  }

  # a 'graph' statement
  graph [overlap = true, fontsize = 10]

  # several 'node' statements
  node [shape = box, fontname = Helvetica]
  A; B; C; D; E; y

  # several 'edge' statements
  A->{B y}
  B->{C D E y}
  C->{D E}
  D->{E y}
  E->y
}      
  ")


write_csv(x, file = "~/Desktop/technology-path-analysis.csv")


################################

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
### Publication model
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

