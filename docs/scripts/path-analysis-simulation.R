library(dagitty)
library(ggdag)
library(tidyverse)


dagified <- dagify(x ~ z,
                   y ~ z,
                   exposure = "x",
                   outcome = "y")
tidy_dagitty(dagified)

ggdag(dagified, layout = "circle")


# SIMULATE
g <- dagitty('dag{z -> x [beta=-.6] x <- y [beta=-.6] }')

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

x <- simulateSEM(g, empirical = TRUE, N = 232) 

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
