library(RColorBrewer)
library(viridis)

# Create user ratings matrix for 5 movies
movies = data.frame(
  Predator     = c(1, 3, 4, 5, 0, 0, 0),
  StarWars     = c(1, 3, 4, 5, 2, 0, 1),
  Terminator   = c(1, 3, 4, 5, 0, 0, 0),
  LoveActually = c(0, 0, 0, 0, 4, 5, 2),
  LadyTramp  = c(0, 0, 0, 0, 4, 5, 2)
)

# Add the user names
row.names(movies) = c("Taylor", "Rhianna", "Beyonce", "Justin", "Snoop", "Martha", "Jay-Z")

# View the matrix
movies

netflix = movies %>%
  scale(center = TRUE, scale = TRUE) %>%
  data.frame() %>%
  prcomp()



netflix %>%
  tidy(matrix = "eigenvalues")

netflix

netflix_svd = movies %>%
  scale(center = TRUE, scale = TRUE) %>%
  svd()

netflix_svd

# Add row/column names to V
row.names(netflix_svd$v) = c("Predator", "StarWars", "Terminator", "LoveActually", "LadyTramp")
colnames(netflix_svd$v) = c("PC1", "PC2", "PC3", "PC4", "PC5")

# Add row/column names to U
row.names(netflix_svd$u) = c("Taylor", "Rhianna", "Beyonce", "Justin", "Snoop", "Martha", "Jay-Z")
colnames(netflix_svd$u) = c("Predator", "StarWars", "Terminator", "LoveActually", "LadyTramp")

netflix_svd



pretty <- brewer.pal(nrow(netflix_svd$u), name = "PiYG")
pretty3 <- colorRampPalette(colors = c("#F7FBFF", "#08306B"))(50)

heatmap(netflix_svd$u, margins = c(8, 8), col = pretty3, RowSideColors = pretty)
