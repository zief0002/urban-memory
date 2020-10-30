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

# Standardize the ratings and turn into matrix
M = movies %>% scale() %>% data.matrix()

# Eigenvalue decomposition to do PCA
eigen(t(M) %*% M)

# Use princomp()
summary(princomp(movies, cor = TRUE))$loadings

# Name the principal components
PC = c("SciFi/Romance", "General Movies", "Arnold/SciFi", "Romance - Anim./Live", "Arnold Campy/Cool")

# Carry out the SVD
svd(M)

# Name the rows and columns in the V matrix which correspond to the PC loadings
# These provide composite values for different movies
V = as.matrix(round(svd(M)$v, 2))
row.names(V) = names(movies)
colnames(V) = PC
V

# Name the rows and columns in the U matrix
# These provide composite values for different users
U = as.matrix(round(svd(M)$u, 2))
row.names(U) = c("Taylor", "Rhianna", "Beyonce", "Justin", "Snoop", "Martha", "Jay-Z")
colnames(U) = PC
U



