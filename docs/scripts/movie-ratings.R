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