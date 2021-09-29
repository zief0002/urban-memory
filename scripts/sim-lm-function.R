# Function to generate and plot data from a given regression model

sim_lm = function(beta_0, beta_1, sigma, n){
  # Draw the x-values
  set.seed(123456) #Make simulation reproducible
  x = runif(n, min = -3, max = 3)
  
  # Generate the y-values
  y = beta_0 + beta_1 * x + rnorm(n, mean = 0, sd = sigma)
  
  # Create a plot
  p1 = data.frame(x, y) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(size = 5, color = "blue") +
    xlim(-3, 3) +
    ylim(-3.5, 3.5) +
    theme_light() +
    geom_abline(intercept = 0.2, slope = 0.5, color = "orange", lwd = 2) + #population regression line
    geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") #sample regression line

  # Output the x-values, y-values, and plot
  return( list(x = x, y = y, p1) )
}


# Run the function
# sim_lm(beta_0 = 0.2, beta_1 = 0.5, sigma = 1, n = 10)

