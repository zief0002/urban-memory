residual_plots = function(object){
  # Get residuals and fitted values
  aug_lm = broom::augment(object)
  
  # Create residual plot
  p1 = ggplot(data = aug_lm, aes(x =.resid)) +
    educate::stat_density_confidence(model = "normal") +
    geom_density() +
    theme_light() +
    xlab("Residuals") +
    ylab("Probability Density")
  
  # Create residual plot
  p2 = ggplot(data = aug_lm, aes(x =.fitted, y = .resid)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    geom_smooth(method = "loess", se = TRUE, n = 50, span = 0.67) +
    theme_light() +
    xlab("FItted values") +
    ylab("Residuals")
  
  
  return(p1 | p2)
}
