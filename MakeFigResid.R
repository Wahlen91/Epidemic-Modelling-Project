# Load packages
library("reshape2")
library("ggplot2")

Resid.FUN <- function(x, AgeClass, Sex){
  # Function which extracts the deviance residuals and anscombe residuals
  # from a glm.nb object
  #
  # Args:
  #   x: glm.nb objects
  #   AgeClass: which age class did the observation belong to? 
  #   Sex: which sex did the observation belong to?
  # 
  # Returns:
  #   A data.frame with deviance residuals, anscombe residuals and the categories
  #   of age and sex.
  #   
  # Note: 
  #   Since the data is ordered we can use the initially category from alldata data.frame.
  
  # Extract deviance residuals
  DevRes <- residuals(x, "deviance")
  
  # Extract Anscombe residuals
  AnsCombRes <- surveillance::anscombe.residuals(x, x$theta)
  
  # Make the data.frame
  Resids.df <-
    data.frame(
      'Deviance_Residual' = DevRes, 'Anscombe_Residual' = AnsCombRes,
      'AgeClass' = AgeClass, 'Sex' = Sex
    )
  return(Resids.df) 
}

Plot.Resids <- function(Z){
  # Create a ggplot for the residuals 
  # stratified on sex and age class.
  #
  # Args:
  #   z: a data.frame with columns for deviance and Anscombe residuals and
  #      two columns for age and sex.
  #
  
  # Change data.frame to long format
  plot.df <- suppressMessages(melt(Z))
  
  # Get nr rows
  N.row <- nrow(Z)
  
  # Make an row id variable
  plot.df$nrow <- seq(1, N.row, 1)
  
  # Do the plotting
  p <- ggplot(plot.df, aes(y = value, x = nrow, color = AgeClass)) +
    geom_point() +
    theme_bw() +
    xlab("Observation") +
    facet_wrap(Sex ~ variable, ncol = 2, scales = "free_y") +
    scale_color_brewer("Age class", palette = 2, type = "div")
  
  return(p)
}
