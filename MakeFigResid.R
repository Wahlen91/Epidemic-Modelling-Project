
Resid.FUN<- function(x, AgeClass, Sex){
  # Function which extracts the deviance residuals and anscombe residuals
  #
  # x: glm (negbin) objects
  # AgeClass:which category did the observation belong to? 
  # since data is ordered we can use the initially category from alldata data.frame.
  
  # Extract deviance residuals
  DevRes<- residuals(x,"deviance")
  # Extract Anscombe residuals
  AnsCombRes <- surveillance::anscombe.residuals(x,x$theta)
  
  Resids.df <- data.frame('Deviance_Residual'=DevRes,'Anscombe_Residual'=AnsCombRes, 
                          'AgeClass'=AgeClass, 'Sex'=Sex)
  return(Resids.df) 
}

Plot.Resids <- function(Z){
  # Create a ggplot for the residuals 
  # stratified on sex and age class.
  #
  # z:a data.frame
  plot.df <- melt(Z)
  
  N.row <- dim(Z)[1]
  plot.df$nrow <- seq(1, N.row, 1)
  p<- ggplot(plot.df, aes(y=value, x=nrow, color=AgeClass)) +
    geom_point(aes(shape=Sex)) +
    theme_bw() +
    facet_grid(.~variable) +
    scale_color_brewer("Age class", palette = 2, type="div")
  return(p)
}
