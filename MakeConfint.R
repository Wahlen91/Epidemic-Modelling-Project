library("xtable")
library("surveillance")
library("gamlss")
library("MASS")

# Load all models from Models directory.
models <- c("GAMlssModel.RData", "hhh4Model.RData", "NegBinModel.RData")
models <- paste0("Models/", models)

ModObj <- unlist(lapply(models, load, .GlobalEnv))

confint.gamlss <- function(x){
  # Takes a gamlss model and calculates coefficients and their 95% wald
  #  confidence intervals.
  #
  # Args:
  #   x: gamlss model object
  #   
  
  # Get summary
  tmp <- capture.output(m.summary <- summary(x, type = "qr"))
  rm(tmp)
  
  # Get standard error
  std.error <- m.summary[, "Std. Error"]
  
  # Get estimate
  est <- m.summary[, "Estimate"]
  
  # Make data.frame to return
  Est <- data.frame(Estimate = est,
                    Lower = est - qnorm(0.975)*std.error,
                    Upper = est + qnorm(0.975)*std.error)
  
  return(Est)
}

Extract_Coef <- function(x) {
  # Takes a model object (gamlss, glm.nb, hhh4) and calculates the estimated
  # coefficients and their 95% wald confidence intervals.
  #
  # Args:
  #   x: Model object (gamlss, glm.nb, hhh4)
  #   
  
  if (class(x)[1] == "gamlss") {
    Est <- confint.gamlss(x)
  }else{
    coef <- coef(x)
    confint <- confint.default(x)
    Est <- cbind(coef, confint)
  }
  
  Est <- Est[grep("o104wk", rownames(Est)), ]
  if (!is.null(rownames(Est)))
    rownames(Est) <- gsub("end.", "", rownames(Est))
  
  return(Est)
}

# Get confidence intervals
Coef.Confint <- lapply(ModObj, function(i) Extract_Coef(get(i)))


# Estimate from basic NegBin model
Est.Model.Smooth <- exp(Coef.Confint[[which(ModObj == "model.Smooth")]])

# USE THIS FOR ARTICLE
Est.Model.Smooth

# Estimate from best model (hhh4)
Est.hhh4 <- exp(Coef.Confint[[which(ModObj == "hhh4Model4.wo.strat.od")]])
Est.hhh4split <- exp(Coef.Confint[[which(ModObj == "hhh4Model4.ba.wo.strat.od")]])
Est.hhh4.before <- Est.hhh4split[grep("before", rownames(Est.hhh4split)), ]
Est.hhh4.after <- Est.hhh4split[grep("after", rownames(Est.hhh4split)), ]

# Make list of intervals
lst <- list(Est.hhh4, Est.hhh4.before, Est.hhh4.after)


MakeTable <- function(lst){
  inner <- function(x){
    # Make x a data.frame
    x <- as.data.frame(x)
    
    # Indexes for males and females
    ind.F <- grep("Female", rownames(x))
    ind.M <- grep("Male", rownames(x))
    
    # Data.frame for males and females
    x.F <- x[ind.F, ]; x.F$sex <- "Females"
    x.M <- x[ind.M, ]; x.M$sex <- "Males"
    
    # Change rownames
    rownames(x.F) <- gsub("o104wk.[A-z]*", "", rownames(x.F))
    rownames(x.M) <- gsub("o104wk.[A-z]*", "", rownames(x.M))
    
    # Bind columns and remove sex
    x <- cbind(x.F, x.M)
    x$sex <- NULL; x$sex <- NULL
    return(x)
  }
  
  tables <- lapply(lst, inner)
  table <- do.call("cbind", tables)
  
  return(table)
}

# Make latex table. Not final product, change in latex.
print(xtable(MakeTable(lst), #digits = 0,
             caption = "blabla", 
             label = "hhh4Model4.wo.strat.od"), include.rownames = TRUE,
      caption.placement = "top", table.placement = "!htbp")
