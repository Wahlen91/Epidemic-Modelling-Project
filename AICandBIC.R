library("xtable")

# Load all models from Models directory.
models <- c("GAMlssModel.RData", "hhh4Model.RData", "NegBinModel.RData")
models <- paste0("Models/", models)

ModObj <- unlist(lapply(models, load, .GlobalEnv))

# Function to extract aic and bic
Extract_AicBic <- function(x) {
  # Function which extracts AIC and BIC from an model object x. 
  # the object x need to be of a class which supports the BIC and AIC functions
  #
  # x: a model from which we want to extract the BIC and AIC.
  bic <- BIC(x)
  aic <- AIC(x)
  return(data.frame('BIC' = bic, 'AIC' = aic))
}

#Models_list<- list(GAMlssModel, hhh4Model, model.Smooth,model.as.Smooth, model.as.ba.Smooth)
#AicBic.df <- lapply(Models_list, Extract_AicBic)
AicBic.df <- lapply(ModObj, function(i) Extract_AicBic(get(i)))
AicBic.df <- do.call("rbind",AicBic.df)

# Fixing strings for xtable Model names 
names <- sub("model.", ModObj, replace="Negative Binomial ")
names <- sub("Smooth", names , replace="")
names <- sub("Model", names, replace="")
names <- gsub("hhh4", "hhh4-", names)
names <- gsub("GAMlss", "GAMLSS", names)
names[2] <- gsub("season", "s", names[2])

AicBic.df$Model <- names
AicBic.df <- select(AicBic.df, Model, AIC, BIC)

print(
  xtable(
    AicBic.df, label = "TabelMods", caption = "AIC and BIC values for all models.
    The abbrevation ''as.'' stands for ''Age and Sex stratified'' and ''ba.''
    stands for ''Before and After the official announcement''",
    align = "lccc"
  ),type = "latex",include.rownames = FALSE, file = "AicBicTable.tex"
)
