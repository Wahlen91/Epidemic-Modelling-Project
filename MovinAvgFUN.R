RollAvg <- function(x,agegroup){
  # x - a object of the form data.table
  # agegroup - agegroup to compute the moving avarage 
  
  # In x there must be a variable (column) with name Age
  #Sub.dat <- x %>% 
  #  dplyr::filter(Age == agegroup)
  
  #popRA <- Sub.dat %>%
  #  select(Population) %>% 
  #  caTools::runmean(k=4,alg="C", "mean", "left")
}

Ages <- unique(alldata$Age)
Sexes <- unique(alldata$Sex)

MovingAvg <- function(data,AgeGroup,Sexe){
  # Function to smooth strata wise population 
  # based on Agegroup and Sex. 
  # Smoothing is done according to a centered five-week moving avarage.
  #
  # data - dataframe which has the variables Sex and Age in it. 
  # AgeGroups - the agegroups to use when smoothing
  # Sex - gender which is also used when smoothing
  
  Sub.dat <- data %>% 
    filter(Age == AgeGroup & Sex == Sexe) %>%
    arrange(desc(Time))
  
  MA.Vals <- transform(Sub.dat,
                       PopSmooth=as.integer(stats::filter(Sub.dat$Population,
                                     rep(1/5,5), sides=2, method="convolution"))
                       )
  
  return(MA.Vals)
}

#test1 <- alldata %>% filter(Age == Ages[1] & Sex==Sexes[1]) %>% arrange(desc(Time))

#stats::filter(test1$Population,rep(1/5,5), sides=2, method="convolution")
#MovingAvg(alldata,Ages[1], Sexes[1])
#grid.Ag.Sx <- expand.grid(Ages,Sexes)

Male.Vals <- lapply(X=Ages, Sexe=Sexes[1], data=alldata, FUN=MovingAvg)
Fem.Vals <- lapply(X=Ages, Sexe=Sexes[2], data=alldata, FUN=MovingAvg)
#MUCHDATA <- do.call("rbind",list(do.call("rbind",Male.Vals), do.call("rbind",Fem.Vals)))
MUCHDATA<- bind_rows(c(Male.Vals,Fem.Vals))
test <- left_join(alldata,MUCHDATA)
#test1 <- function(x) {alldata %>% filter(Time==x) %>% select(Population) %>% sum()}

#stats::filter()
#length(sapply(weeks, test1))#ok


stats::filter()
test2 <- caTools:runmean(test1$Population,k=4,alg="C", "mean", "left")# NOT RIGHT!

norots <- read.csv("Data/noro-ts.csv")

all.equal(test1$Population,test2)

stats::filter(x=test1$Cases,sides=1, filter)

#mav <- function(x,n=1){filter(x,rep(1/n,n), sides=1)}

