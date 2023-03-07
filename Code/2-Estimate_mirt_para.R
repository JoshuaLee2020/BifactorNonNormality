rm(list=ls())
library(tidyr)
library(dplyr)
library(readr)
library(parallel)
library(mirt)

main.folder <- "C:/Research/BifactorNonNormality"
data.folder <- paste0(main.folder,"/Data/")


Factor=3
I=5
N=250
Fg=3
Fs=3
i=1
rep=1


######Function ##################################################
############## Estimate para via mirt::bfactor ##################
#################################################################

#bfactors <- function(vec){
  

  #function for bfactor in parallel 
  bfactorpara <- function(vec) {
    library(tidyr)
    library(dplyr)
    library(readr)
    library(parallel)
    library(mirt)
    
    Fg=as.numeric(vec["Fg"]) 
    Fs=as.numeric(vec["Fs"]) 
    rep=as.numeric(vec["rep"])
    Factor=as.numeric(vec["Factor"]) 
    I=as.numeric(vec["I"])
    N=as.numeric(vec["N"])

    main.folder <- "C:/Research/BifactorNonNormality"
    data.folder <- paste0(main.folder,"/Data/")
    
    
    #Define "specific" para for bfactor() 
    bfactor.index <- matrix(rep(NA,Factor*I),I)
    for (i in 1:Factor){
      bfactor.index[,i] <- rep(i,I)
    }
    bfactor.specific <- as.vector(bfactor.index)
    
    #Define "model" para for bfactor()   
    bfactor.model.1 <- matrix(seq(1:(Factor*I)),I)
    bfactor.model.2 <- ",a1,a"
    bfactor.model.3 <- bfactor.index+1
    bfactor.model.4 <- "), ("
    bfactor.model.all <- paste0(bfactor.model.1 , bfactor.model.2 , bfactor.model.3,"), (")
    bfactor.model.end <- ""
    for (i in 1:Factor) {
      if (i > 1) bfactor.model.end <- paste(bfactor.model.end, ",")
      bfactor.model.end <- paste(bfactor.model.end, paste0("S", i, "*S", i))
    }
    bfactor.model <- paste(bfactor.model.all, collapse = "")
    bfactor.model <- substring(bfactor.model, 1, nchar(bfactor.model)-3)
    bfactor.model <- paste0("G = 1-",Factor*I,"\n","CONSTRAIN = (",bfactor.model,"\n","COV = ",bfactor.model.end)
    

    #Define skewness level
    SkewLevel <- c(0,1.5,2.5)
    G <- S <- length(SkewLevel)
    SkewLevel.name <- c("Normal","Moderate","Severe")
    
    
    data.folder.read <- paste0(data.folder,Factor,"-",I,"-",N,"/",Factor,"-",I,"-",N,"-",
                                 SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"/")

    
    #Read response data from csv
    namehead <- paste0(Factor,"-",I,"-",N,"-",
                            SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"-",rep)
      #input
    data.name <- paste0(data.folder.read, namehead,"-data.csv")
      #output
    mirtpara.name <- paste0(data.folder.read, namehead, "-mirtpara.csv")
    mirttheta.map.name <- paste0(data.folder.read, namehead, "-mirttheta_map.csv")
    mirttheta.ml.name <- paste0(data.folder.read, namehead, "-mirttheta_ml.csv")

    

    
    
    print(data.name)        
    df <- read.csv(data.name,header = FALSE)
    
    #Run mirt::bfactor()
    #mod <- mirt::bfactor(df, bfactor.specific, bfactor.model, technical=list(NCYCLES=1000))
    mod <- mirt::bfactor(df, bfactor.specific, technical=list(NCYCLES=2000))

    #Write mirtpara.csv
    para <- coef(mod,simplify=TRUE)
    para2 <- para$items
    
    #If the result is converged, convergence time will be added, else "0" will be shown.
    converge.iter <- ifelse  (mod@OptimInfo[["converged"]],mod@OptimInfo[["iter"]],0)
    para3 <- cbind(para2,converge.iter)
    write.csv(para3, mirtpara.name, na = "NA")
    
    #Calculate latent ability (theta.mirt)
    scores.map <- fscores(mod, method='MAP')
    scores.ml <- fscores(mod, method='ML')
    
    #Write mirttheta.csv
    write.csv(scores.map, mirttheta.map.name, na = "NA")
    write.csv(scores.ml, mirttheta.ml.name, na = "NA")   
  }    
    # 
    # real <- read.csv(paste0(data.folder.read,namehead,"-realtheta.csv"))
    # 
    # 
    # summary(real[,1])
    #    
    # ggplot()+
    #   geom_density(aes(x = real[,1]))+
    #   geom_density(aes(x = scores.map[,1] , color="#000000"))+
    #   geom_density(aes(x = scores.ml[,1], color="#001231"))   
    # 
    # ggplot()+
    #   geom_density(aes(x = real[,2]))+
    #   geom_density(aes(x = scores.map[,2] , color="#000000"))+
    #   geom_density(aes(x = scores.ml[,2], color="#001231"))   
    # ggplot()+
    #   geom_density(aes(x = real[,3]))+
    #   geom_density(aes(x = scores.map[,3] , color="#000000"))+
    #   geom_density(aes(x = scores.ml[,3], color="#001231"))   
    # 
    # 
    # ggplot()+
    #   geom_density(aes(x = theta[,1]))
    # 
    # 
    # ggplot()+
    #   geom_density(aes(x = real[,2]))+
    #   geom_density(aes(x = scores.map[,2] , color="#000000"))+
    #   geom_density(aes(x = scores.ml[,2], color="#001231"))   
    # ggplot()+
    #   geom_density(aes(x = real[,3]))+
    #   geom_density(aes(x = scores.map[,3] , color="#000000"))+
    #   geom_density(aes(x = scores.ml[,3], color="#001231"))     
    

  






runbf <- function(Factor,I,N,rep){
  Fg=3
  Fs=3

  
  #Run function for bfactor in parallel
  cl <- makeCluster(12)
  grid <- expand.grid(1:Fg,1:Fs,1:rep,Factor,I,N)
  colnames(grid) <- c("Fg","Fs","rep","Factor","I","N")
  clusterExport(cl, c("grid"))
  

  now <- Sys.time()
  parRapply(cl,grid,bfactorpara)
  Sys.time()-now
  stopCluster(cl)
}  






for (Factor in c(2,3,4)){
  for (I in c(5,10)){
    for (N in c(250,500,1000)){
      runbf(Factor,I,N,rep=100)
    }
  }
}


system.time()
runbf(2,5,250,rep=100)
system.time()
runbf(2,5,500,rep=100)
system.time()
runbf(2,5,1000,rep=100)

system.time()
runbf(2,10,250,rep=100)
system.time()
runbf(2,10,500,rep=100)
system.time()
runbf(2,10,1000,rep=100)



system.time()
runbf(3,5,250,rep=100)
system.time()
runbf(3,5,500,rep=100)
system.time()
runbf(3,5,1000,rep=100)

system.time()
runbf(3,10,250,rep=100)
system.time()
runbf(3,10,500,rep=100)
system.time()
runbf(3,10,1000,rep=100)



system.time()
runbf(4,5,250,rep=100)
system.time()
runbf(4,5,500,rep=100)
system.time()
runbf(4,5,1000,rep=100)

system.time()
runbf(4,10,250,rep=100)
system.time()
runbf(4,10,500,rep=100)
system.time()
runbf(4,10,1000,rep=100)
