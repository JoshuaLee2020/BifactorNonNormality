rm(list=ls())
library(tidyr)
library(dplyr)
library(readr)
library(parallel)
library(mirt)

main.folder <- "C:/Research/BifactorNonNormality"
data.folder <- paste0(main.folder,"/Data/")

iter=100
Factor=4
I=10
N=1000
Fg=3
Fs=3
i=1
rep=1


######Function ##################################################
############## Estimate para via mirt::bfactor ##################
#################################################################

bfactors <- function(vec){
  Factor=as.numeric(vec["Factor"]) 
  I=as.numeric(vec["I"])
  N=as.numeric(vec["N"])
  rep=as.numeric(vec["rep"])
  
  library(tidyr)
  library(dplyr)
  library(readr)
  library(parallel)
  library(mirt)
  
  main.folder <- "C:/Research/BifactorNonNormality"
  data.folder <- paste0(main.folder,"/Data/")
  
  #Define skewness level
  SkewLevel <- c(0,1.5,3)
  G <- S <- length(SkewLevel)
  SkewLevel.name <- c("Normal","Moderate","Severe")
  

  
  #Define specific for bfactor() 
  bfactor.index <- matrix(rep(NA,Factor*I),I)
  for (i in 1:Factor){
    bfactor.index[,i] <- rep(i,I)
  }
  bfactor.specific <- as.vector(bfactor.index)
  
  #Define model for bfactor()   
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

  #bfactor.model
  for (Fg in 1:G){
    for (Fs in 1:S){
      data.folder.read <- paste0(data.folder,Factor,"-",I,"-",N,"/",Factor,"-",I,"-",N,"-",
                                 SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"/")
      setwd(data.folder.read)
      print(data.folder.read)      
      for (i in 1:rep){
        #Read response data from csv
        namehead <- paste0(Factor,"-",I,"-",N,"-",
                            SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"-",i)
        data.name <- paste0(namehead,"-data.csv")
        mirtpara.name <- paste0(data.folder.read, namehead, "-mirtpara.csv")
        
        print(paste("|||||ESTIMATING:",data.name,"|||||")) 
        df <- read.csv(paste0(data.folder.read, data.name),header = FALSE)
        
        #Run mirt::bfactor()
        mod <- mirt::bfactor(df, bfactor.specific, bfactor.model, technical=list(NCYCLES=1000))

        #Write mirtpara.csv
        para <- coef(mod,simplify=TRUE)
        return(para)
        para2 <- para$items
        #If the result is converged, convergence time will be added, else "0" will be shown.
        converge.iter <- ifelse  (mod@OptimInfo[["converged"]],mod@OptimInfo[["iter"]],0)
        para3 <- cbind(para2,converge.iter)
        write.csv(para3,mirtpara.name,na = "NA")
        #write.csv(para2,mirtpara.name,na = "NA")
      }
    }
  }
}  


detectCores()

# Create a cluster
cl <- makeCluster(12)

# Generate the input combinations using expand.grid
grid <- expand.grid(c(2,3,4),c(5,10),c(250,500,1000),30)
colnames(grid) = c("Factor","I","N","rep")

clusterExport(cl, c("grid"))

now <- Sys.time()
parRapply(cl,grid,bfactors)
#parLapply(cl,grid, bfactors, time)
Sys.time()-now

stopCluster(cl)

sapply(grid,bfactors)
