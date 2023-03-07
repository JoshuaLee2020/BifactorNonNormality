rm(list=ls())
library(tidyr)
library(dplyr)
main.folder <- "C:/Research/BifactorNonNormality"
data.folder <- paste0(main.folder,"/Data/")

iter=100
Factor=4
I=10
N=1000
Fg=3
Fs=3



######Function 1#################################################
######################### Get parameter #########################
#################################################################

parameter <- function(Factor,I,N,iter){
  SkewLevel <- c(0,1.5,3)
  G <- S <- length(SkewLevel)
  SkewLevel.name <- c("Normal","Moderate","Severe")
  
  item.num <- seq(1:(Factor*I))
  
  for (Fg in 1:G){
    for (Fs in 1:S){
      
      
      
      sim.name <- paste0(Factor,"-",I,"-",N)
      data.folder.function <- paste0(data.folder,sim.name,"/", sim.name,"-",
                                     SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"/")
      setwd(data.folder.function)
      
      #Build the file list of real parameter and hat parameter, column: 1-id, 2-real, 3-estimated      
      file.list <- matrix(seq(1:iter),ncol = 1)
      file.list <- cbind(file.list,paste0(sim.name,"-",SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"-",seq(1:iter),"-realparameters.csv"))
      file.list <- cbind(file.list,paste0("Syn",sim.name,"-",SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"-",seq(1:iter),"-prm.txt"))
      colnames(file.list) <- c("id","real","estimated")
      #Check iterations not convergenced
      file.list.check <- list.files(pattern="*prm.txt")
      file.list.checked <- file.list[(file.list[,3] %in% file.list.check),]
      #noconvergence.list[nrow(noconvergence.list)+1,1:3]<- t(as.matrix(file.list[!(file.list[,3] %in% file.list.check),1:3]))      
      #iter <- nrow(file.list.checked)       
      
      for (i in 1:nrow(file.list.checked)){
        #input parameter.hat  
        para.data.hat <- read.delim(file.list.checked[i,3] , header = FALSE)
        para.data.hat <- head(para.data.hat, - 2) 
        para.data.hat <- para.data.hat[,-(1:6)]
        colnames(para.data.hat) <- c("c3.hat","c2.hat","c1.hat","aG.hat",paste0("aS.hat",1:(length(para.data.hat)-4)))
        para.data.hat$aS.hat <- rowSums(para.data.hat[,5:ncol(para.data.hat)])
        para.data.hat.aS <- para.data.hat[,c("aG.hat","aS.hat","c3.hat","c2.hat","c1.hat")]
        
        #input parameter.real from each realparameters.csv      
        para.data.real <- read.csv(file.list.checked[i,2],header = FALSE)
        colnames(para.data.real) <- c("aG.real",paste0("aS.real",1:Factor),"c3.real","c2.real","c1.real")
        para.data.real.aS <- unite(para.data.real, aS.real, 2:(Factor+1),na.rm=TRUE)
        para.data.real.aS <- para.data.real.aS[,c("aG.real","aS.real","c3.real","c2.real","c1.real")]
        
        #Combine parameter.hat and parameter.real      
        para.data.iter <- cbind(Factor, I, N,SkewLevel.name[Fg],SkewLevel.name[Fs],item.num,file.list.checked[1],para.data.real.aS,para.data.hat.aS,I)
        
        #Gather all parameters of each iterations
        para.data<- rbind(para.data,para.data.iter)
      }
    }
  }
  return(para.data)
}
para.data <- data.frame()
parameter(4,10,1000,100)

######RUN########################################
##########Run Function "parameter.hat"###########
#################################################

#Get a clear table.para.full for storing parameters.
para.data <- data.frame()

for (Factor in c(2,3,4)){
  for (I in c(5,10)){
    for (N in c(250,500,1000)){
      para.data <- parameter(Factor,I,N)
    }
  }
}

#Output results of skewness of theta.hat
result.folder <- paste0(main.folder,"/Result_test/")
dir.create(result.folder)

write.table(para.data, paste0(result.folder,"parameter.csv"),row.names = FALSE, sep=",")


