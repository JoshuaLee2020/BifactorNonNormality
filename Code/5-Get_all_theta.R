rm(list=ls())
library(tidyr)
library(dplyr)
library(readr)
library(parallel)
library(mirt)
library(stringr)
main.folder <- "C:/Research/BifactorNonNormality"
data.folder <- paste0(main.folder,"/Data/")
result.folder <- paste0(main.folder,"/Result/")



#For Test Code
rep=100
Factor=3
I=5
N=250
Fg=1
Fs=3
i=480


################################################################################
################################ Get Parameters ################################
################################################################################
###Set skewness level
SkewLevel <- c(0,1.5,2.5)
G <- S <- length(SkewLevel)
SkewLevel.name <- c("Normal","Moderate","Severe")
  
# Input all files' name and combine real_parameters files' name
files <- read.csv(paste0(result.folder,"convergence.csv")) 


files.all <- files %>%
  mutate(
    realtheta = paste0(id.rep,"-realtheta.csv"),
    mirttheta_map = paste0(id.rep,"-mirttheta_map.csv"),
    mirttheta_ml = paste0(id.rep,"-mirttheta_ml.csv"),         
    data.folder.function = paste0(data.folder,Factor,"-",I,"-",N,"/", 
                                  Factor,"-",I,"-",N,"-",Fg,"-",Fs,"/")
  )

head(files.all)

theta.data.all <- data.frame()
for (i in 1:nrow(files.all)){
  if (!file.exists(files.all[i,17])) {break}  
  setwd(files.all[i,17])

  Factor <- as.numeric(files.all[i,8])
  I <- as.numeric(files.all[i,9])
  N <- as.numeric(files.all[i,10])
  
  #Get id.rep and theta.id
  id.rep <- files.all[i,1]
  theta.id <- 1:N
    
  #input theta.real from each realtheta.csv      
  if (file.exists(files.all[i,14])) {
    theta.data.real <- read.csv(files.all[i,14],header = FALSE)
    theta.data.real <- theta.data.real %>%
      mutate(thetaG.real = theta.data.real[,1], thetaS.avg.real = rowMeans(theta.data.real[,-1]))%>%
      select(thetaG.real,thetaS.avg.real)
  } else {
    theta.data.real <- data.frame(matrix(NA,N,2))
    colnames(theta.data.real) <- c("thetaG.real","thetaS.avg.real")
  }
  
  
  #input mirttheta_map.csv
  if (file.exists(files.all[i,15])) {     
    theta.data.mirt.map <- read.csv(files.all[i,15],header = TRUE)
    theta.data.mirt.map <- theta.data.mirt.map %>%
      mutate(thetaG.mirt.map = theta.data.mirt.map[,2], thetaS.avg.mirt.map = rowMeans(theta.data.mirt.map[,-c(1,2)]))%>%
      select(thetaG.mirt.map,thetaS.avg.mirt.map)
  } else {
    theta.data.mirt.map <- data.frame(matrix(NA,N,2))
    colnames(theta.data.mirt.map) <- c("thetaG.mirt.map","thetaS.avg.mirt.map")
  }
  
  #input mirttheta_ml.csv
  if (file.exists(files.all[i,16])) {     
    theta.data.mirt.ml <- read.csv(files.all[i,16],header = TRUE)
    theta.data.mirt.ml <- theta.data.mirt.ml %>%
      mutate(thetaG.mirt.ml = theta.data.mirt.ml[,2], thetaS.avg.mirt.ml = rowMeans(theta.data.mirt.ml[,-c(1,2)]))%>%
      select(thetaG.mirt.ml,thetaS.avg.mirt.ml)
  } else {
    theta.data.mirt.ml <- data.frame(matrix(NA,N,2))
    colnames(theta.data.mirt.ml) <- c("thetaG.mirt.ml","thetaS.avg.mirt.ml")
  }
  
  
       
  #Combine  parameters.real, parameters.FlexMIRT and parameters.mirt
  theta.data <- cbind(id.rep, theta.id, theta.data.real, theta.data.mirt.map, theta.data.mirt.ml)
  
  #Gather all parameters of each replications
  theta.data.all<- rbind(theta.data.all,theta.data)
}
  


#Merge convergence status and parameters
#theta.data_file.all <- left_join(theta.data.all, files.all ,by="id.rep")
theta.data_file.all <- merge(theta.data.all, files, by="id.rep", all.x=TRUE)


###Output results of skewness of theta.FlexMIRT
result.folder <- paste0(main.folder,"/Result/")
dir.create(result.folder)
write.table(theta.data_file.all, paste0(result.folder,"thetas.csv"),row.names = FALSE, sep=",")











###'################
###'Testing



library(ggplot2)
colnames(theta.data.real)  <- c("thetaG","thetaS1","thetaS2","thetaS3")    

ggplot(data=theta.data.real)+
  geom_density(aes(x = thetaG, color = "thetaG")) +
  geom_density(aes(x = thetaS1, color = "thetaS1")) +
  geom_density(aes(x = thetaS2, color = "thetaS2")) +
  geom_density(aes(x = thetaS3, color = "thetaS3")) 
