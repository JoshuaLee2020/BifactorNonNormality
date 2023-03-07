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





install.packages("tidyverse")
library(tidyverse)
library(ggpubr)
library(rstatix)
install.packages("effectsize")
library(effectsize)

######Function 2#################################################
######################### summary anova #########################
#################################################################
result.folder <- paste0(main.folder,"/Result/parameter/")
anova.data <- read.table(paste0(result.folder,"parameter.csv"), sep=",", header = T)

#Calculate bias of parameters summary anova
bias.anova <- function(para){
  anova.data2 <- anova.data%>%
    convert_as_factor(Factor, I, N, SkewLevel.name.Fg., SkewLevel.name.Fs.)%>%
    rename(Fg=SkewLevel.name.Fg.,Fs=SkewLevel.name.Fs.,checked=file.list.checked.i..1.,t.hat=paste0(para,".hat"),t.real=paste0(para,".real"))%>%
    group_by(Factor,I,N,Fg,Fs,item.num,t.real)%>%
    summarise(t.hat.mean = mean(t.hat), .groups = "drop")%>%
    mutate(t.bias = (t.hat.mean-t.real)/t.real)

t.m <- aov(t.bias ~ Factor*I*N*Fg*Fs, data = anova.data2)
summary(t.m)
}


#Calculate bias of parameters summary anova
bias.omega <- function(para){
  anova.data2 <- anova.data%>%
    convert_as_factor(Factor, I, N, SkewLevel.name.Fg., SkewLevel.name.Fs.)%>%
    rename(Fg=SkewLevel.name.Fg.,Fs=SkewLevel.name.Fs.,checked=file.list.checked.i..1.,t.hat=paste0(para,".hat"),t.real=paste0(para,".real"))%>%
    group_by(Factor,I,N,Fg,Fs,item.num,t.real)%>%
    summarise(t.hat.mean = mean(t.hat), .groups = "drop")%>%
    mutate(t.bias = (t.hat.mean-t.real)/t.real)
  
  t.m <- lm(t.bias ~ Factor*I*N*Fg*Fs, data = anova.data2)
  omega_squared(t.m)
}


bias.anova("aG")# aG, aS, b1, b2, b3
bias.anova("aS")# aG, aS, b1, b2, b3
bias.anova("b1")# aG, aS, b1, b2, b3
bias.anova("b2")# aG, aS, b1, b2, b3
bias.anova("b3")# aG, aS, b1, b2, b3


bias.omega("aG")# aG, aS, b1, b2, b3
bias.omega("aS")# aG, aS, b1, b2, b3
bias.omega("b1")# aG, aS, b1, b2, b3
bias.omega("b2")# aG, aS, b1, b2, b3
bias.omega("b3")# aG, aS, b1, b2, b3









######Function 3#################################################
######################### AIC BIC #########################
#################################################################

Get_AIC <- function(Factor,I,N,iter){
  SkewLevel <- c(0,.4,.8)
  G <- S <- length(SkewLevel)
  SkewLevel.name <- c("Normal","Moderate","Severe")
  
  
  
  for (Fg in 1:G){
    for (Fs in 1:S){
    sim.name <- paste0(Factor,"-",I,"-",N)
    data.folder.function <- paste0(data.folder,sim.name,"/", sim.name,"-",
                                   SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"/")
    
    setwd(data.folder.function)
    
    file_list <- list.files(pattern="*irt.txt")
    post_iter <- length(file_list)#Get num of row, iteration
    
    #Get AIC from "irt.txt"
    for (i in 1:length(file_list)){
      file_str <- read_file(file_list[i])

      position <- str_locate(file_str, "Akaike.*:")
      position2 <- str_locate(file_str, "Bayesia")
      start <- as.numeric(position[1,2])+1
      end <- as.numeric(position2[1,1])-1
      AIC_str <- substr(file_str, start, end)
      AIC[i] <- as.numeric(AIC_str)
      
    }
    
    AIC_mean <- mean(AIC)
    AIC_sum[nrow(AIC_sum) + 1,] <- cbind(sim.name,skew[s],t(AIC_mean))
    #    AIC_sum[nrow(AIC_sum) + 1,] <- cbind(sim.name,skew[1],t(AIC_mean))
    }
  }
  return(AIC_sum)
}



for (Factor in c(2,3,4)){
  for (I in c(5,10)){
    for (N in c(250,500,1000)){
      AIC_sum <- Get_AIC(Factor,I,N,iter=30)
    }
  }
}


result.folder <- paste0(main.folder,"/Result/")
dir.create(result.folder)
write.csv(AIC_sum,paste0(result.folder,"AIC.csv"))


######Function 5#################################################
############################## Bias #############################
#################################################################
install.packages("tidyverse")
library(tidyverse)
library(rstatix)

result.folder <- paste0(main.folder,"/Result/parameter/")
data <- read.table(paste0(result.folder,"parameter.csv"), sep=",", header = T)
data.all <- data%>%
  convert_as_factor(Factor, I, N, SkewLevel.name.Fg., SkewLevel.name.Fs.)%>%
  rename(Fg=SkewLevel.name.Fg.,Fs=SkewLevel.name.Fs.,checked=file.list.checked.i..1.)


#Calculate bias (combining all items and get mean)

for (t in c("aG","aS","b1","b2","b3")){
data2 <- data%>%
  convert_as_factor(Factor, I, N, SkewLevel.name.Fg., SkewLevel.name.Fs.)%>%
  rename(Fg=SkewLevel.name.Fg.,Fs=SkewLevel.name.Fs.,checked=file.list.checked.i..1.,t.hat=paste0(t,".hat"),t.real=paste0(t,".real"))%>%
  group_by(Factor,I,N,Fg,Fs,item.num,t.real)%>%
  summarise(t.hat.mean = mean(t.hat), .groups = "drop")%>%
  mutate(t.bias = t.hat.mean-t.real)

data3 <- data2%>%
  group_by(Factor,I,N,Fg,Fs)%>%
  summarise(t.bias.output = mean(t.bias), .groups = "drop")%>%
  rename(!!paste0(t,".bias") := t.bias.output)

assign(paste0("data.", t), data3)
}


data.bias <- data.all %>%
  left_join(data.aG, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.aS, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.b1, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.b2, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.b3, by=c("Factor","I","N","Fg","Fs"))



######Function 6#################################################
############################## RMSE #############################
#################################################################
#install.packages("tidyverse")
#library(tidyverse)
#library(rstatix)

#result.folder <- paste0(main.folder,"/Result/parameter/")
#data <- read.table(paste0(result.folder,"parameter.csv"), sep=",", header = T)
#data.all <- data%>%
#  convert_as_factor(Factor, I, N, SkewLevel.name.Fg., SkewLevel.name.Fs.)%>%
#  rename(Fg=SkewLevel.name.Fg.,Fs=SkewLevel.name.Fs.,checked=file.list.checked.i..1.)


#Calculate bias (combining all items and get mean)
for (t in c("aG","aS","b1","b2","b3")){
  data2 <- data%>%
    convert_as_factor(Factor, I, N, SkewLevel.name.Fg., SkewLevel.name.Fs.)%>%
    rename(Fg=SkewLevel.name.Fg.,Fs=SkewLevel.name.Fs.,checked=file.list.checked.i..1.,t.hat=paste0(t,".hat"),t.real=paste0(t,".real"))%>%
    group_by(Factor,I,N,Fg,Fs,item.num,t.real)%>%
    summarise(t.hat.mean = mean(t.hat), .groups = "drop")%>%
    #square of difference for each para
    mutate(t.delta.sq = (t.hat.mean-t.real)^2)
  
  data3 <- data2%>%
    group_by(Factor,I,N,Fg,Fs)%>%
    summarise(t.RMSE.sq = mean(t.delta.sq), .groups = "drop")%>%
    mutate(t.RMSE=sqrt(t.RMSE.sq))%>%
    rename(!!paste0(t,".RMSE") := t.RMSE)%>%
    select(-t.RMSE.sq)
  
  assign(paste0("data.RMSE.", t), data3)
}


data.output <- data.bias %>%
  left_join(data.RMSE.aG, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.RMSE.aS, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.RMSE.b1, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.RMSE.b2, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.RMSE.b3, by=c("Factor","I","N","Fg","Fs"))

#Output complete table
result.folder <- paste0(main.folder,"/Result/parameter/")
dir.create(result.folder)

write.table(data.output, paste0(result.folder,"parameter_all.csv"),row.names = F, sep=",")

#Output bias and RMSE
data.bias_RMSE <- data.aG %>%
  left_join(data.aS, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.b1, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.b2, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.b3, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.RMSE.aG, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.RMSE.aS, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.RMSE.b1, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.RMSE.b2, by=c("Factor","I","N","Fg","Fs"))%>%
  left_join(data.RMSE.b3, by=c("Factor","I","N","Fg","Fs"))

write.table(data.bias_RMSE, paste0(result.folder,"bias_RMSE.csv"),row.names = F, sep=",")



######Function 7#################################################
################## Combine Skewness of theta ####################
#################################################################
library(tidyverse)

result.folder <- paste0(main.folder,"/Result/")
data.Parameter <- read.table(paste0(result.folder,"Parameter_BiasRMSE.csv"), sep=",", header = T)
data.skew.hat <- read.table(paste0(result.folder,"skewness_hat.csv"), sep=",", header = T)
data.skew.real <- read.table(paste0(result.folder,"skewness_real.csv"), sep=",", header = T)

data.output <- data.Parameter%>%
  left_join(data.skew.real, by=c("Factor","I","N","GF","SF"))%>%
  left_join(data.skew.hat, by=c("Factor","I","N","GF","SF"))


write.table(data.output, paste0(result.folder,"bias_RMSE_Skewness.csv"),row.names = F, sep=",")



######Function 8#################################################
########################## Check replication ####################
#################################################################
library(tidyverse)
library(rstatix)
main.folder <- "C:/Users/julia/OneDrive - The University of Alabama/UA/Research/Bi-factor/Code"
result.folder <- paste0(main.folder,"/Result/parameter/")
data <- read.table(paste0(result.folder,"parameter.csv"), sep=",", header = T)
data.t <- data%>%
  convert_as_factor(Factor, I, N, SkewLevel.name.Fg., SkewLevel.name.Fs.)%>%
  filter(Factor=="4"& I=="10"& N=="1000"& SkewLevel.name.Fg.=="Severe"& SkewLevel.name.Fs.=="Severe"& item.num=="1")



library(ggplot2)
library(grid)


aG <- ggplot(data.t, aes(x=aG.hat)) + geom_density()
aS <- ggplot(data.t, aes(x=aS.hat)) + geom_density()
b1 <- ggplot(data.t, aes(x=b1.hat)) + geom_density()
b2 <- ggplot(data.t, aes(x=b2.hat)) + geom_density()
b3 <- ggplot(data.t, aes(x=b3.hat)) + geom_density()


library(patchwork)
aG+aS+b1+b2+b3+plot_layout(ncol = 2)

plot(data.t$aG.hat)


######Function 9#################################################
#################### Check FlexMIRT Convergence #################
#################################################################

Convergence <- function(Factor,I,N,iter){
  SkewLevel <- c(0,.4,.8)
  G <- S <- length(SkewLevel)
  SkewLevel.name <- c("Normal","Moderate","Severe")
  
  for (Fg in 1:G){
    for (Fs in 1:S){
      sim.name <- paste0(Factor,"-",I,"-",N)
      data.folder.function <- paste0(data.folder,sim.name,"/", sim.name,"-",
                                     SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"/")
      
      setwd(data.folder.function)
      
      file_list <- list.files(pattern="*irt.txt")
      post_iter <- length(file_list)#Get num of row, iteration
      
      #Get AIC from "irt.txt"
      for (i in 1:length(file_list)){
        file_str <- read_file(file_list[i])
        
        position <- str_locate(file_str, "Akaike.*:")
        position2 <- str_locate(file_str, "Bayesia")
        start <- as.numeric(position[1,2])+1
        end <- as.numeric(position2[1,1])-1
        AIC_str <- substr(file_str, start, end)
        AIC[i] <- as.numeric(AIC_str)
        
      }
      
      AIC_mean <- mean(AIC)
      AIC_sum[nrow(AIC_sum) + 1,] <- cbind(sim.name,skew[s],t(AIC_mean))
      #    AIC_sum[nrow(AIC_sum) + 1,] <- cbind(sim.name,skew[1],t(AIC_mean))
    }
  }
  return(AIC_sum)
}



for (Factor in c(2,3,4)){
  for (I in c(5,10)){
    for (N in c(250,500,1000)){
      AIC_sum <- Get_AIC(Factor,I,N,iter=30)
    }
  }
}
