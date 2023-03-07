rm(list=ls())
library(tidyr)
library(dplyr)
library(readr)
library(parallel)
library(mirt)
library(stringr)
main.folder <- "C:/Research/BifactorNonNormality"
data.folder <- paste0(main.folder,"/Data/")


###Set skewness level
SkewLevel <- c(0,1.5,2.5)
G <- S <- length(SkewLevel)
SkewLevel.name <- c("Normal","Moderate","Severe")

rep=200
Factor=3
I=5
N=250
Fg=3
Fs=3



######Function ####################################################################
################################ Get Convergence ##################################
###################################################################################

Convergence <- function(Factor,I,N,rep){
  SkewLevel <- c(0,1.5,2.5)
  G <- S <- length(SkewLevel)
  SkewLevel.name <- c("Normal","Moderate","Severe")
  
  for (Fg in 1:G){
    for (Fs in 1:S){

      id.rep <- vector()
      id.item <- vector()
      
      id.com <- paste0(Factor,"-",I,"-",N)
      data.folder.function <- paste0(data.folder,id.com,"/", id.com,"-",
                                     SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"/")
      
      if (!file.exists(data.folder.function)) {next}
      setwd(data.folder.function)
      
      ###Create result table
      table <- matrix("Miss",rep,13)
      colnames(table) <- c("id.rep","Flexirtfile","Flexirtready","Flexirtconvergence","mirtcsvfile","mirtcsvready","mirt::bfconvergence","Factor","I","N","Fg","Fs","rep")
      
      ###Create unique id for each condition and each replication
      for (i in 1:rep){
        table[i,1] <- id.rep[i]<- paste0(id.com,"-",SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"-",i)
      }

            
      ###########################################################################
      ###Add FlexMIRT results
      
      ###Check whether irt.txt is existed, for checking convergence 
      table[,2] <- paste0("Syn",id.com,"-",SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"-",seq(1:rep),"-irt.txt")
      
      file.list.check.Flex <- list.files(pattern="*irt.txt")      
      table[which(table[,2] %in% file.list.check.Flex),3] <- "ok"
      
      ###Get Convergence from "irt.txt"
      convergence <- vector()
      
      for (i in 1:rep){
        ###Get string from existing irt.txt files        
        if (table[i,3]=="ok"){
          file_str <- read_file(paste0(data.folder.function,table[i,2]))
        
          ###Get Convergence Setting
          position <- str_locate(file_str, "Maximum number of cycles:")
          position2 <- str_locate(file_str, "Convergence criterion")
          start <- as.numeric(position[1,2])+1
          end <- as.numeric(position2[1,1])-1
          Convergence_setting_str <- substr(file_str, start, end)
          Convergence_setting <- gsub("[^[:digit:]]", "", Convergence_setting_str)
          
          ###Get Convergence cycles completed
          position3 <- str_locate(file_str, "Number of cycles completed:")
          position4 <- str_locate(file_str, "Maximum parameter change")
          start2 <- as.numeric(position3[1,2])+1
          end2 <- as.numeric(position4[1,1])-1
          Convergence_completed_str <- substr(file_str, start2, end2)
          Convergence_completed <- gsub("[^[:digit:]]", "", Convergence_completed_str)       
          
          convergence[i] <- ifelse (Convergence_setting!=Convergence_completed, Convergence_completed, 0)
          
          table[i,4] <- convergence[i]
        } else {
          table[i,4] <- 0
        }
        
      }   
      

      ###########################################################################
      ###Add mirt::bfactor() results
      
      ###Check whether mirtpara.csv is existed, for checking convergence 
      table[,5] <- paste0(id.com,"-",SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"-",seq(1:rep),"-mirtpara.csv")
      
      file.list.check.mirt <- list.files(pattern="*-mirtpara.csv")      
      table[which(table[,5] %in% file.list.check.mirt),6] <- "ok"     
      
      #####Here, we do not analysis convergence of mirtpara. 
      #####We have prepare convergence of mirtpara and store them in mirtpara.csv.
      for (i in 1:rep){
        ###Get table from existing mirtpara.csv files        
        if (table[i,6]=="ok"){
          mirtpara <- read_csv(paste0(data.folder.function,table[i,5]),show_col_types = FALSE)
        
          ###Get first convergence in mirtpara.csv as the convergence for this replication,
          ###because only one convergence status for all items in one replication.
          table[i,7] <- mirtpara$converge.iter[1]
          table[i,8] <- Factor
          table[i,9] <- I
          table[i,10] <- N
          table[i,11] <- SkewLevel.name[Fg]
          table[i,12] <- SkewLevel.name[Fs]
          table[i,13] <- i
        }
      }
      table.all.convergence<- rbind(table.all.convergence,table)      
    

    }#End of loop Fs
  }#End of loop Fg
  return(table.all.convergence)  
}





######RUN########################################
##########Run Function Convergence()#############
#################################################

#Get a clear table.para.full for storing parameters.

table.all.convergence <- data.frame(matrix("NA",0,13))
colnames(table.all.convergence) <- c("id.rep","Flexirtfile","Flexirtready","Flexirtconvergence","mirtcsvfile","mirtcsvready","mirt::bfconvergence","Factor","I","N","Fg","Fs","rep")

table.all.convergence <- Convergence(3,5,250,100)

for (Factor in c(2,3,4)){
  for (I in c(5,10)){
    for (N in c(250,500,1000)){
      table.all.convergence <- Convergence(Factor,I,N,100)
    }
  }
}

#Output results of skewness of theta.hat
result.folder <- paste0(main.folder,"/Result/")
dir.create(result.folder)

write.table(table.all.convergence, paste0(result.folder,"convergence.csv"),row.names = FALSE, sep=",")






###Check the ratio of converged condition for FlexMIRT
table.all.convergence <- read.csv(paste0(result.folder,"convergence.csv"))
convergence.ana <- table.all.convergence %>%
  mutate(condition=paste0(Factor,"-",I,"-",N))%>%
  group_by(condition)%>%
  summarise(Flex.con.ratio = sum(Flexirtconvergence!=0) / n(), Flex.no.irt.ratio = sum(Flexirtconvergence!=0) / n())
write.table(convergence.ana, paste0(result.folder,"convergence_rate_Flex.csv"),row.names = FALSE, sep=",")

###Check the ratio of converged condition for mrit:bfactor
table.all.convergence <- read.csv(paste0(result.folder,"convergence.csv"))
convergence.ana <- table.all.convergence %>%
  mutate(condition=paste0(Factor,"-",I,"-",N))%>%
  group_by(condition)%>%
  summarise(con.ratio = sum(mirt..bfconvergence!=0) / n())
write.table(convergence.ana, paste0(result.folder,"convergence_rate_mirt.csv"),row.names = FALSE, sep=",")



