rm(list=ls())

Factor <- 2
I <- 5
N <- 250
Fg <- 3
Fs <- 3
i <- 1
rep <- 30

Factor <- 4
I <- 10
N <- 1000
Fg <- 3
Fs <- 3
i <- 1
rep <- 30

library(tidyr)
library(dplyr)
library(readr)
library(stringr)
######Function ##################################################
################## Check Convergence FlexMIRT ###################
#################################################################
table.all <- matrix(NA,1,10)
colnames(table.all) <- c("id","irtfile","irtready","convergence","Factor","I","N","Fg","Fs","rep")

Con.flex <- function(Factor,I,N,rep){

  
  main.folder <- "C:/Research/BifactorNonNormality"
  data.folder <- paste0(main.folder,"/Data/")
  
  SkewLevel <- c(0,1.5,2.5)
  G <- S <- length(SkewLevel)
  SkewLevel.name <- c("Normal","Moderate","Severe")
  
  #id.dir1 Combination
  id.com <- paste0(Factor,"-",I,"-",N)
  


  for (Fg in 1:G){
    for (Fs in 1:S){
      data.folder.function <- paste0(data.folder,id.com,"/", id.com,"-",
                                     SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"/")
      setwd(data.folder.function)
      
      #Check FlexMIRT irt.txt files
      file_list <- list.files(pattern="*irt.txt")
      post_iter <- length(file_list)
      
      file_list_all <- paste0("Syn",id.com,"-",SkewLevel.name[Fg],"-",SkewLevel.name[Fs],
                              "-",1:rep,"-","irt.txt")
      
      check_irt_miss <- setdiff(file_list_all,file_list)
      
      file_list2 <- paste0(data.folder.function,file_list)     
      
      #Create result table
      table <- matrix("Miss",rep,10)
      colnames(table) <- c("id","irtfile","irtready","convergence","Factor","I","N","Fg","Fs","rep")

      table[,2] <- file_list_all 
      table[which(table[,2] %in% file_list),3] <- "ok"
      
        
      #Get Convergence from "irt.txt"
      id <- vector()
      convergence <- vector()

      
      for (i in 1:rep){
        id[i] <- paste0(id.com,"-",SkewLevel.name[Fg],"-",SkewLevel.name[Fs],"-",i)

        #Get string from existing irt.txt files        
        if (table[i,3]=="ok"){
          file_str <- read_file(paste0(data.folder.function,table[i,2]))
        }

        #Get Convergence Setting
        position <- str_locate(file_str, "Maximum number of cycles:")
        position2 <- str_locate(file_str, "Convergence criterion")
        start <- as.numeric(position[1,2])+1
        end <- as.numeric(position2[1,1])-1
        Convergence_setting_str <- substr(file_str, start, end)
        Convergence_setting <- gsub("[^[:digit:]]", "", Convergence_setting_str)

        #Get Convergence cycles completed
        position3 <- str_locate(file_str, "Number of cycles completed:")
        position4 <- str_locate(file_str, "Maximum parameter change")
        start2 <- as.numeric(position3[1,2])+1
        end2 <- as.numeric(position4[1,1])-1
        Convergence_completed_str <- substr(file_str, start2, end2)
        Convergence_completed <- gsub("[^[:digit:]]", "", Convergence_completed_str)       
        
        convergence[i] <- ifelse (Convergence_setting!=Convergence_completed, Convergence_completed, 0)
        
        table[i,1] <- id[i]
        table[i,4] <- convergence[i]
        table[i,5] <- Factor
        table[i,6] <- I
        table[i,7] <- N
        table[i,8] <- Fg
        table[i,9] <- Fs
        table[i,10] <- i
     
      }
      table.all <- rbind(table.all,table) 
    }
  }
  return(table.all)
}



for (Factor in c(2,3,4)){
  for (I in c(5,10)){
    for (N in c(250,500,1000)){
      table.all <- Con.flex(Factor,I,N,rep=30)
    }
  }
}


#table.all <- Con.flex(2,5,250,rep=30)


result.folder <- paste0(main.folder,"/Result/")
dir.create(result.folder)
write.csv(table.all,paste0(result.folder,"convegence_FlexMIRT.csv"))




######Function2 ################################################################
####### Creat FlexMIRT File with more E Cycles (4000) ##########################
################################################################################

# identify the folders
main.folder <- "C:/Research/BifactorNonNormality"
data.folder <- paste0(main.folder,"/Data/")
convergence2nd.folder <- paste0(main.folder,"/Data_Convergence_2nd/")
result.folder <- paste0(main.folder,"/Result/")

dir.create(convergence2nd.folder)


################################################################################
# Copy all simulated response data (data.csv) into 2nd convergence2nd folder
for (Factor in c(2,3,4)){
  for (I in c(5,10)){
    for (N in c(250,500,1000)){
      new.folder.1 <- paste0(convergence2nd.folder,Factor,"-",I,"-",N,"/")
      dir.create(new.folder.1)      
            
      for (Fg.level in c("Normal","Moderate","Severe")){
        for (Fs.level in c("Normal","Moderate","Severe")){

          current.folder <- paste0(data.folder,Factor,"-",I,"-",N,"/",Factor,
                                   "-",I,"-",N,"-",Fg.level,"-",Fs.level,"/")
          
          new.folder.2 <- paste0(convergence2nd.folder,Factor,"-",I,"-",N,"/",Factor,
                               "-",I,"-",N,"-",Fg.level,"-",Fs.level,"/")
          dir.create(new.folder.2)
          
          list.of.files <- list.files(current.folder, "data.csv$")
          file.copy(paste0(current.folder,list.of.files), new.folder.2)
        }
      }
    }
  }
}


# Read list of no converged FlexMIRT files
file_convergence <- read_csv(paste0(result.folder,"convegence_FlexMIRT.csv"))



SkewLevel.name <- c("Normal","Moderate","Severe")

### write flexmirt syntax file with 4000 E cycles
file_no_converged <- file_convergence %>%
  filter(convergence==0)

data.name <- vector()
syn.name <- vector()

for (i in 1:nrow(file_no_converged)){
  #Get parameters from file_no_converged
  Factor <- file_no_converged$Factor[i]
  I <- file_no_converged$I[i]  
  N <- file_no_converged$N[i]
  Fg <- file_no_converged$Fg[i]
  Fs <- file_no_converged$Fs[i]
  rep <- file_no_converged$rep[i]
  id <- file_no_converged$id[i]
  
  data.name <- paste0(id,"-data.csv")
  syn.name <- paste0(convergence2nd.folder,Factor,"-", I,"-", N,"/",Factor,"-", I,"-", N,"-",SkewLevel.name[Fg],"-",SkewLevel.name[Fs], "/","Syn", id,".flexmirt")

  
  Flex.Constraints <- c()
  FC<- c()
  
  for (j in 1:Factor){
    FC <- paste0("Free(v",(j-1)*I+1,"-v",j*I,"), Slope(",j+1,");")
    Flex.Constraints <- paste(Flex.Constraints,FC)
  }
  
  flexsyn <- paste0("
        <Project>
          Title = '",syn.name,"';
        Description = 'Output from r';
        
        <Options>
          Mode = Calibration;
          saveSCO = Yes;
          Score = MAP;
          GOF = Complete;
          Quadrature = 21,3.5;
          Processors = 8;
          SavePRM = Yes;
          MaxE = 4000;
        
        <Groups>
          %Group1%
          File = '",data.name,"';
          Varnames = v1-v",Factor*I,";
          N = ",N,";
          Ncats(v1-v",Factor*I,") = 4;
          Model(v1-v",Factor*I,") = graded(4);
          Dimensions = ",Factor+1,";
          Primary = 1;
        
        <Constraints>
          Fix(v1-v",Factor*I,"), Slope;
          Free(v1-v",Factor*I,"), Slope(1);",
                    Flex.Constraints)
  
  writeLines(flexsyn, syn.name)  
  
}








rep=30
Factor=4
I=10
N=1000
Fg.level="Severe"
Fs.level="Severe"



