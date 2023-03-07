rm(list=ls())
library(mirt)
library(covsim)
main.folder <- "C:/Research/BifactorNonNormality"
data.folder <- paste0(main.folder,"/Data/")
dir.create(data.folder)


################################################################################
######### Sim Data and Output Flexmirt Execute Files ###########################
################################################################################
Factor <- 4
I <- 10
N <- 1000

  K <- Factor + 1 # number of all Factors (General F + Specific F)
  
  #Skewness of theta (personal ability)
  SkewLevel <- c(0,1.5,2.5)
  G <- S <- length(SkewLevel)
  SkewLevel.name <- c("Normal","Moderate","Severe")
  
  for (Fg in 2:3){
      
      ############# Work folder setting ########################################  
      setwd(data.folder)
      data.2.folder <- paste0(data.folder,Factor,"-",I,"-",N,"/")
      dir.create(data.2.folder) 
      data.3.folder <- paste0(data.2.folder,Factor,"-",I,"-",N,"-",SkewLevel.name[Fg],"-",SkewLevel.name[Fg],"/")
      dir.create(data.3.folder)
      
      
      for (iter in 1:200){ 
        ########################################################################
        ############# parameter Setting#########################################
        #a[,1] is discrimination of general factor (slope parameters in mirt::simdata)
        a <- matrix(rep(NA,Factor*I*K),Factor*I)
        a[,1] <- runif(Factor*I,1.1,2.8)
        
        #a[,2:K] is discrimination of specific factor (slope parameters in mirt::simdata)
        for (i in 2:K){
          a[((i-2)*I+1):((i-2)*I+I),i] <- runif(I,0,1.5)
        }
        
        #d is threshold (a matrix/vector of intercepts in mirt::simdata)
        #We have 4 categories with 3 thresholds
        d <- matrix(rep(NA,Factor*I*3),Factor*I)
        d[,3] <- runif(Factor*I,-2,-0.67)
        d[,2] <- runif(Factor*I,-0.67,0.67)
        d[,1] <- runif(Factor*I,0.67,2)
        ########################################################################
        
        
        
        
        ########################################################################
        ############# Theta [Skewed]############################################
        if (SkewLevel[Fg]==0){kurt.Fg = 0} else {kurt.Fg = 7}
        if (SkewLevel[Fg]==0){kurt.Fg = 0} else {kurt.Fg = 7}      
        
        
        #sigma.target  <- matrix(.50,K,K)
        sigma.target  <- matrix(0,K,K)
        diag(sigma.target) <- 1
        
        theta.sim  <- covsim::rPLSIM(N, sigma.target, skewness=c(SkewLevel[Fg], rep(SkewLevel[Fg], K-1)),
                                     excesskurtosis=c(kurt.Fg,rep(kurt.Fg,K-1)))
        theta  <- theta.sim[[1]][[1]]
        #theta[,1]
        #plot(density(theta[,2]))
        ########################################################################
        
        
        
        
        
        ########################################################################
        ############# Simulate response data####################################
        #item type - graded response model (GRM)
        items <- rep('graded', Factor*I)
        
        
        dataset <- simdata(a,d,N,itemtype=items, Theta = theta)
        ########################################################################
        #dataset <- simdata(a,d,N,itemtype=items,sigma=sigma_data, Theta = theta)
        
        
        
        
        ### write parameters of real file (from runif()) 
        parameters <- cbind(a,d)
        parameters.name <- paste0(data.3.folder,Factor,"-",I,"-",N,"-",SkewLevel.name[Fg],"-",SkewLevel.name[Fg],"-",iter,"-realparameters.csv")      
        write.table(parameters,file = parameters.name,sep=",",row.names=FALSE, col.names=FALSE)
        
        ### write theta of real (population)  file    
        theta.name <- paste0(data.3.folder,Factor,"-",I,"-",N,"-",SkewLevel.name[Fg],"-",SkewLevel.name[Fg],"-",iter,"-realtheta.csv")
        write.table(theta,file = theta.name,sep=",",row.names=FALSE, col.names=FALSE)
        
        ### write data (response scores) file
        data.name <- paste0(Factor,"-",I,"-",N,"-",SkewLevel.name[Fg],"-",SkewLevel.name[Fg],"-",iter,"-data.csv")
        data.name.f <- paste0(data.3.folder,data.name)
        write.table(dataset,file = data.name.f,sep=",",row.names=FALSE, col.names=FALSE)
        
        ### write normal distribution flexmirt syntax file
        syn.name <- paste0(data.3.folder,"Syn",Factor,"-",I,"-",N,"-",SkewLevel.name[Fg],"-",SkewLevel.name[Fg],"-",iter,".flexmirt")
        
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
        
        <Groups>
          %Group1%
          File = '",data.name,"';
          Varnames = v1-v",Factor*I,";
          N = ",N,";
          Ncats(v1-v",Factor*I,") = 4;
          Model(v1-v",Factor*I,") = graded(4);
          Dimensions = ",K,";
          Primary = 1;
        
        <Constraints>
          Fix(v1-v",Factor*I,"), Slope;
          Free(v1-v",Factor*I,"), Slope(1);",
                          Flex.Constraints)
        
        writeLines(flexsyn, syn.name)
        
      }
  }
}

#Run 3*2*4 conditions  
for (Factor in c(2,3,4)){
  for (I in c(5,10)){
    for (N in c(250,500,1000)){
      
      sim_bf(Factor,I,N,iter=100)
      
      
    }
  }
}


#Test 1 condition 
sim_bf(4,10,1000,200)

###############################################
###Estimate parameter with bfactor()###########
###############################################
specific <- matrix(rep(NA,Factor*I),I)
for (i in 1:Factor){
  specific[,i] <- rep(i,I)
}
specific <- as.vector(specific)
mod <- bfactor(dataset, specific, technical=list(NCYCLES=1000))
summary(mod)
a
###############################################
###############################################     

