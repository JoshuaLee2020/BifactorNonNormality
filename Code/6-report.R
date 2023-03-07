#Merge convergence status and parameters
#para.data_file.all <- left_join(para.data.all, files.all ,by="id.rep")
rm(list=ls())
library(tidyr)
library(dplyr)
library(readr)
library(parallel)
library(mirt)
library(stringr)
library(ggplot2)
main.folder <- "C:/Research/BifactorNonNormality"
data.folder <- paste0(main.folder,"/Data/")
result.folder <- paste0(main.folder,"/Result/")

para.data_file.all <- read.csv(paste0(result.folder,"parameters.csv"),header = TRUE)


para.data_file.report <- para.data_file.all%>%
  #filter(!is.na(aG.FlexMIRT))%>%
  mutate(#aG.Flex.delta= as.numeric(aG.FlexMIRT)-as.numeric(aG.real),
         #aS.Flex.delta= as.numeric(aS.FlexMIRT)-as.numeric(aS.real),
         aG.mirt.delta= as.numeric(aG.mirt)-as.numeric(aG.real),
         aS.mirt.delta= as.numeric(aS.mirt)-as.numeric(aS.real),
         c1.mirt.delta= as.numeric(c1.mirt)-as.numeric(c1.real),
         c2.mirt.delta= as.numeric(c2.mirt)-as.numeric(c2.real),
         c3.mirt.delta= as.numeric(c3.mirt)-as.numeric(c3.real)
         )%>%
  filter(!is.na(c1.mirt.delta))


order_vec <- c("Normal", "Moderate", "Severe")


para.data_file.summarise <- para.data_file.report%>%
  group_by(Factor, I, N, Fg, Fs)%>%
  summarise(aG.mirt.bias = round(mean(aG.mirt.delta),3), 
            aS.mirt.bias = round(mean(aS.mirt.delta),3),
            c1.mirt.bias = round(mean(c1.mirt.delta),3),
            c2.mirt.bias = round(mean(c2.mirt.delta),3),
            c3.mirt.bias = round(mean(c3.mirt.delta),3),
            aG.mirt.rmse = round(sqrt(mean((aG.mirt.delta)^2)),3), 
            aS.mirt.rmse = round(sqrt(mean((aS.mirt.delta)^2)),3), 
            c1.mirt.rmse = round(sqrt(mean((c1.mirt.delta)^2)),3), 
            c2.mirt.rmse = round(sqrt(mean((c2.mirt.delta)^2)),3), 
            c3.mirt.rmse = round(sqrt(mean((c3.mirt.delta)^2)),3)
            )%>%
  mutate(Fg = factor(Fg, levels = order_vec),
         Fs = factor(Fs, levels = order_vec)) %>%
  arrange(Fg,Fs) %>%
  mutate(N = as.factor(N))

para.data_file.summarise

write.csv(para.data_file.summarise,paste0(result.folder,"summa.csv"))


para.data_file.long <- para.data_file.summarise %>%
  pivot_longer(
    cols = aG.mirt.bias : c3.mirt.rmse,
    names_to = "metrics_name",
    values_to = "metrics"
  ) 


summarise(para.data_file.long)




###Plot boxplot with outliers limited in 3
df <- para.data_file.report%>%
  mutate(Fg = factor(Fg, levels = order_vec),
         Fs = factor(Fs, levels = order_vec)) %>%
  arrange(Fg,Fs) %>%
  mutate(N = as.factor(N))


df <- para.data_file.report%>%
  filter(aG.mirt<=3,aS.mirt<=3, c3.mirt<=3,c2.mirt<=3,c1.mirt<=3)%>%
  mutate(Fg = factor(Fg, levels = order_vec),
         Fs = factor(Fs, levels = order_vec)) %>%
  arrange(Fg,Fs) %>%
  mutate(N = as.factor(N))

summary(df)

p1 <- ggplot(data = df, aes (x = interaction(Fs,Fg), y = aG.mirt.delta, fill = N)) +
  geom_boxplot(color="#555555")+
  facet_wrap(I~Factor)+
  #facet_grid(cols=vars(smoke, black))+
  labs(title = "Bias of a in General Factor (Trim Outliers > 3)")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1.2),
        plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = c(3.5,6.5) , linetype = "dashed")+
  scale_fill_manual(values = c("#999999", "#E69F00", "#009E73", "#0072B2"))

p1







###Plot comparison between aG aS c1 c2 c3, under 1000 sample size
df_1000 <- para.data_file.long %>%
  filter(N==1000,
  metrics_name == c("aG.mirt.bias")|
    metrics_name == c("aS.mirt.bias")|
    metrics_name == c("c1.mirt.bias")|
    metrics_name == c("c2.mirt.bias")|
    metrics_name == c("c3.mirt.bias")
  )

p1 <- ggplot(data = df_1000, aes (x = interaction(Fs,Fg), y = metrics, color=metrics_name)) +
  geom_point()+
  scale_color_manual(values = c("#E69F00", "#999999", "#009E73", "#0072B2","#806070" ))+
  facet_wrap(Factor~I)+
  #facet_grid(cols=vars(smoke, black))+
  labs(title = "Bias of all parameters under 1000 Sample Size")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1.2),
        plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = c(3.5,6.5) , linetype = "dashed")
p1

###Plot comparison between aG and aS, under 1000 sample size
df_aG_aS <- para.data_file.long %>%
  filter(bias == "aG.mirt.bias"| bias =="aS.mirt.bias")%>%
  filter(N==1000)

p1 <- ggplot(data = df_1000, aes (x = interaction(Fs,Fg), y = metrics, color=bias)) +
  geom_point()+
  scale_color_manual(values = c("#E69F00", "#999999", "#009E73", "#0072B2","#806070" ))+
  facet_wrap(Factor~I)+
  #facet_grid(cols=vars(smoke, black))+
  labs(title = "Bias of a of General Factor and in Specific Factor, under 1000 Sample Size")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1.2),
        plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = c(3.5,6.5) , linetype = "dashed")
p1

###Plot comparison between aG and aS, under 1000 sample size
df_aG_aS <- para.data_file.long %>%
  filter(bias == "c1.mirt.bias"| bias =="c2.mirt.bias" | bias =="c3.mirt.bias")%>%
  filter(N==1000)

p1 <- ggplot(data = df_1000, aes (x = interaction(Fs,Fg), y = metrics, color=bias)) +
  geom_point()+
  scale_color_manual(values = c("#009E73", "#0072B2","#806070"))+
  facet_wrap(Factor~I)+
  #facet_grid(cols=vars(smoke, black))+
  labs(title = "Bias of c1 c2 c3, under 1000 Sample Size")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1.2),
        plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = c(3.5,6.5) , linetype = "dashed")
p1







#ANOVA for aG - a of General Factor
model.a.G <- aov(aG.mirt.bias ~ Factor*I*N*Fg*Fs, data=para.data_file.summarise)
s.a.G <- summary(model.a.G)
s.a.G[[1]][s.a.G[[1]][["Pr(>F)"]] < 0.05 , ]


model.a.G2 <- aov(aG.mirt.bias ~ Factor+I+N+Fg+Fs, data=para.data_file.summarise)
s.a.G2 <- summary(model.a.G2)
s.a.G2
tukey.a.G2 <- TukeyHSD(model.a.G2)
tukey.a.G2




model.a.S <- aov(aS.mirt.bias ~ Factor*I*N*Fg*Fs, data=para.data_file.summarise)
summary(model.a.S)

model.c1 <- aov(c1.mirt.bias ~ Factor*I*N*Fg*Fs, data=para.data_file.summarise)
summary(model.c1)

model.c2 <- aov(c2.mirt.bias ~ Factor*I*N*Fg*Fs, data=para.data_file.summarise)
summary(model.c2)

model.c3 <- aov(c3.mirt.bias ~ Factor*I*N*Fg*Fs, data=para.data_file.summarise)
summary(model.c3)

#model <- anova(aG.mirt.bias ~ Factor*I*N*Fg*Fs, data=data.aG)




#########################################################
#########Backup




p1 <- ggplot(data = para.data_file.long, aes (x = interaction(Fs,Fg), y = metrics, fill=N)) +
  geom_boxplot(color="#222222")+
  facet_wrap(Factor~I)+
  #facet_grid(cols=vars(smoke, black))+
  labs(title = "Bias of a in General Factor")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1.2),
        plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = c("#999999", "#E69F00", "#009E73", "#0072B2"))
p1


###Plot boxplot with outliers limited in 5
df <- para.data_file.report%>%
  filter(aG.mirt<=5,aS.mirt<=5, c3.mirt<=5,c2.mirt<=5,c1.mirt<=5)%>%
  mutate(Fg = factor(Fg, levels = order_vec),
         Fs = factor(Fs, levels = order_vec)) %>%
  arrange(Fg,Fs) %>%
  mutate(N = as.factor(N))

summary(df)

p1 <- ggplot(data = df, aes (x = interaction(Fs,Fg), y = aG.mirt.delta, fill = N)) +
  geom_boxplot(color="#555555")+
  facet_wrap(I~Factor)+
  #facet_grid(cols=vars(smoke, black))+
  labs(title = "Bias of a in General Factor")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1.2),
        plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = c("#999999", "#E69F00", "#009E73", "#0072B2"))

p1




###Plot 1

p1 <- ggplot(data = para.data_file.long, aes (x = interaction(Fs,Fg), y = metrics, fill=N)) +
  geom_boxplot(color="#222222")+
  facet_wrap(Factor~I)+
  #facet_grid(cols=vars(smoke, black))+
  labs(title = "Bias of a in General Factor")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1.2),
        plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = c("#999999", "#E69F00", "#009E73", "#0072B2"))
p1

