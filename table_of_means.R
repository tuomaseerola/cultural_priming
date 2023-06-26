# table_of_means.R

library(dplyr)

table(MIN$Block)
table(AUG$Block)
table(DIM$Block)
table(SUS$Block)

DF <- rbind(MIN,AUG,DIM,SUS)

S <- DF %>%
  group_by(Block,TargetValence, PrimeValence) %>%
  summarise(n=n(),m=mean(RT,na.rm = TRUE),sd=sd(RT,na.rm = TRUE),.groups = "drop") %>%
  mutate(se=sd/sqrt(n),LCI=m+qnorm(0.025)*se,UCI=m+qnorm(0.975)*se) 

knitr::kable(S, digits = 0)

col2<-c('Neg. Word','Pos. Word')

T1<-NULL
col1<-c('Min','')
S1<-dplyr::filter(S,Block=='MAJ_MIN')
T1$col1[1] <- paste0(round(S1$m[1]), ' (',round(S1$LCI[1]),'--',round(S1$UCI[1]),')')
T1$col1[2] <- paste0(round(S1$m[3]), ' (',round(S1$LCI[3]),'--',round(S1$UCI[3]),')')
T1$col2[1] <- paste0(round(S1$m[2]), ' (',round(S1$LCI[2]),'--',round(S1$UCI[2]),')')
T1$col2[2] <- paste0(round(S1$m[4]), ' (',round(S1$LCI[4]),'--',round(S1$UCI[4]),')')
T1 <- data.frame(col1,col2,T1)
T1

T2<-NULL
col1<-c('Augm','')
S2<-dplyr::filter(S,Block=='MAJ_AUG')
T2$col1[1] <- paste0(round(S2$m[1]), ' (',round(S2$LCI[1]),'--',round(S2$UCI[1]),')')
T2$col1[2] <- paste0(round(S2$m[3]), ' (',round(S2$LCI[3]),'--',round(S2$UCI[3]),')')
T2$col2[1] <- paste0(round(S2$m[2]), ' (',round(S2$LCI[2]),'--',round(S2$UCI[2]),')')
T2$col2[2] <- paste0(round(S2$m[4]), ' (',round(S2$LCI[4]),'--',round(S2$UCI[4]),')')
T2 <- data.frame(col1,col2,T2)

T3<-NULL
col1<-c('Dim','')
S3<-dplyr::filter(S,Block=='MAJ_DIM')
T3$col1[1] <- paste0(round(S3$m[1]), ' (',round(S3$LCI[1]),'--',round(S3$UCI[1]),')')
T3$col1[2] <- paste0(round(S3$m[3]), ' (',round(S3$LCI[3]),'--',round(S3$UCI[3]),')')
T3$col2[1] <- paste0(round(S3$m[2]), ' (',round(S3$LCI[2]),'--',round(S3$UCI[2]),')')
T3$col2[2] <- paste0(round(S3$m[4]), ' (',round(S3$LCI[4]),'--',round(S3$UCI[4]),')')
T3 <- data.frame(col1,col2,T3)

T4<-NULL
col1<-c('Sus','')
S4<-dplyr::filter(S,Block=='MAJ_SUS')
T4$col1[1] <- paste0(round(S4$m[1]), ' (',round(S4$LCI[1]),'--',round(S4$UCI[1]),')')
T4$col1[2] <- paste0(round(S4$m[3]), ' (',round(S4$LCI[3]),'--',round(S4$UCI[3]),')')
T4$col2[1] <- paste0(round(S4$m[2]), ' (',round(S4$LCI[2]),'--',round(S4$UCI[3]),')')
T4$col2[2] <- paste0(round(S4$m[4]), ' (',round(S4$LCI[4]),'--',round(S4$UCI[4]),')')
T4 <- data.frame(col1,col2,T4)

TABLE <- rbind(T1,T2,T3,T4)
colnames(TABLE)<-c('Sub-experiment','Target','Prime Neg. chord','Prime Pos. chord')
print(knitr::kable(TABLE))

