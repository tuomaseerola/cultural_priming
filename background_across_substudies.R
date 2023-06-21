# background_across_substudies.R

d1$study<-'MIN'
d2$study<-'AUG'
d3$study<-'DIM'
d4$study<-'SUS'
df<-rbind(d1,d2,d3,d4)
print(table(df$study))

print(summary(aov(age ~ study,data=df)))

print(chisq.test(df$gender,df$study,simulate.p.value = TRUE))
#chisq.test(df$study, df$gender,simulate.p.value = TRUE)

#chisq.test(df$study, df$omsi,simulate.p.value = TRUE)

print(chisq.test(df$study, df$MusExpertise,simulate.p.value = TRUE))

# NONMUSICIANS: nonmusician, music-loving nonmusician, amateur musician
# MUSICIANS: serious amateur musician, semiprofessional musician, professional musician

print("across all sub-experiments")
print("--------------------------")

print(paste('Age M:',round(mean(df$age),2)))
print(paste('Age SD:',round(sd(df$age),2)))

print("Gender distr:")
print(table(df$gender)/sum(table(df$gender)))

print("MusExpertise distr:")
print(table(df$MusExpertise)/sum(table(df$MusExpertise)))
