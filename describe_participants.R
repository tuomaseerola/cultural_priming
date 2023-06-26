describe_participants <- function(data = NULL) {
  # describe_participants.R
  
  U <- unique(data$Id)
  N <- length(U)
  
  # AGE, MUSICAL EXPERTISE, GENDER
  
  age <- NULL
  omsi <- NULL
  MusExpertise <- NULL
  gender <- NULL
  for (k in 1:N) {
    ind <- which(data$Id == U[k])
    age[k] <- data$AGE[ind[1]]
    omsi[k] <- data$OMSI[ind[1]]
    MusExpertise[k] <- data$MusExpertise[ind[1]]
    gender[k] <- data$GENDER[ind[1]]
  }
  
  df <- data.frame(age, omsi, MusExpertise, gender)

  print("-----Age-------")
  print(paste('Mean Age:',round(mean(df$age),1)))
  print(paste('SD Age:',  round(sd(df$age),1)))

  print("-----Musical Expertise (%)-------")
#  print(paste('Median OMSI:',median(df$omsi)))
#  print(table(df$omsi))
  print(round(100*(table(df$MusExpertise)/nrow(df))))
  print("-----Gender (%)-------")
  #print(paste('Gender:',round(100*sum(df$gender==1)/nrow(df),2)))
  print(round(100*table(df$gender)/nrow(df)))
  
  return <- df
  
}
