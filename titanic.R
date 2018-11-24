#load library
library(tidyr)
library(dplyr)

#Load the data in Rstudio
#chaining short cut is cmd + shift + m 
getwd()

titanic <- read.csv("titanic_original.csv")

#Port of embarkation - finding the missing values and replace them with S
# ^ Begin / $End 
titanic <- titanic %>% 
  mutate(embarked = gsub("^$", "S", embarked))
titanic
# Age

# Calculate the mean of the Age column and use that value to populate the missing values
str(titanic$age)
meanage <- mean(titanic$age, na.rm = TRUE)
meanage
titanic <- titanic %>% mutate(age = if_else(is.na(age), meanage , age))

# Think about other ways you could have populated the missing values in the age column
#median
medianage <- median(titanic$age, na.rm = TRUE)
medianage

#finding lowest and highest and dividing by 2
maxage <- max(titanic$age, na.rm = TRUE)
minage <- min(titanic$age, na.rm = TRUE)
minmax <- (minage+maxage)/2


# Lifeboat Fill empty slots with a dummy value e.g. the string 'None' or 'NA'

titanic <- titanic %>% mutate(boat = gsub( "^$", "NA", boat))

# Cabin Does it make sense to fill missing cabin numbers with value/What does missing value mean.  

#create column has_cabin_number which has 1 if there is a cabin number and 0 otherwise
titanic <- titanic %>% mutate(has_cabin_number = ifelse(grepl("^$", cabin), 0, 1))
#submit the project on github titanic_original.csv and titanic_clean.csv
write.csv(titanic, "titanic_clean.csv")