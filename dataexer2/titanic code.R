titanic_original<- read.csv("dataexer2submission/titanic_original.csv", header= TRUE, na.strings= c("", "NA"))
titanic_draft<- titanic_original

#1 replace missing value with "S" in embarked variable
titanic_draft$embarked[is.na(titanic_draft$embarked)] <- "S"

#2 calculate the mean of the existing age data
age_mean <- mean(titanic_draft$age, trim = 0, na.rm = TRUE)
age_mean

# replace NA with mean age (extreme age values may skew the mean)
titanic_draft$age[is.na(titanic_draft$age)] <- age_mean

#3 NA is created for missing values of boat in line 1 (read.csv argument)

#4 create a binary variable (missing/not missing) for cabin number
titanic_draft$has_cabin_number <- ifelse(is.na(titanic_draft$cabin), 0, 1)

#5 store in titanic_clean.csv and submit 
titanic_clean <- titanic_draft
write.csv(titanic_clean, "dataexer2submission/titanic_clean.csv")


