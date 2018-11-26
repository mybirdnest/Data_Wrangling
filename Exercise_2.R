library(tidyverse)

#0: Load the data in RStudio
titanic_original <- read_csv("titanic_original.csv")


titanic_clean <- titanic_original %>%
  mutate(
    #1: Port of embarkation
    embarked = replace(embarked, embarked == "", "S"),
    #2 Age
    age = replace(age, is.na(age), round(mean(age, na.rm = TRUE))),
    #3 Lifeboat
    boat = replace(boat, boat == "", "NA"),
    #4 Cabin
    has_cabin_number = ifelse(cabin == "", 0, 1)
  )


#5 Submit the project on Github
write.csv(titanic_clean, file = "titanic_clean_csv", row.names = FALSE)