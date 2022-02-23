#title: "Trial_1 - Yohana"
#output: word_document

#Read the CSV file

yr_2021<-read.csv("D:/Data Science 2021-2023/Spring 2022/Advanced R/Project/Datasets/Excel/Test_CSV/yr_2021_test.csv")
head(yr_2021)

#Import Necessary Libraries
library(dplyr)

## Pivot the Gender
#create a new dataframe with identity column
#create two subsets with the identity 
#1) identity,men,women 
#2)identity,Race, Age, Marital.status

#After gather is performed, join using the identity column
yr_2021_gathered <- yr_2021 %>%
  select(Year,Men,Women) %>%
  gather(key = Gender, value = Unemployement_Rate, -Year)

#View data sample
head(head(yr_2021_gathered))
