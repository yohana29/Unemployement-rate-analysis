#title: "Trial_1 - Yohana"
#output: word_document

#Import Necessary Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

#-----------------------Reading data and cleaning data--------------------------------#
#create empty dataframe
testing_merge <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(testing_merge) <- c('Year','Gender','Age','Race', 'Marital.status','Unemployement_Rate')

#code for dynamic reading of files
file_names<-c('yr_2021_test.csv','yr_2020_test.csv','yr_2019_test.csv'
              ,'yr_2018_test.csv','yr_2017_test.csv','yr_2016_test.csv'
              ,'yr_2015_test.csv','yr_2014_test.csv','yr_2013_test.csv'
              ,'yr_2012_test.csv','yr_2011_test.csv','yr_2010_test.csv'
              ,'yr_2009_test.csv','yr_2008_test.csv','yr_2007_test.csv'
              ,'yr_2006_test.csv','yr_2005_test.csv','yr_2004_test.csv'
              ,'yr_2003_test.csv','yr_2002_test.csv')

for (val in file_names){
  #Read the CSV file
  yr_2021<-read.csv(paste0("D:/Data Science 2021-2023/Spring 2022/Advanced R/Project/Datasets/Excel/Test_CSV/",val))
  
  #introduce identity column
  yr_2021 <- tibble::rowid_to_column(yr_2021, "index")
  
  #create two subsets with the identity 
  #1) identity,men,women 
  yr_2021_gender<-yr_2021%>%
    select(index,Men,Women)
  
  #2)identity,Year,Race, Age, Marital.status
  yr_2021_othercol<-yr_2021%>%
    select(index,Year,Race, Age, Marital.status)
  
  ## Pivot the Gender and unemployement rates
  yr_2021_gender <- yr_2021_gender %>%
    select(index,Men,Women) %>%
    gather(key = Gender, value = Unemployement_Rate, -index)

  #join the othercol and gender df wrt to the index column
  yr_2021_fin<-yr_2021_gender%>%
    inner_join(yr_2021_othercol, by = c("index" = "index")) %>% 
    select(Year,Gender,Age,Race, Marital.status,Unemployement_Rate)
  
  #Append all the datasets together
  testing_merge<-rbind(testing_merge,yr_2021_fin)
  
}
View(testing_merge)

#Checking if all the year is read
testing_merge %>% distinct(Year,Marital.status)

#Each year has 48 records
testing_merge %>% count()

#------------------------------Visualizing Data--------------------------------#

#bar chart
ggplot(data = testing_merge) +
  geom_col(mapping = aes(x = Age, y = Unemployement_Rate))

ggplot(data = testing_merge) +
  geom_col(mapping = aes(x = Gender, y = Unemployement_Rate))

ggplot(data = testing_merge) +
  geom_col(mapping = aes(x = Marital.status, y = Unemployement_Rate))

ggplot(data = testing_merge) +
  geom_col(mapping = aes(x = Race, y = Unemployement_Rate))

#Coloured point chart
ggplot(data = testing_merge) +
  geom_point(mapping = aes(x = Year, y = Unemployement_Rate, color = Race))

ggplot(data = testing_merge) +
  geom_point(mapping = aes(x = Year, y = Unemployement_Rate))

#------------------------------Applying Prediction Models to the Data--------------------------------#

# #Training set
# Unemployement_Train = testing_merge%>%
#   filter(Year<='2019')
# #%>%  count()
# #Unemployement_Train
# 
# #Test set
# Unemployement_Test = testing_merge%>%
#   filter(Year>'2019')
# #%>%  count()
# #Unemployement_Test
# 
# #testing
# res <- model.matrix(~Race, data = Unemployement_Train)
# head(res[, -1])
# 
# Unemployement_Train_t <- Unemployement_Train %>%
#   mutate(Gender = relevel(Gender, ref = "Female"))
# 
# model1= lm(Unemployement_Rate ~ Gender , data=Unemployement_Train)
# summary(model1)$coef
# 


#Building a prediction model
colnames(testing_merge)
model1= lm(Unemployement_Rate ~ Gender + Age + Race + Marital.status, data=testing_merge)
anova(model1)#all the variables are significant
summary(model1) #to interpret the effect of each factor


# # Testing the Linear Regression Model
# 
# predictTest= predict(model1, newdata= Unemployement_Test)
# predictTest
# 
# SSE= sum((Unemployement_Test$Unemployement_Rate - predictTest)^2)
# SST = sum ((Unemployement_Test$Unemployement_Rate - mean(Unemployement_Train$Unemployement_Rate))^2)
# 1-SSE/SST

#------------------------------GDP Testing--------------------------------#
gdpdata<-read.csv("D:/Data Science 2021-2023/Spring 2022/Advanced R/Project/Datasets/GDP.csv")

yr_groupby<-testing_merge%>%
  group_by(Year) %>% 
  summarise_at(vars(Unemployement_Rate), list(Unemployement_Rate = mean))

yr_groupby

yr_unemp_gdp<-yr_groupby%>%
  inner_join(gdpdata, by = c("Year" = "Year")) %>% 
  select(Year,GDP,Unemployement_Rate)

yr_unemp_gdp

model_test= lm(Unemployement_Rate ~ GDP ,data=yr_unemp_gdp)
summary(model_test)


ggplot(data = yr_unemp_gdp) +
  geom_point(mapping = aes(x = GDP, y = Unemployement_Rate))

ggplot(data = yr_unemp_gdp) +
  geom_point(mapping = aes(x = Unemployement_Rate, y =GDP ))

#------------------------------Criminal Testing--------------------------------#

