# Unemployement-rate-analysis

Objectives
The main goal of the project is to find the factors that influence the unemployement rate.
Further, to find if unemployement truly is a factor that affects the country's economy (GDP) and crime rates.

Code description

1.We start off by importing the necessary libraries in our R file.
2.The file names are stored in a list and iterated in a custom function which has a series of data cleaning steps, to prepare the data for analysis.
3.Inside the function, an index column is first added to the dataset. It is then split into two subsets.
4.The subset with index, men and women is pivoted to have the unemployement rate as a column value.
5.The subset at step 4 is joined with the first subset (race, age, marital status) using the index column as the key column.
6.The data for all the years foes through the above steps and gets appended to form a single large dataset for analysis.
7.The final dataset is now fit to a linear regression model for the purpose of inference.
8.The model is inferred using the ANOVA table.
9.The GDP dataset is read.
10.The dataset with unemployement rates is summarised by taking the mean for each year, which results in one unempoyement rate per year.
11.The dataset at step 10 is joined with GDP data for each year and fitted to a linear regression model for inference.
12.Steps 9,10 and 11 are repeated again for crime rate analysis 



