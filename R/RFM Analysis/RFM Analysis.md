# RFM Analysis
## Introduction
RFM analysis is a very popular marketing technique to analyse customer's behaviour, it was firstly introduced 1995 by Bult and Wansbeek and it was proven that this kind of analysis is very effective on sales/marketing databases.RFM stands for: 
**R(Recency)**: refers to the interval between the time and the last consuming behaviour happens.
**F(Frequency)**:is the total number of transactions that a customer has made in a specific period.
**M(Monetary)**: is the cumulative total of money spent by a customer.
Those three variables are important because they allow companies to keep track of their customer's profiles inorder to increases sales and profitability. Moreover, RFM analysis is very useful when it comes to dividing customers into different groups for future enhanced services and identification of loyal customers.

Integrating data mining techniques and RFM analysis provides very useful information about the current and new customers. For instance, association rules based on RFM model could analyse the relation between the products that are bought together frequently to better understand customers buying behaviours and suggest recommendation to satisfy thier needs. Clustering based on RFM provides knowledge about the customer's marketing segments and give a better opportunity for successful marketing campaigns.
 Customer's demographic information and RFM clusters are very crucial to predict the new customer's behaviours like the probability of purchases and how often the customer will purchase and what is the value of the purchases.

Finally, the following sections are divided as the following, section 1 introduces the data set and some explorations techniques used to understand the data, section 2 is about calculating RFM scores and segments and prepare the dataset for data mining algorithms, section 3 presents customer segmentation based on RFM using DBscan, section 4  shows how to predict new customers type based on RFM values using decision trees and random forest and it will depend on ad boost to determine the   importance of the customer's demographic variables which are used to build the prediction, section 5 provides the basis of basket analysis for a specific customer segments which allow understanding the behaviors of existing customers, and finally conclusion to summarize the result and the findings.

## Section 1:  Data Exploration
The dataset which will be used for this analysis is a transactional dataset for Chinese retail clothing company. It consists of 34 variables and 2360 observation . **READXL** library is required to load the data into the memory as it's in excel file. The following R code is to load the data set into the memory and explore the dimensions, variables names and types

```R
#install.packages("readxl",dependencies = TRUE,repos = "http://cran.us.r-project.org")
library("readxl")
ds <- read_excel("sales.xls")
#Explore dimensions 
dim(ds)
#Explore variable s' name
names(ds)
#Explore variables data types 
sapply(ds,class)
#First Row of the data set
head(ds, n=1)
```
The variables have a different types(Numeric, Characters, POSIXct) and their names are self-exploratory except GP (Gross Profit) and COS (Cost of sales), there are no issues with the observations (NAs, invalid values, duplication, etc.), so there will no need for cleaning phase.

Now Let us now turn to the second phase of the exploration, 4 different types of charts will be presented to understand the trends and the nature of the data.
**Note:**For the analysis purposes, not all the variables will be included in this study for instance: RFM focuses only on [recency (Order date), frequency (Order Number), monetary (GP)], basket analysis focuses on transactions (OrderID) and products (Product Name), and customer segmentations will focus on the demographic information of the customers like education, income level and marital status.
### 1.  Sales By Gender:
To find the total sales by gender, a temporary data frame will be created using an aggregate function. The aggregate function will accumulate all GP (Gross profit) values for Males and Females separately then this data frame will be the input for barplot function.
```R
#salesby gender
GP_Gender <- aggregate(GP ~ Gender, ds, FUN=sum) 
barplot(height = GP_Gender$GP,names.arg = GP_Gender$Gender, col = c("mistyrose","lightblue" ),xlab = "Sales by gender")
```
The plot shows that the females are the dominant gender in sales and most important customers to the company are females.
### 1.  Sales By Gender:
