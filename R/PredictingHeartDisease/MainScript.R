
#set working directory

setwd("z:/")

#load packages for NB-matrix-charts
#install.packages("psych",dependencies = TRUE)
#install.packages("e1071",dependencies = TRUE)
#install.packages("caret",dependencies = TRUE)

#install.packages("ggpubr", dependencies = TRUE)

library("psych")
library("e1071")
library("caret")


library("ggpubr")
#load heart ds 
ds <- read.csv("Heart.csv")

#remove id column

ds$X <- NULL

# remove null values 

ds <- na.omit(ds)

#summary
summary(ds)

#Dim
dim(ds)

#variables classes
library(mlbench)
sapply(ds, class)


# composing numeric data set 
ds.numerics <- ds[,c(1:2,4:12)]

#getting to know categorical data
ds.cat <- ds[c(3,13:14)]

pairs.panels(ds.cat)

#getting to know numeric data
#calculate correlation

corrMatrix <- cor(ds.numerics,use = "complete.obs")
#round the result to 2 decimal points
corrMatrix <- round(corrMatrix,2)
corrMatrix

#install.packages("Hmisc")
library("Hmisc")
pvalues <- rcorr(as.matrix(ds.numerics))
pvalues$P

#install.packages("corrplot")
library("corrplot")
corrplot(corrMatrix, type = "upper", order = "hclust",tl.col = "black", tl.srt = 4)

#boxplot
boxplot (ds$Age ~ ds$AHD, data = ds, 
         main = "Age distribution",
         xlab = "AHD", ylab = "Age", col = "salmon")


		 
#Explore Categorical Variables
qplot (ds$AHD, data = ds, fill = ChestPain) + facet_grid (. ~ ChestPain)


#NB Model AHD our target class is factor
ds.nb.num <- ds[c(1:2,4:12,14)]
#convert AHD to 0 and 1
ds.nb.num$AHD <- ifelse(ds.nb.num$AHD == "Yes",1,0)
library("PerformanceAnalytics")
chart.Correlation(ds.nb.num, histogram=TRUE, pch=19)
#Explore chestpain by AHD
qplot (AHD, data = ds, fill = ChestPain) + facet_grid (. ~ ChestPain)
#Explore Thal by AHD
qplot (AHD, data = ds, fill = Thal) + facet_grid (. ~ Thal)

library("e1071")
ds.nb <- ds[c(1:4,7:14)]
library("e1071")
set.seed(1000)
index<-sample(1:nrow(ds.nb),round(0.7*nrow(ds.nb)))
train<-ds.nb[index,]
test<-ds.nb[-index,]
NBmodel<-naiveBayes(train[,1:11],train[,12],laplace=1)
NBmodel
table(predict(NBmodel, test[,-12]),test[,12])
#test the calssifier
library("caret")
confusionMatrix(table(predict(NBmodel, test[,-12]),test[,12]))

#KNN Model works on numberic data and the target calss should be afactor
library("class")
#extract numeric variables with target class AHD
ds.knn <- ds[c(1:2,4:12,14)]
set.seed(1000)
index <- sample(1:nrow(ds.knn),round(0.7*nrow(ds.knn)))
train<-ds.knn[index,]
test<-ds.knn[-index,]
#k=3
k3nn<-  knn(train[,1:11],test[,1:11],train[,12],k=3)
table(k3nn,test[,12])
confusionMatrix(table(k3nn,test[,12]))
#k=7
k7nn<-  knn(train[,1:11],test[,1:11],train[,12],k=7)
table(k7nn,test[,12])
confusionMatrix(table(k7nn,test[,12]))


#KNNcat
library("class")
library("knncat")
#extract numeric variables with target class AHD
ds.knn <- ds[c(1:4,7:14)]
set.seed(1000)
index <- sample(1:nrow(ds.knn),round(0.7*nrow(ds.knn)))
train<-ds.knn[index,]
test<-ds.knn[-index,]
#k=3
k3nn<-  knncat(train[,1:11],test[,1:11],train[,12],k=3)
table(k3nn,test[,12])
confusionMatrix(table(k3nn,test[,12]))
#k=7
k7nn<-  knncat(train[,1:11],test[,1:11],train[,12],k=7)
table(k7nn,test[,12])
confusionMatrix(table(k7nn,test[,12]))
