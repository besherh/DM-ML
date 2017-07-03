#install.packages("readxl",dependencies = TRUE)
#install.packages("readr")
#install.packages("dplyr")
#install.packages("DataExplorer")

library(dplyr)
library(ggplot2)
library(readr)
library(DataExplorer)
library(readxl)
###############################################################
#Data Exploration
##############################################################
ds <- read_excel("sales.xls")
dim(ds)
names(ds)
sapply(ds,class)
head(ds, n=1)

#salesby gender
GP_Gender <- aggregate(GP ~ Gender, ds, FUN=sum) 
barplot(height = GP_Gender$GP,names.arg = GP_Gender$Gender, col = c("mistyrose","lightblue" ),xlab = "Sales by gender")


#sales by product category
Sales_ProductCat <- aggregate(ds$GP ~ ds$CategoryName, ds, FUN=sum) 

dotchart(Sales_ProductCat$`ds$GP`,labels=Sales_ProductCat$`ds$CategoryName`,cex=.7,
         main="Sales by product categories", 
         xlab="sales")


# order number by martial status
orders_ms <- aggregate(ds$OrderID ~ ds$MartialStatus,data = ds, FUN=length) 
names(orders_ms) <- c("ms","count")
pie(orders_ms$count, labels = orders_ms$ms, main="Martial Status/Order Counts",col = c("#999999", "#E69F00", "#56B4E9"))



# income level / sum sales
sales_income <- aggregate(ds$GP ~ ds$IncomeLevel,data = ds, FUN=sum) 
names(sales_income) <- c("il","sales")

barplot(sales_income$sales,  names.arg = sales_income $il, col = c("#4682B4", "#B4464B", "#B4464B","#B4AF46"), xlab = "Income Levels")

###############################################################
#Calculating RFM scores and segments
##############################################################


rfmdata <- ds[c(22,23,10,26,20,11,12,13,14)]
names(rfmdata) <- c("Date", "OrderNumber", "CustomerId","ProductId", "Value","Gender","Education","MartialStatus","IncoveLevel")

summary(rfmdata)
sapply(rfmdata, class)
rfmdata <- na.omit(rfmdata)
rfmdata$Date <- as.Date(as.POSIXct(rfmdata$Date, tz='GMT'))

NOW <- as.Date("2012-12-31", "%Y-%m-%d")

# Calculate R
R_table <- aggregate(Date ~ CustomerId, rfmdata, FUN=max) 
R_table$R <- as.numeric(NOW - R_table$Date)

# Calculate F
F_table <- aggregate(OrderNumber ~ CustomerId, rfmdata, FUN=length) 
# Calculate M
M_table <- aggregate(Value ~ CustomerId, rfmdata, FUN=sum) 
 # Merge R with F
RFM_table <- merge(R_table,F_table,by.x="CustomerId", by.y="CustomerId")
# Merge M into RF
RFM_table <- merge(RFM_table,M_table, by.x="CustomerId", by.y="CustomerId") 
# Remove unnecessary column
RFM_table$Date<- NULL 
# change names

names(RFM_table) <- c("CustomerId", "R", "F", "M") 
RFM_table <- RFM_table[order(RFM_table$R,-RFM_table$F,-RFM_table$M),]

RFM_table$NewR <- RFM_table$R * -1

RFM_table$Rsegment <- findInterval(RFM_table$NewR, quantile(RFM_table$NewR, c(0.0, 0.25, 0.50, 0.75, 1.0)))
RFM_table$Fsegment <- findInterval(RFM_table$F, quantile(RFM_table$F, c(0.0, 0.25, 0.50, 0.75, 1.0)))
RFM_table$Msegment <- findInterval(RFM_table$M, quantile(RFM_table$M, c(0.0, 0.25, 0.50, 0.75, 1.0)))
summary(RFM_table)




RFM_Score <- count(RFM_table, Rsegment, Fsegment,Msegment)
RFM_Score <- RFM_table$Rsegment*100 + RFM_table$Fsegment*10 + RFM_table$Msegment
salesRFM <- cbind(RFM_table,RFM_Score)

#plot
ggplot(salesRFM, aes(factor(RFM_Score))) +
  geom_bar() +
  ggtitle('Customer Distribution by RFM') +
  labs(x="RFM",y="# Customers") + 
  theme(plot.title = element_text(color="#666666", face="bold", size=16, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold"))

###############################################################
#customer segmentation (DBscan and Kmeans)
##############################################################

RFM_F <- RFM_table[c(1,6,7,8)]

#install.packages("fpc",dependencies = TRUE)
library(fpc)
install.packages("dbscan",dependencies = TRUE)
#choose best Eps
dbscan::kNNdistplot(RFM_F, k =  5)
abline(h = 5, lty = 2)

library(cluster)

dbscanOutput <- dbscan(RFM_F, eps=5, MinPts =5)
dbscanOutput$cluster

plotcluster(RFM_F, dbscanOutput$cluster)
clusplot(RFM_F, dbscanOutput$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

dbscanData <- data.frame(salesRFM,dbscanOutput$cluster)
summary(dbscanData)


kmeans <- kmeans(RFM_F[,-1],5)
plotcluster(RFM_F[,-1], kmeans$cluster)
library("cluster")

clusplot(RFM_F[,-1], kmeans$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
with(RFM_F[,-1], pairs(RFM_F, col=c(1:3)[kmeans$cluster])) 
factoextra::fviz_cluster(kmeans, data = RFM_F[,-1], frame.type = "convex")+ theme_minimal()

kmeansData <- data.frame(salesRFM,kmeans$cluster)

###############################################################
#Predicting customer’s segments (decision trees vs Random forest)
##############################################################





customersProfile <- unique(rfmdata[,c(3,6:9)])
df <- merge(kmeansData,customersProfile,by="CustomerId")
# divide customer by segmants
df$CustomerType <- ifelse(df$kmeans.cluster == 1, "New",ifelse(df$kmeans.cluster == 2,"Need-Based",ifelse(df$kmeans.cluster == 3,"Need-Based",ifelse(df$kmeans.cluster == 4,"Need-Based",ifelse(df$kmeans.cluster == 5,"Loyal", "NA")))))

barplot(table(df$CustomerType), col = c("lightblue", "mistyrose", "lightcyan", 
                                        "lavender"), xlab = "Customer Types")




df <- df[order(df$kmeans.cluster),]

#building model 
predict.model <- df[,11:15]

cols <- c('Gender', 'Education', 'MartialStatus', 'IncoveLevel', 'CustomerType')
predict.model[cols] <- lapply(predict.model[cols], as.factor)
#training and testing sets
#install.packages("rpart.plot",dependencies = TRUE)
library("rpart.plot")



set.seed(1)
index <- sample(1:nrow(predict.model),round(0.7*nrow(predict.model)))
train <- predict.model[index,]
test <- predict.model[-index,]


#attribute importance
#adaboost to figure attribute importance
install.packages("adabag",dependencies = TRUE)
library("adabag")
adaboost.result <- boosting(CustomerType~., data=train, mfinal=50)
adaboost.result$importance


#deceion trees
DT <- rpart(CustomerType ~ ., method= "class", control = rpart.control(minsplit = 1),
            data = train)


prp(DT, main="Decision Tree", box.palette="auto", faclen=0)
preict.model.test <- predict(DT, test, type = 'class')
#install.packages("caret",dependencies = TRUE)
library("caret")
confusionMatrix(table(preict.model.test, test$CustomerType))


#Random Forest
#install.packages("randomForest",dependencies =  TRUE)
library(randomForest)
modelRF <- randomForest(CustomerType ~ ., data = train,mtry =3)
modelRF.test <- predict(modelRF, test, type = 'class')
#install.packages("party",dependencies = TRUE)
library(party)
summary(modelRF)
plot(modelRF,log="y")
importance(modelRF)
varImpPlot(modelRF)


confusionMatrix(table(modelRF.test, test$CustomerType))






###############################################################
#Basket Analysis
##############################################################

basket <- ds[c(19,23)]

write.csv(basket,"ItemList.csv", row.names = TRUE)

#convert to transaction 
#install.packages("plyr", dependencies= TRUE)
trans <- read.transactions("ItemList.csv",rm.duplicates = TRUE, format = "single", sep = ",", cols = c("OrderID", "ProductName"))
summary(trans)


#install.packages("arules", dependencies=TRUE)
#install.packages("arulesViz", dependencies=TRUE)

library(arules)
library(arulesViz)

itemFrequencyPlot(trans,topN=5,type="absolute")
rules <- apriori(trans,parameter = list(supp = 0.001, conf = 0.9,minlen=2,maxlen=5));
rules<-sort(rules, by="confidence", decreasing=TRUE)
summary(rules)

#rules <- apriori(trans,parameter = list(supp = 0.005, conf = 0.002));

inspect(rules[1:25])
plot(rules[1:25],method="graph",interactive=TRUE,shading=NA)

