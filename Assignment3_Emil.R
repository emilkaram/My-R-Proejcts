#Preparation:

library(RCurl)

my_url <- getURI("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv")
                
my_data <- read.csv(text = my_url,header = TRUE, sep = ";")


#Q1
str(my_data)
summary(my_data)
head(my_data)
sum(is.na.data.frame(my_data))
# No missing data


#Q2
my_cor <- cor(my_data[,-12], method = "pearson")
my_cor
library(corrplot)
corrplot(my_cor, method="circle")


#Q3
barplot(my_data$quality)


#Q4
class(my_data$quality)
levels(my_data$quality)
summary(my_data$quality)

my_data$quality_Levels <- cut(my_data$quality ,breaks = c(3,5,7,9),labels = c('low','medium','high'))

class(my_data$quality_Levels)
levels(my_data$quality_Levels)



#Q5
my_data_scaled <- scale(my_data[,-12:-13])
my_data_scaled<- data.frame(my_data_scaled ,my_data$quality ,my_data$quality_Levels)


#Q6
samp_size <- floor(0.80* nrow(my_data_scaled))
samp_size
set.seed(100)
train_index <- sample(nrow(my_data_scaled),size = samp_size)
train_index

my_data_train <- my_data_scaled[train_index,-12:-13]
my_data_test  <- my_data_scaled[-train_index,-12:-13]

my_data_train_label <- my_data_scaled[train_index,12]
my_data_test_label <- my_data_scaled[-train_index,12]


#Q7
#install.packages("class")
#install.packages("gmodels")
library(class)
library(gmodels)
my_test_pred <- knn(train = my_data_train, test = my_data_test ,cl = my_data_train_label, k=10)
my_test_pred


#Q8
CrossTable(x=my_data_test_label, y=my_test_pred, prop.chisq=FALSE)




