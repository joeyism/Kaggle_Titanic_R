library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

readkey <- function()
{
    cat ("Press [enter] to continue")
    line <- readline()
}

train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)
  
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)

# Viewing
#table(train$Survived)
#prop.table(table(train$Survived))
#table(train$Sex, train$Survived)
#prop.table(table(train$Sex, train$Survived), 1)

train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0

# Assuming all females survive 
test_one <- test
test_one$Survived <- 0
test_one$Survived[test_one$Sex == "female"] <- 1

# Classification tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")

plot(my_tree_two)
readkey()
text(my_tree_two)
readkey()


pdf("plots.pdf")
fancyRpartPlot(my_tree_two)

# Classification with control
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))
fancyRpartPlot(my_tree_three)

train_two <- train
train_two$family_size <- train_two$SibSp + train_two$Parch + 1
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, data = train_two, method = "class")

fancyRpartPlot(my_tree_four)


dev.off()



# Prediction 
my_prediction <- predict(my_tree_two, newdata = test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)


print("Done")
