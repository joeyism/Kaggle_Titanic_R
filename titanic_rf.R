load("all_data.RData")
library(rpart)

all_data$Embarked[c(62, 830)] <- "S" # fill in empty ones with S


all_data$Embarked <- factor(all_data$Embarked) 
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE) # replace with median price

predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size, data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),]) # replace NA ages with predicted

train <- all_data[1:891,]
test <- all_data[892:1309,]

library(randomForest)

# Display structure
# str(test)

my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = train, importance = TRUE, ntree = 1000)

my_prediction <- predict(my_forest, test)

# Plot importance in forest, which shows Title is the most important, as the prediction would be most worse without it
varImpPlot(my_forest)

my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
