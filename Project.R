setwd("C:/Users/Tony/Coursera/08_pml/Project")
pml.training <- read.csv("./pml-training.csv")
pml.testing <- read.csv("./pml-testing.csv")

table(pml.training$classe)
head(pml.training)

# remove near-zero variance variables
nzv <- nearZeroVar(pml.training)
pml.training <- pml.training[, -nzv]
nzv <- nearZeroVar(pml.testing)
pml.testing <- pml.testing[, -nzv]

# remove NA variables from training dataset
columnNACounts <- colSums(is.na(pml.training))      
badColumns <- columnNACounts >= 19000          
pml.training <- pml.training[!badColumns]      
sum(is.na(pml.training))                    

# remove NA variables from testing dataset
columnNACounts <- colSums(is.na(pml.testing))       
badColumns <- columnNACounts >= 20            
pml.testing <- pml.testing[!badColumns]   
sum(is.na(pml.testing))                   

library(caret)

inTrain <- createDataPartition(y = pml.training$classe, p = 0.4, list = FALSE)
pml.training2 <- pml.training[inTrain, ]  # 3927 obs. of 56 variables
pml.cv <- pml.training[-inTrain, ]  # test set for cross validation

modFit <- train(pml.training2$classe ~ ., data = pml.training2, method = "rpart")
modFit$results

modelFit <- train(classe ~ ., method = "rf", data = pml.training2, trControl = trainControl(method = "cv", number = 4), importance = TRUE)
modFit2

cv <- predict(modFit, pml.cv)
table(cv==pml.cv$classe)
confusionMatrix(cv, pml.cv$classe)
#Out of sample error is 1-.9998 = 0.0002%


pred2 <- predict(modFit, pml.testing)
pml.testing$classe <- pred2

pml_write_files = function(x) {
        n = length(x)
        for (i in 1:n) {
                filename = paste0("problem_id_", i, ".txt")
                write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                            col.names = FALSE)
        }
}


answers <- pml.testing$classe

pml_write_files(answers)
answers

