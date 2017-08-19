        rm(list = ls())

        library(caret)
        library(rpart)
        library(rpart.plot)
        library(randomForest)
        library(corrplot)

        #Get the url of the file to be downloaded
        trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

        #check if file exists or else download from the link
        if(!file.exists("pml-training.csv")) {
                download.file(trainUrl, destfile = "pml-training.csv", method = "curl")
        }

        if(!file.exists("pml-testing.csv")) {
                download.file(testUrl, destfile = "pml-testing.csv", method = "curl")
        }

        #Read data file
        data.testing <- read.csv("pml-testing.csv")
        data.training <- read.csv("pml-training.csv")


        #remove columns with data not available
        data.training <- data.training[, colSums(is.na(data.training)) == 0]
        data.testing <- data.testing[, colSums(is.na(data.testing)) == 0]
        
        classe <- data.training$classe
        
        #Remove unwanted columns
        colRemove <- grepl("^X|timestamp|window", names(data.training))
        data.training <- data.training[, !colRemove]

        colRemove <- grepl("^X|timestamp|window", names(data.testing))
        data.testing <- data.testing[, !colRemove]
        
        
        #clean the data which is not numeric
        data.training <- data.training[, sapply(data.training, is.numeric)]
        data.testing <- data.testing[, sapply(data.testing, is.numeric)]

        data.training$classe <- classe
        
              
        

        #prep data for model training
        set.seed(22519)
        inTrain <- createDataPartition(data.training$classe, p = 0.7, list = FALSE)
        data4training <- data.training[inTrain, ]
        data4testing <- data.training[-inTrain, ]



        #Data modelling
        controlRf <- trainControl(method = "cv", number = 10)
        modelRf <- train(classe ~ ., data = data4training, method = "rf", trControl = controlRf, ntree =250)

 
        predictRf <- predict(modelRf, data4testing)
        confusionMatrix(data4testing$classe, predictRf)


        accuracy <- postResample(predictRf, data4testing$classe)
        accuracy
        oose <- 1 - as.numeric(confusionMatrix(data4testing$classe, predictRf)$overall[1])
        oose
        
        result <- predict(modelRf, data.testing[, -length(names(data.testing))])
        result
        
        corrPlot <- cor(data4training[, -length(names(data4training))])
        corrplot(corrPlot, method="color")
        
        treeModel <- rpart(classe ~ ., data=data4training, method="class")
        prp(treeModel) # fast plot