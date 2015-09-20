library(caret)
library(dplyr)
library(ggplot2)
library(randomForest)

rm(list=ls())
setwd("D:/coursera/sandbox/predmach")
trainUrl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
verifyUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

fbigdata <- "pml-training.csv"
fverify <- "pml-testing.csv"

if(!file.exists(fbigdata)) {
    download.file(trainUrl, fbigdata, method="auto")
    message("Training file downloaded from URL")
}

if(!file.exists(fverify)) {
    download.file(verifyUrl, fverify, method="auto")
    message("Testset file downloaded from URL")
}

dDB <- read.csv(fbigdata) 
DB_dim <- dim(dDB)
paste("Train file dimension is ", DB_dim[1],"x",DB_dim[2])

dVerify <- read.csv(fverify)
verify_dim <- dim(dVerify)
paste0("Verify file dimension is ", verify_dim[1],"x",verify_dim[2])

cnames <- data.frame(coln=colnames(dDB))
cnames <- mutate(cnames, idx = as.numeric(rownames(cnames)), measurement = !grepl("^X|timestamp|window|user|classe", coln))
mcol <- filter(cnames, measurement==TRUE) %>% select(idx)

# for (i in seq(mcol$idx)) {
#     dDB[,mcol$idx[i]] <- as.numeric(dDB[,mcol$idx[i]])
# }

v<-data.frame() # temporary variable
for (i  in 1:DB_dim[2]) {
    r <- data.frame(idx=i, coln=names(dDB)[i], isna = length(which(is.na(dDB[,i]))))
    v<-rbind(v,r)
}

v <- mutate(v, extra_info = grepl("^X|timestamp|window", coln),measurement = !grepl("^X|timestamp|window|user|classe", coln))


measurementCols <- filter(v, extra_info==FALSE, isna==0)
#complete_cols <- filter(v, isna==0)
rm("v") # remove temporary
rm("r") # remove temporary

complete_measurement_cols <- measurementCols
dDB_subset <- select(dDB, complete_measurement_cols$idx)

#attempt to use numerical fields only, due to memory limitation
classetemp <- dDB_subset$classe

dDB_num_subset_idx <- sapply(dDB_subset, is.numeric)

dDB_num_subset <- cbind(classe=classetemp, dDB_subset[,dDB_num_subset_idx])


## pseudo cleaning complete ... 

set.seed(77892) ## process with subset
# dDB_subset_part <- createDataPartition(dDB_subset$classe, p=0.65, list=F)
# dDB_subset_train <- dDB_subset[dDB_subset_part, ]
# dDB_subset_test <- dDB_subset[-dDB_subset_part, ]

#dDB_model_def <- train(classe ~ ., data=dDB_subset_train)
# dDB_model_pca <- train(classe ~ ., data=dDB_subset_train, preProcess="pca")
#dDB_model_rpart <- train(classe ~ ., data=dDB_subset_train, method="rpart")
#dDB_model_rf <- train(classe ~ ., data=dDB_subset_train, method="rf")
#dDB_model_boost <- train(classe ~ ., data=dDB_subset_train, method="rpart")


set.seed(13927) ## process with complete cases
dDB_num_subset_part <- createDataPartition(dDB_num_subset$classe, p=0.65, list=F)
dDB_num_subset_train <- dDB_num_subset[dDB_num_subset_part, ]
dDB_num_subset_test <- dDB_num_subset[-dDB_num_subset_part, ]

ctrlRf <- trainControl(method="cv", 5)
dDB_cc_model_def <- train(classe ~ ., data=dDB_num_subset_train, method="rf", 
                          preProcess="pca", trControl=ctrlRf, ntree=250)
#dDB_cc_model_def <- train(classe ~ ., data=dDB_num_subset_train, method="rf", preProcess="pca")
#dDB_cc_model_pca <- train(classe ~ ., data=dDB_num_subset_train, preProcess="pca")
#dDB_cc_model_rpart <- train(classe ~ ., data=dDB_num_subset_train, method="rpart")
#dDB_cc_model_rf <- train(classe ~ ., data=dDB_num_subset_train, method="rf")
#dDB_cc_model_boost <- train(classe ~ ., data=dDB_num_subset_train, method="rpart")
