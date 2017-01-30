library(caret)
library(MASS)
library(klaR)
mainDir <- getwd()
subDir <- "data"
Dir <- paste(mainDir, subDir, sep = "/")
if(!file.exists(Dir)){
dir.create(file.path(mainDir, subDir))
}

train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
dest_file1 <- "./data/pml-training.csv"
dest_file2 <- "./data/pml-testing.csv"

if(!file.exists(dest_file1)){
    res_1 <- tryCatch(download.file(train_url, dest_file1, method= "auto"), error=function(e) 1)
    if(res_1!=1){
        load("./data/pml-training.csv")
    }
}
Pr_dataset <- read.csv("./data/pml-training.csv", header = TRUE, sep = ",", na.strings = "NA")
Pr_dataset[is.na(Pr_dataset)]<-0

if(!file.exists(dest_file2)){
    res_1 <- tryCatch(download.file(test_url, dest_file2, method= "auto"), error=function(e) 1)
    if(res_1!= 1) {
        load("./data/pml-testing.csv")
    }
}
Pr_test <- read.csv("./data/pml-testing.csv", header = TRUE, sep = ",", na.strings = "NA")
Pr_test[is.na(Pr_test)] <- "0"
classe<- c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ")
Pr_test$classe <- classe 

## feature selection
library(Boruta)
Pr_train <- Pr_dataset[complete.cases(Pr_dataset),]
set.seed(123)
boruta.train <- Boruta(classe~., data = Pr_train, doTrace= 2)
##png(file= "Projectplot.png")
print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
    boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
##dev.off()
final.boruta <- TentativeRoughFix(boruta.train)
fields <- getSelectedAttributes(final.boruta, withTentative = F)
fields <- c(fields,"classe")
flds <- fields[6:59]
##flds <- c(fields, -"X")
Pr_Select <- subset(Pr_train, select = flds)

## Model 
train_ctrl3 <- trainControl(method = "cv", number =5)
mod_3 <- train(classe~., data = Pr_Select, method= "rf", trControl = train_ctrl3)
pred_3 <- predict(mod_3, Pr_test)