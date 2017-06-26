#source("https://bioconductor.org/biocLite.R")
#biocLite("BiocParallel")
library (readr)
library (lubridate)
library (stringr)
library (dplyr)
library (caret)
library (ggplot2)
library (plyr)
library ("BiocParallel")
library ("BatchJobs")
### Read&Initialize Apple Data #####
AppleData_Original = as.data.frame (read_csv ("aapl.csv"))
AppleData = AppleData_Original
MyColNames = colnames (AppleData)
MyColNames[1] = "MyDate"
colnames (AppleData) = MyColNames
AppleData$MyDate = dmy(AppleData$MyDate)
TempOpen = AppleData$Open
TempOpen = TempOpen[1:length(TempOpen)]
ChangeDayPrcnt = ((AppleData$Close[1:length(TempOpen)] - TempOpen) / AppleData$Open[1:length(TempOpen)]) * 100
ChangeDayPrcnt = c(ChangeDayPrcnt,rep("NA", times = nrow(AppleData) - length(ChangeDayPrcnt)))
ChangeDayPrcnt = as.numeric(ChangeDayPrcnt)
AppleData = cbind (AppleData, ChangeDayPrcnt)
MyColnames = colnames (AppleData)
MyColnames[length (MyColnames)] = "TodaysChange_Prcnt"
colnames (AppleData) = MyColnames
### Read&Initialize MSFT Data ####
MicroSoftData_Original = as.data.frame (read_csv ("msft.csv"))
MicroSoftData = MicroSoftData_Original
MyColNames = colnames (MicroSoftData)
MyColNames[1] = "MyDate"
colnames (MicroSoftData) = MyColNames
MicroSoftData$MyDate = dmy(MicroSoftData$MyDate)
TempOpen = MicroSoftData$Open
TempOpen = TempOpen[1:length(TempOpen)]
ChangeDayPrcnt = ((MicroSoftData$Close[1:length(TempOpen)] - TempOpen) / MicroSoftData$Open[1:length(TempOpen)]) * 100
ChangeDayPrcnt = c(ChangeDayPrcnt,rep("NA", times = nrow(MicroSoftData) - length(ChangeDayPrcnt)))
ChangeDayPrcnt = as.numeric(ChangeDayPrcnt)
MicroSoftData = cbind (MicroSoftData, ChangeDayPrcnt)
MyColnames = colnames (MicroSoftData)
MyColnames[length (MyColnames)] = "TodaysChange_Prcnt"
colnames (MicroSoftData) = MyColnames
### Pre_Process Apple Data ####
for (i in 1:7)
{
  TempOpen = AppleData$Open[(i+1):(length(AppleData$Open))]
  ChangeDayPrcnt = (((AppleData$Open[1:length(TempOpen)] - TempOpen) / TempOpen) *100)
  ChangeDayPrcnt = c(ChangeDayPrcnt,rep("NA", times = nrow(AppleData) - length(ChangeDayPrcnt)))
  ChangeDayPrcnt = as.numeric(ChangeDayPrcnt)
  AppleData = cbind (AppleData, ChangeDayPrcnt)
  MyColnames = colnames (AppleData)
  MyColnames[length (MyColnames)] = paste0("Prcnt_", i, "_DayChange")
  colnames (AppleData) = MyColnames
}
SDVolatile = NULL
for (i in 1:nrow(AppleData))
{
  TempVolatile = diff (AppleData$Open[i:(i+7)]) * -1
  SDVolatile = c(SDVolatile, sd(TempVolatile))
}
AppleData = cbind (AppleData, SDVolatile)
MyColnames = colnames (AppleData)
MyColnames[2:length(MyColnames)] = unlist (lapply ("Appl", paste0,MyColnames[2:length(MyColnames)]))
colnames (AppleData) = MyColnames
### Pre_Process MSFT Data ####
for (i in 1:7)
{
  TempOpen = MicroSoftData$Open[(i+1):(length(MicroSoftData$Open))]
  ChangeDayPrcnt = (((MicroSoftData$Open[1:length(TempOpen)] - TempOpen) / TempOpen) *100)
  ChangeDayPrcnt = c(ChangeDayPrcnt,rep("NA", times = nrow(MicroSoftData) - length(ChangeDayPrcnt)))
  ChangeDayPrcnt = as.numeric(ChangeDayPrcnt)
  MicroSoftData = cbind (MicroSoftData, ChangeDayPrcnt)
  MyColnames = colnames (MicroSoftData)
  MyColnames[length (MyColnames)] = paste0("Prcnt_", i, "_DayChange")
  colnames (MicroSoftData) = MyColnames
}
SDVolatile = NULL
for (i in 1:nrow(MicroSoftData))
{
  TempVolatile = diff (MicroSoftData$Open[i:(i+7)]) * -1
  SDVolatile = c(SDVolatile, sd(TempVolatile))
}
MicroSoftData = cbind (MicroSoftData, SDVolatile)
MyColnames = colnames (MicroSoftData)
MyColnames[2:length(MyColnames)] = unlist (lapply ("Msft", paste0,MyColnames[2:length(MyColnames)]))
colnames (MicroSoftData) = MyColnames
### Make Final Table ####
FinalDataTable = full_join (AppleData,MicroSoftData)
ToRemove = NULL
for (j in 1:ncol(FinalDataTable))
{
  ToRemove = c(ToRemove, which (is.na(FinalDataTable[,j]) == "TRUE"))
}
ToRemove = sort (unique(ToRemove))
FinalDataTable = FinalDataTable[-ToRemove,]
FinalDataTable$DayOfWeek = wday(FinalDataTable$MyDate)
FinalDataTable$WeekOfYear = week (FinalDataTable$MyDate)
FinalDataTable$DayOfYear = yday (FinalDataTable$MyDate)
FinalDataTable_Reg <- FinalDataTable[,c(1,6:15,20:32)]
FinalDataTable_Reg <- FinalDataTable_Reg[,c(3,1:2,4:24)]
FinalDataTable_Binary <- FinalDataTable_Reg[,c(1,4:10,13:20,22,23)]
FinalDataTable_Binary[,1:16] <- FinalDataTable_Binary[,1:16] > 0
FinalDataTable_Binary[,17] <- FinalDataTable_Binary[,17] > 3
FinalDataTable_Binary[,18] <- FinalDataTable_Binary[,18] > 46
for (i in 1:ncol (FinalDataTable_Binary))
{
  FinalDataTable_Binary[,i] <- as.factor (FinalDataTable_Binary[,i])
}

ChangeBin <- FinalDataTable$ApplTodaysChange_Prcnt
ChangeBin <- (ChangeBin > 0)
ChangeBin[which (ChangeBin == "TRUE")] <- "Gain"
ChangeBin[which (ChangeBin == "FALSE")] <- "Loss"
FinalDataTable$AppleTodaysChange_Bin <- ChangeBin
FinalDataTable_Preditors = FinalDataTable[,c(6,8:15,20, 22:33)]
FinalDataTable_Preditors = FinalDataTable_Preditors[,c(22,1:21)]
ExtremeValues <- which (FinalDataTable_Reg$ApplTodaysChange_Prcnt <= -4 | FinalDataTable_Reg$ApplTodaysChange_Prcnt >= 4)
FinalDataTable_Reg <- FinalDataTable_Reg[-ExtremeValues,]
# Rearrange Apple Volume
AppleVolume <- FinalDataTable_Reg$ApplVolume
AppleVolume <- AppleVolume[2:length(AppleVolume)]
# Same for Msft Data
MsftVolume <- FinalDataTable_Reg$MsftVolume
MsftVolume <- MsftVolume[2:length(MsftVolume)]
FinalDataTable_Reg <- FinalDataTable_Reg[1:(nrow(FinalDataTable_Reg)-1),] ## Remove last row as it does not have a previous day Volume
FinalDataTable_Reg$ApplPreviousDayVolume <- AppleVolume
FinalDataTable_Reg$MsftPreviousDayVolume <- MsftVolume
FinalDataTable_Reg <- FinalDataTable_Reg[,c(1,2,25,4:11,26,13:24)] ## Rearrange columns
#### Create Binary Table
FinalDataTable_Binary <- FinalDataTable_Reg
# Modify Volume column so that it can be made into binary
MedianVolume <- FinalDataTable_Binary[,c("ApplPreviousDayVolume")]
MedianVolume <- median (MedianVolume)
VolumeBin <- FinalDataTable_Binary[,c("ApplPreviousDayVolume")] > MedianVolume
FinalDataTable_Binary[,c("ApplPreviousDayVolume")] <- VolumeBin
# Do the same for Microsfot volume
MedianVolume <- FinalDataTable_Binary[,c("MsftPreviousDayVolume")]
MedianVolume <- median (MedianVolume)
VolumeBin <- FinalDataTable_Binary[,c("MsftPreviousDayVolume")] > MedianVolume
FinalDataTable_Binary[,c("MsftPreviousDayVolume")] <- VolumeBin
# Modify Volatile Column so it can be made binary
MedianVolatile <- FinalDataTable_Binary[,c("ApplSDVolatile")]
MedianVolatile <- median (MedianVolatile)
VolatileBin <- FinalDataTable_Binary[,c("ApplSDVolatile")] > MedianVolatile
FinalDataTable_Binary[,c("ApplSDVolatile")] <- VolatileBin
# For Msft now
MedianVolatile <- FinalDataTable_Binary[,c("MsftSDVolatile")]
MedianVolatile <- median (MedianVolatile)
VolatileBin <- FinalDataTable_Binary[,c("MsftSDVolatile")] > MedianVolatile
FinalDataTable_Binary[,c("MsftSDVolatile")] <- VolatileBin
# Remove Columns that cannot be encoded in binary
FinalDataTable_Binary <- FinalDataTable_Binary[,-c(2,24)]
FinalDataTable_Binary[,c(1,3:9,12:19)] <- FinalDataTable_Binary[,c(1,3:9,12:19)] > 0
FinalDataTable_Binary[,21] <- FinalDataTable_Binary[,21] > 3
FinalDataTable_Binary[,22] <- FinalDataTable_Binary[,22] > 46
ToDelete = ls ()
ToDelete = ToDelete[- which (ToDelete == "FinalDataTable_Reg" | ToDelete == "FinalDataTable_Binary")]
remove (list = ToDelete)
###### Exploratory Plots ###########
p1 = ggplot(data = FinalDataTable_Reg, aes(WeekOfYear, ApplTodaysChange_Prcnt))
p1 = p1 + geom_point()
p1
###### Caret Prediction Data Subsetting Scaling ############
FinalDataTable_Reg <- FinalDataTable_Reg[,-13]
FinalDataTable_Binary <- FinalDataTable_Binary[,-12]
TrainingSet = createDataPartition(FinalDataTable_Reg$ApplTodaysChange_Prcnt,p=0.75,list = F)
FinalDataTablePredictors_Train = FinalDataTable_Reg[TrainingSet,]
FinalDataTablePredictors_TEST = FinalDataTable_Reg[-TrainingSet,]
procValues = preProcess(FinalDataTablePredictors_Train[,-1], method = c("center","scale","YeoJohnson"))
trainScaled = predict (procValues, FinalDataTablePredictors_Train[,-1])
trainScaled$ApplTodaysChange_Prcnt = FinalDataTablePredictors_Train$ApplTodaysChange_Prcnt
trainScaled = trainScaled[c(ncol(trainScaled), 1:(ncol(trainScaled)-1))]
testScaled = predict (procValues, FinalDataTablePredictors_TEST[,-1])
testScaled$ApplTodaysChange_Prcnt = factor (FinalDataTablePredictors_TEST$ApplTodaysChange_Prcnt)
testScaled = testScaled[c(ncol(testScaled), 1:(ncol(testScaled)-1))]
MyColnames <- colnames (trainScaled)
###### Create Binay DataSet ########
#TrainingSet_Bin = createDataPartition(FinalDataTable_Binary$ApplTodaysChange_Prcnt,p=0.75,list = F)
FinalDataTable_Binary_Train <- FinalDataTable_Binary[TrainingSet,]
FinalDataTable_Binary_TEST <- FinalDataTable_Binary[-TrainingSet,]
for (i in 1:ncol(FinalDataTable_Binary_Train))
{
  FinalDataTable_Binary_Train[,i] <- as.factor(FinalDataTable_Binary_Train[,i])
  FinalDataTable_Binary_TEST[,i] <- as.factor(FinalDataTable_Binary_TEST[,i])
}
###### Optimising gbm ##############
#grid = expand.grid(.interaction.depth = seq(1,7,by=2), .n.trees = seq(100,1000, by = 50), .shrinkage = c(0.01,0.1))
#cntrl <- trainControl (method = "repeatedcv", repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE)
#MyVariables = ls()
#IndexToKeep = c(grep (MyVariables, pattern = "FinalDataTable_Train"), grep (MyVariables, pattern = "FinalDataTable_Train"))
#MyVariables = MyVariables[-IndexToKeep]
#remove(list = MyVariables)
#remove(MyVariables)
#gbmTune <- train (ApplTodaysChange_Prcnt ~ ApplPrcnt_1_DayChange,data = FinalDataTable_Train, method = "gbm", metric = "ROC", tuneGrid = grid, verbose = FALSE, trControl = cntrl)
## BRNN model
#modelFit_brnn = train (ApplTodaysChange_Prcnt ~ ., method = "brnn", data = trainScaled)
MyPredictors <- c(3:23) ## Choose predictors
MyPredictors_Combn <- NULL
MyPredictors_Combn <- list()
#modelFit_brnn <- train (x = trainScaled[,MyPredictors], y = trainScaled$ApplTodaysChange_Prcnt, method = "brnn") ## New syntax
## Test which predictors work best
for (i in 2:3)
{
  MyPredictors_Combn_Temp <- t(combn (MyPredictors, m = i))
  MyPredictors_Combn_Temp <- lapply(1:nrow(MyPredictors_Combn_Temp), FUN = function(i){MyPredictors_Combn_Temp[i,]})
  MyPredictors_Combn <- c (MyPredictors_Combn, MyPredictors_Combn_Temp)
}
#### Slit list into multiple lists ############
Chunks <- 19
if (length(MyPredictors_Combn)%%Chunks <= 1)
{
  Chunks <- Chunks - 1
}
MyPredictors_Combn_Chunks <- NULL
for (i in 1:Chunks)
{
  StartChunk <- (i * ceiling(length(MyPredictors_Combn)/Chunks)) - floor(length(MyPredictors_Combn)/Chunks)
  EndChunk <- i * ceiling(length(MyPredictors_Combn)/Chunks)
  if (EndChunk > length (MyPredictors_Combn))
  {
    EndChunk <- length (MyPredictors_Combn)
  }
  print (paste(StartChunk, EndChunk, (EndChunk-StartChunk)+1))
  MyPredictors_Combn_Chunks <- c(MyPredictors_Combn_Chunks, list (MyPredictors_Combn[c(StartChunk:EndChunk)]))
}

##### Function to perform BRNN on a list of preditors ######################
MyBRNNFun = function (MyPredictorSubset, trainScaled, testScaled, FinalDataTablePredictors_TEST)
{
  library (caret)
  OutData <- NULL
  OutData <- as.data.frame (OutData)
  for (i in 1:length (MyPredictorSubset))
  {
    Model_BRNN <- train (x = trainScaled[,MyPredictorSubset[[i]]], y = trainScaled$ApplTodaysChange_Prcnt, method = "svmRadial") ## New syntax
    BRNN_Matrix <- confusionMatrix (reference = (FinalDataTablePredictors_TEST$ApplTodaysChange_Prcnt > 0),
                                    data = predict (Model_BRNN, testScaled[,MyPredictorSubset[[i]]]) > 0 , positive = "TRUE")
    TotalPredictiveValue <- BRNN_Matrix$byClass[1] + BRNN_Matrix$byClass[2]
    OutDataTemp <- as.data.frame (TotalPredictiveValue)
    rownames (OutDataTemp) <- NULL
    OutDataTemp$CurrentPredictors <- paste0 (MyPredictorSubset[[i]], collapse = ",")
    OutData <- rbind(OutData,OutDataTemp)
  }
  return (OutData)
}
### Initialize BP parameters #####
setConfig(conf = list(fs.timeout=18000000))
setConfig (conf = list (staged.queries =TRUE))
funs = makeClusterFunctionsSGE("VerySimple.tmpl")
param = BatchJobsParam(workers = 100, resources = list(job.delay = TRUE, fs.timeout=18000000),cluster.function = funs)

OutDataAll <- bplapply (MyPredictors_Combn_Chunks, FUN = MyBRNNFun, BPPARAM = param, trainScaled = trainScaled, testScaled = testScaled, FinalDataTablePredictors_TEST = FinalDataTablePredictors_TEST)
#OutDataAll <- lapply (MyPredictors_Combn_Chunks, FUN = MyBRNNFun, trainScaled = trainScaled, testScaled = testScaled, FinalDataTablePredictors_TEST = FinalDataTablePredictors_TEST)
if (!file.exists ("OutDataAll_BP_svmRadial.rds"))
{
  saveRDS (OutDataAll, file = "OutDataAll_BP_svmRadial.rds")
}
OutDataAll_DF <- readRDS ("OutDataAll_BP_svmRadial.rds")
OutDataAll_DF <- ldply(OutDataAll_DF, data.frame)
NewBestFound <- TRUE
WhileLoopNumber = 1
while (NewBestFound)
{
  Best_TotalPredValue <- which (OutDataAll_DF$TotalPredictiveValue == max (OutDataAll_DF$TotalPredictiveValue))
  
  for (BestIndex in Best_TotalPredValue)
  {
    Best_Predictors <- OutDataAll_DF$CurrentPredictors[BestIndex]
    if (length(as.numeric (unlist (str_split (Best_Predictors, pattern = ",")))) < length(MyPredictors))
    {
      Remaining_Predictors <- setdiff (MyPredictors, as.numeric (unlist (str_split (Best_Predictors, pattern = ","))))
      for (CurrentRemaingPredictor in Remaining_Predictors)
      {
        Temp_Model_BRNN <- train (x = trainScaled[,c(as.numeric (unlist (str_split (Best_Predictors, pattern = ","))),CurrentRemaingPredictor)], 
                                  y = trainScaled$ApplTodaysChange_Prcnt, method = "glmStepAIC") ## New syntax
        Temp_BRNN_Matrix <- confusionMatrix (reference = (FinalDataTablePredictors_TEST$ApplTodaysChange_Prcnt > 0),
                                             data = predict (Temp_Model_BRNN, testScaled[,c(as.numeric (unlist (str_split (Best_Predictors, pattern = ","))),CurrentRemaingPredictor)]) > 0 ,
                                             positive = "TRUE")
        Temp_TotalPredictiveValue <- Temp_BRNN_Matrix$byClass[1] + Temp_BRNN_Matrix$byClass[2]
        if (Temp_TotalPredictiveValue > OutDataAll_DF$TotalPredictiveValue[BestIndex])
        {
          Temp_DF <- as.data.frame (Temp_TotalPredictiveValue)
          rownames (Temp_DF) <- NULL
          colnames(Temp_DF) <- "TotalPredictiveValue"
          Temp_DF$CurrentPredictors <- paste0 (c(as.numeric (unlist (str_split (Best_Predictors, pattern = ","))),CurrentRemaingPredictor), collapse = ",")
          OutDataAll_DF <- rbind (OutDataAll_DF, Temp_DF)
          NewBestFound <- TRUE
        } else (NewBestFound <- FALSE)
      }
      
    } else (NewBestFound <- FALSE)
    
  }
  print (paste ("While Loop number = ", WhileLoopNumber))
  WhileLoopNumber <- WhileLoopNumber + 1
}
if (!file.exists ("OutDataAll_BP_Mod_svmRadial.rds"))
{
  saveRDS (OutDataAll_DF, file = "OutDataAll_BP_Mod_svmRadial.rds")
}
OutDataAll_DF_Mod <- readRDS ("OutDataAll_BP_Mod_svmRadial.rds")
OutDataAll_DF_Mod <- ldply(OutDataAll_DF_Mod, data.frame)
