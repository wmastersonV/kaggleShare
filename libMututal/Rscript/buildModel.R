setwd("~/Desktop/gitRepos/kaggle/libMututal/Rscript")
train = read.csv("train.csv")
test = read.csv("test.csv")
table(train$Hazard)


#"NormalizedGini" is the other half of the metric. This function does most of the work, though
SumModelGini <- function(solution, submission) {
  df = data.frame(solution = solution, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  df
  df$random = (1:nrow(df))/nrow(df)
  df
  totalPos <- sum(df$solution)
  df$cumPosFound <- cumsum(df$solution) # this will store the cumulative number of positive examples found (used for computing "Model Lorentz")
  df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion of positive examples found ("Model Lorentz")
  df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
  print(df)
  return(sum(df$Gini))
}

NormalizedGini <- function(solution, submission) {
  SumModelGini(solution, submission) / SumModelGini(solution, solution)
}

library(neuralnet)
train1 = cbind(class.ind(paste0("h",train$Hazard)), train[,-c(1,2)])
names(train1)
# formula <- as.formula(paste(paste(names(train1)[1:50], collapse='+') ,  
#                  '~', paste(names(train1)[51:82], collapse='+') ))
# nn <- neuralnet(
#   formula ,
#   data=train1, hidden=20, err.fct='ce',
#   linear.output=FALSE)
facIndex = names(train) %in% c('T1_V4','T1_V5','T1_V6','T1_V7','T1_V8','T1_V9','T1_V11','T1_V12','T1_V15',
                               'T1_V16','T1_V17','T2_V3','T2_V5','T2_V11','T2_V12','T2_V13')
table(facIndex)
trainDummy = data.frame(lapply(names(train)[facIndex], function(x) class.ind(train[, x])))
train1 = cbind(train[,!facIndex], trainDummy)
formula <- as.formula(paste0('Hazard ~ ', paste(names(train1)[-c(1:2)], collapse='+') ))

nnSSE = neuralnet(
  formula ,
  data=train1[,-c(1)], hidden=50, err.fct='sse',
  linear.output=FALSE)
plot(nnSSE)
#change error.fct to use gini!
nnSSE$err.fct

# predFactor = prediction(nnFactor)
predTrain =  compute(nnSSE, train1[,-c(1:2)])
summary(predTrain$net.result)


#tranform Y's from numeric to categorical dummy
train2 = cbind(class.ind(paste0("h",train$Hazard)), train1)
formula2 <- as.formula(paste(paste(names(train2)[1:50], collapse='+') ,  
                 '~', paste(names(train2)[53:163], collapse='+') ))
nnCe <- neuralnet(formula2 , data=train2[,!(names(train2) %in%  c('Id','Hazard'))], hidden=20, err.fct='ce', linear.output=FALSE)
nnSSE$err.fct
predTrain =  compute(nnCe, train2[,-c(1:2)])
summary(predTrain$net.result)
