library(car)
data=N2_nf4
#data=N2_nf5
# DELETING THE EMPTY ROWS AND ANGLE OVER 270.
#skewerAngle
summary(dt$SkewerAngle)
summary(dt_n2nf5$SkewerAngle)
#summary(tph1_f6$SkewerAngle)
par(mfrow=c(2,1))
plot(dt_n2nf5$ElapsedTime,dt_n2nf5$SkewerAngle)
plot(dt$ElapsedTime,dt$SkewerAngle)


# Ixx
summary(dt$Ixx)
summary(dt_n2nf5$Ixx)
#summary(tph1_f6$Ixx)
par(mfrow=c(2,1))
plot(dt_n2nf5$ElapsedTime,dt_n2nf5$Ixx)
plot(dt$ElapsedTime,dt$Ixx)

# Ixy
summary(dt$Ixy)
summary(dt_n2nf5$Ixy)
#summary(tph1_f6$Ixy)
par(mfrow=c(2,1))
plot(dt_n2nf5$ElapsedTime,dt_n2nf5$Ixy)
plot(dt$ElapsedTime,dt$Ixy)


# Iyy
summary(dt$Iyy)
summary(dt_n2nf5$Iyy)
#summary(tph1_f6$Iyy)
par(mfrow=c(2,1))
plot(dt_n2nf5$ElapsedTime,dt_n2nf5$Iyy)
plot(dt$ElapsedTime,dt$Iyy)


tapply(N2_nf4$SkewerAngle,N2_nf4$Kim, summary)
tapply(N2_nf4$ElapsedTime,N2_nf4$Kim, summary)





#data <- data[!apply(is.na(data) | data == "", 1, all),]
data[data==""] <- NA
data<-na.omit(data)
data$Kim<-recode(data$Kim,"c('Forward-NTD','Forward-Shallow')='Foward'")
data$Kim<-recode(data$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")

summary(data$SkewerAngle)
summary(data$ComptFactor)
boxplot(data$ComptFactor)
dt=data[data$SkewerAngle<=270, ]
dt=dt[dt$ElapsedTime<=1400,]
Num_data=dt[,which(names(dt) %in% c('ElapsedTime','MaxWidth','ComptFactor','Ixx','Iyy','Ixy','SkewerAngle'))]
cor(Num_data)

summary(dt$SkewerAngle)
tapply(dt$SkewerAngle,dt$Kim, summary)
tapply(dt$ElapsedTime,dt$Kim, summary)



#nums <- sapply(data, is.numeric)
#cor(data[,nums])
# REFERENCE variables: SeqNum, FrameNum, Resol, CameraStartRow, CameraStartCol,
#CameraStepRows CameraStepCols CameraOffsetRows CameraOffsetCols CropOffsetRows
#CropOffsetCols TotalOffsetRows TotalOffsetCols LclCentroidRow LclCentroidCol
# GblCentroidRow GblCentroidCol NumRows NumCols HeadRow      HeadCol      TailRow 
# TailCol HeadCurvPtRow HeadCurvPtCol TailCurvPtRow TailCurvPtCol IntH IntT
# COLUMNS NOT CORRELATED WITH LABEL: ISLOOP
# COLUMNS WITH UNIQUE VALUE
##### 51 features - 9 columns with one value=42 columns
uniquelength <- sapply(data,function(x) length(unique(x)))
data <- subset(data, select=uniquelength>1) 
##### 42 features - 1 variables'isloop' not correlated with label, and 20 reference variables.
plot(data$Posture, data$Kim)
library(lubridate)

sapply(data, class)
data <-data[ , -which(names(data) %in% c('SeqNum','FrameNum','CameraStepRows','CameraStepCols','CameraOffsetRows',
                                       'CameraOffsetCols','TotalOffsetRows','TotalOffsetCols','LclCentroidRow',
                                       'LclCentroidCol','GblCentroidRow','GblCentroidCol','HeadRow','HeadCol',
                                       'TailRow','TailCol','HeadCurvPtRow','HeadCurvPtCol','TailCurvPtRow','TailCurvPtCol','IsLoop'))]

###### now 21 variables
### 2 categoricals: 'Posture', and 'Kim'
### 2 varaibles with i(maybe error) in cells: 'RectBigSide', 'RectRatio'.
# calculate the correlation between numerica data
Num_data=data[,-which(names(data) %in% c('Posture','Kim','RectBigSide', 'RectRatio'))]
sapply(Num_data, class)
## it seems Area is an type integer, need to change into numeric
Num_data$Area <- sapply(Num_data$Area, as.numeric)
## error in variables 'RectBigSide', 'RectRatio'.
#Num_data$RectRatio <- sapply(Num_data$RectRatio, as.character)
#Num_data$RectBigSide <- sapply(Num_data$RectBigSide, as.character)

#Num_data[!grepl("[A-Za-z]", Num_data$RectRatio),]#delete error rows contain character i, firstly convert this column to char, then convert back to numeric
#Num_data[!grepl("[A-Za-z]", Num_data$RectBigSide),]
#Num_data$RectRatio <- sapply(Num_data$RectRatio, as.numeric)
#Num_data$RectBigSide <- sapply(Num_data$RectBigSide, as.numeric)
#Num_data<-Num_data[!(class(Num_data$RectRatio)=="complex"),]
#Num_data=Num_data[-which(sapply(Num_data$RectBigSide, class) == "Complex"),]
cor(Num_data)
# Num_data including variables that are highly correled with others, for example, Heywood, Elongation, 
# MajorAxisLength,MinorAxisLength,Perimeter,Ixx,CurHead,CurvTail
ds=Num_data[,-which(names(Num_data) %in% c('Heywood','CurvHead','Area','Hydraulic', 'Elongation', 'Length','MajorAxisLength','MinorAxisLength','Perimeter','Ixx','CurHead','CurvTail'))]
cor(ds)
#!!!!!!! the numerica data we are going to keep are :ElapsedTime,ComptFactor,Iyy,Ixy,MaxWidth,SkewerAngle !!!!!!
# now ,check the categorical data Posture 
library(car)
summary(data$Kim)

plot(data$Posture,data$Kim) # do not need to keep this categorical data
## now normalize the data
#ds=Num_data
normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}
ds.norm=as.data.frame(lapply(ds, normalized))
#so we just add label:
ds.norm$Kim=data$Kim
#ds.norm$Kim=dt$Kim
# now try random forest
# try random forest
newdf=ds.norm
library(randomForest)
library(readr)

###### START-TRY SOMEING DIFFERENT HERE, YOU CAN IGNORE IT LATER...##################
#install.packages('party')
#library(party)
#set.seed(415)
#fit <- cforest(as.factor(Kim) ~  ElapsedTime+ComptFactor+Iyy+Ixy+MaxWidth+SkewerAngle,
#                 data = newdf, 
#                 controls=cforest_unbiased(ntree=2000, mtry=3))

###### END-TRY SOMEING DIFFERENT HERE, YOU CAN IGNORE IT LATER...##################
smp_size <- floor(0.75 * nrow(newdf))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(newdf)), size = smp_size)
train <- newdf[train_ind, ]
test <- newdf[-train_ind, ]

## SVM

#library(e1071)

#x=subset(newdf, -Status_Label)

#model <- svm(as.factor(Kim) ~ .,train)



#pred <- predict(model,test)
#table(pred,test$Kim)

## RF
set.seed(415)
fit <- randomForest(as.factor(Kim) ~ ComptFactor+Ixx+Iyy+Ixy+SkewerAngle,data=train, 
                    importance=TRUE, 
                    ntree=200)
Prediction <- predict(fit, test)
table(Prediction,test$Kim)

varImpPlot(fit)
rf_n2nf4<-table(test$Kim, Prediction, dnn=c("Label by Kim","Prediction by RF"))

write.csv(rf_n2nf4,"rf_n2nf4.csv")

# now use the model to predict N2_nf5 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
df=N2_nf5
#df <- df[!apply(is.na(df) | df == "", 1, all),]
df[df==""] <- NA
df<-na.omit(df)
df$Kim<-recode(df$Kim,"c('Forward-NTD','Forward-Shallow')='Foward'")
df$Kim<-recode(df$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")
df1=df[,which(names(df) %in% c('ElapsedTime', 'ComptFactor','Iyy','Ixy','MaxWidth', 'SkewerAngle','Kim'))]
df=df[,which(names(df) %in% c('ElapsedTime', 'ComptFactor','Iyy','Ixy','MaxWidth', 'SkewerAngle'))]
cor(df)
library(car)
## now normalize the df
normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}
df.norm=as.data.frame(lapply(df, normalized))
#so we just add label:
df.norm$Kim=df1$Kim

# now do the prediction for N2_nf1, using model from N2_nf4 !!!!!!!!!!!!!!!!!!

Prediction <- predict(fit, df.norm)
table(Prediction,df.norm$Kim)

#rf_n2nf4<-table(test$Kim, Prediction, dnn=c("Label by Kim","Prediction by RF"))

#write.csv(rf_n2nf4,"rf_n2nf4.csv")
########## how about predicitng tph1_f6:
df=New_N2_f1
#df <- df[!apply(is.na(df) | df == "", 1, all),]
df[df==""] <- NA
df<-na.omit(df)
df$Kim<-recode(df$Kim,"c('Forward-NTD','Forward-Shallow')='Foward'")
df$Kim<-recode(df$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")
df1=df[,which(names(df) %in% c('ElapsedTime', 'ComptFactor','Iyy','Ixy','MaxWidth', 'SkewerAngle','Kim'))]
df=df[,which(names(df) %in% c('ElapsedTime', 'ComptFactor','Iyy','Ixy','MaxWidth', 'SkewerAngle'))]
cor(df)
library(car)
## now normalize the df
normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}
df.norm=as.data.frame(lapply(df, normalized))
#so we just add label:
df.norm$Kim=df1$Kim

# now do the prediction, using model from N2_nf4

Prediction <- predict(fit, df.norm)
table(Prediction,df.norm$Kim)

############################# use N2_f1 to train a model predict iteself and other two N2

data=New_N2_f1[,-51]
#colnames(data)[which(names(data) == "Me")] <- "Kim"

#data <- data[!apply(is.na(data) | data == "", 1, all),]
data[data==""] <- NA
data<-na.omit(data)
data$Kim<-recode(data$Kim,"c('Forward-NTD','Forward-Shallow')='Foward'")
data$Kim<-recode(data$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")

#nums <- sapply(data, is.numeric)
#cor(data[,nums])
# REFERENCE variables: SeqNum, FrameNum, Resol, CameraStartRow, CameraStartCol,
#CameraStepRows CameraStepCols CameraOffsetRows CameraOffsetCols CropOffsetRows
#CropOffsetCols TotalOffsetRows TotalOffsetCols LclCentroidRow LclCentroidCol
# GblCentroidRow GblCentroidCol NumRows NumCols HeadRow      HeadCol      TailRow 
# TailCol HeadCurvPtRow HeadCurvPtCol TailCurvPtRow TailCurvPtCol IntH IntT
# COLUMNS NOT CORRELATED WITH LABEL: ISLOOP
# COLUMNS WITH UNIQUE VALUE
##### 51 features - 9 columns with one value=42 columns
uniquelength <- sapply(data,function(x) length(unique(x)))
data <- subset(data, select=uniquelength>1) 
##### 42 features - 1 variables'isloop' not correlated with label, and 20 reference variables.
plot(data$Posture, data$Kim)
library(lubridate)

sapply(data, class)
data <-data[ , -which(names(data) %in% c('SeqNum','FrameNum','CameraStepRows','CameraStepCols','CameraOffsetRows',
                                         'CameraOffsetCols','TotalOffsetRows','TotalOffsetCols','LclCentroidRow',
                                         'LclCentroidCol','GblCentroidRow','GblCentroidCol','HeadRow','HeadCol',
                                         'TailRow','TailCol','HeadCurvPtRow','HeadCurvPtCol','TailCurvPtRow','TailCurvPtCol','IsLoop'))]

###### now 21 variables
### 2 categoricals: 'Posture', and 'Kim'
### 2 varaibles with i(maybe error) in cells: 'RectBigSide', 'RectRatio'.
# calculate the correlation between numerica data
Num_data=data[,-which(names(data) %in% c('Posture','Kim','RectBigSide', 'RectRatio'))]
sapply(Num_data, class)
## it seems Area is an type integer, need to change into numeric
Num_data$Area <- sapply(Num_data$Area, as.numeric)
## error in variables 'RectBigSide', 'RectRatio'.
#Num_data$RectRatio <- sapply(Num_data$RectRatio, as.character)
#Num_data$RectBigSide <- sapply(Num_data$RectBigSide, as.character)

#Num_data[!grepl("[A-Za-z]", Num_data$RectRatio),]#delete error rows contain character i, firstly convert this column to char, then convert back to numeric
#Num_data[!grepl("[A-Za-z]", Num_data$RectBigSide),]
#Num_data$RectRatio <- sapply(Num_data$RectRatio, as.numeric)
#Num_data$RectBigSide <- sapply(Num_data$RectBigSide, as.numeric)
#Num_data<-Num_data[!(class(Num_data$RectRatio)=="complex"),]
#Num_data=Num_data[-which(sapply(Num_data$RectBigSide, class) == "Complex"),]
cor(Num_data)
# Num_data including variables that are highly correled with others, for example, Heywood, Elongation, 
# MajorAxisLength,MinorAxisLength,Perimeter,Ixx,Length
#different from n2_nf4, this time CurHead,CurvTail could be saved
#ds=Num_data[,-which(names(Num_data) %in% c('Heywood','Hydraulic', 'Elongation', 'Length','MajorAxisLength','MinorAxisLength','Perimeter','Ixx','CurHead','CurvTail'))]
ds=Num_data[,-which(names(Num_data) %in% c('Heywood', 'Hydraulic','Elongation', 'Length','MajorAxisLength','MinorAxisLength','Perimeter','Ixx'))]

cor(ds)
#!!!!!!! the numerica data we are going to keep are :ElapsedTime,ComptFactor,Iyy,Ixy,MaxWidth,SkewerAngle, Area, CurvHead,CurvTail !!!!!!
# now ,check the categorical data Posture 
library(car)
summary(data$Kim)

plot(data$Posture,data$Kim) #need to keep this categorical data
## now normalize the data
normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}
ds.norm=as.data.frame(lapply(ds, normalized))
#so we just add label:
ds.norm$Kim=data$Kim
ds.norm$Posture=data$Posture
# now try random forest
# try random forest
newdf=ds.norm
library(randomForest)
library(readr)
smp_size <- floor(0.75 * nrow(newdf))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(newdf)), size = smp_size)
train <- newdf[train_ind, ]
test <- newdf[-train_ind, ]

set.seed(415)
fit <- randomForest(as.factor(Kim) ~  ElapsedTime+ComptFactor+Iyy+Ixy+MaxWidth+SkewerAngle+Area+CurvHead+CurvTail+Posture,data=train, 
                    importance=TRUE, 
                    ntree=200)
Prediction <- predict(fit, test)
table(Prediction,test$Kim)
varImpPlot(fit)
rf_n2f1<-table(test$Kim, Prediction, dnn=c("Label by Kim","Prediction by RF"))
write.csv(rf_n2f1,"rf_n2f1.csv")

## predict n2_NF4
ds=Num_data[,-which(names(Num_data) %in% c('Heywood','Hydraulic', 'Elongation', 'Length','MajorAxisLength','MinorAxisLength','Perimeter','Ixx'))]

## now normalize the data
normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}
ds.norm=as.data.frame(lapply(ds, normalized))
#so we just add label:
ds.norm$Kim=data$Kim
ds.norm$Posture=data$Posture
# now try random forest
# try random forest
newdf=ds.norm
Prediction <- predict(fit, newdf)
table(Prediction,newdf$Kim)

### now combine n2_f1 & n2_nf4 to predict n2_nf5!!!!!!!!!!!!!!!!!!!!!!!!!!!



data=rbind(N2_nf4,New_N2_f1[,-51])
#data <- data[!apply(is.na(data) | data == "", 1, all),]
data[data==""] <- NA
data<-na.omit(data)
data$Kim<-recode(data$Kim,"c('Forward-NTD','Forward-Shallow')='Foward'")
data$Kim<-recode(data$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")

#nums <- sapply(data, is.numeric)
#cor(data[,nums])
# REFERENCE variables: SeqNum, FrameNum, Resol, CameraStartRow, CameraStartCol,
#CameraStepRows CameraStepCols CameraOffsetRows CameraOffsetCols CropOffsetRows
#CropOffsetCols TotalOffsetRows TotalOffsetCols LclCentroidRow LclCentroidCol
# GblCentroidRow GblCentroidCol NumRows NumCols HeadRow      HeadCol      TailRow 
# TailCol HeadCurvPtRow HeadCurvPtCol TailCurvPtRow TailCurvPtCol IntH IntT
# COLUMNS NOT CORRELATED WITH LABEL: ISLOOP
# COLUMNS WITH UNIQUE VALUE
##### 51 features - 9 columns with one value=42 columns
uniquelength <- sapply(data,function(x) length(unique(x)))
data <- subset(data, select=uniquelength>1) 
##### 42 features - 1 variables'isloop' not correlated with label, and 20 reference variables.
plot(data$Posture, data$Kim)
library(lubridate)

sapply(data, class)
data <-data[ , -which(names(data) %in% c('SeqNum','FrameNum','CameraStepRows','CameraStepCols','CameraOffsetRows',
                                         'CameraOffsetCols','TotalOffsetRows','TotalOffsetCols','LclCentroidRow',
                                         'LclCentroidCol','GblCentroidRow','GblCentroidCol','HeadRow','HeadCol',
                                         'TailRow','TailCol','HeadCurvPtRow','HeadCurvPtCol','TailCurvPtRow','TailCurvPtCol','IsLoop'))]

###### now 21 variables
### 2 categoricals: 'Posture', and 'Kim'
### 2 varaibles with i(maybe error) in cells: 'RectBigSide', 'RectRatio'.
# calculate the correlation between numerica data
Num_data=data[,-which(names(data) %in% c('Posture','Kim','RectBigSide', 'RectRatio'))]
sapply(Num_data, class)
## it seems Area is an type integer, need to change into numeric
Num_data$Area <- sapply(Num_data$Area, as.numeric)
## error in variables 'RectBigSide', 'RectRatio'.
#Num_data$RectRatio <- sapply(Num_data$RectRatio, as.character)
#Num_data$RectBigSide <- sapply(Num_data$RectBigSide, as.character)

#Num_data[!grepl("[A-Za-z]", Num_data$RectRatio),]#delete error rows contain character i, firstly convert this column to char, then convert back to numeric
#Num_data[!grepl("[A-Za-z]", Num_data$RectBigSide),]
#Num_data$RectRatio <- sapply(Num_data$RectRatio, as.numeric)
#Num_data$RectBigSide <- sapply(Num_data$RectBigSide, as.numeric)
#Num_data<-Num_data[!(class(Num_data$RectRatio)=="complex"),]
#Num_data=Num_data[-which(sapply(Num_data$RectBigSide, class) == "Complex"),]
cor(Num_data)
# Num_data including variables that are highly correled with others, for example, Heywood, Elongation, 
# MajorAxisLength,MinorAxisLength,Perimeter,Ixx
ds=Num_data[,-which(names(Num_data) %in% c('Heywood','Hydraulic', 'Elongation', 'Length','MajorAxisLength','MinorAxisLength','Perimeter','Ixx','CurvTail','CurvHead'))]
cor(ds)
#!!!!!!! the numerica data we are going to keep are :ElapsedTime,ComptFactor,Iyy,Ixy,MaxWidth,SkewerAngle !!!!!!
# now ,check the categorical data Posture 
library(car)
summary(data$Kim)

plot(data$Posture,data$Kim) # do not need to keep this categorical data
## now normalize the data
normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}
ds.norm=as.data.frame(lapply(ds, normalized))
#so we just add label:
ds.norm$Kim=data$Kim
# now try random forest
# try random forest
newdf=ds.norm
library(randomForest)
library(readr)
smp_size <- floor(0.75 * nrow(newdf))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(newdf)), size = smp_size)
train <- newdf[train_ind, ]
test <- newdf[-train_ind, ]

set.seed(415)
fit <- randomForest(as.factor(Kim) ~  ElapsedTime+ComptFactor+Iyy+Ixy+MaxWidth+SkewerAngle,data=train, 
                    importance=TRUE, 
                    ntree=200)
Prediction <- predict(fit, test)
table(Prediction,test$Kim)
varImpPlot(fit)
rf_n2nf4n1self<-table(test$Kim, Prediction, dnn=c("Label by Kim","Prediction by RF"))

write.csv(rf_n2nf4n1self,"rf_n2nf4n1self.csv")

#^^^^^^^^^^^^^ now use the previous model to predict n2_nf5^^^^^^^^^^^^^^
df=N2_nf5
#df <- df[!apply(is.na(df) | df == "", 1, all),]
df[df==""] <- NA
df<-na.omit(df)
df$Kim<-recode(df$Kim,"c('Forward-NTD','Forward-Shallow')='Foward'")
df$Kim<-recode(df$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")
df1=df[,which(names(df) %in% c('ElapsedTime', 'Area','ComptFactor','Iyy','Ixy','MaxWidth', 'SkewerAngle','Kim'))]
df=df[,which(names(df) %in% c('ElapsedTime', 'Area','ComptFactor','Iyy','Ixy','MaxWidth', 'SkewerAngle'))]
cor(df)
library(car)
## now normalize the df
normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}
df.norm=as.data.frame(lapply(df, normalized))
#so we just add label:
df.norm$Kim=df1$Kim

# now do the prediction for N2_nf1, using model from N2_nf4 !!!!!!!!!!!!!!!!!!

Prediction <- predict(fit, df.norm)
table(Prediction,df.norm$Kim)
rf_n2nf4n1nf5<-table(df.norm$Kim, Prediction, dnn=c("Label by Kim","Prediction by RF"))

write.csv(rf_n2nf4n1nf5,"rf_n2nf4n1nf5.csv")


############ combine 3 dataset n2_nf4,tph1f6 , n2f1, to predict n2nf5






data=rbind(N2_nf4,New_N2_f1[,-51], tph1_f6)
#data <- data[!apply(is.na(data) | data == "", 1, all),]
data[data==""] <- NA
data<-na.omit(data)
data$Kim<-recode(data$Kim,"c('Forward-NTD','Forward-Shallow')='Foward'")
data$Kim<-recode(data$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")

#nums <- sapply(data, is.numeric)
#cor(data[,nums])
# REFERENCE variables: SeqNum, FrameNum, Resol, CameraStartRow, CameraStartCol,
#CameraStepRows CameraStepCols CameraOffsetRows CameraOffsetCols CropOffsetRows
#CropOffsetCols TotalOffsetRows TotalOffsetCols LclCentroidRow LclCentroidCol
# GblCentroidRow GblCentroidCol NumRows NumCols HeadRow      HeadCol      TailRow 
# TailCol HeadCurvPtRow HeadCurvPtCol TailCurvPtRow TailCurvPtCol IntH IntT
# COLUMNS NOT CORRELATED WITH LABEL: ISLOOP
# COLUMNS WITH UNIQUE VALUE
##### 51 features - 9 columns with one value=42 columns
uniquelength <- sapply(data,function(x) length(unique(x)))
data <- subset(data, select=uniquelength>1) 
##### 42 features - 1 variables'isloop' not correlated with label, and 20 reference variables.
plot(data$Posture, data$Kim)
library(lubridate)

sapply(data, class)
data <-data[ , -which(names(data) %in% c('SeqNum','FrameNum','CameraStepRows','CameraStepCols','CameraOffsetRows',
                                         'CameraOffsetCols','TotalOffsetRows','TotalOffsetCols','LclCentroidRow',
                                         'LclCentroidCol','GblCentroidRow','GblCentroidCol','HeadRow','HeadCol',
                                         'TailRow','TailCol','HeadCurvPtRow','HeadCurvPtCol','TailCurvPtRow','TailCurvPtCol','IsLoop'))]

###### now 21 variables
### 2 categoricals: 'Posture', and 'Kim'
### 2 varaibles with i(maybe error) in cells: 'RectBigSide', 'RectRatio'.
# calculate the correlation between numerica data
Num_data=data[,-which(names(data) %in% c('Posture','Kim','RectBigSide', 'RectRatio'))]
sapply(Num_data, class)
## it seems Area is an type integer, need to change into numeric
Num_data$Area <- sapply(Num_data$Area, as.numeric)
## error in variables 'RectBigSide', 'RectRatio'.
#Num_data$RectRatio <- sapply(Num_data$RectRatio, as.character)
#Num_data$RectBigSide <- sapply(Num_data$RectBigSide, as.character)

#Num_data[!grepl("[A-Za-z]", Num_data$RectRatio),]#delete error rows contain character i, firstly convert this column to char, then convert back to numeric
#Num_data[!grepl("[A-Za-z]", Num_data$RectBigSide),]
#Num_data$RectRatio <- sapply(Num_data$RectRatio, as.numeric)
#Num_data$RectBigSide <- sapply(Num_data$RectBigSide, as.numeric)
#Num_data<-Num_data[!(class(Num_data$RectRatio)=="complex"),]
#Num_data=Num_data[-which(sapply(Num_data$RectBigSide, class) == "Complex"),]
cor(Num_data)
# Num_data including variables that are highly correled with others, for example, Heywood, Elongation, 
# MajorAxisLength,MinorAxisLength,Perimeter,Ixx
ds=Num_data[,-which(names(Num_data) %in% c('Heywood','Hydraulic', 'Elongation', 'Length','MajorAxisLength','MinorAxisLength','Perimeter','Ixx','CurvTail','CurvHead'))]
cor(ds)
#!!!!!!! the numerica data we are going to keep are :ElapsedTime,ComptFactor,Iyy,Ixy,MaxWidth,SkewerAngle !!!!!!
# now ,check the categorical data Posture 
library(car)
summary(data$Kim)

plot(data$Posture,data$Kim) # do not need to keep this categorical data
## now normalize the data
normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}
ds.norm=as.data.frame(lapply(ds, normalized))
#so we just add label:
ds.norm$Kim=data$Kim
# now try random forest
# try random forest
newdf=ds.norm
library(randomForest)
library(readr)
smp_size <- floor(0.75 * nrow(newdf))

### use all the data as train data and N2_nf5 AS test data
set.seed(415)
fit <- randomForest(as.factor(Kim) ~  ElapsedTime+Area+ComptFactor+Iyy+Ixy+MaxWidth+SkewerAngle,data=newdf, 
                    importance=TRUE, 
                    ntree=200)
# test data is N2_nf5
data=N2_nf5
#data <- data[!apply(is.na(data) | data == "", 1, all),]
data[data==""] <- NA
data<-na.omit(data)
data$Kim<-recode(data$Kim,"c('Forward-NTD','Forward-Shallow')='Forward'")
data$Kim<-recode(data$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")

data=data[,which(names(data) %in% c('Kim','ElapsedTime', 'Area', 'ComptFactor','Iyy','Ixy','MaxWidth', 'SkewerAngle'))]

data.norm=as.data.frame(lapply(data[,-8], normalized))
#so we just add label:
data.norm$Kim=data$Kim
Prediction <- predict(fit, data.norm[,-8])
table(Prediction,data.norm$Kim)
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(newdf)), size = smp_size)
train <- newdf[train_ind, ]
test <- newdf[-train_ind, ]

set.seed(415)
fit <- randomForest(as.factor(Kim) ~  ElapsedTime+ComptFactor+Iyy+Ixy+MaxWidth+SkewerAngle,data=train, 
                    importance=TRUE, 
                    ntree=200)
Prediction <- predict(fit, test)
table(Prediction,test$Kim)
rf_3predictself<-table(test$Kim, Prediction, dnn=c("Label by Kim","Prediction by RF"))

write.csv(rf_3predictself,"3predictself.csv")
# now predict n2nf5
Prediction <- predict(fit, df.norm)
table(Prediction,df.norm$Kim)
rf_n2nf4n1nf5<-table(df.norm$Kim, Prediction, dnn=c("Label by Kim","Prediction by RF"))

write.csv(rf_n2nf4n1nf5,"3predictnf5.csv")

####### hidden markov model try out
library(car)
data=N2_nf4
#data=N2_nf5
#data <- data[!apply(is.na(data) | data == "", 1, all),]
data[data==""] <- NA
data<-na.omit(data)
data$Kim<-recode(data$Kim,"c('Forward-NTD','Forward-Shallow','Forward-Sharp')='Forward'")
data$Kim<-recode(data$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")
data$Kim<-recode(data$Kim,"c('Backward-ReverseShort','Backward-ReverseLong')='Reverse'")

# Use HMM
# Create model, 2 states: either going up or going down
library(depmixS4)
newdf=
ret=newdf$SkewerAngle
### 2 STATES, REVERSE, NON-REVERSE
newdf=ds.norm
newdf$Kim<-recode(newdf$Kim,"c('Forward-NTD','Foward','Forward-Shallow','Forward-Sharp','Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop','Stop')='Non-Reverse'")
#newdf$Kim<-recode(newdf$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")
newdf$Kim<-recode(newdf$Kim,"c('Backward-ReverseShort','Backward-ReverseLong')='Reverse'")

### 3 STATES: forward, stop, reverse
newdf$Kim<-recode(newdf$Kim,"c('Forward-NTD','Foward','Forward-Shallow','Forward-Sharp')='Forward'")
newdf$Kim<-recode(newdf$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")
newdf$Kim<-recode(newdf$Kim,"c('Backward-ReverseShort','Backward-ReverseLong')='Reverse'")

### 5 STATES: 
newdf$Kim<-recode(newdf$Kim,"c('Forward-NTD','Foward','Forward-Shallow')='Forward'")
newdf$Kim<-recode(newdf$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")
#newdf$Kim<-recode(newdf$Kim,"c('Backward-ReverseShort','Backward-ReverseLong')='Reverse'")



ret=data$SkewerAngle
mod <- depmix(ret ~ 1, data=data.frame(ret=ret), nstates=5)
f <- fit(mod)
summary(f)

ret=newdf$SkewerAngle
mod <- depmix(ret ~ 1, data=data.frame(ret=ret), family=exGAUS(),nstates=2)
f <- fit(mod)
summary(f)
########################
## MULTIVARIATE HMM
library(depmixS4)
ret=data$SkewerAngle
ret1=data$Area
ret2=data$ComptFactor
ret3=data$ComptFactor
mod <- depmix(list(ret ~ 1,ret1 ~ 1,ret2 ~ 1), data = data.frame(ret=ret,ret1=ret1,ret1=ret2), nstates = 3,
              family = list(gaussian(), gaussian(),gaussian()),instart = runif(3))
f <- fit(mod)
#########################
# Get the estimated state for each timestep 
esttrans <- posterior(f)
unique(esttrans[,1])
# Plot
par(mfrow=c(3,1))
plot(newdf$ElapsedTime, newdf$Kim, type='l', main="N2_nf4: Kim's movement label")
plot(newdf$ElapsedTime, esttrans[,1], type='l', main='N2_nf4: Estimated state')
plot(newdf$ElapsedTime, ret, type='l', main='N2_nf4: SkewerAngle')

table(esttrans[,1],newdf$Kim)


par(mfrow=c(3,1))
plot(data$ElapsedTime, data$Kim, type='l', main='Trajectory')
plot(data$ElapsedTime, esttrans[,1], type='l', main='Estimated state')
plot(data$ElapsedTime, ret, type='l', main='Returns')


s=data$ElapsedTime
t=data$ElapsedTime[-nrow(data)]
########### multivariate HMM
mod <- depmix(list(rt ~ 1,corr ~ 1), data = speed, nstates = 2,
              + family = list(gaussian(), multinomial("identity")),
              + transition = ~ scale(Pacc), instart = runif(2))
###################### try PCA then HMM##################
library(car)
data=N2_nf4
#data=N2_nf5
#data <- data[!apply(is.na(data) | data == "", 1, all),]
data[data==""] <- NA
data<-na.omit(data)
data$Kim<-recode(data$Kim,"c('Forward-NTD','Forward-Shallow','Forward-Sharp')='Forward'")
data$Kim<-recode(data$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")
data$Kim<-recode(data$Kim,"c('Backward-ReverseShort','Backward-ReverseLong')='Reverse'")

data=data[,which(names(data) %in% c('Kim','ElapsedTime',  'ComptFactor','Iyy','Ixy','MaxWidth', 'SkewerAngle'))]
### PCA
ds <- data[, -7]


# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ds.pca <- prcomp(ds,
                 center = TRUE,
                 scale. = TRUE) 
ds.pca$x
print(ds.pca)
plot(ds.pca, type = "l")
loadings = ds.pca$x[,1:3]
df1 <- as.data.frame(loadings)
newdf1=cbind(data$Kim,df1)
colnames(newdf1)[1] <- "Kim"
### now run HMM on the PCA component
### one componeng
library(depmixS4)
ret=newdf1$PC1
ret1=newdf1$PC2
ret2=newdf1$PC3
mod <- depmix(ret ~ 1, data=data.frame(ret=ret), nstates=3)
### three component
mod <- depmix(list(ret ~ 1,ret1 ~ 1,ret2 ~ 1),data = data.frame(ret=ret,ret1=ret1,ret1=ret2), nstates = 3,
              ,family = list(gaussian(), gaussian(),gaussian()), instart = runif(3))

### 5 component
library(depmixS4)
ret=newdf$SkewerAngle
ret1=newdf$Ixx
ret2=newdf$Ixy
ret3=newdf$Iyy
ret4=newdf$ComptFactor
#ret3=newdf$ComptFactor
mod <- depmix(list(ret ~ 1,ret1 ~ 1,ret2 ~ 1,ret3 ~ 1,ret4 ~ 1), data = data.frame(ret=ret,ret1=ret1,ret2=ret2,ret3=ret3,ret4=ret4), nstates = 5,
              family = list(gaussian(), gaussian(),gaussian(),gaussian(),gaussian()),instart = runif(5))
f <- fit(mod)
esttrans <- posterior(f)
table(esttrans[,1],newdf$Kim)
#############
f <- fit(mod)
summary(f)
esttrans <- posterior(f)
unique(esttrans[,1])
# Plot
par(mfrow=c(3,1))
plot(data$ElapsedTime, data$Kim, type='l', main='Trajectory')
plot(data$ElapsedTime, esttrans[,1], type='l', main='Estimated state')
plot(data$ElapsedTime, ret, type='l', main='Returns')

######### COMPARE N2_nf4 N2_nf5