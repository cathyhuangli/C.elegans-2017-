# Fill dr kim's hand annotation column 'Kim'blank cells with 'Still'

ds=N2_nf4
ds <- ds[!is.na(ds$Kim), ]
ds.rle <- rle(ds$Kim)
sort(ds.rle$lengths, decreasing = TRUE)
hist(ds.rle$lengths,breaks=100)
ds.rle$values
pos<-cumsum(ds.rle$lengths)
rows<-unlist(pos)-ds.rle$lengths+1#give the row at start of each new cluster number. if use unlist(pos) only,
#, will give a row at the end position of each tiny group of cluster.
ds_pos<-ds[rows,]#take the ones marked at lengths.
ds_pos$value<-ds.rle$values
ds_pos$length<-ds.rle$lengths
write.csv(ds_pos,file='n2nf4_mv_rle.csv')
unique(n2nf4_mv_rle$value)
rlesub=n2nf4_mv_rle[n2nf4_mv_rle$value=='Stopped-ReverseLong',]
summary(rlesub$length)
rlesub=n2nf4_mv_rle[n2nf4_mv_rle$value=='Stopped-ReverseLong',]
summary(rlesub$length)
rlesub=n2nf4_mv_rle[n2nf4_mv_rle$value=='Stopped-ReverseLong',]
summary(rlesub$length)
rlesub=n2nf4_mv_rle[n2nf4_mv_rle$value=='Stopped-ReverseLong',]
summary(rlesub$length)
rlesub=n2nf4_mv_rle[n2nf4_mv_rle$value=='Stopped-ReverseLong',]
summary(rlesub$length)
plot(n2nf4_mv_rle$length, type='l')
boxplot(n2nf4_mv_rle$length~n2nf4_mv_rle$value)
library(ggplot2)
ggplot(n2nf4_mv_rle)+
  geom_point(aes(x=n2nf4_mv_rle$value, y=n2nf4_mv_rle$length),size=3)
#ds <- read.csv("N2_nf4.csv", header=T, na.strings=c("","Still"))

#ds$Kim[which(ds$Kim=="")]<-"Still"

#ds$Kim <- ifelse(nchar(ds$Kim)==0, "Still", ds$Kim)

#length(ds$Kim)

## calculate difference between rows of data:

#newdata=tail(df, -1) - head(df, -1)

#   newdata=apply( df , 2 , diff )

#   df[-1, ] - df[-nrow(df), ]

#   data.frame(diff(as.matrix(df)))

##################################################

df=saved[,-1]

library(car)

summary(df$Status_Label)

df$Status_Label<-recode(df$Status_Label,"c('Forward-NTD','Forward-Shallow')='Foward'")

df$Status_Label<-recode(df$Status_Label,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")



# which columns to delete: reference variables, lsLoop, IntH 
summary(df)

which( colnames(df)==" HeadCurvPtRow" )

newdf= df[ , c(1,4,20:33,37,39,48,49)]
normalized<- function(x) {(2*(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))) -1}

dfNorm <- as.data.frame(lapply(newdf[,-1], normalized))
newdf=cbind(newdf[,'Status_Label'],dfNorm)
#newdf=df[,c('ElapsedTime','')]

#plot(newdf)

#plot(newdf$Status_Label,newdf$LclCentroidRow)

#plot(newdf)
#summary(newdf)
#### PERFORM PCA ######################


ds <- newdf[, -1]


# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ds.pca <- prcomp(ds,
                 center = TRUE,
                 scale. = TRUE) 
ds.pca$x
print(ds.pca)
plot(ds.pca, type = "l")
summary(ds.pca)

### take the pc1 ~ pc8 for classification
std_dev <- ds.pca$sdev

#compute variance
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

loadings = ds.pca$x[,1:8]
df1 <- as.data.frame(loadings)
newdf1=cbind(newdf$Status_Label,df1)
colnames(newdf1)[1] <- "Status_Label"





# try svm

set.seed(123)
smp_size <- floor(0.75 * nrow(newdf1))
train_ind <- sample(seq_len(nrow(newdf1)), size = smp_size)

train <- newdf1[train_ind, ]

test <- newdf1[-train_ind, ]

write.csv(newdf1,"PCA-8-com_updated.csv")


library(e1071)

#x=subset(newdf, -Status_Label)

model <- svm(as.factor(Status_Label) ~ .,train)



pred <- predict(model,test)

# Check accuracy:

table(pred, test$Status_Label)

### try clustering 
k.f = 5 # <--------------------||                  # Change this for desired number of cluster if you forget to input in the beginning
km.allofdata<-kmeans(newdf1[,2:9],k.f,iter.max = 1000,nstart = 25)
df.f.labels = cbind(newdf1,cluster=factor(km.allofdata$cluster))
report(df.f.labels, "cluster")
write.csv(df.f.labels,newdf1[,1])

table(km.allofdata$cluster, newdf1[,1])

# try random forest

library(randomForest)

library(readr)

smp_size <- floor(0.75 * nrow(newdf1))



## set the seed to make your partition reproductible

set.seed(123)

train_ind <- sample(seq_len(nrow(newdf1)), size = smp_size)

train <- newdf1[train_ind, ]
summary(train$Status_Label)

test <- newdf1[-train_ind, ]



set.seed(415)

formula_forest<-formula("Status_Label~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8")

fit.forest_sample<-randomForest(formula_forest, data=train,importance=TRUE,mtry=4,ntree=250,na.action=na.roughfix)

fit.forest_sample
plot(fit.forest_sample$err.rate[,1], ylab='OOB Error', xlab='Number of trees')

################ KNN
smp_size <- floor(0.75 * nrow(newdf1))



## set the seed to make your partition reproductible

set.seed(123)

train_ind <- sample(seq_len(nrow(newdf1)), size = smp_size)

train <- newdf1[train_ind, ]
test <- newdf1[-train_ind, ]
summary(train$Status_Label)

#################################################### week 2 in august 2017########################

ds=N2_nf4
ds <- ds[!is.na(ds$Kim), ]
ds.rle <- rle(ds$Kim)
sort(ds.rle$lengths, decreasing = TRUE)
hist(ds.rle$lengths,breaks=100)
ds.rle$values
pos<-cumsum(ds.rle$lengths)
rows<-unlist(pos)-ds.rle$lengths+1#give the row at start of each new cluster number. if use unlist(pos) only,
#, will give a row at the end position of each tiny group of cluster.
ds_pos<-ds[rows,]#take the ones marked at lengths.
ds_pos$value<-ds.rle$values
ds_pos$length<-ds.rle$lengths
#write.csv(ds_pos,file='n2nf4_mv_rle.csv')
unique(n2nf4_mv_rle$value)
rlesub=n2nf4_mv_rle[n2nf4_mv_rle$value=='Stopped-ReverseLong',]
summary(rlesub$length)
rlesub=n2nf4_mv_rle[n2nf4_mv_rle$value=='Stopped-ReverseLong',]
summary(rlesub$length)
rlesub=n2nf4_mv_rle[n2nf4_mv_rle$value=='Stopped-ReverseLong',]
summary(rlesub$length)
rlesub=n2nf4_mv_rle[n2nf4_mv_rle$value=='Stopped-ReverseLong',]
summary(rlesub$length)
rlesub=n2nf4_mv_rle[n2nf4_mv_rle$value=='Stopped-ReverseLong',]
summary(rlesub$length)

setwd("C:/Users/LHUANG37/Downloads/Wormlabelled/")
ds=N2_nf5
unique(N2_nf5$Kim)
ds <- ds[!is.na(ds$Kim), ]
ds.rle <- rle(ds$Kim)
sort(ds.rle$lengths, decreasing = TRUE)
hist(ds.rle$lengths,breaks=100)
ds.rle$values
pos<-cumsum(ds.rle$lengths)
rows<-unlist(pos)-ds.rle$lengths+1#give the row at start of each new cluster number. if use unlist(pos) only,
#, will give a row at the end position of each tiny group of cluster.
ds_pos<-ds[rows,]#take the ones marked at lengths.
ds_pos$value<-ds.rle$values
ds_pos$length<-ds.rle$lengths
#write.csv(ds_pos,file='ds_pos.csv')
unique(ds_pos$value)
rlesub=ds_pos[ds_pos$value=='Forward-NTD',]
summary(rlesub$length)
rlesub=ds_pos[ds_pos$value=='Stopped-ReverseShort',]
summary(rlesub$length)
rlesub=ds_pos[ds_pos$value=='Stopped-ReverseLong',]
summary(rlesub$length)
rlesub=ds_pos[ds_pos$value=='Backward-ReverseShort',]
summary(rlesub$length)
rlesub=ds_pos[ds_pos$value=='Forward-Sharp',]
summary(rlesub$length)
rlesub=ds_pos[ds_pos$value=='Stopped-Stop',]
summary(rlesub$length)
rlesub=ds_pos[ds_pos$value=='Forward-Shallow',]
summary(rlesub$length)
rlesub=ds_pos[ds_pos$value=='Backward-ReverseLong',]
summary(rlesub$length)













