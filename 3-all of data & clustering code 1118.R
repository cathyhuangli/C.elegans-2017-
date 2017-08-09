#require(VIDA.MEDIX)

#install.packages("fpc")
library(fpc)
require(data.table) # to use rbindlist
require(fpc) # cluster.stats
library(cluster) # using dissimilarity matrix (daisy) function, and silhouette, clusplot


inPath <- "C:/Users/lhuang37/Downloads/40 test 1021"

on_food  <- c("N2_f", "tph1_f","egl19_f")
off_food <- c("N2_nf", "tph1_nf","N2_nnf","egl19_nf","egl19h20_nf")

f_folders  <- unlist(lapply(on_food, function(x) list.files(path=inPath, pattern=x)) )#list all the folders under the on_food and off_food
nf_folders <- unlist(lapply(off_food, function(x) list.files(path=inPath, pattern=x)))
rm(on_food,off_food)#remove objects
help(rm)

f_paths  <- lapply(f_folders, function(x) { 
  path = paste(inPath,x,"turningAngle.csv", sep ="/")
  return(list(path =path, v_name = x))
})
#f_paths

nf_paths  <- lapply(nf_folders, function(x) { 
  path = paste(inPath,x,"turningAngle.csv", sep ="/")
  return(list(path =path, v_name = x ))
})


keeps <- c("Angle.between.2.step.lengths", "Step.length.in.mm", "Duration","Time.Elapsed", "Speed.in.mm.per.sec","x","y")


#read list of data frame of the same treatment and keep relevant features for clustering.
# --------------------------------------------------
# Read ON FOOD DATA
list.df.on.food <- lapply(f_paths, function(x) {
  df = read.csv(x$path)[-1,keeps]
  len = nrow(df)
  type = sub(x = x$v_name,pattern="_f[0-9]*", replacement = "_f")
  v_name = rep(x$v_name,len)
  tmp = data.frame(v_name=v_name, type = type)
  return ( cbind(df, tmp))
}) 

# read OFF FOOD DATA
list.df.off.food <- lapply(nf_paths, function(x) {
  df = read.csv(x$path)[,keeps]
  len = nrow(df)
  type= ifelse(
    grepl(x=x$v_name,pattern = "_nf[0-9]*"), sub(x= x$v_name, pattern = "_nf[0-9]*",replacement = "_nf") 
    ,sub(x= x$v_name, pattern = "_nn{0,1}[a-z][0-9]*",replacement = "_nnf"))#changed "_nf{0,1}[a-z][0-9]*" to "_nn{0,1}[a-z][0-9]*"
  v_name = rep(x$v_name,len)
  tmp = data.frame(v_name=v_name, type = type)
  return ( cbind(df,tmp ) )
}) 


#rm(f_folders,nf_folders,f_paths,nf_paths,keeps,inPath)

# -------------------------------------------------
# merge all df to one df
#install.packages("data.table")
library(data.table)
#help(rbindlist)
df.on.food.all <- as.data.frame(rbindlist(list.df.on.food))# rbindlist in data.table library
df.off.food.all <- as.data.frame(rbindlist(list.df.off.food))

allofdata<-rbind(df.on.food.all,df.off.food.all)
write.csv(allofdata,file="all_data_0113.csv")


#rm(list.df.on.food,list.df.off.food)

# remove any NA
df.on.food.all <- na.omit(df.on.food.all)
df.off.food.all <- na.omit(df.off.food.all)

# write out file
write.csv(df.off.food.all, "off_food_0113.csv")
write.csv(df.on.food.all, "on_food_0113.csv")







###########################################################################################################

#nstandardize data
# means.on.food <- apply(df.on.food,2, mean, na.rm=TRUE)
# means.off.food<- apply(df.off.food,2, mean, na.rm=TRUE)
# 
# 
# sds.on.food <- apply(df.on.food,2, sd, na.rm=TRUE)
# sds.off.food<- apply(df.off.food,2, sd, na.rm=TRUE)

# df.on.food.norm <- scale(df.on.food, center=means.on.food, scale=sds.on.food)
# df.on.food.norm <- na.omit(as.data.frame(df.on.food.norm))
# 
# df.off.food.norm <- scale(df.off.food, center=means.off.food, scale=sds.off.food)
# df.off.food.norm <- na.omit(as.data.frame(df.off.food.norm))


###########################################################################################################
#                           min max normalize
# Normalize 
df.on.food <- df.on.food.all[,keeps]
df.off.food <- df.off.food.all[,1:4]

normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}

df.on.food.norm <- lapply(df.on.food,normalized)
df.on.food.norm <- data.frame(df.on.food.norm)

df.off.food.norm  <- lapply(df.off.food,normalized)
df.off.food.norm  <-  data.frame(df.off.food.norm)

###########################################################################################################
###########################################################################################################
# evaluate how many clusters for on food situation


#run k-means method

# Determine number of clusters for on food
wss <- (nrow(df.on.food.norm))*sum(apply(df.on.food.norm,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df.on.food.norm, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",main="on food situation") 



library(fpc)
pamk.best <- pamk(df.on.food.norm)
pamk.best
pamk.best$nc
plot(pamk.best$pamobject)


#huu's code
#Use average silhouette width (look for the max)
d <- dist(df.on.food.norm)
ks <- 2:8

ASW <- sapply(ks, FUN=function(k) {
  cluster.stats(d, kmeans(df.on.food.norm, centers=k, nstart=5)$cluster)$avg.silwidth
})
png(filename = "Silhoutte on food.png", height = 10, width=12.72, res = 300,units = "in")
plot(ks, ASW, type="l", main = " On food silhoutte coefficient", ylab ="Average silhoutte width", xlab="number of clusters")
text(ks,ASW,labels = sapply(ASW,round,2), cex = 0.8, pos = 1)
dev.off()

ks[which.max(ASW)]



###########################################################################################################



# evaluate how many clusters for off food situation





#run k-means method

set.seed(1234)
clusters.kmeans<-kmeans(df.on.food.norm,3)
clusters.kmeans$centers
clusters.kmeans$size
clusters.kmeans$withinss
clusters.kmeans$tot.withinss
clusters.kmeans$betweenss

###huu's code

###########################################################################################################

# set seed
set.seed(1234)

############################################################################

#--------------------------------------------------------------------------

##### LABELS DATA FRAME WITH CLUSTER

# ----------------------------------------------------


# NORMALIZED IS HELPER FUNCTION ONLY, TO USE IN min_max_scale
normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}

min_max_scale <- function(df, keeps){
  df.norm = lapply(X = df[,keeps], FUN=normalized)
  df.norm = data.frame(df.norm)
}

# Function to process data and print out summary detail

report <- function(df, col_name){
  #' Function to print out the summary table
  #' Helper function to use inside scatterMatrix ONLY.
  #' @param df: a data frame
  #' @param col_name: a column name of factor/categorical attribute
  #' @return void
  #' 
  k <- length(levels(df[[col_name]]))
  sapply( seq(k), function(x) {
    cat(paste("\n--------------------------------------------------------\n"),"Cluster: ",x,"\n")
    print(summary(subset(df, df[[col_name]]==x), na.rm=TRUE))
    
  })
  rm(envir = environment()) # Only remove whatever variable in this scope
}



# ----------------------------------------------------
scatterMatrix <- function(df,k , keeps = c("Angle.between.2.step.lengths", "Step.length.in.mm", "Duration", "Speed.in.mm.per.sec")){
  #' Function to plot the scatter matrix with number of cluster
  #' @param df: a data frame
  #' @param k: number of cluster
  #'  @param keeps: attribute that can be processed
  #' @return kmeans object see help(kmeans) for more details
  #' 
  df.norm = min_max_scale(df, keeps)
  
  res.kmeans  = kmeans(x = df.norm, centers = k, iter.max = 1000,nstart = 25)
  
  df.f.labels = cbind(df,cluster=factor(res.kmeans$cluster))
  
  report(df.f.labels, "cluster") 
  #       Copy the print out in console window to NOTEPAD, then copy the cell to Ms Word table
  treatment <- ifelse(test = grepl(x = levels(df.f.labels[["type"]])[1], pattern = "*_n"), yes = "Off", no = "On")
  filename =paste(treatment," Food with k",k,".png", sep="")
  png(filename = filename, height = 10, width = 10, res = 300, units="in")
  plot(df.f.labels[,keeps],col=df.f.labels$cluster, main = paste(treatment," Food With k = ",k) )
  dev.off()
  cat("\nCHECK OUT THE SAVED GRAPH\n---------------------------------------------------------------\nSave file to ............\n")
  print(paste(getwd(),filename,sep="/"))
  return(list(res.kmeans = res.kmeans, df = df.f.labels))
 
}


#--------------------------------------------------------------------------
#                    On food cluster
#--------------------------------------------------------------------------
#        3 cluster
#        IMPORTANT! READ THE IMMEDIATED LINE
# you don't need to re run the program to change the number of cluster
# simply change k.f  <- 5 or
#k.f  <- 3

k.f = 3 # <--------------------||                  # Change this for desired number of cluster if you forget to input in the beginning

res.f <- scatterMatrix(df.on.food.all, k.f)  # can intepret kmeans result by accessing its detail here.

km.food  <- res.f$res.kmeans # kmeans result
df.food.all.labels <- res.f$df
rm(res.f)
# Investigate the centroid of each cluster
km.food$centers
write.xlsx(km.food$centers, "C:/Users/lhuang37/Downloads/km.food.centers.xlsx")
onfood_clus_1 <- df.on.food.all[km.food$cluster == 1,]
summary(onfood_clus_1)
write.xlsx(onfood_clus_1, "C:/Users/lhuang37/Downloads/km.food.cluster1.xlsx")
write.xlsx(summary(onfood_clus_1), "C:/Users/lhuang37/Downloads/km.food.cluster1.summary.xlsx")

denormalized<- function(x) {x*(max(x,na.rm=TRUE)-min(x, na.rm=TRUE))+min(x, na.rm=TRUE)}
range(df.on.food$Angle.between.2.step.lengths)+min(df.on.food$Angle.between.2.step.lengths)
max(df.on.food$Step.length.in.mm)-min(df.on.food$Step.length.in.mm)
min(df.on.food$Step.length.in.mm)
range(df.on.food$Angle.between.2.step.lengths)+min(df.on.food$Angle.between.2.step.lengths)
range(df.on.food$Angle.between.2.step.lengths)+min(df.on.food$Angle.between.2.step.lengths)
max(df.on.food$Duration)-min(df.on.food$Duration)
min(df.on.food$Duration)
max(df.on.food$Speed.in.mm.per.sec)-min(df.on.food$Speed.in.mm.per.sec)
min(df.on.food$Speed.in.mm.per.sec)


onfood.assigned<-data.frame(df.on.food,km.food$cluster)
onfood.assigned$x<-df.on.food$x
onfood.assigned$y<-df.on.food$y

write.csv(onfood.assigned,file="onfood.assigned.csv",row.names=TRUE,quote=FALSE)

subset1<-onfood.assigned[(onfood.assigned$km.food.cluster==1),]
subset2<-onfood.assigned[(onfood.assigned$km.food.cluster==2),]
subset3<-onfood.assigned[(onfood.assigned$km.food.cluster==3),]
plot(x = onfood.assigned$x, y = onfood.assigned$y)
points(x = subset1$x, y = subset1$y, col="red", pch=16)
points(x = subset2$x, y = subset2$y, col="blue", pch=17)
points(x = subset3$x, y = subset3$y, col="green", pch=18)
title("on food by k-means group")

qplot(onfood.assigned$Angle.between.2.step.lengths,onfood.assigned$km.food.cluster,col="pink")
points(onfood.assigned[17746,], col="black", pch=100,size=4)
help(points)


#--------------------------------------------------------------------------
#                    Off food cluster
#--------------------------------------------------------------------------
##for off food
# Determine number of clusters for on food
wss <- (nrow(df.off.food.norm))*sum(apply(df.off.food.norm,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df.off.food.norm, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",main="off food situation") 



library(fpc)
pamk.best <- pamk(df.off.food.norm)
pamk.best
pamk.best$nc
plot(pamk.best$pamobject)


#huu's code
#Use average silhouette width (look for the max)
d <- dist(df.off.food.norm)
ks <- 2:8

ASW <- sapply(ks, FUN=function(k) {
  cluster.stats(d, kmeans(df.off.food.norm, centers=k, nstart=5)$cluster)$avg.silwidth
})
png(filename = "Silhoutte on food.png", height = 10, width=12.72, res = 300,units = "in")
plot(ks, ASW, type="l", main = " On food silhoutte coefficient", ylab ="Average silhoutte width", xlab="number of clusters")
text(ks,ASW,labels = sapply(ASW,round,2), cex = 0.8, pos = 1)
dev.off()

ks[which.max(ASW)]


#        20 cluster
#        IMPORTANT! READ THE IMMEDIATED LINE
# you don't need to re run the program to change the number of cluster
# simply change k.nf  <- 2
#k.nf  <- 2
k.nf  <-  2 # <--------------------||                  # Change this for desired number of cluster

res.nf <- scatterMatrix(df.off.food.all, k.nf)  # can intepret kmeans result by accessing its detail here.

km.nofood  <- res.nf$res.kmeans # kmeans result
df.nofood.all.labels <- res.nf$df
rm(res.nf)

# Investigate the centroid of each cluster
km.nofood$centers
install.packages("xlsx")
library(xlsx)
write.xlsx(km.nofood$centers, "C:/Users/lhuang37/Downloads/km.nofood.centers.xlsx")

max(df.off.food$Step.length.in.mm)-min(df.off.food$Step.length.in.mm)
min(df.off.food$Step.length.in.mm)
range(df.off.food$Angle.between.2.step.lengths)+min(df.off.food$Angle.between.2.step.lengths)
range(df.off.food$Angle.between.2.step.lengths)+min(df.off.food$Angle.between.2.step.lengths)
max(df.off.food$Duration)-min(df.off.food$Duration)
min(df.off.food$Duration)
max(df.off.food$Speed.in.mm.per.sec)-min(df.off.food$Speed.in.mm.per.sec)
min(df.off.food$Speed.in.mm.per.sec)

nofood.assigned<-data.frame(df.off.food.all,km.nofood$cluster)
nofoodnorm.assigned<-data.frame(df.off.food.norm,km.nofood$cluster)
write.csv(nofoodnorm.assigned,file="nofoodnorm.assigned.csv",row.names=TRUE,quote=FALSE)
write.xlsx(nofoodnorm.assigned, "C:/Users/lhuang37/Downloads/nofoodnorm.assigned.xlsx")

write.xlsx(nofood.assigned, "C:/Users/lhuang37/Downloads/nofood.assigned.xlsx")


write.csv(nofood.assigned,file="nofood.assigned.csv",row.names=TRUE,quote=FALSE)

subset1<-nofood.assigned[(nofood.assigned$km.food.cluster==1),]
subset2<-nofood.assigned[(nofood.assigned$km.food.cluster==2),]

plot(x = nofood.assigned$x, y = nofood.assigned$y)
points(x = subset1$x, y = subset1$y, col="red", pch=16)
points(x = subset2$x, y = subset2$y, col="blue", pch=17)

title("off food by k-means group")


##################################################### Result compare between on and off food###
install.packages("ggplot2")
library(ggplot2)
qplot(onfood.assigned$Angle.between.2.step.lengths,onfood.assigned$km.food.cluster,col="pink")
title("on food angle between two step length by cluster")
qplot(nofood.assigned$Angle.between.2.step.lengths,nofood.assigned$km.nofood.cluster,col="pink")
title("off food angle between two step length by cluster")

plot(nofood.assigned[,1:4],col=nofood.assigned$km.nofood.cluster)
title("off food plot colored by cluster", outer=TRUE, line=-1)



