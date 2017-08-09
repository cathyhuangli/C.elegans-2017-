#First, for the redo the on/off food all file, adding the "time elapsed" as one attribute
# later add cluster back to this file, easy for the encoding part.


# add on food all & off food all into one file: "allofdata.csv"

normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}
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

##################################### choosing only N2 and Tph1 for all the analysis################################





#define cluster number for all as 2:5 and add back to the file.
s<-cbind(normalized(allofdata$Angle.between.2.step.lengths),normalized(allofdata$Step.length.in.mm))
t<-cbind(s,normalized(allofdata$Speed.in.mm.per.sec))
allofdata.norm<-data.frame(t)
library(reshape)
allofdata.norm<-rename(allofdata.norm,c(X1="Angle.between.2.step.lengths", X2="Step.length.in.mm",X3="Speed.in.mm.per.sec"))





for (i in 2:5){
  k.f = i # <--------------------||                  # Change this for desired number of cluster if you forget to input in the beginning
  km.allofdata<-kmeans(allofdata.norm[,1:3],k.f,iter.max = 1000,nstart = 25)
  df.f.labels = cbind(allofdata,cluster=factor(km.allofdata$cluster))
  report(df.f.labels, "cluster")
  outfile=paste("allofdata.assigned", i, ".csv",sep = "")
  write.csv(df.f.labels,file=outfile)
}


##go through the four files, and use decision tree to decide which cluster number is the best:
#install.packages("tree")
path = "C:/Users/lhuang37/Desktop/week11.12~11.18/Assignedall"
file.names <- dir(path, pattern =".csv")#same as list.files()
for (i in 2:5){
  filename=paste("allofdata.assigned",i,".csv", sep ="")
  filepath = paste(path,filename, sep ="/")
  t1 <- read.csv(filepath,header=T)
  t<-t1[,c(3:9,12)]
  library(tree)
  set.seed(2)
  train=sample(1:nrow(t),nrow(t)/2)#using half of dataset for training
  test=-train
  training_data=t[train,]
  testing_data=t[test,]
  testing_cluster=t$cluster[test]
  #fit the tree model using training data
  tree_model=tree(as.factor(cluster)~.,training_data)
  k=length(unique(t$cluster))
  m=paste("Tree plot with cluster= ",k,sep ="")
  plot(tree_model)
  text(tree_model,pretty=0)#pretty=0 means categorical value
  title(m)
  #check how the tree doing using testing data
  tree_pred=predict(tree_model,testing_data,type="class")
  st=mean(tree_pred==testing_cluster)
  table(tree_pred, testing_cluster)# added 11
  # write to file
  #cat('\n',"The prediction presion of tree model for dataset with cluster",k,"is: ", st)
}


###########################for cluster number=2
#sort the data according to first by v-name, then by time-elapsed.
#eg. smallData[order(smallData$value, smallData$name),]#change order
#eg. smallData[with(smallData, order(value, name)),]#rewrite the table.

cluster2=read.csv("C:/Users/lhuang37/Desktop/week11.12~11.18/Assignedall/allofdata.assigned2.csv",header=T)
cluster2=cluster2[with(cluster2,order(v_name,Time.Elapsed)),]#rewrite the data

#run-length encoding:
#http://www.r-bloggers.com/r-function-of-the-day-rle/
cluster2.rle <- rle(cluster2$cluster)
sort(cluster2.rle$lengths, decreasing = TRUE)
hist(cluster2.rle$lengths,breaks=100)

typetotal<-length(video[[1]])
video<-list(unique(unlist(cluster2$v_name)))
#run-length encoding for each video
for (i in 1:64){
  v=video[[1]][i]
  df<-cluster2[cluster2$v_name==v,]
  df.rle<-rle(df$cluster)
#ls <- list(value=df.rle$lengths, length=df.rle$values) 
#df <- as.data.frame(ls) 
  pos<-cumsum(df.rle$lengths)
  rows<-unlist(pos)-df.rle$lengths+1#give the row at start of each new cluster number. if use unlist(pos) only,
  #, will give a row at the end position of each tiny group of cluster.
  df_pos<-df[rows,]#take the ones marked at lengths.
  df_pos$value<-df.rle$values
  df_pos$length<-df.rle$lengths
  name=paste(v,"rle",".csv", sep ="")
  write.csv(df_pos,file=name)
            #as.numeric(rownames(df))=ROW NUMBER
} 

#combine everything in the folder rleforall

path = "C:/Users/lhuang37/Desktop/week11.12~11.18/rleforall"
file.names <- dir(path, pattern =".csv")#same as list.files()
file_paths  <- lapply(file.names, function(x) { 
  inpath = paste(path,x, sep ="/")
  return(list(path =inpath))
})

list.readfiles <- lapply(file_paths, function(x) {
  df = read.csv(x$path)
  return (df)
}) 

library(data.table)
rle.all <- as.data.frame(rbindlist(list.readfiles))

write.csv(rle.all,file="rle.all.csv")


#analysis the rle.all file to find pattern.
cluster2=read.csv("C:/Users/lhuang37/Desktop/week11.12~11.18/Assignedall/allofdata.assigned2nt.csv",header=T)
cluster2=cluster2[with(cluster2,order(v_name,Time.Elapsed)),]#rewrite the data

#run-length encoding:
#http://www.r-bloggers.com/r-function-of-the-day-rle/
cluster2.rle <- rle(cluster2$cluster)
#sort(cluster2.rle$lengths, decreasing = TRUE)
hist(cluster2.rle$lengths,breaks=100)
summary(cluster2$type) 

























install.packages("tree")
library(tree)
attach(carseat)
set.seed(2)
train=sample(1:nrow(carseat),nrow(carseat)/2)#using half of dataset for training
test=-train
training_data=carseat[train,]
testing_data=carseat[test,]
testing_cluster=cluster[test]
#fit the tree model using training data
tree_model=tree(cluster~.,training_data)
plot(tree_model)
text(tree_model,pretty=0)#pretty=0 means categorical value
#check how the tree doing using testing data
tree_pred=predict(tree_model,testing_data,type="class")
#table(tree.pred,testing_cluster)
mean(tree_pred==testing_cluster)#give precision of prediction
#mean(tree_pred!=testing_cluster)#give misprediction








