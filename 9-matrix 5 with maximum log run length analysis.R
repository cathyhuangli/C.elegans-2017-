normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}
#########################################################################
############################### matrix 2: raw-length matrix to the maximum
#########################################################################
inPath="C:/Users/lhuang37/Desktop/11.30~/rlefor5types" # change it to the folder
videofiles<-list.files(inPath)
video_paths  <- lapply(videofiles, function(x) { 
  path = paste(inPath,x, sep ="/")
  return(list(path =path, video_name = x))
})

##extract 11 descriptors from raw-length matrix
#create an empty dataframe:
mydf=data.frame(videoname=character(),SRE=numeric(),LRE=numeric(),GLN=numeric(),RLN=numeric(),
                RP=numeric(),LGRE=numeric(),HGREV=numeric(),SRLGE=numeric(),SRHGE=numeric(),
                LRLGE=numeric(),LRHGE= numeric())

#start to extract value for each video and add the row of values into mydf
extraction <- lapply(video_paths, function(x) {
  #cls2_n2f1=read.csv("/Users/cathy/Downloads/N2_f1rle.csv",header=T)
  df = read.csv(x$path, header = TRUE)
  videoname=gsub("rle.csv",'', x$video_name)
  #hist(df$length,breaks=100)
  #col<-sort(unique(df$length))# j in  papaer
  col=c(1:25,30,34,36,37)
  row<-c(1,2)# i in paper
  #create aN EMPTY raw matrix for n2f1
  mymatrix=matrix(, nrow = length(row), ncol =length(col),dimnames=list(row,col))
  #extract 11 descripotrs for the matrix:
  M<-length(row)
  N<-max(col)
  nr<-length(col)#total number of runs
  #np<-sum(mymatrix) #total number of pixels
  # create the raw-lenth matrix:
  for( i in row ){
    for( j in col ){
      value=sum(df$value==i & df$length==j)
      t = which(col == j)#return index of j in the column values=index in matrix
      mymatrix[i,t]=value
    }
  }
  np<-sum(mymatrix) #total number of pixels
  # 1. calculate Short Run Emphasis (SRE):
  temp=0
  for (j in 1:nr){
    t=as.numeric(colnames(mymatrix)[j])
    t=log(t+1)
    s=(colSums(mymatrix)[j])/(t*t*nr)
    temp=temp+s
  }
  SRE=temp
  
  #or:
  #df=as.data.frame(mymatrix)
  #for(j in names(df)){
  #  rl=as.numeric(j)
  #  df[j] <- df[j]/(rl*rl*nr)
  #}
  #SRE=sum(df)
  
  # 2. calculate Long Run Emphasis (LRE):
  temp=mymatrix
  for (j in 1:nr){
    s=as.numeric(colnames(temp)[j])
    s=log(s+1)
    temp[,j]=temp[,j]*(s*s)/nr
  }
  LRE=sum(temp)
  #SAME AS:
  #temp=0
  #for (j in 1:nr){
  #  t=as.numeric(colnames(mymatrix)[j])
  # s=colSums(mymatrix)[j]*(t*t)/nr
  #temp=temp+s
  #}
  #LRE=temp
  
  # 3. calculate Gray-Level Nonuniformity (GLN):
  temp=0
  for (i in 1:M){
    s=(rowSums(mymatrix)[i])*(rowSums(mymatrix)[i])/nr
    temp=temp+s
  }
  GLN=temp
  
  # 4. calculate Run Length Nonuniformity (RLN):
  temp=0
  for (j in 1:nr){
    s=(colSums(mymatrix)[j])*(colSums(mymatrix)[j])/nr
    temp=temp+s
  }
  RLN=temp
  
  # 5. calculate Run Percentage (RP):
  RP=nr/np
  
  # 6. calculate Low Gray-Level Run Emphasis (LGRE):
  temp=mymatrix
  for (i in 1:M){
    s=as.numeric(colnames(temp)[i])# the meaning of i in paper
    s=log(s+1)
    temp[i,]=temp[i,]/(s*s*nr)
  }
  LGRE=sum(temp)
  
  # 7. calculate High Gray-Level Run Emphasis (HGRE):
  temp=mymatrix
  for (i in 1:M){
    s=as.numeric(colnames(temp)[i])
    s=log(s+1)
    temp[i,]=temp[i,]*(s*s)/nr
  }
  HGRE=sum(temp)
  
  # 8. calculate Short Run Low Gray-Level Emphasis (SRLGE):
  temp=mymatrix
  for( i in row ){
    for( j in col ){
      t = which(col == j)#return index of j in the column values=index in matrix
      temp[i,t]=mymatrix[i,t]/(i*i*log(j+1)*log(j+1)*nr)
    }
  }
  SRLGE=sum(temp)
  # 9. calculate Short Run High Gray-Level Emphasis (SRHGE):
  temp=mymatrix
  for( i in row ){
    for( j in col ){
      t = which(col == j)#return index of j in the column values=index in matrix
      temp[i,t]=mymatrix[i,t]*(i*i)/(log(j+1)*log(j+1)*nr)
    }
  }
  SRHGE=sum(temp)
  
  
  
  # 10. calculate Long Run Low Gray-Level Emphasis (LRLGE):
  temp=mymatrix
  for( i in row ){
    for( j in col ){
      t = which(col == j)#return index of j in the column values=index in matrix
      temp[i,t]=mymatrix[i,t]*(log(j+1)*log(j+1))/(i*i*nr)
    }
  }
  LRLGE=sum(temp)
  
  
  # 11. calculate Long Run High Gray-Level Emphasis (LRHGE):
  temp=mymatrix
  for( i in row ){
    for( j in col ){
      t = which(col == j)#return index of j in the column values=index in matrix
      temp[i,t]=mymatrix[i,t]*(i*i*log(j+1)*log(j+1))/nr
    }
  }
  LRHGE=sum(temp)
  
  
  vrow=c(videoname,SRE,LRE,GLN,RLN,RP,LGRE,HGRE,SRLGE,SRHGE,LRLGE,LRHGE) 
  library(data.table)
  mydf=rbindlist(list(mydf, as.list(vrow)))
  return(mydf)
}) 

mydf=rbindlist(extraction)
write.csv(mydf,file='rle_5type_11extraction_ToMaxLenLog.csv')

########################################################
######### calculate correlation & kmeans all features:

mydf=rle_5type_11extraction_ToMaxLenLog[,-c(1,2)]
cor(mydf)
########## K-MEANS CLUSTERING: K=5 TYPES
set.seed(1234)
rle.norm<-normalized(mydf)
k.f = 5 # <--------------------||                  # Change this for desired number of cluster if you forget to input in the beginning
km.rle<-kmeans(rle.norm,k.f,iter.max = 1000,nstart = 25)
df.f.labels = cbind(rle_5type_11extraction_ToMaxLenLog,cluster=factor(km.rle$cluster))
df.f.labels$v_type=gsub("_f[0-9]*", "_f", gsub("_nf[0-9]*", "_nf",gsub("_nnf[0-9]*", "_nnf", df.f.labels$videoname)))
df.f.labels$v_name=gsub("_f[0-9]*", "", gsub("_nf[0-9]*", "",gsub("_nnf[0-9]*", "", df.f.labels$videoname)))
# no need: df.f.labels <- as.data.frame(lapply(df.f.labels, function(x){replace(x, x=='N2_nnf','N2_nf')}))

table(df.f.labels$v_type,df.f.labels$cluster)
############################################## for 6 supposed levy flight videos---k-means
#mydiag=c('N2_nf4','N2_nf5','N2_nf9','N2_nf21','N2_nf24','N2_nf15')
subrows <- apply(df.f.labels, 1, function(x) any(x %in% mydiag))
dd=df.f.labels[subrows,c('videoname','cluster')]
dd[ order(dd[,2], dd[,1]), ]
####################################################################
#write.csv(df.f.labels,file="raw-length-tothemax matrix 11 descriptors.csv")
#df.norm.labels=cbind(rle.norm,cluster=factor(km.rle$cluster))
#report(df.f.labels, "cluster")
#plot(df.norm.labels[,1:3],col=df.norm.labels[,4])
#df.norm.labels$videoname=df.f.labels$videoname
#write.csv(df.f.labels,file="cls2_rle.assigned.csv")

#installed.packages("class")
library(class)
rle.norm$type=df.f.labels$v_type
m1=knn.cv(rle.norm[,1:11], rle.norm$type, k = 1, l = 0, prob = FALSE, use.all = TRUE)
table(rle.norm$type,m1)

############################################## for 6 supposed levy flight videos----knn
df.f.labels$knn=m1
#mydiag=c('N2_nf4','N2_nf5','N2_nf9','N2_nf21','N2_nf24','N2_nf15')
subrows <- apply(df.f.labels, 1, function(x) any(x %in% mydiag))
dd=df.f.labels[subrows,c('videoname','knn')]
dd[ order(dd[,2], dd[,1]), ]
####################################################################

#########highly correlated, ignore this part
########################################################
######### calculate correlation & kmeans selected features:

mydf=rle_5type_11extraction_ToMaxLenLog[,-c(1,2)]
#cor(mydf)
#(mydf[,c('RP','LRHGE','GLN')])
mydf=mydf[,c('RP','LRHGE','GLN')]
########## K-MEANS CLUSTERING: K=7 TYPES

set.seed(1234)
rle.norm<-normalized(mydf)
k.f = 5 # <--------------------||                  # Change this for desired number of cluster if you forget to input in the beginning
km.rle<-kmeans(rle.norm,k.f,iter.max = 1000,nstart = 25)
df.f.labels = cbind(rle_5type_11extraction_LenToMax,cluster=factor(km.rle$cluster))
df.f.labels$v_type=gsub("_f[0-9]*", "_f", gsub("_nf[0-9]*", "_nf",gsub("_nnf[0-9]*", "_nnf", df.f.labels$videoname)))
df.f.labels$v_name=gsub("_f[0-9]*", "", gsub("_nf[0-9]*", "",gsub("_nnf[0-9]*", "", df.f.labels$videoname)))
# no need: df.f.labels <- as.data.frame(lapply(df.f.labels, function(x){replace(x, x=='N2_nnf','N2_nf')}))

table(df.f.labels$v_type,df.f.labels$cluster)
#write.csv(df.f.labels,file="raw-length matrix 11 to the maximum descriptors.csv")
#df.norm.labels=cbind(rle.norm,cluster=factor(km.rle$cluster))
#report(df.f.labels, "cluster")
#plot(df.norm.labels[,1:3],col=df.norm.labels[,4])
#df.norm.labels$videoname=df.f.labels$videoname
#write.csv(df.f.labels,file="cls2_rle.assigned.csv")

############################################## for 6 supposed levy flight videos---k-means
#mydiag=c('N2_nf4','N2_nf5','N2_nf9','N2_nf21','N2_nf24','N2_nf15')
subrows <- apply(df.f.labels, 1, function(x) any(x %in% mydiag))
dd=df.f.labels[subrows,c('videoname','cluster')]
dd[ order(dd[,2], dd[,1]), ]
####################################################################

#installed.packages("class")
library(class)
rle.norm$type=df.f.labels$v_type
m1=knn.cv(rle.norm[,1:3], rle.norm$type, k = 1, l = 0, prob = FALSE, use.all = TRUE)
table(rle.norm$type,m1)
############################################## for 6 supposed levy flight videos----knn
df.f.labels$knn=m1
#mydiag=c('N2_nf4','N2_nf5','N2_nf9','N2_nf21','N2_nf24','N2_nf15')
subrows <- apply(df.f.labels, 1, function(x) any(x %in% mydiag))
dd=df.f.labels[subrows,c('videoname','knn')]
dd[ order(dd[,2], dd[,1]), ]
####################################################################