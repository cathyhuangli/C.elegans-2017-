
install.packages("fpc")
install.packages("data.table")
library(fpc)
require(data.table) # to use rbindlist


inPath <- "C:/Users/lhuang37/Downloads/worms download 20170814 n2_nf RIM_nf AIB_nf"

N2_nf  <- c("N2_nf", "N2_HR_nf")
AIB_nf <- c("AIB_nf", "AIB_HR_nf")
RIM_nf <-c("RIM_HR_nf")

n2folders  <- unlist(lapply(N2_nf, function(x) list.files(path=inPath, pattern=x)) )#list all the folders under the on_food and off_food
AIBfolders <- unlist(lapply(AIB_nf, function(x) list.files(path=inPath, pattern=x)))
RIMfolders <- unlist(lapply(RIM_nf, function(x) list.files(path=inPath, pattern=x)))


n2_paths  <- lapply(n2folders, function(x) { 
  path = paste(inPath,x,"ContourAndSkel.csv", sep ="/")
  return(list(path =path, v_name = x))
})
#f_paths

aib_paths  <- lapply(AIBfolders, function(x) { 
  path = paste(inPath,x,"ContourAndSkel.csv", sep ="/")
  return(list(path =path, v_name = x ))
})
rim_paths  <- lapply(RIMfolders, function(x) { 
  path = paste(inPath,x,"ContourAndSkel.csv", sep ="/")
  return(list(path =path, v_name = x))
})

keeps <- c("Area","ComptFactor","Hydraulic","SkewerAngle","Length","CurvHead","CurvTail",
           "Ixx","Ixy","Iyy","Heywood","ElapsedTime","MajorAxisLength","MinorAxisLength",
           "Elongation","Perimeter","MaxWidth","Posture","IsLoop")



#read list of data frame of the same treatment and keep relevant features for clustering.
# --------------------------------------------------
# Read data, n2_nf7, n2_nf22 problem dataset exclude.
n2folders  <- unlist(lapply(N2_nf, function(x) list.files(path=inPath, pattern=x)) )#list all the folders under the on_food and off_food

n2_paths  <- lapply(n2folders, function(x) { 
  path = paste(inPath,x,"ContourAndSkel.csv", sep ="/")
  return(list(path =path, v_name = x))
})

list.n2 <- lapply(n2_paths, function(x) {
  df = read.csv(x$path)[,keeps]
  len = nrow(df)
  v_name = rep(x$v_name,len)
  tmp = data.frame(v_name=v_name)
  return ( cbind(df,tmp ) )
}) 



n2.nf.all <- as.data.frame(rbindlist(list.n2))
write.csv(n2.nf.all,file="n2.nf.201708.csv")

# read OFF FOOD DATA
list.aib <- lapply(aib_paths, function(x) {
  df = read.csv(x$path)[,keeps]
  len = nrow(df)
  v_name = rep(x$v_name,len)
  tmp = data.frame(v_name=v_name)
  return ( cbind(df,tmp ) )
}) 
aib.nf.all <- as.data.frame(rbindlist(list.aib))
write.csv(aib.nf.all,file="aib.nf.201708.csv")



list.rim <- lapply(rim_paths, function(x) {
  df = read.csv(x$path)[,keeps]
  len = nrow(df)
  v_name = rep(x$v_name,len)
  tmp = data.frame(v_name=v_name)
  return ( cbind(df,tmp ) )
}) 
rim.nf.all <- as.data.frame(rbindlist(list.rim))
write.csv(rim.nf.all,file="rim.nf.201708.csv")
