# POSTURE: 4 – Omega Loop Posture supposed correlated with reversal
setwd("C:/Users/lhuang37/Documents/")
nlevels(unique(rim$v_name))
nlevels(unique(aib$v_name))
nlevels(unique(n2$v_name))



data=n2.nf.201708.csv
rim <- read.csv(file="rim.nf.201708.csv", header=TRUE, sep=",")

rim[rim==""] <- NA
rim<-na.omit(rim)
rim<-rim[rim$SkewerAngle<=270,]
rim<-rim[rim$Area<=10000,]
summary(rim$SkewerAngle)
plot(rim$ElapsedTime,rim$SkewerAngle)
plot(rim$Posture,rim$SkewerAngle)
plot(rim$IsLoop,rim$SkewerAngle)

plot(rim$ElapsedTime,rim$ComptFactor)

n2 <- read.csv(file="n2.nf.201708.csv", header=TRUE, sep=",")
n2<-na.omit(n2)
n2<-n2[n2$SkewerAngle<=270,]
summary(n2$SkewerAngle)
plot(n2$ElapsedTime,n2$SkewerAngle)
plot(n2$Posture,n2$SkewerAngle)
plot(n2$IsLoop,n2$SkewerAngle)



aib <- read.csv(file="aib.nf.201708.csv", header=TRUE, sep=",")
aib<-na.omit(aib)
aib<-aib[aib$SkewerAngle<=270,]
summary(aib$SkewerAngle)
plot(aib$ElapsedTime,aib$SkewerAngle)
plot(aib$Posture,aib$SkewerAngle)
plot(aib$IsLoop,aib$SkewerAngle)

boxplot(aib$SkewerAngle, at=1, xlim=c(0, 4),main = "Compare of SkewerAngle for rim_nf & aib_nf & n2_nf")
boxplot(rim$SkewerAngle, at=2, add=TRUE)
boxplot(n2$SkewerAngle, at=3, add=TRUE)
axis(1,at=c(1,2,3),adj=1,padj=0.5,labels=c("aib_nf","rim_nf","n2_nf")) 
abline(h = 153,col = "red")
abline(h = -0.5,col = "red")


par(mfrow=c(3,1))
hist(aib$Posture)
#table(aib$Posture)
#table(rim$Posture)
hist(rim$Posture,xlim=c(0,4))
hist(n2$Posture)
#summary(aib$Posture)
par(mfrow=c(3,1))
barplot(table(aib$Posture),xlim=c(1,4),xlab='AIB$Posture')
barplot(prop.table(table(rim$Posture)),xlim=c(0,4),xlab='RIM$Posture')
barplot(prop.table(table(n2$Posture)),xlim=c(0,4),xlab='N2$Posture')

table(aib$Posture)
table(n2$Posture)
table(rim$Posture)
prop.table(table(aib$Posture))
prop.table(table(rim$Posture))
prop.table(table(n2$Posture))

# IsLoop
par(mfrow=c(3,1))
barplot(table(aib$IsLoop),xlim=c(1,4),xlab='AIB$IsLoop')
barplot(prop.table(table(rim$IsLoop)),xlim=c(0,4),xlab='RIM$IsLoop')
barplot(prop.table(table(n2$IsLoop)),xlim=c(0,4),xlab='N2$IsLoop')

table(aib$IsLoop)
table(n2$IsLoop)
table(rim$IsLoop)
prop.table(table(aib$IsLoop))
prop.table(table(rim$IsLoop))
prop.table(table(n2$IsLoop))


# Area

plot(n2[n2$Area<5000,]$ElapsedTime,n2[n2$Area<5000,]$Area)
plot(rim$ElapsedTime,rim$Area)
plot(aib$ElapsedTime,aib$Area)

boxplot(n2[n2$Area<5000,]$Area)

boxplot(rim[rim$Area<5000,]$Area)
boxplot(rim[rim$Area<5000,]$Area)

boxplot(aib$Area, at=1,ylim=c(0,4500),xlim=c(0, 4),main = "Compare variable Area for rim_nf & aib_nf & n2_nf")
boxplot(rim$Area, at=2, add=TRUE)
boxplot(n2$Area, at=3, add=TRUE)
axis(1,at=c(1,2,3),adj=1,padj=0.5,labels=c("aib_nf","rim_nf","n2_nf")) 
abline(h = 153,col = "red")
abline(h = -0.5,col = "red")
summary(aib[aib$Area<5000,]$Area)
summary(rim[rim$Area<5000,]$Area)
summary(n2[n2$Area<5000,]$Area)

par(mfrow=c(3,1))
hist(aib[aib$Area<5000,]$Area)
hist(rim[rim$Area<5000,]$Area)
hist(n2[n2$Area<5000,]$Area)



# Variable ComptFactor
boxplot(aib$ComptFactor, at=1,xlim=c(0, 4),main = "Compare variable ComptFactor for rim_nf & aib_nf & n2_nf")
boxplot(rim$ComptFactor, at=2, add=TRUE)
boxplot(n2$ComptFactor, at=3, add=TRUE)
axis(1,at=c(1,2,3),adj=1,padj=0.5,labels=c("aib_nf","rim_nf","n2_nf")) 
abline(h = 0.443,col = "red")
summary(aib$ComptFactor)
summary(rim$ComptFactor)
summary(n2$ComptFactor)
plot(aib$ElapsedTime,aib$ComptFactor)
plot(rim$ElapsedTime,rim$ComptFactor)


# Hydraulic
par(mfrow=c(1,1))
boxplot(aib$Hydraulic, at=1,xlim=c(0, 4),main = "Compare variable Hydraulic for rim_nf & aib_nf & n2_nf")
boxplot(rim$Hydraulic, at=2, add=TRUE)
boxplot(n2$Hydraulic, at=3, add=TRUE)
axis(1,at=c(1,2,3),adj=1,padj=0.5,labels=c("aib_nf","rim_nf","n2_nf")) 

summary(aib$Hydraulic)
summary(rim$Hydraulic)
summary(n2$Hydraulic)

par(mfrow=c(3,1))
hist(aib$Hydraulic)
hist(rim$Hydraulic)
hist(n2$Hydraulic)

# Length
par(mfrow=c(1,1))
boxplot(aib$Length, at=1,xlim=c(0, 4),main = "Compare variable Length for rim_nf & aib_nf & n2_nf")
boxplot(rim$Length, at=2, add=TRUE)
boxplot(n2$Length, at=3, add=TRUE)
axis(1,at=c(1,2,3),adj=1,padj=0.5,labels=c("aib_nf","rim_nf","n2_nf")) 

summary(aib$Length)
summary(rim$Length)
summary(n2$Length)

par(mfrow=c(3,1))
hist(aib$Length)
hist(rim$Length)
hist(n2$Length)

# CurHead
par(mfrow=c(1,1))
boxplot(aib$CurvHead, at=1,xlim=c(0, 4),main = "Compare variable CurvHead for rim_nf & aib_nf & n2_nf")
boxplot(rim$CurvHead, at=2, add=TRUE)
boxplot(n2$CurvHead, at=3, add=TRUE)
axis(1,at=c(1,2,3),adj=1,padj=0.5,labels=c("aib_nf","rim_nf","n2_nf")) 

summary(aib$CurvHead)
summary(rim$CurvHead)
summary(n2$CurvHead)

par(mfrow=c(3,1))
hist(aib$CurvHead)
hist(rim$CurvHead)
hist(n2$CurvHead)

# CurvTail
par(mfrow=c(1,1))
boxplot(aib$CurvTail, at=1,xlim=c(0, 4),main = "Compare variable CurvTail for rim_nf & aib_nf & n2_nf")
boxplot(rim$CurvTail, at=2, add=TRUE)
boxplot(n2$CurvTail, at=3, add=TRUE)
axis(1,at=c(1,2,3),adj=1,padj=0.5,labels=c("aib_nf","rim_nf","n2_nf")) 

summary(aib$CurvTail)
summary(rim$CurvTail)
summary(n2$CurvTail)

par(mfrow=c(3,1))
hist(aib$CurvTail)
hist(rim$CurvTail)
hist(n2$CurvTail)

#MAXWIDTH
par(mfrow=c(1,1))
boxplot(aib$MaxWidth, at=1,xlim=c(0, 4),main = "Compare variable MaxWidth for rim_nf & aib_nf & n2_nf")
boxplot(rim$MaxWidth, at=2, add=TRUE)
boxplot(n2$MaxWidth, at=3, add=TRUE)
axis(1,at=c(1,2,3),adj=1,padj=0.5,labels=c("aib_nf","rim_nf","n2_nf")) 

summary(aib$MaxWidth)
summary(rim$MaxWidth)
summary(n2$MaxWidth)

par(mfrow=c(3,1))
hist(aib$MaxWidth)
hist(rim$MaxWidth)
hist(n2$MaxWidth)

#IXX


#IYY


#Ixy
par(mfrow=c(1,1))
boxplot(aib$Ixy, at=1,xlim=c(0, 4),main = "Compare variable Ixy for rim_nf & aib_nf & n2_nf")
boxplot(rim$Ixy, at=2, add=TRUE)
boxplot(n2$Ixy, at=3, add=TRUE)
axis(1,at=c(1,2,3),adj=1,padj=0.5,labels=c("aib_nf","rim_nf","n2_nf")) 

summary(aib$Ixy)
summary(rim$Ixy)
summary(n2$Ixy)

par(mfrow=c(3,1))
hist(aib$Ixy)
hist(rim$Ixy)
hist(n2$Ixy)


































