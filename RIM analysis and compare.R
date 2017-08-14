# POSTURE: 4 – Omega Loop Posture supposed correlated with reversal
setwd("C:/Users/lhuang37/Documents/")
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
hist(rim$Posture)
hist(n2$Posture)
#summary(aib$Posture)

table(aib$Posture)
table(rim$Posture)
table(n2$Posture)

prop.table(table(aib$Posture))
prop.table(table(rim$Posture))
prop.table(table(n2$Posture))






















