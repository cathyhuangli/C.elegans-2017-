#setwd("/Users/li/Downloads/to compare/")
setwd("C://Users/lhuang37/Downloads/to compare/")
getwd()

install.packages("fBasics")
library(fBasics)
basicStats(aib[,6:9])
# speed over 500 is outlier
aib=aib[aib$Speed<500,]
rim=rim[rim$Speed<500,]
n2=n2[n2$Speed<500,]
# angle is all right
#accleration <-4000 all are outliers
aib=aib[aib$Acceleration>-4000,]
rim=rim[rim$Acceleration>-4000,]
n2=n2[n2$Acceleration>-4000,]
# angular velocity < 4.0
aib=aib[aib$Angular_Velocity<4.0,]
rim=rim[rim$Angular_Velocity<4.0,]
n2=n2[n2$Angular_Velocity<4.0,]

t1=basicStats(aib)[c("Mean", "Stdev", "Median", "Minimum", "Maximum"),6:9]
t2=basicStats(rim)[c("Mean", "Stdev", "Median", "Minimum", "Maximum"),6:9]
t3=basicStats(n2)[c("Mean", "Stdev", "Median", "Minimum", "Maximum"),6:9]
tall=rbind(t1,t2,t3)
write.csv(tall,'summary3type.csv')


aib <- read.csv(file="aib.csv", header=TRUE, sep=",")
rim <- read.csv(file="rim.csv", header=TRUE, sep=",")
n2 <- read.csv(file="n2.csv", header=TRUE, sep=",")


aib1 <- read.csv(file="movementFeatures_aibhrnf3.csv", header=FALSE, sep=",")
aib2 <- read.csv(file="movementFeatures_aibhrnf7.csv", header=FALSE, sep=",")
aib3 <- read.csv(file="movementFeatures_aibhrnf9.csv", header=FALSE, sep=",")

aib=rbind(aib1,aib2,aib3)
colnames(aib) <- c("Frame","TimeElapsed","Duration","X","Y","Speed","Acceleration","Angle","Angular_Velocity")
colnames(aib1) <- c("Frame","TimeElapsed","Duration","X","Y","Speed","Acceleration","Angle","Angular_Velocity")


n2_1 <- read.csv(file="movementFeatures_n2hrnf1.csv", header=FALSE, sep=",")
n2_2 <- read.csv(file="movementFeatures_n2hrnf3.csv", header=FALSE, sep=",")
n2_3 <- read.csv(file="movementFeatures_n2hrnf5.csv", header=FALSE, sep=",")

n2=rbind(n2_1,n2_2,n2_3)
colnames(n2) <- c("Frame","TimeElapsed","Duration","X","Y","Speed","Acceleration","Angle","Angular_Velocity")
colnames(n2_1) <- c("Frame","TimeElapsed","Duration","X","Y","Speed","Acceleration","Angle","Angular_Velocity")

rim1 <- read.csv(file="movementFeatures_rimhrnf7.csv", header=FALSE, sep=",")
rim2 <- read.csv(file="movementFeatures_rimhrnf3.csv", header=FALSE, sep=",")
rim3 <- read.csv(file="movementFeatures_rimhrnf5.csv", header=FALSE, sep=",")

rim=rbind(rim1,rim2,rim3)
colnames(rim) <- c("Frame","TimeElapsed","Duration","X","Y","Speed","Acceleration","Angle","Angular_Velocity")
colnames(rim1) <- c("Frame","TimeElapsed","Duration","X","Y","Speed","Acceleration","Angle","Angular_Velocity")

write.csv(aib,'aib.csv')
write.csv(n2,'n2.csv')
write.csv(rim,'rim.csv')

summary(rim$Angle)

# variable Speed
boxplot(aib$Speed, at=1, xlim=c(0, 4),main = "Compare of Speed for rim_nf & aib_nf & n2_nf")
boxplot(rim$Speed, at=2, add=TRUE)
boxplot(n2$Speed, at=3, add=TRUE)
axis(1,at=c(1,2,3),adj=1,padj=0.5,labels=c("aib_nf","rim_nf","n2_nf")) 

summary(aib$Speed)
summary(rim$Speed)
summary(n2$Speed)

par(mfrow=c(3,1))
plot(aib$TimeElapsed,aib$Speed,type='l',xlim=c(0,2000),ylim=c(0,500))
plot(rim$TimeElapsed,rim$Speed,type='l',xlim=c(0,2000),ylim=c(0,500))
plot(n2$TimeElapsed,n2$Speed,type='l'),xlim=c(0,2000),ylim=c(0,500))

# if we use one video
par(mfrow=c(3,1))
plot(aib1$TimeElapsed,aib1$Speed,type='l',xlim=c(0,1200),ylim=c(0,400),main='AIB_HR_nf3')
plot(rim1$TimeElapsed,rim1$Speed,type='l',xlim=c(0,1200),ylim=c(0,400),main='RIM_HR_nf7')
plot(n2_1$TimeElapsed,n2_1$Speed,type='l',xlim=c(0,1200),ylim=c(0,400),main='N2_HR_nf1')

# Acceleration
par(mfrow=c(1,1))
boxplot(aib$Acceleration, at=1, xlim=c(0, 4),main = "Compare of Acceleration for rim_nf & aib_nf & n2_nf")
boxplot(rim$Acceleration, at=2, add=TRUE)
boxplot(n2$Acceleration, at=3, add=TRUE)
axis(1,at=c(1,2,3),adj=1,padj=0.5,labels=c("aib_nf","rim_nf","n2_nf")) 

summary(aib$Acceleration)
summary(rim$Acceleration)
summary(n2$Acceleration)

par(mfrow=c(3,1))
plot(aib$TimeElapsed,aib$Acceleration,type='l')
plot(rim$TimeElapsed,rim$Acceleration,type='l')
plot(n2$TimeElapsed,n2$Acceleration,type='l')

# if we use one video,xlim=c(0,1000),ylim=c(-1000,1000),
par(mfrow=c(3,1))
plot(aib1$TimeElapsed,aib1$Acceleration,type='l',xlim=c(0,1000),ylim=c(-1000,1000),main="Acceleration of AIB_HR_nf3 over Time")
plot(rim1$TimeElapsed,rim1$Acceleration,type='l',xlim=c(0,1000),ylim=c(-1000,1000),main='Acceleration of RIM_HR_nf7 over Time')
plot(n2_1$TimeElapsed,n2_1$Acceleration,type='l',xlim=c(0,1000),ylim=c(-1000,1000),main='Acceleration of N2_HR_nf1 over Time')


# Angle

par(mfrow=c(1,1))
boxplot(aib$Angle, at=1, xlim=c(0, 4),main = "Compare of Angle for rim_nf & aib_nf & n2_nf")
boxplot(rim$Angle, at=2, add=TRUE)
boxplot(n2$Angle, at=3, add=TRUE)
axis(1,at=c(1,2,3),adj=1,padj=0.5,labels=c("aib_nf","rim_nf","n2_nf")) 
abline(h=-18.40,col='red')
text(2.3, -10, "Median",col='red',cex = .8)

summary(aib$Angle)
summary(rim$Angle)
summary(n2$Angle)

par(mfrow=c(3,1))
plot(aib$TimeElapsed,aib$Angle,type='l')
plot(rim$TimeElapsed,rim$Angle,type='l')
plot(n2$TimeElapsed,n2$Angle,type='l')

# if we use one video,xlim=c(0,1000),ylim=c(-1000,1000),
par(mfrow=c(3,1))
plot(aib1$TimeElapsed,aib1$Angle,type='l',xlim=c(0,1000),ylim=c(-1000,1000),main="Angle of AIB_HR_nf3 over Time")
plot(rim1$TimeElapsed,rim1$Angle,type='l',xlim=c(0,1000),ylim=c(-1000,1000),main='Angle of RIM_HR_nf7 over Time')
plot(n2_1$TimeElapsed,n2_1$Angle,type='l',xlim=c(0,1000),ylim=c(-1000,1000),main='Angle of N2_HR_nf1 over Time')
# Angular_Velocity

par(mfrow=c(1,1))
boxplot(aib$Angular_Velocity, at=1, xlim=c(0, 4),main = "Compare of Angular_Velocity for rim_nf & aib_nf & n2_nf")
boxplot(rim$Angular_Velocity, at=2, add=TRUE)
boxplot(n2$Angular_Velocity, at=3, add=TRUE)
axis(1,at=c(1,2,3),adj=1,padj=0.5,labels=c("aib_nf","rim_nf","n2_nf")) 
abline(h=-18.40,col='red')
text(2.3, -10, "Median",col='red',cex = .8)

summary(aib$Angular_Velocity)
summary(rim$Angular_Velocity)
summary(n2$Angular_Velocity)

par(mfrow=c(3,1))
plot(aib$TimeElapsed,aib$Angular_Velocity,type='l')
plot(rim$TimeElapsed,rim$Angular_Velocity,type='l')
plot(n2$TimeElapsed,n2$Angular_Velocity,type='l')

# if we use one video,xlim=c(0,1000),ylim=c(0,4.0),
par(mfrow=c(3,1))
plot(aib1$TimeElapsed,aib1$Angular_Velocity,type='l',xlim=c(0,1000),ylim=c(0,3.0),main="Angular_Velocity of AIB_HR_nf3 over Time")
plot(rim1$TimeElapsed,rim1$Angular_Velocity,type='l',xlim=c(0,1000),ylim=c(0,3.0),main='Angular_Velocity of RIM_HR_nf7 over Time')
plot(n2_1$TimeElapsed,n2_1$Angular_Velocity,type='l',xlim=c(0,1000),ylim=c(0,3.0),main='Angular_Velocity of N2_HR_nf1 over Time')







