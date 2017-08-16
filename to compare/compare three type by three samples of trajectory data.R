setwd("C:/Users/lhuang37/Downloads/to compare/")

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
write.csv(n2,'aib.csv')
write.csv(rim,'aib.csv')

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
plot(n2$TimeElapsed,n2$Speed,type='l',xlim=c(0,2000),ylim=c(0,500))

# if we use one video
par(mfrow=c(3,1))
plot(aib1$TimeElapsed,aib1$Speed,type='l',xlim=c(0,1200),ylim=c(0,400),main='AIB_HR_nf3')
plot(rim1$TimeElapsed,rim1$Speed,type='l',xlim=c(0,1200),ylim=c(0,400),main='RIM_HR_nf7')
plot(n2_1$TimeElapsed,n2_1$Speed,type='l',xlim=c(0,1200),ylim=c(0,400),main='N2_HR_nf1')




# Acceleration



# Angle


# Angular_Velocity









