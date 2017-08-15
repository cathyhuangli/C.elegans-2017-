data=n2nf4allf[,which(names(n2nf4allf) %in% c('Ixy','Iyy','ComptFactor','SkewerAngle','Kim'))]
data[data==""] <- NA
data<-na.omit(data)
data=data[data$ElapsedTime<=1400,]
data=data[data$SkewerAngle<=270,]
plot(data$ElapsedTime,data$SkewerAngle,main='Worm N2_nf4')
abline(v=160,col='red') 
text(170, 150, "Forward", col = "red") 
abline(v=883,col='red') 
text(850, 190, "Forward-SharpTurn", col = "red") 
abline(v=334,col='red') 
text(350, 190, "ReverseShort", col = "red") 
abline(v=972,col='red') 
text(1100, 60, "ReverseLong", col = "red") 

plot(data$ElapsedTime,data$ComptFactor,main='Worm N2_nf4')
abline(v=160,col='red') 
text(170, 150, "Forward", col = "red") 
abline(v=883,col='red') 
text(850, 190, "Forward-SharpTurn", col = "red") 
abline(v=334,col='red') 
text(350, 190, "ReverseShort", col = "red") 
abline(v=972,col='red') 
text(1100, 60, "ReverseLong", col = "red") 




normalized<- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                      min(x, na.rm=TRUE))}
ds.norm=as.data.frame(lapply(data[,1:5], normalized))
#so we just add label:
ds.norm$Kim=data$Kim
#ds.norm$Kim=dt$Kim
# now try random forest
# try random forest
newdf=ds.norm

par(mfrow=c(1,1))
plot(newdf$ElapsedTime,newdf$SkewerAngle)

dt1=aibhrnf1
dt1=dt1[,which(names(aibhrnf1) %in% c('ElapsedTime','SkewerAngle'))]
dt1=dt1[dt1$ElapsedTime<=1400,]
dt1=dt1[dt1$SkewerAngle<=270,]
plot(dt1$ElapsedTime,dt1$SkewerAngle,main='Worm AIB_HR_nf1')


dt2=rimhrnf2
dt2=dt2[,which(names(aibhrnf1) %in% c('SeqNum','ElapsedTime','SkewerAngle'))]
dt2=dt2[dt2$ElapsedTime<=1400,]
dt2=dt2[dt2$SkewerAngle<=270,]
plot(dt2$ElapsedTime,dt2$SkewerAngle,main='Worm RIM_HR_nf2')
#plot(dt2$SeqNum,dt2$SkewerAngle,main='Worm RIM_HR_nf2')














library(car)
newdf$Kim<-recode(newdf$Kim,"c('Forward-NTD','Forward-Shallow','Forward-Sharp')='Forward'")
newdf$Kim<-recode(newdf$Kim,"c('Stopped-ReverseShort','Stopped-ReverseLong','Stopped-Stop')='Stop'")
newdf$Kim<-recode(newdf$Kim,"c('Backward-ReverseShort','Backward-ReverseLong')='Reverse'")
library(depmixS4)

ret=newdf$SkewerAngle
mod <- depmix(ret ~ 1, data=data.frame(ret=ret),nstates=3)
f <- fit(mod)
summary
esttrans <- posterior(f)
table(esttrans[,1],newdf$Kim)
par(mfrow=c(3,1))
plot(newdf$ElapsedTime, newdf$Kim, type='l', main="N2_nf4: Kim's movement label")
plot(newdf$ElapsedTime, esttrans[,1], type='l', main='N2_nf4: Estimated state')
plot(newdf$ElapsedTime, ret, type='l', main='N2_nf4: SkewerAngle')







