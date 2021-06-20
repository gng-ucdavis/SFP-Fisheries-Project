###Script is to determine length weight relationship (assume it is linear on a log-log scale)
data=read.csv('~/Dropbox/Postdoc/Blue crab stock enhancement project/SFP/SFP Model Code/SFP-Fisheries-Project/Length weight data.csv')
names(data)
data$Length=data$X.Length
data$Weight=data$Weight..g
head(data)
str(data)
plot(Weight~Length, data=data)

data$log.weight=log(data$Weight)
data$log.length=log(data$Length)

data.lm=lm(log.weight~log.length, data=data)
summary(data.lm) #Fits with excel data

plot(log.weight~log.length, data=data)
qqnorm(resid(data.lm))
qqline(resid(data.lm))
shapiro.test(resid(data.lm))

new.data=data.frame(log.length=c(log(10.2), log(11.5)))
exp(predict(data.lm, newdata=new.data))