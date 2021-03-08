##The goal of this script is to get estimates for the fishing mortality equation in Lorenzen paper using the fishing mortality data provided by Sheryl
##Process is to use non linear regression to get parameter estimates
	##We know from plotting the Lorenzen model that that is a sigmoidal curve as a function of size
##Using nls function for the most part
####Last part: Data is in cm and I think you were working in mm. Keep units consistent!

data=read.csv('~/Dropbox/Postdoc/Blue crab stock enhancement project/SFP/SFP Model Code/Fishing mortality.csv')
head(data)
Mid.length.mm=data$Mid.length*10
plot(Fishing.mortality~Mid.length, data=data)
lines(Fishing.mortality~Mid.length, data=data)

##First try running nls model
f.nls=nls(Fishing.mortality~f.inf/(1+exp(sel*(Mid.length.mm-lc))), data=data, start=list(f.inf=4, sel=-1.5, lc=110))
summary(f.nls)
##Hey it works!! Plus it spits out std error bars
plot(Fishing.mortality~Mid.length.mm, data=data, ylab='Fishing mortality', xlab='Length (mm)', pch=16, cex.axis=1.5, cex.lab=1.5)
lines(predict(f.nls)~Mid.length.mm, data=data) ##What this tells me is that it is badly modeling the higher end of the model because of weird low catches at large sizes, but it doesn't really matter because I am only interested in <10mm CW and it fits really well there!

##Rough bootstrap 
plot(Fishing.mortality~Mid.length.mm, data=data)
for(i in 1:1000)
{
x=seq(from=min(Mid.length.mm), to=max(Mid.length.mm), by =0.01)
y=rnorm(1,2.96, 0.315)/(1+exp(-abs(rnorm(1, -0.157, .126))*(x-rnorm(1, 108.3, 5.8)))) ###The big issue with this model is that the selectivity coefficient is not strongly signficant. If sel goes positive, the whole s-shape is flipped
lines(y~x)
}
points(Fishing.mortality~Mid.length.mm, data=data, col='red', pch=16)