#####Let's brute force and caluculate the mortality rate for juveniles building upon the code for larval mortality
##Things that need to change are the growth function and the targeted mortality rates


##Juvenile growth function
l.inf=mean(c(219.8, 204.1, 208, 211.8, 211, 188.6)) ##average adult size (CW) in mm
l.rel=mean(c(2.38, 2.43)) ##average size at crab stage 1
# K=mean(c(1.82, 1.8, 1.9, 1.7, 1.64, 1.62)) ####Based on the modeled outputs and the results from Josileen and Menon 2005, I'm pretty sure these K values are annual growth rates. So I'm going to divide them by 365 and work in days
K=mean(c(1.82, 1.8, 1.9, 1.7, 1.64, 1.62))/365

size=function(t){
	l=l.inf-(l.inf-l.rel)*exp(-K*t)
}

t=seq(0, 365, by =1) ##Time is in dauys
length=size(t)
plot(length~t) 


###Okay, for this trial, we are using Azra et al. 2019 parameters
##Found 75% survival after 48 days starting from C1
mortality=function(l){
	m=mr*(lr/l)
}

mort.test=seq(from=0.01, to=0.03, by=0.0001)
pop.at.48days=rep(5, length(mort.test))

for(j in 1:length(mort.test))
{
	mr=mort.test[j]
	lr=5 #Arbitrarily making lr =5

#Sub in mortality function for just r constant
numbers=function(t, i) {
	num=n[i]*exp(-(mortality(size(t)))*delta.t) ###Removed fishing mortrality
}

delta.t=0.5 ##Resolution between time points
t=seq(from=0, to=50, by=delta.t) 
n=rep(0, length(t))
m=rep(0, length(t))
s=rep(0, length(t))

n[1]=1 ##Starting population size

for(i in 1:length(t))
{
	n[i+1]=numbers(t=t[i], i=i)
	s[i+1]=size(t[i])
	m[i+1]=mortality(size(t[i]))
}

t.1=c(t, max(t)+delta.t)
x=data.frame(n, t.1)
pop.at.48days[j]=x[x$t.1==48,]$n
par(mfrow=(c(2,2)))
plot(n~t.1)
plot(m~t.1)
plot(s~t.1)
}

plot(pop.at.48days~mort.test) ###Looks like it works!! Side detour where I muck around with the growth function just to make sure everything is working (yes, fwiw, size seems to play a role in your simulation )
cbind(pop.at.48days, mort.test)

####Done! mr= 0.0176 for 75% surivval after 48 days with lr = 5
