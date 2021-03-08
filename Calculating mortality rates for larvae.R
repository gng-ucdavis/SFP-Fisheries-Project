###This is just a copied version of the calculating mortality rates (jank) script except it's been cleaned up for larval mortality
##Reminder that the goal of this script is to calculate mr when givent the percentage of larvae that survived in experiments until day n
	#Not a simple constant proportion because larvae grow over time and we explicitly say that mortality is related to size
	##Basically brute forcing this approach
##Also TLDR but given the survival rate from Liao et al., we get a mr of 0.12 at a lr of 1

##Need to create a larvae growth function (I have one but it incorproates noise which is just too hard for this simulation. Going to just use the mean for each stage)
larvae.size=function(t){
	if(t<3.5){
	cl=0.49 #runif(1, 0.44, 0.54)	
	}
	
	else if(t<7.5 & t>=3.5){
	cl= 0.75 #runif(1, 0.72, 0.77)
	}
	
	else if(t<9.5 & t>=7.5){
	cl=0.83 #runif(1, 0.79, 0.87)
	}
	
	else if(t<12 & t>=9.5){
	cl=1.02 #runif(1, 0.98, 1.06)
	}
	
	else if(t<=15.5 & t>=12){
	cl=1.75 #runif(1, 1.69, 1.81)
	}
	
	else print('not larva')
}

##Quick plot just to make sure that function is working
t=seq(from =0, to =15.5, by =0.5)
cl=rep(0, length(t))
for(i in 1:length(t))
{
	cl[i]=larvae.size(t[i])
}
plot(cl~t, ylab='Larval length (mm)', xlab='Days', cex.axis=1.5, cex.lab=1.5)


###From Liao et al. 2011: Get 39% survival after 4 days (my time funciton is in days)
mortality=function(l){
	m=mr*(lr/l)
}

mort.test=seq(from=0.1, to=0.15, by=0.001)
pop.at.4days=rep(5, length(mort.test))

for(j in 1:length(mort.test))
{
	mr=mort.test[j]
	lr=1 #Arbitrarily making lr =1

#Sub in mortality function for just r constant
numbers=function(t, i) {
	num=n[i]*exp(-(mortality(larvae.size(t)))*delta.t) 
}

delta.t=0.5 ##Resolution between time points
t=seq(from=0, to=15.5, by=delta.t) 
n=rep(0, length(t))
m=rep(0, length(t))
s=rep(0, length(t))

n[1]=1 ##Starting population size

for(i in 1:length(t))
{
	n[i+1]=numbers(t=t[i], i=i)
	s[i+1]=larvae.size(t[i])
	m[i+1]=mortality(larvae.size(t[i]))
}

t.1=c(t, max(t)+delta.t)
x=data.frame(n, t.1)
pop.at.4days[j]=x[x$t.1==4,]$n
par(mfrow=(c(2,2)))
plot(n~t.1)
plot(m~t.1)
plot(s~t.1)
}

plot(pop.at.4days~mort.test) 
cbind(pop.at.4days, mort.test)

###Done!!! Mortality is at 0.12 a lr of 1