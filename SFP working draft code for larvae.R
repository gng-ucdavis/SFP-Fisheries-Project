###Creating code for larvae (I want someestimate of mortaltiy for the entire cohort)

##What data do we have for growth?
	##Look at the script on "modeling zoea growth" for details but the function is as below
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
t=seq(from =0, to =15.5, by =1)
cl=rep(0, length(t))
for(i in 1:length(t))
{
	cl[i]=larvae.size(t[i])
}
plot(cl~t)

##Add in mortality function
mr=0.12 #Got mortality rates from data using 'Calculating mortality rates for larvae' script
lr=1

mortality=function(l){
	m=mr*(lr/l)
}

###Let's create the actual model forloop for total mortality
numbers=function(t, i) {
	num=n[i]*exp(-(mortality(larvae.size(t)))*delta.t) ###Removed fishing mortrality
}

##Hmm.. for larvae, time span is limited to 15.5 days and in this case, maybe want higher resolution time point (like 0.5 days?)
delta.t=0.5 ##Resolution between time points. In case you're wondering, your final estimates do not change whether you use 0.5 or 1 for delta.t (as long as those intervals are in the same unit as mortality (per day))
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

t.1=c(t, max(t)+1)
par(mfrow=(c(2,2)))
plot(s~t.1, ylab='Crab length (mm)', xlab='Days',  cex.lab=1.5, cex.axis=1.5)
text(1, 1.6, labels=c('a'), cex=2 )
plot(m~t.1, ylab='Mortality rate', xlab='Days', cex.lab=1.5, cex.axis=1.5)
text(1, 0.22, labels=c('b'), cex=2 )
plot(n~t.1, ylab='Cohort proportion survival', xlab='Days', cex.lab=1.5, cex.axis=1.5)
text(1, 0.9, labels=c('c'), cex=2 )

larval.data=data.frame(n,s,m)
#n~0.09 after larvae (9% survival is decent)