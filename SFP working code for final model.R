###Creating (potentially) final code incorporating both larvae/juveniles stage
	##Have to be careful not to get my variables crossed
	##Also start to incorporate some uncertainty
		##Trying this on 3/l5/21
		##Big issue is how to store all this data and be able to recover it for plotting purposes
			##Basically create a dataframe and keep adding to it...
			##3/26/21 Need to tweak field data calculations to account for different referenced length and incorporate variability 

##What data do we have for growth?
	##Look at the script on "modeling zoea growth" for details but the function is as below


l=10000 ##Number of reps
final.larval.data=NULL
final.juv.data=NULL
results=rep(0, l)

##Moving functions out of forloop as it seems that takes a while to run when looping and I don't need to create an identical function 10,000 times
	#And yes, I've checked, it will still be random values as an output even if you don't loop it
larvae.size=function(t){
	if(t<3.5){
	cl=runif(1, 0.44, 0.54)	#0.49 #
	}
	
	else if(t<7.5 & t>=3.5){
	cl= runif(1, 0.72, 0.77) #0.75 #
	}
	
	else if(t<9.5 & t>=7.5){
	cl=runif(1, 0.79, 0.87) #0.83 #
	}
	
	else if(t<12 & t>=9.5){
	cl=runif(1, 0.98, 1.06) #1.02 #
	}
	
	else if(t<=15.5 & t>=12){
	cl=runif(1, 1.69, 1.81) #1.75 #
	}
	
	else print('not larva')
}

size=function(t){
	l=l.inf-(l.inf-l.rel)*exp(-K*t)
}

mortality.larvae=function(l){
	m=mr.larvae*(lr.larvae/l)
}

mortality=function(l){
	m=mr*(lr/l)
}



par(mfrow=c(2,2))
for(k in 1:l) 
{
plot(k, pch=16)

t=seq(from =0, to =15.5, by =0.5)
cl=rep(0, length(t))
for(i in 1:length(t))
{
	cl[i]=larvae.size(t[i])
}
# plot(cl~t)

##Add in mortality function
# mr.larvae=rnorm(1, mean=mean(c(0.114, 0.12)), sd=sd(c(0.114, 0.12))) #Got mortality rates from data using 'Calculating mortality rates for larvae' script
mr.larvae=rnorm(1, mean=0.3972, sd=sd(c(0.114, 0.12))) ##Mortality rates from the field and using sd from lab studies (better than nothing)
lr.larvae=1



###Let's create the actual model forloop for total mortality
numbers=function(t, i) {
	num=n.larvae[i]*exp(-(mortality.larvae(larvae.size(t)))*delta.t) ###Removed fishing mortrality
}

##Hmm.. for larvae, time span is limited to 15.5 days and in this case, maybe want higher resolution time point (like 0.5 days?)
delta.t=0.5 ##Resolution between time points
t.larvae=seq(from=0, to=15.5, by=delta.t)
run.larvae=rep(k, length(t.larvae)+1)
n.larvae=rep(0, length(t))
m.larvae=rep(0, length(t))
s.larvae=rep(0, length(t))

n.larvae[1]=1 ##Starting population size

for(i in 1:length(t.larvae))
{
	n.larvae[i+1]=numbers(t=t.larvae[i], i=i)
	s.larvae[i+1]=larvae.size(t.larvae[i])
	m.larvae[i+1]=mortality.larvae(larvae.size(t.larvae[i]))
}

t.1.larvae=c(t.larvae, max(t.larvae)+delta.t)
# par(mfrow=(c(2,2)))
# plot(s.larvae~t.1.larvae, ylab='Crab length (mm)', xlab='Days',  cex.lab=1.5, cex.axis=1.5)
# text(1, 1.6, labels=c('a'), cex=2 )
# plot(m.larvae~t.1.larvae, ylab='Mortality rate', xlab='Days', cex.lab=1.5, cex.axis=1.5)
# text(1, 0.22, labels=c('b'), cex=2 )
# plot(n.larvae~t.1.larvae, ylab='Cohort proportion survival', xlab='Days', cex.lab=1.5, cex.axis=1.5)
# text(1, 0.9, labels=c('c'), cex=2 )

larval.data=data.frame(run.larvae, n.larvae,s.larvae,m.larvae, t.1.larvae)
surv.larvae=larval.data[nrow(larval.data),]$n.larvae ###4/6/21 This line of code only works if you're running 1 simulation. If greater than 1 (like 10,000), then need to do something else. It's just grabbing the estimates for the first simulation which is arbitrary
#n~0.09 after larvae (9% survival is decent)


final.larval.data=rbind(final.larval.data, larval.data)



######Code for juveniles: for simplicity sake, I will leave variables as is and only change variable names for larvae
l.inf=abs(rnorm(1, mean(c(207.2, 162.5, 206, 169.5, 188.7)), sd(c(207.2, 162.5, 206, 169.5, 188.7)))) ##average adult size (CW) in mm
l.rel=mean(c(2.38, 2.43)) ##average size at crab stage 1
# K=mean(c(1.82, 1.8, 1.9, 1.7, 1.64, 1.62)) ####Based on the modeled outputs and the results from Josileen and Menon 2005, I'm pretty sure these K values are annual growth rates. So I'm going to divide them by 365 and work in days
K=abs(rnorm(1,mean(c(0.0048, 0.0022, 0.0029, 0.0037, 0.0037)), sd(c(0.0048, 0.0022, 0.0029, 0.0037, 0.0037))))


t=seq(0, 365, by =1) ##assume day
length=size(t)
# plot(length~t, ylab='Carapace width (mm)', xlab='Days', cex.axis=1.5, cex.lab=1.5) ##excellent, growth output seems to match paper's data

##Mortality function
mr=abs(rnorm(1,mean(c(0.309, 0.284, 0.986, 0.851, 0.736)), sd(c(0.309, 0.284, 0.986, 0.851, 0.736)))) ###Using values from 'Calculating mortality rates for juveniles' #Removing the 0.088 value since that is hatchery derived
lr=1


# plot(m~length)
# plot(m~t)


##Fishing function, may have actual data here=
##Should note that f and m are 'rate' in Nexp(rt) so can be any positive numbers (greater than 1 less than 1, it's all good). But doesn't have any meaningful units. It's not %/time period
f.inf=abs(rnorm(1, 4.51, 0.131))
# # q=-1.58 ##Pretty sure q has to be a negative number or equation should have a (-) sign. Going to make q solely positive and put (-) in equation
q=abs(rnorm(1, 0.093, 0.0074)) ##Maybe I want a different distribution than normal for this parameter (has to be solely positive, gamma or use absolute values)
lc=rnorm(1, 117.76, 1.14) ##SE 5.8 #

fishing=function(l) {
	f=f.inf/(1+exp(-q*(l-lc)))
}

f=fishing(length)/365
# plot(f~length)
# plot(f~t)

##Construct actual forloop
##Great everything works in theory and really, all I need now is to fill in numbers
numbers=function(t, i) {
	num=n[i]*exp(-(mortality(size(t))+fishing(size(t))/365)*delta.t)
}

t=seq(from=0, to=365, by=1) 
run=rep(k, length(t)+1)
n=rep(0, length(t))
h=rep(0, length(t))
m=rep(0, length(t))
s=rep(0, length(t))
f=rep(0, length(t))

n[1]=1
delta.t=1


for(i in 1:length(t))
{
	n[i+1]=numbers(t=t[i], i=i)
	s[i+1]=size(t[i])
	m[i+1]=mortality(size(t[i]))
	f[i+1]=fishing(size(t[i]))/365
}

t.1=c(t, max(t)+1)


final.data=data.frame(run, n, m, s,f, t.1)
# final.data[final.data$s<100,]$n
surv.juv=final.data[final.data$s<100,]$n[length(final.data[final.data$s<100,]$n)] ##This weird gross piece of code finds the last value for proportion survivng where crabs are right before 100mm.  ###4/6/21 This line of code only works if you're running 1 simulation. If greater than 1 (like 10,000), then need to do something else. It's just grabbing the estimates for the first simulation which is arbitrary. ###On second thought, because this is within the forloop, the code actually works. Except it means I can't retroactively get the results once I save it into a dataframe

surv.larvae*surv.juv

##Within the total forloop, create a dataframe of all parameters
final.juv.data=rbind(final.juv.data, final.data)

##Get a dataframe for just results
results[k]=surv.larvae*surv.juv #Note that this is in porportion
}

####Messing with results
head(final.larval.data)
head(final.juv.data)
hist(results)
mean(results)
quantile(results, c(0.025, 0.975))
sd(results)

##Writing 10000 runs into csv
# setwd('~/Dropbox/Postdoc/Blue crab stock enhancement project/SFP/SFP Model Code/Results')
# write.table(final.larval.data, 'larval.field.data.csv', sep=',')
# write.table(final.juv.data, 'juvenile.field.data.csv', sep=',')


###Getting survival estimates post-running code
final.larval.data=read.csv('~/Dropbox/Postdoc/Blue crab stock enhancement project/SFP/SFP Model Code/Results/larval.data.csv')#for lab based mortality
head(final.larval.data)
str(final.larval.data)

final.larval.field.data=read.csv('~/Dropbox/Postdoc/Blue crab stock enhancement project/SFP/SFP Model Code/Results/larval.field.data.csv') #for field based mortality
head(final.larval.field.data)
str(final.larval.field.data)


final.juv.data=read.csv('~/Dropbox/Postdoc/Blue crab stock enhancement project/SFP/SFP Model Code/Results/juvenile.data.csv') #both this and final.juv.field have the same code and only differ in that parameters are drawn from rnorm. I just wanted to see how different they are based on randomization
head(final.juv.data)

final.juv.field.data=read.csv('~/Dropbox/Postdoc/Blue crab stock enhancement project/SFP/SFP Model Code/Results/juvenile.field.data.csv')
head(final.juv.field.data)

tailx=function(x){
	res=tail(x,1)
}

##Results using lab based larval mortality
surv.larvae=with(final.larval.data, aggregate(list(n.larvae=n.larvae), list(run=run.larvae), tailx))
surv.juv=with(final.juv.data[final.juv.data$s<115,], aggregate(list(n.juv=n), list(run=run), tailx))
results=surv.larvae$n.larvae*surv.juv$n.juv
hist(results)
mean(results)
median(results)
quantile(results, c(0.025, 0.975))


##Results using field based larval mortality
surv.larvae=with(final.larval.field.data, aggregate(list(n.larvae=n.larvae), list(run=run.larvae), tailx))
surv.juv=with(final.juv.data[final.juv.data$s<102,], aggregate(list(n.juv=n), list(run=run), tailx)) #Keeping juvenile data the same since that doesn't change whether which larval mortaltity we choose
results=surv.larvae$n.larvae*surv.juv$n.juv
hist(results)
mean(results)
median(results)
quantile(results, c(0.025, 0.975))