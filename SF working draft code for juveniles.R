####This code is to recreate the Lorenzen model using actual data for juveniles to adult

##Growth model (I guess with a von bertalanffy model, I dont' have to do stepwise function. Implicitly assuming continuous growth)
l.inf=mean(c(219.8, 204.1, 208, 211.8, 211, 188.6)) ##average adult size (CW) in mm
l.rel=mean(c(2.38, 2.43)) ##average size at crab stage 1
# K=mean(c(1.82, 1.8, 1.9, 1.7, 1.64, 1.62)) ####Based on the modeled outputs and the results from Josileen and Menon 2005, I'm pretty sure these K values are annual growth rates. So I'm going to divide them by 365 and work in days
K=mean(c(1.82, 1.8, 1.9, 1.7, 1.64, 1.62))/365

size=function(t){
	l=l.inf-(l.inf-l.rel)*exp(-K*t)
}

t=seq(0, 365, by =1) ##assume day
length=size(t)
plot(length~t, ylab='Carapace width (mm)', xlab='Days', cex.axis=1.5, cex.lab=1.5) ##excellent, growth output seems to match paper's data

##Mortality function
mr=0.0176 ###Using values from 'Calculating mortality rates for juveniles'
lr=5

mortality=function(l){
	m=mr*(lr/l)
}
m=mortality(length)
plot(m~length)

plot(m~t)


##Fishing function, may have actual data here=
##Should note that f and m are 'rate' in Nexp(rt) so can be any positive numbers (greater than 1 less than 1, it's all good). But doesn't have any meaningful units. It's not %/time period
f.inf=2.96 #SE 0.315
# # q=-1.58 ##Pretty sure q has to be a negative number or equation should have a (-) sign. Going to make q solely positive and put (-) in equation
q=0.158 ##SE 0.126 ##Maybe I want a different distribution than normal for this parameter (has to be solely positive, gamma or use absolute values)
lc=108.3 ##SE 5.8 #

fishing=function(l) {
	f=f.inf/(1+exp(-q*(l-lc)))
}

f=fishing(length)
plot(f~length)
plot(f~t)

##Construct actual forloop
##Great everything works in theory and really, all I need now is to fill in numbers
numbers=function(t, i) {
	num=n[i]*exp(-(mortality(size(t))+fishing(size(t)))*delta.t)
}

t=seq(from=0, to=365, by=1) ##Need to figure out if starts at 1 or 0 for time. ##Might actually want 0 because that will give reference size for crabs (and relevant parameters following that)
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
	f[i+1]=fishing(size(t[i]))
}

t.1=c(t, max(t)+1)
par(mfrow=(c(2,2)))
plot(s~t.1, ylab='Crab width (mm)', xlab='Days', cex.lab=1.5, cex.axis=1.5)
text(1, 160, labels=c('a'), cex=2 )
plot(m~t.1, ylab='Mortality rate', xlab='Days', cex.lab=1.5, cex.axis=1.5)
text(1, 0.035, labels=c('b'), cex=2 )
plot(f~t.1, ylab='Fishing mortality', xlab='Days', cex.lab=1.5, cex.axis=1.5)
text(1, 2.8, labels=c('c'), cex=2 )
plot(n~t.1, ylab='Cohort proportion survival', xlab='Days', cex.lab=1.5, cex.axis=1.5)
text(1, 0.9, labels=c('d'), cex=2 )

final.data=data.frame(n, m, s,f, t.1)
final.data[final.data$s<100,]$n
##So it looks like 0.000153 make it to 100mm (mostly because of high fishing mortality)
0.000153*100 #percent

##0.09*0.000153 (larval and juvenile survival)
0.09*0.000153 
1800000*1.377e-05

0.09*0.000153*100