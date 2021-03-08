##This script is to generate estimates of zoea growth data for von Bertlanffy using Josileen and Menon 2004
data=read.csv('~/Dropbox/Postdoc/Blue crab stock enhancement project/SFP/SFP Model Code/Zoea growth data.csv')
head(data)
data$length=data$Carapace.length..mm.
data$Day.cu=cumsum(data$Day)
plot(length~Day.cu, data=data)

##I'm realizing that I shouldn't necessarily use the end date for each stage when modeling growth
	##Because it's discrete growth, I should use the mid-point of each stage to represet their size 
		##Or better yet have multiple time points for each stage... (would that narrow CI too much for parameter estimates?)
		##Also also, big jump in size when going to megalopae which could impact von Bertalanffy curve...
			##Ingoring that aspect for now and just see how well the curve fits (no one says asymptote has to occur)
			
			
day=seq(from =0, to =15.5, by =0.5)
cw=c(rep(0.48, 8), rep(0.75, 7), rep(0.83, 5), rep(1.02, 5), rep(1.75, 7))
plot(cw~day)

l.rel=0.48 ##average size at 1st zoea stage
##So now fit von?
z.nls=nls(cw~l.inf-(l.inf-l.rel)*exp(-K*day), start=list(l.inf=1.9, K=0.8), trace=T )


l.inf=7.2
K=0.008
cw.sim=l.inf-(l.inf-l.rel)*exp(-K*day)

plot(cw~day, pch=16)
lines(cw.sim~day)


###Okay vonBerlannfy is a fail. Can't get model to fit
###2 possible train of thoughts
	##Remove megalopa stage and try again? (Could work but now I need to somehow incorporate megalopa into it)
	###The method I'm leaning towards: we dont' need a regression to model growth because we have explict data at each time point...
		###Obviously, obviously we are overfiting here
		##But we can add some error in length for the growth data...
		##So it's a stepwise function where given the day, we will give you size of larvae... (hm)
			##And can still incorporate mortality/length relationship...
##Okay, work with me here, create a step-wise function
larvae.size=function(day){
	if(day<3.5){
	cl=0.48	
	}
	
	else if(day<7.5 & day>=3.5){
	cl=0.75
	}
	
	else if(day<9.5 & day>=7.5){
	cl=0.83
	}
	
	else if(day<12 & day>=9.5){
	cl=1.02
	}
	
	else if(day<=15.5 & day>=12){
	cl=1.75
	}
	
	else print('not larva')
}

day=seq(from =0, to =15.5, by =0.5)
cl=larvae.size(day) ###Fascinating, function can only look at one value at a time and so uses only the first value in the day vector
					##Seeems to counter the whole point of vectorization... forloop then?
cl=rep(0, length(day))
for(i in 1:length(day))
{
	cl[i]=larvae.size(day[i])
}
plot(cl~day) ##Cool, I"ve just reconstructed the data from the paper.


##Next step is to incorporate some noise

larvae.size=function(day){
	if(day<3.5){
	cl=runif(1, 0.44, 0.54)	
	}
	
	else if(day<7.5 & day>=3.5){
	cl=runif(1, 0.72, 0.77)
	}
	
	else if(day<9.5 & day>=7.5){
	cl=runif(1, 0.79, 0.87)
	}
	
	else if(day<12 & day>=9.5){
	cl=runif(1, 0.98, 1.06)
	}
	
	else if(day<=15.5 & day>=12){
	cl=runif(1, 1.69, 1.81)
	}
	
	else print('not larva')
}
day=seq(from =0, to =15.5, by =0.5)
cl=rep(0, length(day))
for(i in 1:length(day))
{
	cl[i]=larvae.size(day[i])
}
plot(cl~day) ##I'm okay with this