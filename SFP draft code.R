##Starting fisheries project (maybe want to git this?)
###Let's start simple with Lorenzen model first

##How to create functions
# name=function(var) {
	# equation (var)
# }


##Growth model based on von Bertalanffy
l=function(t){
	l=l.inf-(l.inf-l.rel)*exp(-K*t) #Shows growth of organism over time where l.inf is adult size, l.rel is the size at release, K is growth rate, and t is time
}

##Mortliaty function
m=function(l) {
	m=mr*(lr/l). #This creates an asymptotic decline in mortality rate (m) as a function of length (l). mr and lr are referenced mortality rates and length
}

#Fishing function
f=function(l) {
	f=f.inf/(1+exp(q*(l-lc))) #This describes fishing mortality as a function of fish size. f.inf is the fishing mortality at adult size, q is the selectivity curve, and lc is the length at 50% gear selection (similar to Michaelis-Menten equation?)
}

#Number of released organism at time t: n(t)
##These aren't final functions because I'm not sure how R works with nested functions
n=function(t) {
	n.1*exp(-(m(l)+f(l))*delta.t) ##where n.1 is number of released organism at the prior time point, and delta.t is the change in time frame between n.1 and n
}

#Crab harvested (this is probably the most important one)
h=function(t) {
	h=n.1*f(l)/(m(l)+f(l))*(1-exp(-(m(l)+f(l))*delta.t))
}
##So what is this function actually saying?
	#It might help to first distribute just n.1 across the parantheses
		#What you're left with is n.1-n aka the number of crabs at the previous time point in the wild minus the number of crabs at the current time point
			##The difference is the number of crabs lost from the system during delta.t
			##f(l)/(m(l)+f(l)) gives you the proportion of crabs lost due to fishing compared to overall mortality
			#Multiply those 2 and you get number of crabs lost due to fishing which is harvest