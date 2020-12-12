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
	n=n.1*exp(-(m(l)+f(l))*delta.t) ##where n.1 is number of released organism at the prior time point, and delta.t is the change in time frame between n.1 and n
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




########okay actual code starts here

##Growth model
l.inf=1
l.rel=0.5
K=1

##Fudge! length is a function already :(
size=function(t){
	l<<-l.inf-(l.inf-l.rel)*exp(-K*t) #Shows growth of organism over time where l.inf is adult size, l.rel is the size at release, K is growth rate, and t is time
	print(l)
}

#Mortality function
mr=1
lr=1

mortality=function(l) {
	m<<-mr*(lr/l) #This creates an asymptotic decline in mortality rate (m) as a function of length (l). mr and lr are referenced mortality rates and length
	print(m)
}

##Fishing function
f.inf=1
q=1
lc=1

fishing=function(l) {
	f<<-f.inf/(1+exp(q*(l-lc))) #This describes fishing mortality as a function of fish size. f.inf is the fishing mortality at adult size, q is the selectivity curve, and lc is the length at 50% gear selection (similar to Michaelis-Menten equation?)
	print(f)
}


#Number of released organism in the wild at time t: n(t)
n.1=1 #Technically not a parameter
delta.t=1

##Trying nesting functions ###I actually like this better
numbers=function(t) {
	n<<-n.1*exp(-(mortality(size(t))+fishing(size(t)))*delta.t) ##where n.1 is number of released organism at the prior time point, and delta.t is the change in time frame between n.1 and n
	print(n)
}

numbers(1)

##Number of crabs harvested within a particular time period from tn to tn-1
harvest=function(t) {
	h<<-n.1*fishing(size(t))/(mortality(size(l))+fishing(size(t)))*(1-exp(-(mortality(size(t))+fishing(size(t)))*delta.t))
	print(c('size',l))
	print(c('mortality', m))
	print(c('fishing', f))
	print(c('numbers', n))
	print(c('harvest', h))
}

###Continue from here by running through a vector of t's