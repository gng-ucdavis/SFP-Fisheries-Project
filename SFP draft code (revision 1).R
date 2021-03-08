##Starting from scratch
####Summary after working through this script
	##It works! I will have to double check the graphs and math but I think the code and math are correct here
		##It doesn't have all the annotations from the previous code (to keep it cleanish) but it also has a fair bit of junk to it
			##Going to keep it that way. look at old code if you want explanation on functions
			##Next step is to find relevant parameters
				####And appropriate time steps! (which means units on your parameters!!)
			##Also also, think about improving it for crab/biphasic life cycle
				##Merge with IBMs?

##Growth model
l.inf=1
l.rel=0.5
K=1

size=function(t){
	l=l.inf-(l.inf-l.rel)*exp(-K*t) #Shows growth of organism over time where l.inf is adult size, l.rel is the size at release, K is growth rate, and t is time
}

#Mortality function
mr=0.01
lr=1

mortality=function(l) {
	m=mr*(lr/l) #This creates an asymptotic decline in mortality rate (m) as a function of length (l). mr and lr are referenced mortality rates and length
}

##Fishing function
f.inf=0.01
q=1
lc=1

fishing=function(l) {
	f=f.inf/(1+exp(q*(l-lc))) #This describes fishing mortality as a function of fish size. f.inf is the fishing mortality at adult size, q is the selectivity curve, and lc is the length at 50% gear selection (similar to Michaelis-Menten equation?)
}

##Basis for number functions but don't actually run it because need to do this in a forloop
# numbers=function(t) {
	# n[i+1]<<-n.1*exp(-(mortality(size(t))+fishing(size(t)))*delta.t) ##where n.1 is number of released organism at the prior time point, and delta.t is the change in time frame between n.1 and n
# }

# numbers=function(t, i) {
	# num=n[i]*exp(-(mortality(size(t[i]))+fishing(size(t[i])))*delta.t) ###And technically, i is a variable in the function (Yep)
# } ###Dumbass, don't need t[i] in function when you already specify that t=t[i] in forloop!!

numbers=function(t, i) {
	num=n[i]*exp(-(mortality(size(t))+fishing(size(t)))*delta.t) ###And technically, i is a variable in the function (Yep)
}


t=seq(from=0, to=40, by=0.1) ##Need to figure out if starts at 1 or 0 for time. ##Might actually want 0 because that will give reference size for crabs (and relevant parameters following that)
n=rep(0, length(t))
h=rep(0, length(t))
m=rep(0, length(t))
s=rep(0, length(t))
f=rep(0, length(t))


n[1]=1000
delta.t=1


##Clean up the above code and make into forloop ####Yes, This works
for(i in 1:length(t))
{
	n[i+1]=numbers(t=t[i], i=i)
	s[i+1]=size(t[i])
	m[i+1]=mortality(size(t[i]))
	f[i+1]=fishing(size(t[i]))
}

length(t)
t.1=c(t, max(t)+1)
par(mfrow=(c(2,2)))
plot(n~t.1)
plot(m~t.1)
plot(s~t.1)
plot(f~t.1)