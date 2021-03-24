###This script is to determine Mr when given field mortality data with a constant M
	##Pros of these methods is that it is actual field data! (with predation and stuff)
		##Cons These field mortalities are from India, Indonesia, Oman, Australia..
		
##Two approach:
	##Figure out the size class of crabs where these mortalities are measured and work from there
			## This will provide the Mr and Lr
	##Assume mortalities measured are the averge mortatlity across all size classes
		##Figure out proportion loss at that point
			##Treat it like a laboratory experiment and derive Mr from that
			##Still need an end date for this approach
			
##Let's work with the Abdul and Wardiantno study:
#M is 0.86, 1.09 (per year!) (for females and males respectively)
M=0.86
t=seq(from =0, to=2, by=0.1)
N=exp(-M*t)

plot(N~t)

exp(-M) ##42% surival after a year

##From the Abdul and Wardiatno paper, they have the size class of the crabs caught (so can just take the average of that and use that as Lr)
##So given mean(c1.09, 0.86) Mr and Lr of (mean c(89, 139.8))
mr=`mean(c(1.09, 0.86))/365` ##per day
lr=mean(c(89, 139.8)) ##mm

##head's up: mr*lr is just mr when lr is 1 (which is good for comparing across studies)
mr*lr

(0.0176*5)/(mr*lr)


###Some thoughts after messing around with it. So converting paper data to Lorenzen data (where it is size dpeendent) seems to work in that it is within an order of magnitude of the surival rate measured with juveniles measured in hatchery. The question is whether it is approriate to convert field mortality (that is asummed to be constant) to a value that is dependent on size?? (I think so because mortality would still look relatively constant if you are only seeing one size class as they did)
##Plus!!!!! Field mortality is actually higher than hatchery mortality (that's great because that's what we would expect!)

mr=1.05/365
lr=mean(c(23,173))