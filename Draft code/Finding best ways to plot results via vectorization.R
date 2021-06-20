##Rough script to plot lines 10000 times
	##Right now forloop is taking too long and I'm wondering if vectorizing is the solution... (shrug)
	##What does the function do? Feed in a given run number and then plot line for said number

	s.graph=function(i){ ##input is a vector, what do I want to do with the vector?
		for(j in 1:length(i))
		{
		lines(s~t.1, col=col.alpha('blue', 0.1), data=final.juv.data[final.juv.data$run==i[j],])
		} ##something wonky with i if i is not just one character
	}

plot(s~t.1, ylab='Crab width (mm)', xlab='Days', cex.lab=1.5, cex.axis=1.5, data=final.juv.data, type='n')
text(1, 160, labels=c('a'), cex=2 )
s.graph(seq(1,1000))


for(i in 1:10){
	lines(s~t.1, col=col.alpha('red', 0.1), data=final.juv.data[final.juv.data$run==i,])
}

#Putting a function in a forloop doesn't make it faster (sadly)	
	#What about putting a forloop in a function? See newest iteration of s.graph, doesn't make it faster either...
for(i in 60:100){
	s.graph(i)
}


final.juv.data[final.juv.data$run==c(1,2),]

##Here's a good baseline. Plotting all the points is vastly much faster than a forloop suggesting it is not the putting points on a graph that is the bottle neck. It is the forloop
lines(s~t.1, ylab='Crab width (mm)', xlab='Days', col=col.alpha('green', 0.1), data=final.juv.data)


##Let's try apply...
# tapply(x, index, fun) #Normally, what this says is, I want you to perform a function (fun) using the values of x, subsetted by index
					##The easiest example is, I want you to take the mean (fun) using the values of x, subsetted by index...
					###Okay, index is run
					###What's fun and what's x??
					###Don't need to subset dataset in fun if already done by index...
						##So given the fact that you have a subsetted dataset, what's the function you want to do with x?
							##Begs the question, what is x??
								##Okay, x is the response variable...
								##So function is just to plot the response variable...
s.graph=function(s){
	lines(s~final.juv.data[final.juv.data$run==1,]$t.1, col=col.alpha('green', 0.1))
}


test.d=final.juv.data[final.juv.data$run==seq(1,2),]
with(final.juv.data, tapply(s, run, s.graph)) ##Holy shit, this works and actually could be faster!!

s.graph(final.juv.data[final.juv.data$run==3,]$s) 


###Let's actually do a race on this?
##Time with for loop for 2000 runs 1min 3 seconds
##Time with tapply for 2000 runs 48 seconds
##Tim with just plotting everything at once... (2 secconds??) ##There is room for efficiencies but I don't know how to make it better

###Want to try what Jason suggest and not have it 'index' aka search for a number of run, just brute force coding explicit sections (would be a forloop...)
head(final.juv.data)
final.juv.data[((i-1)*367+c(1:367)),]

for(i in 1:length(unique(final.juv.data$run))){
	lines(s~t.1, col=col.alpha('red', 0.1), data=final.juv.data[((i-1)*367+c(1:367)),])
} ##28s!! Hmm 2nd run was 58s