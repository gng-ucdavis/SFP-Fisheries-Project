final.juv.data[final.juv.data$s<102,]$n[length(final.juv.data[final.juv.data$s<102,]$n)]


final.juv.data ##Entire data frame with 10000 simulations
final.juv.data[final.juv.data$s<3,] ####All the data frame where it is smaller than the requisite size
final.juv.data[final.juv.data$s<3,]$n ##Same thing as above but now we are just looking at vector n, rather than the entire data frame
length(final.juv.data[final.juv.data$s<102,]$n) ##This bit of code gives you the last number of the previous vector so....
final.juv.data[final.juv.data$s<102,]$n[length(final.juv.data[final.juv.data$s<102,]$n)] ##This gives you the last value in the vector of n after filtering size down to 102 and smaller...


###how to make this work for multiple simulations?
##Filter by time point?
head(final.juv.data[final.juv.data$s<102,]$n, 1)
tail(final.juv.data[final.juv.data$s<102,]$n, 1)

tailx=function(x){
	res=tail(x,1)
}

with(final.juv.data, tapply(n,run, tailx))

blah=seq(1,10)
blah
print(tailx(blah))


blah*blah

bloh=seq(1:3)
blah*bloh
bloh*blah 
