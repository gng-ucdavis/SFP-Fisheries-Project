###This code is to graph the results from 'SFP working code for final model'
	##Basically, to plot all the bootstrapped results
library(rethinking)

###For larvae data
final.larval.data=read.csv('~/Dropbox/Postdoc/Blue crab stock enhancement project/SFP/SFP Model Code/Results/larval.data.csv') #for lab based mortality
final.larval.data=read.csv('~/Dropbox/Postdoc/Blue crab stock enhancement project/SFP/SFP Model Code/Results/larval.field.data.csv') #for field based mortality
str(final.larval.data)
final.larval.data=final.larval.data[final.larval.data$run.larvae<=500,] ##Truncating dataset to just be 500 for plotting purposes

s.larvae.graph=function(s){
	lines(s~final.larval.data[final.larval.data$run.larvae==1,]$t.1.larvae, col=col.alpha('blue', 0.1))
}

m.larvae.graph=function(m){
	lines(m~final.larval.data[final.larval.data$run.larvae==1,]$t.1.larvae, col=col.alpha('red', 0.1))
}

n.larvae.graph=function(n){
	lines(n~final.larval.data[final.larval.data$run.larvae==1,]$t.1.larvae, col=col.alpha('purple', 0.1))
}



par(mfrow=(c(2,2)), mar=c(4,6,4,3), oma=c(4,2,4,4))
plot(s.larvae~t.1.larvae, ylab='Larval length (mm)', xlab='Days', cex.lab=2, cex.axis=2, data=final.larval.data, type='n')
text(1, 1.6, labels=c('a'), cex=2 )
with(final.larval.data, tapply(s.larvae, run.larvae, s.larvae.graph))

plot(m.larvae~t.1.larvae, ylab='Natural mortality', xlab='Days', cex.lab=2, cex.axis=2,final.larval.data, type='n')
with(final.larval.data, tapply(m.larvae, run.larvae, m.larvae.graph))
text(1, 0.25, labels=c('b'), cex=2 ) #0.82

plot(n.larvae~t.1.larvae, ylab='Cohort proportion survival', xlab='Days', cex.lab=2, cex.axis=2, final.larval.data, type='n')
text(1, 0.9, labels=c('c'), cex=2 )
with(final.larval.data, tapply(n.larvae, run.larvae, n.larvae.graph))



##Let's do it for juvenile data
final.juv.data=read.csv('~/Dropbox/Postdoc/Blue crab stock enhancement project/SFP/SFP Model Code/Results/juvenile.data.csv')
head(final.juv.data)
final.juv.data=final.juv.data[final.juv.data$run<=500 & final.juv.data$t.1!=0,]
##Functions needed for tapply
s.graph=function(s){
	lines(s~final.juv.data[final.juv.data$run==1,]$t.1, col=col.alpha('blue', 0.1))
}

m.graph=function(m){
	lines(m~final.juv.data[final.juv.data$run==1,]$t.1, col=col.alpha('red', 0.1))
}

f.graph=function(f){
	lines(f~final.juv.data[final.juv.data$run==1,]$t.1, col=col.alpha('green', 0.1))
}

n.graph=function(n){
	lines(n~final.juv.data[final.juv.data$run==1,]$t.1, col=col.alpha('purple', 0.1))
}

##Create skeleton of plots
par(mfrow=(c(2,2)), mar=c(4,6,4,3), oma=c(4,2,4,4))
plot(s~t.1, ylab='Crab width (mm)', xlab='Days', cex.lab=2, cex.axis=2, data=final.juv.data, type='n')
text(1, 160, labels=c('a'), cex=2 )
with(final.juv.data, tapply(s, run, s.graph))


plot(m~t.1, ylab='Natural mortality (day^-1)', xlab='Days', cex.lab=2, cex.axis=2, data=final.juv.data, type='n')
with(final.juv.data, tapply(m, run, m.graph))
text(1, 0.75, labels=c('b'), cex=2 )
	

plot(f~t.1, ylab='Fishing mortality (day^-1)', xlab='Days', cex.lab=2, cex.axis=2, data=final.juv.data, type='n')
with(final.juv.data, tapply(f, run, f.graph))
text(1,0.012, labels=c('c'), cex=2 )


plot(n~t.1, ylab='Cohort proportion survival', xlab='Days', cex.lab=2, cex.axis=2, data=final.juv.data, type='n')
with(final.juv.data, tapply(n, run, n.graph))
text(1, 0.9, labels=c('d'), cex=2 )



