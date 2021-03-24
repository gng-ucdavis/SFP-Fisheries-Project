###This code is to graph the results from 'SFP working code for final model'
	##Basically, to plot all the bootstrapped results

###For larvae data
final.larval.data
par(mfrow=(c(2,2)))
plot(s.larvae~t.1.larvae, ylab='Crab length (mm)', xlab='Days',  cex.lab=1.5, cex.axis=1.5, final.larval.data)
text(1, 1.6, labels=c('a'), cex=2 )
plot(m.larvae~t.1.larvae, ylab='Mortality rate', xlab='Days', cex.lab=1.5, cex.axis=1.5,final.larval.data)
text(1, 0.22, labels=c('b'), cex=2 )
plot(n.larvae~t.1.larvae, ylab='Cohort proportion survival', xlab='Days', cex.lab=1.5, cex.axis=1.5,final.larval.data, type='n')
lines(n.larvae~t.1.larvae,data=final.larval.data)
text(1, 0.9, labels=c('c'), cex=2 )



##Let's do it for juvenile data
final.juv.data

par(mfrow=(c(2,2)))
plot(s~t.1, ylab='Crab width (mm)', xlab='Days', cex.lab=1.5, cex.axis=1.5, data=final.juv.data)
text(1, 160, labels=c('a'), cex=2 )
plot(m~t.1, ylab='Mortality rate', xlab='Days', cex.lab=1.5, cex.axis=1.5, data=final.juv.data)
text(1, 0.035, labels=c('b'), cex=2 )
plot(f~t.1, ylab='Fishing mortality', xlab='Days', cex.lab=1.5, cex.axis=1.5, data=final.juv.data)
text(1, 2.8, labels=c('c'), cex=2 )
plot(n~t.1, ylab='Cohort proportion survival', xlab='Days', cex.lab=1.5, cex.axis=1.5, data=final.juv.data)
text(1, 0.9, labels=c('d'), cex=2 )