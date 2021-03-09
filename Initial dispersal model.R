##Let's create a dispersal function to see if it works
##Assume that most dispersal will ocurr in planktonic phase so whole process lasts around 15 days

##Equation
# f(ti)=1/sqrt(4*pi*D*(ti-t0))*exp(-d^2/(4*D*(ti-t0)))
##This equation (from Lorenzen) broadly says that if you give me the time since dispersal, I will give you the relative density of larvae at point d (where d is the distance between release site and point of interest)
###I'm going to tweak the equation a bit where ti is a constatn (PLD and I want to see how d changes with larval density)


dispersal=function(d)
{
	dens=1/sqrt(4*pi*D*(ti-t0))*exp(-d^2/(4*D*(ti-t0)))
}

ti=15
t0=0
D=795/365 #Diffusion constant should be in distance^2/time units (interesting about the squred term but makes sense if d is squared) ##Lorenzen 2010 says 795km^2/year (changing it to days)

distance=seq(-10,10, by=0.1)
dens=dispersal(distance)

plot(dens~distance)
###This is a very interesting question! What's the proportion of larvae at a certain distance??	
	##It's not jsut the value spit out by the function: that doesn't make sense if you try to sum it up across distances because it adds up more than 1
		##It's like a likelihood function where I have to take into account the width of the area I am trying to sum up (hmmmm)
			#And maybe for a given point, the number of larvae is 0 (until you look at an interval between points????)
		##The answer I get should not be influenced by how fine the resolution between distances are (maybe better estimates but not a larger nubmer per se)
##Think of it as many rectangles?
	##So sum up area
		#Area=width * height
			#Height is given by function
				#Width is given by number of intervals, which is a consant
					##So take my value for by and multiply by dens, and then sum it up??
sum(0.1*dens) ###Huh!! Number is less than 1 (doesn't say much; just says it hasn't failed yet)

##Now try 0.01 resolution
distance=seq(-10,10, by=0.01)
dens=dispersal(distance)
plot(dens~distance)
sum(0.01*dens) #####Ayyyyyy, similar estimate as above!! (Wooot!!)

##So what this tells me is that I can't estimate number of larvae at release site (aka, distance =0). I need some arbitrary range dfined as release sites

distance=seq(-1,1, by=0.01)
dens=dispersal(distance)
plot(dens~distance)
sum(0.01*dens) #####Given sea bass diffusion rate (doubtful, only 10% of larvae stay within 1km^2 area by the time they settle)

###Whoa!! Okay, we hit the tail end of the diffusion curve at 50km
distance=seq(-50,50, by=0.01)
dens=dispersal(distance)
plot(dens~distance) 
sum(0.01*dens) ####It sums up to 1 which is pretty cool (so 50km is defintiely reasonable)