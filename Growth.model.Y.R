##### Jeannine Cavender-Bares, University of Minnesota, cavender@umn.edu
##### Nov. 9, 2012 - Jan. 17, 2015
##### An ecological model comparing the performance of dormancy states in perennial plants


rm(list=ls())
# quartz(width=11, height =7 )
# op <- par(mfcol = c(2, 3))
# par(mar=c(5, 5, 4, 4))

seed.mass     <-     c(0.01,0.1, 0.2, 0.35, 0.45, 0.6, 0.8, 1.3, 1.5, 3,5) # seed mass values
L             <-     seq(80, 365, 5) #growing season lengths from 80 to 365 days

w             =      0.5   # seedling mortality #w = 0.9
Lmax          =      365   # length of year
tau           =      100   # days to germination ; change from 0 to 100
p             =      0.5   # probability of germination or germination fraction
Y             =      100   # repoductive output; total mass of seeds of population cohort
r             =      0.001 # growth rate
s             =      1     # seed mortality constant


i             <-     1
SM            <-     seed.mass


##############################################
#Function to calculate growth over two seasons
##############################################

y2<-function(L, params, SM) 
{
  n           <-     params$n      #number of seeds
  D           <-     params$D      #Dormancy D=1 ; (D=0 for nondormancy)
	s           <-     params$s      #seed mortality constant
	G           <-     params$G      #Growth under non-dormancy model; G=0 for dormancy; G=1 for non-dormancy 
	Y           <-     params$Y      #cohort yield
	p           <-     params$p      #probability of seed germination
	r           <-     params$r      #intrinsic growth rate of seedlings
	Lmax        <-     params$Lmax   #maximum length of growing season - 365 days
	w           <-     params$w      #mortality parameter for seedlings
	tau         <-     params$tau    #time it takes from start of growing season for seeds to mature

	n           <-     Y/SM
	n           <-     as.numeric(n)
	
	Gf          <-     G*Y*p*exp(r*(L-tau))      # for non-dormancy G=1
	Mg          <-     Gf/(n*p)                  # Mass of plant following germination
	LL          <-     (Lmax-L)/Lmax             # fraction of the year that is favorable
	M           <-     1-(w-(1/(1+exp(Mg))))	   # M<-(1/Mg)^w
	Yfu         <-     Gf*(1-(M*(LL)))           # growth during favorable and unfavorable season; this is only calculated for non-dormancy, otherwise 0
	R           <-     s-(1/(s+exp(log(SM)))) # Risk of seed predation - daily mortality rate of seeds 
	S           <-     D*Y*(1-(R*LL))            # Cohort biomass at the beginning of the second growing season; this is calculated for dormancy model
	y2          <-     (Yfu+S)*D*p*exp(r*L)+((Yfu*G)*exp(r*L)) #Cohort biomass at the end of the second year of growth; with dormancy, Yfu + S is just S, since Yfu goes to 0
	
	return(y2)
}

#####################################
####Calculate growth for NON-DORMANCY
#####################################

params        <-     list("s"=s, "Lmax"=Lmax, "tau"=tau, "p"=p, "Y"=Y,"r"=r, "D"=0, "G"=1,  "n"=n, "w"=w)

Y2            <-     y2(L, params,SM[10]) #non-dormancy

Y2            #print growth values for different lengths of the growing season


#####################################
####Calculate growth for DORMANCY
#####################################

# D              =      1 #Dormancy (0 for non-dormancy)
# G              =      0 #Dormancy (1 for non-dormancy)

params1        <-     list("w"=w, "s"=s, "Lmax"=Lmax, "tau"=tau, "p"=p, "Y"=Y,"r"=r, "D"=1, "G"=0, "n"=n )

Y2D            <-     y2(L, params1,SM[1]) #non-dormancy

Y2D                   #print growth values for different lengths of the growing season



#####################################
dev.new(width=5, height=5)
par(mar=c(5, 5, 4, 4))


plot(L, y2(L, params1,SM[i+6]), xlab="Favorable season (days)", ylab="Cohort Growth", cex=.1, cex.lab = 1.5, ylim=c(0,100))
lines(L, y2(L, params,SM[i+6]), type = "l", lty="solid", lwd =3, col="darkred")
lines(L, y2(L, params1,SM[i+6]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[10] )
box(which = "plot", lty = "solid", col="black", lwd=3)


plot(L, y2(L, params1,SM[i]), xlab="Favorable season (days)", ylab="Cohort Growth", cex=.1, cex.lab = 1.5, ylim=c(0,100))
#lines(L, y2(L, params,SM[i]), type = "l", lty="solid", lwd =3, col="darkred")
lines(L, y2(L, params1,SM[i]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[3] )
lines(L, y2(L, params1,SM[i+3]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[6] )
lines(L, y2(L, params1,SM[i+6]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[10] )
box(which = "plot", lty = "solid", col="black", lwd=3)

#Figure.1B
#dev.new(width=5, height=5)
#par(mar=c(5, 5, 4, 4))
plot(L, y2(L, params,SM[1]), xlab="Favorable season (days)", ylab="Cohort Growth", cex=.1, cex.lab = 1.5)
lines(L, y2(L, params,SM[1]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[2] )
lines(L, y2(L, params,SM[i+3]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[6] )
lines(L, y2(L, params,SM[7]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[10] )
box(which = "plot", lty = "solid", col="black", lwd=3)

#Figure.1C
#dev.new(width=5, height=5)
#par(mar=c(5, 5, 4, 4))
plot(L, y2(L, params,SM[i]), xlab="Favorable season (days)", ylab="Cohort Growth", main="Growth variation with seed size", cex=.1, cex.lab = 1.5, ylim=c(0,100))
lines(L, y2(L, params1,SM[i]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[2] )
lines(L, y2(L, params1,SM[5]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[6] )
lines(L, y2(L, params1,SM[i+8]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[11] )
lines(L, y2(L, params,SM[1]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[2] )
lines(L, y2(L, params,SM[5]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[6] )
lines(L, y2(L, params,SM[9]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[11] )
box(which = "plot", lty = "solid", col="black", lwd=3)


#Dormancy
dev.new(width=5, height=5)
par(mar=c(5, 5, 4, 4))
plot(L, y2(L, params1,SM[i]), xlab="Favorable season (days)", ylab="Cohort Growth", cex=.1, cex.lab = 1.5, ylim=c(20,100), main="Dormancy - growth variation with seed size")
lines(L, y2(L, params1,SM[i]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[1] )
lines(L, y2(L, params1,SM[i+1]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[2] )
lines(L, y2(L, params1,SM[i+2]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[3] )
lines(L, y2(L, params1,SM[i+3]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[4] )
lines(L, y2(L, params1,SM[i+4]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[5] )
lines(L, y2(L, params1,SM[i+5]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[6] )
lines(L, y2(L, params1,SM[i+6]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[7] )
lines(L, y2(L, params1,SM[i+7]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[8] )
lines(L, y2(L, params1,SM[i+8]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[9] )
lines(L, y2(L, params1,SM[i+9]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[10] )
lines(L, y2(L, params1,SM[i+10]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[11] )
#lines(L, y2(L, params,SM[i]), type = "l", lty="solid", lwd =3, col="darkred")
box(which = "plot", lty = "solid", col="black", lwd=3)

#Figure: Appendix - Non-dormancy
dev.new(width=5, height=5)
par(mar=c(5, 5, 4, 4))
plot(L, y2(L, params,SM[i]), xlab="Favorable season (days)", ylab="Cohort Growth", cex=.1, cex.lab = 1.5, ylim=c(20,100), main="Growth variation with seed size - Non-dormancy")
lines(L, y2(L, params,SM[i]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "darkred"))( 11 )[2] )
lines(L, y2(L, params,SM[i+1]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[3] )
lines(L, y2(L, params,SM[i+2]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[4] )
lines(L, y2(L, params,SM[i+3]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[5] )
lines(L, y2(L, params,SM[i+4]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[6] )
lines(L, y2(L, params,SM[i+5]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[7] )
lines(L, y2(L, params,SM[i+6]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[8] )
lines(L, y2(L, params,SM[i+7]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[9] )
lines(L, y2(L, params,SM[i+8]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[10] )
lines(L, y2(L, params,SM[i+9]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[11] )
lines(L, y2(L, params,SM[i+10]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "red"))( 11 )[11] )

box(which = "plot", lty = "solid", col="black", lwd=3)


#Figure: 1B
dev.new(width=5, height=5)
par(mar=c(5, 5, 4, 4))
plot(L, Y2, xlab="Favorable season (days)", ylab="Cohort Growth", cex=.1, cex.lab = 1.5)
lines(L, y2(L, params1,SM[i]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("darkblue", "blue"))( 11 )[1] )
# lines(L, y2(L, params1,SM[i+1]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("darkblue", "blue"))( 11 )[2] )
# lines(L, y2(L, params1,SM[i+2]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("darkblue", "blue"))( 11 )[3] )
lines(L, y2(L, params1,SM[i+3]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("darkblue", "blue"))( 11 )[4] )
# lines(L, y2(L, params1,SM[i+4]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("darkblue", "blue"))( 11 )[5] )
# lines(L, y2(L, params1,SM[i+5]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("darkblue", "blue"))( 11 )[6] )
lines(L, y2(L, params1,SM[i+6]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("darkblue", "blue"))( 11 )[7] )
# lines(L, y2(L, params1,SM[i+7]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("darkblue", "blue"))( 11 )[8] )
lines(L, y2(L, params1,SM[i+8]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("darkblue", "blue"))( 11 )[9] )
# lines(L, y2(L, params1,SM[i+9]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("darkblue", "blue"))( 11 )[10] )
lines(L, y2(L, params1,SM[i+10]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("darkblue", "blue"))( 11 )[11] )
#1,4,7,9,11
lines(L, y2(L, params,SM[i]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "darkred"))( 11 )[1] )
# lines(L, y2(L, params,SM[i+1]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "darkred"))( 11 )[2] )
# lines(L, y2(L, params,SM[i+2]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "red"))( 11 )[3] )
lines(L, y2(L, params,SM[i+3]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "darkred"))( 11 )[4] )
# lines(L, y2(L, params,SM[i+4]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "darkred"))( 11 )[5] )
# lines(L, y2(L, params,SM[i+5]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "darkred"))( 11 )[6] )
lines(L, y2(L, params,SM[i+6]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "darkred"))( 11 )[7] )
# lines(L, y2(L, params,SM[i+7]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "darkred"))( 11 )[8] )
lines(L, y2(L, params,SM[i+8]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "darkred"))( 11 )[9] )
# lines(L, y2(L, params,SM[i+9]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "darkred"))( 11 )[10] )
lines(L, y2(L, params,SM[i+10]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "darkred"))( 11 )[11] )
#1,4,7,9,11
box(which = "plot", lty = "solid", col="black", lwd=3)


# Function to plot color bar
color.bar <- function(lut, min=0.01, max=10, nticks=11, ticks=seq(min, max, len=nticks), title='') {
    scale = (length(lut)-1)/(max-min)

    dev.new(width=1.75, height=5)
    plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
    axis(2, ticks, las=1)
    for (i in 1:(length(lut)-1)) {
     y = (i-1)/scale + min
     rect(0,y,10,y+1/scale, col=lut[i], border=NA)
    }
}
color.bar(colorRampPalette(c("yellow", "blue"))( 12 ), -1)
color.bar(colorRampPalette(c("yellow", "red"))( 12 ), -1)

#Plot mortality vs plant mass
Mg<-seq(0, 10, .01)
M<-function(w, Mg) #L is lenght of the favorable season
{
	M<-1-(w-(1/(1+exp(Mg))))
	return(M)
}

op <- par(mfcol = c(2, 3))
par(mar=c(5, 5, 4, 4))

dev.new(width=5, height=5)
par(mar=c(5, 5, 4, 4))
plot(Mg, M(0.1,Mg), xlab="Plant size (g)", ylab="Mortality rate (g g-1 day-1)", cex=.1, cex.lab = 1.5, xlim=c(0,7),ylim=c(0,2))
lines(Mg, M(0.1,Mg), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "blue"))( 11 )[1] )
lines(Mg, M(0.2,Mg), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "blue"))( 11 )[2] )
lines(Mg, M(0.3,Mg), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "blue"))( 11 )[3] )
lines(Mg, M(0.4,Mg), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "blue"))( 11 )[4] )
lines(Mg, M(0.5,Mg), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "blue"))( 11 )[5] )
lines(Mg, M(0.6,Mg), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "blue"))( 11 )[6] )
lines(Mg, M(0.7,Mg), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "blue"))( 11 )[7] )
lines(Mg, M(0.8,Mg), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "blue"))( 11 )[8] )
lines(Mg, M(0.9,Mg), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "blue"))( 11 )[9] )
lines(Mg, M(1,Mg), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("red", "blue"))( 11 )[10] )
box(which = "plot", lty = "solid", col="black", lwd=3)
color.bar(colorRampPalette(c("red", "blue"))(10), 1)


#Plant mass as a function of L for different seed sizes


params <- list("s"=s, "Lmax"=Lmax, "tau"=tau, "p"=p,"g"=g, "b"=b, "Y"=Y,"r"=r, "D"=0, "G"=1, "w"=w)


Mg<-function(L, params, SM) #L is length of the favorable season
{
	
	n         <-    Y/SM
	D         <-    params$D
	s         <-    params$s
	G         <-    params$G
	Y         <-    params$Y
	p         <-    params$p
	r         <-    params$r
	Lmax      <-    params$Lmax
	w         <-    params$w
	tau       <-    params$tau
	Gf        <-    G*Y*p*exp(r*(L-tau)) # for non-dormancy G=1
	Mg        <-    Gf/(n*p) #plant mass
	
	return(Mg)
}

plot(L, Mg(L,params,0.01), xlab="Length of favorable season", ylab="Plant size (g)", cex=.1, cex.lab = 1.5, ylim=c(0,5), main="r=0.005")

lines(L, Mg(L,params,0.01), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[1] )
lines(L, Mg(L,params,0.1), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[2] )
lines(L, Mg(L,params,0.2), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[3] )
lines(L, Mg(L,params,0.3), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[4] )
lines(L, Mg(L,params,0.45), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[5] )
lines(L, Mg(L,params,0.6), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[6] )
lines(L, Mg(L,params,0.8), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[7] )
lines(L, Mg(L,params,1), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[8] )
lines(L, Mg(L,params,1.5), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[9] )
lines(L, Mg(L,params,3), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[10] )

box(which = "plot", lty = "solid", col="black", lwd=3)

#Risk of seed mortality as a function of seed size

params <- list("Lmax"=Lmax, "tau"=tau, "p"=p, "Y"=Y,"r"=r,  "w"=w)

SM<-seq(0,5,0.01)
R<-function(L, params, SM) #L is length of the favorable season
{

	D<-1
	s<-1
	G<-0
	Y<-params$Y
	Lmax<-params$Lmax
	R<- s-(1/(s+exp(log(SM))))
	LL<-(Lmax-L)/Lmax #fraction of the year that is favorable
	S<-D*Y*(1-(R*LL)) #this is calculated for dormancy
	return(R)
}
dev.new(width=5, height=5)
par(mar=c(5, 5, 4, 4))
plot(SM, R(L,params,SM), xlab="Seed size (g)", ylab="Risk", cex=.1, cex.lab = 1.5, ylim=c(0,1))
lines(SM, R(L,params,SM), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[11] )
box(which = "plot", lty = "solid", col="black", lwd=3)

params <- list("Lmax"=Lmax, "tau"=tau, "p"=p, "Y"=Y,"r"=r,  "m"=m)

#SM<-seq(0,5,0.01)

seed.mass<-c(0.01,0.1, 0.2, 0.3, 0.45, 0.6, 0.8, 1, 1.5, 3)
SM<-seed.mass
survival<-function(L, params, SM) #L is length of the favorable season
{

	D<-1
	s<-1
	Y<-100
	Lmax<-params$Lmax
	R<- s-(1/(s+exp(log(SM))))
	LL<-(Lmax-L)/Lmax #fraction of the year that is favorable
	S<-Y*(1-(R*LL)) #this is calculated for dormancy
	return(S)
}
dev.new(width=5, height=5)
par(mar=c(5, 5, 4, 4))
plot(L, survival(L,params,SM[1]), xlab="Length of the favorable season (days)", ylab="Seed survival", cex=.1, cex.lab = 1.5)
lines(L, survival(L,params,SM[1]), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[11] )
box(which = "plot", lty = "solid", col="black", lwd=3)

#Persistence of seeds in the soil as a function of length of the favorable season
i=1
SM<-seed.mass
w=0.9
Lmax= 365 # length of year
tau=100 #days to germination ; change from 0 to 100
p= 0.5 #probability of germination or germination fraction
Y=100 #repoductive output
r=0.001 #growth rate
s=1
n<-Y/SM[i]
n=as.numeric(n)

D=1 #Dormancy (1 for dormancy)
G=abs(1-D) #Dormancy (1 for non-dormancy)
params1 <- list("m"=m, "s"=s, "Lmax"=Lmax, "tp"=tp, "p"=p, "Y"=Y,"r"=r, "D"=D, "G"=G, "n"=n )

S<-function(L, params, SM) #L is length of the favorable season
{
	D<-params$D
	s<-params$s
	G<-params$G
	Y<-params$Y
	p<-params$p
	r<-params$r
	Lmax<-params$Lmax
	m<-params$m
	n<-params$n
	LL<-(Lmax-L)/Lmax #fraction of the year that is favorable
	R<- s-(1/(s+exp(log(SM))))
	S<-D*Y*(1-(R*LL)) #this is calculated for dormancy
	return(S)
}

survival<- S(L, params1,SM[i]) #non-dormancy
dev.new(width=5, height=5)
par(mar=c(5, 5, 4, 4))
plot(L, S(L,params1,0.01), xlab="Length of favorable season", ylab="Seed persistence in the soil (g)", cex=.1, cex.lab = 1.5, ylim=c(0,100))
lines(L, S(L,params1,0.01), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[1] )
lines(L, S(L,params1,0.1), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[2] )
lines(L, S(L,params1,0.2), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[3] )
lines(L, S(L,params1,0.3), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[4] )
lines(L, S(L,params1,0.45), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[5] )
lines(L, S(L,params1,0.6), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[6] )
lines(L, S(L,params1,0.8), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[7] )
lines(L, S(L,params1,1), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[8] )
lines(L, S(L,params1,1.2), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[9] )
lines(L, S(L,params1,1.5), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[9] )
lines(L, S(L,params1,2), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[9] )
lines(L, S(L,params1,3), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[10] )
lines(L, S(L,params1,5), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[11] )
lines(L, S(L,params1,10), type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[11] )
box(which = "plot", lty = "solid", col="black", lwd=3)

dev.new(width=5, height=5)
par(mar=c(5, 5, 4, 4))
plot(L, S(L,params1,0.01)/Y, xlab="Length of favorable season", ylab="Seed survival fraction", cex=.1, cex.lab = 1.5, ylim=c(0,1))
lines(L, S(L,params1,0.01)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[1] )
lines(L, S(L,params1,0.1)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[2] )
lines(L, S(L,params1,0.2)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[3] )
lines(L, S(L,params1,0.3)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[4] )
lines(L, S(L,params1,0.45)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[5] )
lines(L, S(L,params1,0.6)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[6] )
lines(L, S(L,params1,0.8)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[7] )
lines(L, S(L,params1,1)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[8] )
lines(L, S(L,params1,1.2)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[9] )
lines(L, S(L,params1,1.5)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[9] )
lines(L, S(L,params1,2)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[9] )
lines(L, S(L,params1,3)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[10] )
lines(L, S(L,params1,5)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[11] )
lines(L, S(L,params1,10)/Y, type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[11] )
box(which = "plot", lty = "solid", col="black", lwd=3)


#The optim function allows evaluation of the L values for a given SM value at the point which ND and D are equivalent (or evaluation of the SM values for a given L.)
#spits out optimized value SM, given L

wrapper <- function(L, SM) {
	ND<-L*SM
	D<-L-SM
	E<-ND-D
	return(abs(E))
}

results<-optim(rnorm(1), wrapper, SM=2)

results[[2]]


#######
###### Use numerical methods to find the equilibrium point between the two equations; the optim function is applied to minimize the difference between the dormancy and the non-dormancy equations


seed.mass<-seq(0.01,5, 0.01)
#L<-seq(80, 365, 1)

#m<-c(0.5,0.6,0.7,0.8,0.9)
SM<-seed.mass
w = 0.9
#m=0.5
Lmax= 365 # length of year
tau=80 #days to germination ; change from 0 to 100
p= 0.5 #probability of germination or germination fraction
Y=100 #repoductive output
r=0.001 #growth rate
s=1


evaluate.L<-function(L, SM, w) #L is length of the favorable season
{
	n<-Y/SM #number of seeds is the cohort seed yield/seed mass
	Gf<- Y*p*exp(r*(L-tau)) # for non-dormancy G=1
	Mg<-Gf/(n*p) #plant size
	LL<-(Lmax-L)/Lmax #fraction of the year that is favorable
	
	M<-1-(w-(1/(1+exp(Mg)))) #mortality of seedlings in in the unfavorable season for the ND case	
	
	Yfu<-Gf*(1-(M*(LL))) #growth during favorable and unfavorable season; this is only calculated for non-dormancy, otherwise 0

	R<- s-(1/(s+exp(log(SM)))) #the risk of predation as a function of seed size
	S<-Y*(1-(R*LL)) #this is calculated for dormancy
	
	D<- S*p*exp(r*L)#with dormancy Yfu + S is just S, since Yfu goes to 0
	ND<- Yfu*exp(r*L) #with dormancy Yfu + S is just S, since Yfu goes to 0
	
	E <- D-ND # take the difference between the two equations for a given SM
	return(abs(E))
}

#Function to numerical find L values where growth of ND and D are equivalent
L        <-      c()
SM.L     <-      c()
	for (i in 1:500){
		results   <-optim(rnorm(1), evaluate.L, SM=SM[i], w=0.9)
		L[i]      <-results[[1]]
	}
	
	L      <-      data.frame(L)
	SM     <-      data.frame(SM)
	SM.L   <-      cbind(SM,L)
#	SM.L<-merge(SM.L,SM.L,by.x=SM, by.y=SM)
#}
lines(SM.L$SM, SM.L$L,type = "l", lty="solid", lwd =3, col=colorRampPalette(c("yellow", "blue"))( 11 )[11] )


##SM<-data.frame(SM)

#SM.L<-cbind(SM,L)

#plot(SM.L$SM, SM.L$L, ylim=c(30,365), xlim=c(0,2))

dev.new(width=5, height=5)
par(mar=c(5, 5, 4, 4))
plot(SM.L$SM, SM.L$L, xlab="Seed mass (g)", ylab="Length of favorable season (d)", cex=.1, cex.lab = 1.5, ylim=c(30,365), xlim=c(0,1.5))
lines(SM.L$SM, SM.L$L,type = "l", lty="solid", lwd =4, col=colorRampPalette(c("yellow", "blue"))( 11 )[11] )
box(which = "plot", lty = "solid", col="black", lwd=3)
