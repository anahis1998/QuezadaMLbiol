#This is for analyzing climate data (temperature and precipitation) to see how it has changed. 
#We use weather station data provided in an R format. 
#
#This is solutions to the homework for the modularity unit for BIOL 701: Machine learning in 
#biology, KU, 2024. See the assignment pdf, where more details are given. This assignment focusses
#on modularity, so there is no machine learning, per se, in this one.
#
#Reuman

#MODULARITY EXAMPLE - I divided up my code with headers, which I wrote first, prior to filling them
#in, outlining the main tasks. These are delineated with "#***", to separate them from 
#ordinary comments. 

#***setup 

rm(list=ls())
graphics.off()
#MODULARITY EXAMPLE - I remove all other variables which may be int he workspace before starting.
#Though this was not discussed in class, this is an example of modularity since it ensures
#that this script is independent of others scripts which may have been run before hand. Sometimes
#one has several scripts that perform the analysis for a project. If the analyses in two scripts
#are different enough to warrant being separated into two scripts, then often or usually they
#will also be different enough to warrant saving outputs from the first script, and then re-loading
#whichever of those outputs may be needed by the second script, to have explicit control of 
#the inputs required by the first script. This is controlling the interface between the two
#scripts.

library(maps)

#***Load the data and check it out

dt<-readRDS(file="USAAnnualTemp1950_2008.rds")
dp<-readRDS(file="USAAnnualPcpn1950_2008.rds")

class(dt)
head(dt)
dim(dt)

class(dp)
head(dp)
dim(dp)

#MODULARITY EXAMPLE - Note the format of both data frames is the same, so we ought to be able to
#modularize - write functions and then apply them twice. That is what is done below in the
#next section.

#***clean the data, dropping locations which have too few data

source("getlonglocs.R")
#MODULARITY EXAMPLE - Note this function is in a separate file. That makes it easier to 
#test (though we are not testing it now), and also easier to bring into another project,
#if needed.

minlen<-40 
#MODULARITY EXAMPLE - give variable names to constants in the code, so it will be easy
#to change the values later if you need to

dt<-getlonglocs(dt,minpts=minlen)
dp<-getlonglocs(dp,minpts=minlen)

head(dt)
dim(dt)

head(dp)
dim(dp)
#So the data frames are a lot smaller now, but same format

#***do regressions to create data frames of locations and regressions slopes

source("getslopes.R")
#MODULARITY EXAMPLE - We again store the function in a separate file.

dt_slopes<-getslopes(dt)
dp_slopes<-getslopes(dp)
  
#***make some histograms and get basic stats

pdf(file="TempHist.pdf")
hist(dt_slopes$slope,50,xlab="Temp slope")
mn<-mean(dt_slopes$slope)
mn
points(mn,0,col="red",pch=20) #The mean is positive, but only slightly. Is it meaningful?
sd(dt_slopes$slope)/sqrt(dim(dt_slopes)[1]) #Standard error of the mean is much smaller than 
#the mean itself, so yes, meaningful. Spatial autocorrelation will degrees of freedom in a
#manner that may invalidate this results, but as a tentative, working result it is useful.
#And we are learning modularity right now, not spatial statistics, so we don't take this 
#any further.
dev.off()

pdf(file="PrecipHist.pdf")
hist(dp_slopes$slope,20,xlab="Precip slope")
mn<-mean(dp_slopes$slope)
mn
points(mn,0,col="red",pch=20) #The mean is positive, but only slightly. Is it meaningful?
sd(dp_slopes$slope)/sqrt(dim(dp_slopes)[1]) #Standard error of the mean is much smaller than 
#the mean itself, so yes, could be meaningful. Spatial autocorrelation will degrees of freedom in a
#manner that may invalidate this results, but as a tentative, working result it is useful.
#And we are learning modularity right now, not spatial statistics, so we don't take this 
#any further.
dev.off()

#MODULARITY LESSON - OK, technically there is repeated code above, so I could have written a
#function, here. However, the code is simple, so this is a judgement call, and in this case of
#limited benefit. Also, it may happen that we want to plot the data a bit differently for the
#different variables (e.g., different histogram bin sizes are already used, more differences
#may emerge at in making publication-quality plots), in which case a function would become less
#useful. I frequently find that plotting is very individual and often I don't write a function
#in cases like this when we are talking about plotting.

#***make maps

#MODULARITY LESSON - Again, could write a function, but won't because the code is pretty simple
#and it is reasonably likely that publication-quality plots would require different code for the
#two climate variables.

pdf(file="TempMap.pdf")
map(database="world",ylim=range(dt_slopes$lat),xlim=range(dt_slopes$lon),mar=c(.15,.15,.15,.15))   
Msl<-max(abs(dt_slopes$slope))
inp<-which(dt_slopes$slope>0)
inn<-which(dt_slopes$slope<=0)
points(dt_slopes$lon[inp],dt_slopes$lat[inp],type='p',pch=20,cex=0.5,
       col=rgb(1,0,0,dt_slopes$slope[inp]/Msl))
points(dt_slopes$lon[inn],dt_slopes$lat[inn],type='p',pch=20,cex=0.5,
       col=rgb(0,0,1,abs(dt_slopes$slope[inn])/Msl))
#So, many more locations getting warmer than cooler, especially in the west and Alaska
dev.off()

pdf(file="PrecipMap.pdf")
map(database="world",ylim=range(dp_slopes$lat),xlim=range(dp_slopes$lon),mar=c(.15,.15,.15,.15))   
Msl<-max(abs(dp_slopes$slope))
inp<-which(dp_slopes$slope>0)
inn<-which(dp_slopes$slope<=0)
points(dp_slopes$lon[inp],dp_slopes$lat[inp],type='p',pch=20,cex=0.5,
       col=rgb(1,0,0,dp_slopes$slope[inp]/Msl))
points(dp_slopes$lon[inn],dp_slopes$lat[inn],type='p',pch=20,cex=0.5,
       col=rgb(0,0,1,abs(dp_slopes$slope[inn])/Msl))
#Far fewer points across a much smaller geographic range, but still most locations show an 
#increase
dev.off()
