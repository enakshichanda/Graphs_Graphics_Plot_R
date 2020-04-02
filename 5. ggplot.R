################################## GRAPHICS ###########################
GGPLOT

EWhelan 

install.packages("ggplot2")

require(ggplot2)

d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/pgr.txt",header=TRUE)
# response of a grass species (FR) to soil pH and total hay yield (biomass of all plant species)
head(d)
str(d)
plot(d$hay,d$pH)
text(d$hay,d$pH,labels=round(d$FR,2),pos=1,offset=0.5,cex=0.7)
qplot(d$hay,d$pH)
qplot(d$hay,d$pH,label=round(d$FR,2),geom=c("point","text"),size=1,shape=1,vjust=1)+opts(legend.position = "none")
# pos = position 1=below, 2=left, 3=above, 4=right of point
# offset = distance from point
plot(d$hay,d$pH,col=ifelse(d$FR>median(d$FR),"red","black"))
d$median=factor(ifelse(d$FR>median(d$FR),"> median","< median"))
qplot(hay,pH,data=d,color=median)

# adding text to scatterplots
p=read.csv("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/map.places.csv",header=TRUE)
# names of places we want to plot
head(p)
str(p)
p$wanted=as.character(p$wanted)
d=read.csv("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/bowens.csv",header=TRUE)
# a list of locations, including places we do not want to plot
d$nn=ifelse(d$north<60,d$north+100,d$north) # need to recode northings
head(d)
str(d)
d$place=as.character(d$place)
range(d$nn)
range(d$east)
plot(c(20,100),c(60,110),type="n",xlab="Easting",ylab="Northing")
# plot place names
for (i in 1:length(p$wanted)) {
  j=which(d$place==p$wanted[i])
  text(d$east[j],d$nn[j],d$place[j],cex=0.6)
  }
# now do it with qplot
str(p); str(d)
m=merge(d,p,by.x="place",by.y="wanted"); head(m)
str(m)
qplot(east,north,data=m,geom="text",label=place,size=1) +coord_equal() +labs(legend.position = "none")
m=m[m$north>40,] 
qplot(east,north,data=m,geom="text",label=place,size=1) +coord_equal() +labs(legend.position = "none")
  
# plotting mathematical functions, p. 144
curve(x^3-3*x,-2,2)
# another way to plot it
x=seq(-2,2,0.01); head(x)
y=x^3-3*x; head(y)
plot(x,y,type="l") # type is lower case L for lines
qplot(x,y,geom="line")

# adding shapeS
plot(0:10,0:10,xlab="",ylab="",type="n",xaxt="n",yaxt="n")
# xaxt="n",yaxt="n" suppresses plotting of axes, see ?par for plotting parameters
rect(6,6,9,9) # rectangle (xleft,ybottom,xright,ytop) these define the corners of the retangle
# use locator() to place rectangle
drawRect=function(col=NA) {
  co=c(unlist(locator(1)),unlist(locator(1)))
  rect(co[1],co[2],co[3],co[4],col=col)
}    # have to use 'unlist' when we want to specify location with 'locator'
# 'co' are our corner positions
drawRect()
drawRect(col="blue")

drawArrow=function(head=2,angle=45, len=0.25) {
  co=c(unlist(locator(1)),unlist(locator(1)))
  arrows(co[1],co[2],co[3],co[4],code=head,angle=angle,length=len)
}
# angle of arrow head, code 1=head at first point, 2=head at second point, 3=head at both ends
# len is length of head
Arrows(x0, y0, x1, y1, code = 2, arr.length = 0.4, 
       arr.width = arr.length/2, arr.adj = 0.5, arr.type = "curved",
       segment = TRUE, col = "black", lcol = col, lty = 1, arr.col = lcol, 
       lwd = 1, arr.lwd = lwd, ...)

arrows(x0 = 3, y0 = 4, x1 = 3, y1 = 2)
 
require(plot.new)   

drawPoly=function(n=3,col=NA) {
  loc=locator(n)
  polygon(loc,col=col)
}
drawPoly()
drawPoly(n=5,col="lavender")
colors() # available colors

drawLabel=function(lab="x") text(locator(1),labels=lab)
drawLabel("text")

# fill in normal curve
x=seq(-3,3,0.01)
y=dnorm(x)
plot(x,y,type="l") # type l (lc L) is line
polygon(c(x[x<=-1],-1), c(y[x<=-1],y[x==-3]),col="red")
# point (-1,y[x==-3]) draws polygon down to axis
p=qplot(x,y,geom="line"); p  # using qplot
p + geom_area(aes(x[x<=-1],y[x<=-1]),fill="red")

# fitting non-linear parametric curves through a scatterplot
rm(list= ls() ) # remove unused objects
d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/plotfit.txt",header=TRUE)
# y=recruits to a fishery, x=size of fish stock
head(d)
str(d)
plot(d$x,d$y,xlab="stock",ylab="recruits",pch=16)
m=nls(y~a*x*exp(-b*x),start=list(a=500,b=0.05),data=d); m # fit a model
ix=order(d$x)
lines(d$x[ix],predict(m,list(x=d$x[ix])),lty=2) # lty = line type
t=480*d$x*exp(-0.047*d$x) # theoretical model
lines(d$x[ix],t[ix],lty=1)
legend(locator(1),c("theoretical","predicted"),lty=1:2)
# now do it with ggplot2
d=d[ix,]; head(d) # sorted
d1=data.frame(x=d$x,y=predict(m,list(x=d$x)))
d1$line="predicted"
d2=data.frame(x=d$x,y=480*d$x*exp(-0.047*d$x))
d2$line="theoretical"
d3=rbind(d1,d2); some(d3)
p=qplot(x,y,data=d,xlab="stock",ylab="recruits",pch=16); p
p+geom_line(aes(x,y,color=line),data=d3)

# fitting non-parametric curves through a scatterplot, p. 151
d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/jaws.txt",header=TRUE)
# jaw bone length(bone) as a function of deer age
head(d)
str(d)
par(mfrow=c(2,2)) # multiple function plots with 2 rows and 2 columns
plot(d$age,d$bone,pch=16)
text(45,20,"lowess",pos=2) # locally-weighted polynomial regression
lines(lowess(d$age,d$bone))
plot(d$age,d$bone,pch=16)
text(45,20,"loess",pos=2) # local fitted polynomial
m=loess(bone~age,method="loess",data=d); m
x=0:53
y=predict(m,data.frame(age=x))
lines(x,y)
plot(d$age,d$bone,pch=16)
text(45,20,"gam",pos=2) # generalized additive model
library(mgcv)
m=gam(bone~s(age),data=d); m # s() is a smooth
y=predict(m,list(age=x))
lines(x,y)
plot(d$age,d$bone,pch=16)
text(45,20,"polynomial",pos=2)
m=lm(bone~age+I(age^2)+I(age^3),data=d) # lm is linear model, I() as is, that is power
y=predict(m,list(age=x))
lines(x,y)
par(mfrow=c(1,1)) # reset
# now do it with ggplot2
p=qplot(age,bone,data=d); p # qplot is a wrapper for ggplot using plot syntax
p=ggplot(d,aes(x=age,y=bone))+geom_point(); p # using ggplot
p1=p+stat_smooth(method="loess",label="loess"); p1
p1+ geom_text(aes(x=30,y=40,label="loess"))
p2=p+stat_smooth(method="loess",span=0.25); p2
p2+ geom_text(aes(x=30,y=40,label="loess\nspan=0.5")) # \n is a new line
p3=p+stat_smooth(method="gam",formula=y~s(x)); p3 # use x and y, not original age and bone
p3+ geom_text(aes(x=30,y=40,label="gam \ny~s(x)"))
p4=p+stat_smooth(method="lm",formula=y~x+I(x^2)+I(x^3)); p4
p4+ geom_text(aes(x=30,y=40,label="lm\ny~x+I(x^2)+I(x^3)"))

# joining the dots, p. 153
d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/smoothing.txt",header=TRUE)
head(d)
str(d)
plot(d$x,d$y,pch=16)
lines(d$x,d$y) # a mess because s values are not ordered
ix=order(d$x)
plot(d$x,d$y,pch=16)
lines(d$x[ix],d$y[ix])
d=d[ix,] # order
qplot(x,y,data=d)+geom_line()

# plotting with a categorical explanatory variable, p. 154
d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/SilwoodWeather.txt",header=TRUE)
head(d)
str(d)
d$month=factor(d$month)
plot(d$month,d$upper,ylab="daily maximum temperature",xlab="month") # boxplot
qplot(d$month,d$upper,ylab="daily maximum temperature",xlab="month",geom="boxplot") 
setwd("C:/Program Files/Internet Explorer") # set working directory, use forward slashes
shell("iexplore.exe http://en.wikipedia.org/wiki/Boxplot") # execute Internet Explorer
x=c(1,2,3,30,40,50)
boxplot(x) # asymetrical limits
?boxplot

d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/compexpt.txt",header=TRUE)
head(d) # plant competition experiment with with 5 treatments, a control
# 2 levels of root clipping treatments (r5 and r10), and  2 levels of shoot clipping (n25 and n50).
str(d)                                                                                  
levels(d$clipping)
plot(d$clipping,d$biomass,xlab="treatment",ylab="yield")
qplot(d$clipping,d$biomass,xlab="treatment",ylab="yield",geom="boxplot")

# boxplots with notches to indicate significant differences, p. 156
# "Boxes in which the notches do not overlap are likely to prove to have
# significantly different medians under an appropriate test.
# the size of the notch increases with the magnitude of the interquartile range
# and declines with the square root of the replication.
# notch = +/- 1.58 * IRQ / sqrt(n) roughly equal a 95% confidence interval for
# the difference between two medians, but the theory behind this is somewhat vague."
par(mfrow=c(1,2))
boxplot(d$biomass~d$clipping)
boxplot(d$biomass~d$clipping,notch=TRUE)
par(mfrow=c(1,1))
# Notches extend beyond quartiles as a warning that the test may not be valid
# with small sample sizes or large within-sample variance.
# The notches on the 3 right boxes do not overlap the control.
m=tapply(d$biomass,d$clipping,mean); m # means 
barplot(m,xlab="treatment",ylab="yield",main="Treatment Means")
library(sciplot)
bargraph.CI(d$clipping,d$biomass,xlab="treatment",ylab="yield",main="Treatment Means")
bar=function(x) { # margin of error
  n=length(x)
  t=qt(0.975,n-1) # t value for 95% confidence interval
  me=se(x)*t
  m=mean(x)
  return(c(m-me,m+me))
  }
bargraph.CI(d$clipping,d$biomass,xlab="treatment",ylab="yield",
  main="Treatment Means and 95% Confidence Intervals",fun=mean,ci.fun=bar)
p=qplot(rownames(m),m,xlab="treatment",ylab="yield",main="Treatment Means",geom="bar",fill=I("grey")); p
m=tapply(d$biomass, list(clipping=d$clipping), mean); m
s=tapply(d$biomass, list(clipping=d$clipping), sd); s
n=tapply(d$biomass, d$clipping, length); n
tmt=rownames(m); tmt
ymin=m - s/sqrt(n)*qt(0.975,n-1); ymin
ymax=m + s/sqrt(n)*qt(0.975,n-1); ymax
dd=data.frame(tmt,m,s,n,ymin,ymax); dd
p=qplot(rownames(m),m,xlab="treatment",ylab="yield",main="Treatment Means",geom="bar",fill=I("grey")); p
p+geom_errorbar(aes(x=tmt,y=m,ymin=ymin,ymax=ymax)) 
p+geom_errorbar(aes(x=tmt,y=m,ymin=ymin,ymax=ymax),width=0.5) 

# plots for multiple comparisons, p.158
rm(list=ls())
d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/box.txt",header=TRUE)
head(d)
str(d)
d$fact=factor(d$fact)
levels(d$fact)
boxplot(d$response~d$fact)
ix=order(tapply(d$response,d$fact,mean)); ix
d$ofact=factor(d$fact,levels=ix)
levels(d$ofact)
boxplot(d$response~d$ofact,notch=T,xlab="ranked treatments",ylab="response")
# each comparison has about 5% significance, but 1 in 20 will be significant due to change alone.
qplot(ofact,response,data=d,xlab="ranked treatments",ylab="response",geom="boxplot")
m=aov(response~fact,data=d) # analysis of variance
summary(m)
plot(TukeyHSD(m)) # experiment-wise 5% significance level
# see http://www.wekaleamstudios.co.uk/posts/two-way-analysis-of-variance-anova/ for plotting HSD with ggplot
hsd=TukeyHSD(m,which="fact"); str(hsd)
hsd=as.data.frame(hsd$fact); str(hsd)
hsd
ggplot(hsd,aes(x=rownames(hsd),y=diff,ymin=lwr,ymax=upr)) + geom_pointrange() +
coord_flip() + xlab("differences in mean levels of fact") + ylab("factor comparisons") +
geom_hline(yintercept=0)

# histograms, p. 162
x=rpois(1000,1.7); head(x) # Poisson random numbers
hist(x,main="",xlab="random numbers from a Poisson distribution with mean 1.7")
# not clear what the bins are
hist(x,main="",xlab="random numbers from a Poisson distribution with mean 1.7",
  breaks= seq(-0.5,9.5,1) )
range(x) 
xv=seq(min(x),max(x),1) 
yv=dpois(xv,1.7)*length(x)
lines(xv,yv)
p=qplot(x,main="",xlab="random numbers from a Poisson distribution with mean 1.7",geom="histogram"); p  
p1=p+stat_bin(breaks= seq(-0.5,9.5,1),fill="lightblue"); p1 
p1+geom_line(aes(x=xv,y=yv))
ggplot(mtcars, aes(x = mpg)) + geom_histogram(aes(y = ..density..),breaks=seq(10,35,2.5),fill="lightblue") + 
  stat_function(fun = dnorm, args = c(mean = mean(mtcars$mpg), sd = sd(mtcars$mpg)) )  
  opts(title = "Histogram with Normal Curve")

# density estimation for continuous variables, p. 164
library(MASS)
str(faithful)
hist(faithful$eruptions,breaks=15,freq=FALSE,col="lightblue")
lines(density(faithful$eruptions))
p=ggplot(faithful,aes(x=eruptions))+geom_histogram(aes(y=..density..),fill="lightblue"); p  
p+geom_density()
dn=density(faithful$eruptions); dn
p+geom_line(aes(y=dn$y,x=dn$x))
 
# index plots, p. 165
d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/das.txt",header=TRUE)
head(d)
str(d)
d$y[50]=d$y[50]+20 # introduce an error
plot(d$y) # plot y values in the order they occur in the data frame
# the error stands out clearly
ix=which(d$y>15); ix
d$y[ix] # extreme value, checking the lab notebook, it should have been 2.17939
qplot(as.numeric(rownames(d)),d$y)

# time series plots, p. 167
data(UKLungDeaths)
?UKLungDeaths
ts.plot(ldeaths,mdeaths,fdeaths,xlab="year",ylab="deaths",lty=c(1:3),
  main="UK Lung Deaths")
legend(locator(1),legend=c("total deaths","male deaths","female deaths"),lty=1:3)
data(sunspots)
str(sunspots)
plot(sunspots)

# pie charts, p. 168
# Not recommended because difficult to perceive quantities.
d=read.csv("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/piedata.csv",header=TRUE)
head(d)
str(d)
pie(d$amount,labels=as.character(d$names))

# stripchart, p. 169
data(OrchardSprays)
?OrchardSprays
str(OrchardSprays)
d=OrchardSprays
head(d)
stripchart(d$decrease~d$treatment,ylab="decrease",xlab="treatment",vertical=TRUE,log="y")
# shows individual values
qplot(d$treatment,d$decrease)

# pairs function, p. 170
d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/ozone.data.txt",header=TRUE)
head(d)
str(d)
pairs(d,panel=panel.smooth) # scatterplot matrix
plotmatrix(d) # ggplot2 package

# coplot function, p. 171
coplot(ozone~wind|temp,panel=panel.smooth,data=d)
# ordered lower left to upper right
d$temp10=floor(d$temp/10)*10; d$temp10 
attach(d)
p=qplot(d$wind,d$ozone); p
p+facet_wrap(~temp10)

# interaction plot, p. 172
d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/splityield.txt",header=TRUE)
head(d)
str(d)
interaction.plot(d$fertilizer,d$irrigation,d$yield) # response variable comes last

#trellis graphics, p. 173
d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/panels.txt",header=TRUE)
head(d)
str(d)
library(lattice) # latice graphics has many of the same plots as the base grephics, with different names
xyplot(d$weight~d$age | d$gender) # | = given, indicated conditioning
d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/daphnia.txt",header=TRUE)
head(d)
str(d)
#trellis.par.set(col.whitebg())
bwplot(d$Growth.rate~d$Water+d$Daphnia | d$Detergent) # box and whisker plot

# design plots, p. 176
par(mfrow=c(1,2))
plot.design(d$Growth.rate~d$Water*d$Detergent*d$Daphnia)
plot.design(d$Growth.rate~d$Water*d$Detergent*d$Daphnia,fun="sd")
par(mfrow=c(1,1))

# effect sizes, p. 178
library(effects)
m=lm(Growth.rate~Water*Detergent*Daphnia,data=d) # linear model
anova(m)
e=allEffects(m); e
plot(e,"Water:Detergent:Daphnia")

# bubble plots, p. 179
d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/pgr.txt",header=TRUE)
head(d)
str(d)
symbols(d$hay,d$pH,circles=d$FR,inches=0.2) # in the R Graphics windor click History > Recording
symbols(d$hay,d$pH,thermometers=cbind(0.2,1,d$FR/max(d$FR)),inches=0.2) # therometer width, height, proportion filled
sunflowerplot(d$hay,d$pH,d$FR)

# plots with many identical values, p. 180
d=read.table("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/longdata.txt",header=TRUE)
head(d)
str(d)
sunflowerplot(d$xlong,d$ylong)
#########################################################################################################
