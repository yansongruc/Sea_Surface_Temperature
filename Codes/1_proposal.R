library("R.matlab")
library("fields")
library("geoR")

################           Data Information           ################
### import data
#SST=readMat("F:/data/data_SST.mat",maxLength = NULL,fixNames = TRUE)

### data discription
class(SST)
names(SST)
dim(SST$SST.zone.period)
summary(SST$lon.zone)[c(1,4,6)]
summary(SST$lat.zone)[c(1,4,6)]
head(SST$time.period)
SST$SST.zone.period[1:5,1:5,10]

################           Exploration Data Analysis           ################
### data visualization (Day 10)
lat=SST$lat.zone
lon=SST$lon.zone
SST10=SST$SST.zone.period[,,10]

dev.new()
par(mar=c(5,5,5,5))
image(t(SST10),col=tim.colors(25),xaxt="n",yaxt="n",ylim=c(0,1),
      main="Sea Surface Temperature - Day 10",xlab="Longitude(o)",ylab="Latitude(o)")
axis(1, at = seq(0, 1, by = 1/5), labels=seq(min(lon), max(lon),(max(lon)-min(lon))/5))
axis(2, at = seq(0, 1, by = 1/2), labels=rev(seq(min(lat), max(lat),(max(lat)-min(lat))/2)))
image.plot(SST10, legend.only=T)

dev.new()
hist(SST10,sqrt(240*2),main="Distribution of Sea Surface Temp. - Day 10",xlab = "Temperature(C)")

### Model Selection
#### Trend Selection
dev.new()
par(mfrow=c(2,2))
##### mean & latitude
latMean=rowMeans(SST10,na.rm = TRUE)
plot(lat,latMean,type="n",main="Mean SST by Latitude",ylab="Temperature(C)",xlab = "Latitude(o)")
lines(lat,latMean)
##### variance & latitude
var1=function(x){return(var(x,na.rm = TRUE))}
latVar=apply(SST10,1,var1)
plot(lat,latVar,type="n",main="Variance in SST by Latitude",ylab="Variance(C^2)",xlab = "Latitude(o)")
lines(lat,latVar)
##### mean & longitude
lonMean=colMeans(SST10,na.rm = TRUE)
plot(lon,lonMean,type="n",main="Mean SST by Longitude",ylab="Temperature(C)",xlab = "Longitude(o)")
lines(lon,lonMean)
##### var & longitude
lonVar=apply(SST10,2,var1)
plot(lon,lonVar,type="n",main="Variance in SST by Longitude",ylab="Variance(C^2)",xlab = "Longitude(o)")
lines(lon,lonVar)

#### directional semivariogram
loc.x1=rep(lon,each=72)
loc.x2=rep(lat,times=240)
y=c(SST10)
geoSST10=as.geodata(cbind(loc.x1,loc.x2,y))

Variog=variog4(geoSST10,direction = c(0,pi/4,pi/2,3*pi/4))
dev.new()
plot(Variog$`0`,main="Directional Semivariograms",xlab="Grid Distance",ylab="Semivariance(C^2)",xlim=c(0,40))
lines(Variog$`0`,col="red")
lines(Variog$`45`,col="blue")
lines(Variog$`90`,col="green")
lines(Variog$`135`,col="purple")
legend(0,130,c("N-S","NE-SW","E-W","NW-SE"),lty=c(1,1,1,1),lwd=c(1,1,1,1),col=c("red","blue","green","purple"))

### Variance Function selection (removing latitudinal means)
latmean=rep(latMean,times=240)
y_latm=y-latmean
SST10latm=cbind(loc.x1,loc.x2,y_latm)
SST10_latm=matrix(y_latm,nrow=72,ncol=240)
dev.new()
par(mar=c(5,5,5,5))
image(t(SST10_latm),col=tim.colors(25),xaxt="n",yaxt="n",ylim=c(0,1),
      main="Latitudinal Means Removed - Day 10",xlab="Longitude(o)",ylab="Latitude(o)")
axis(1, at = seq(0, 1, by = 1/5), labels=seq(min(lon), max(lon),(max(lon)-min(lon))/5))
axis(2, at = seq(0, 1, by = 1/2), labels=rev(seq(min(lat), max(lat),(max(lat)-min(lat))/2)))
image.plot(SST10_latm, legend.only=T)

#### histogram with gaussian curve
dev.new()
h=hist(y_latm,main = "Distribution with Latitudinal Means Removed",xlab="Residual Temperature(C)")
xfit=seq(min(y_latm,na.rm = TRUE),max(y_latm,na.rm = TRUE),length=40)
yfit=dnorm(xfit,mean=mean(y_latm,na.rm = TRUE),sd=sd(y_latm,na.rm = TRUE))
yfit=yfit*diff(h$mids[1:2])*(length(y_latm)-6335)
lines(xfit,yfit,col="blue",lwd=2)

#### directional semivariograms, mean removed
geoNew=as.geodata(SST10latm)
VariogNew=variog4(geoNew,direction = c(0,pi/4,pi/2,3*pi/4))
dev.new()
plot(VariogNew$`0`,main="Directional Semivariograms - Mean Removed",xlab="Grid Distance",ylab="Semivariance(C^2)",xlim=c(0,40))
lines(VariogNew$`0`,col="red")
lines(VariogNew$`45`,col="blue")
lines(VariogNew$`90`,col="green")
lines(VariogNew$`135`,col="purple")
legend("bottom",c("N-S","NE-SW","E-W","NW-SE"),lty=c(1,1,1,1),lwd=c(1,1,1,1),col=c("red","blue","green","purple"))
dev.new()
Var0=VariogNew$`0`
geoNewS=sample.geodata(geoNew,500)
VarS=variog(geoNewS)
fitVAR=variofit(VarS,ini=c(0.5,1),cov.model = "exponential",fix.nugget = FALSE,nugget=1,max.dist = 50)
plot(Var0,main = "Residual Empirical Omnivariogram and Fit",ylab="Semivariance",xlab="Grid Distance")
lines(fitVAR,col="red",lwd=2)





