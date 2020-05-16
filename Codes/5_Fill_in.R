#########        Treat Data        ########
geoSST10=as.geodata(Data0)
sst10=data.frame(loc.x1=geoSST10$coords[,1],loc.x2=geoSST10$coords[,2],y=geoSST10$data)
ilocmis=which(is.na(Data0$y))
locmis=cbind(Data0$loc.x1,Data0$loc.x2)[ilocmis,]

#########       Fill in missing values with 300 MaxPro subsamples (Figure 7)       ########
# remove missing values lead by land
M=matrix(0,72,240)
for(i in 1:72)
{
  for(j in 1:240)
  {
    M[i,j]=sum(is.na(SST$SST.zone.period[i,j,]))
  }
}
irealmiss=which(c(M)==331)
ilocmiss=setdiff(ilocmis,irealmiss)
locmiss=Data0[ilocmiss,-3]

# select 300 MaxPro subsamples from full data
map=MaxProLHD(300,2)$Design
Map=map
Map[,1]=map[,1]*(max(sst10$loc.x1)-min(sst10$loc.x1))+min(sst10$loc.x1)
Map[,2]=map[,2]*(max(sst10$loc.x2)-min(sst10$loc.x2))+min(sst10$loc.x2)
Mdist=rdist(Map,sst10[,-3])
imap=rep(0,300)
for(i in 1:300)
{
  rkm=rank(Mdist[i,],ties.method = "first")
  imap[i]=which(rkm==1)
}
imap=unique(as.vector(imap))
MAP=sst10[imap,]

# estimate parameters and fill in missing values
MAPGPres=likfit(as.geodata(MAP),trend = "1st",ini.cov.pars = c(1,1),
                limits = pars.limits(phi = c(1e-3,+Inf),sigmasq = c(1e-3,+Inf),nugget.rel = c(1e-3,+Inf))) 
MAPpred=krige.conv(as.geodata(MAP),locations =locmiss,
                   krige=krige.control(trend.d = "1st",trend.l = "1st",beta = MAPGPres$beta,
                                       cov.pars = MAPGPres$cov.pars,nugget = MAPGPres$nugget))$predict
# plot results after filling in 
fill=data.frame(loc.x1=locmiss[,1],loc.x2=locmiss[,2],y=MAPpred)
Datafill=rbind(sst10,fill)
p_datafill=ggplot(Datafill,aes(x=loc.x1,y=loc.x2,color=y))+geom_tile()+scale_color_gradientn(colours = tim.colors(25))+
  theme_light()+labs(x="Longitude",y="Latitude",color="Sea Surface")
p_datafill
