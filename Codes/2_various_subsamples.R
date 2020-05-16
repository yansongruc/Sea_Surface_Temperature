#########        Treat Data        ########
geoSST10=as.geodata(Data0)
sst10=data.frame(loc.x1=geoSST10$coords[,1],loc.x2=geoSST10$coords[,2],y=geoSST10$data)
ilocmis=which(is.na(Data0$y))
locmis=cbind(Data0$loc.x1,Data0$loc.x2)[ilocmis,]


#########        Various Subsamples and Plot (in Figure 1 - Figure 4 of final report)        ########
dev.new()
p_data=ggplot(sst10,aes(x=loc.x1,y=loc.x2,color=y))+geom_tile()+scale_color_gradientn(colours = tim.colors(25))+
  theme_light()+labs(x="Longitude",y="Latitude",color="Sea Surface")
p_data

### random subsamples
m=100
dev.new()
ir=sample(1:nrow(sst10),m)
R=sst10[ir,]
r_sub=ggplot(R,aes(x=R[,1],y=R[,2],color=R[,3]))+geom_point()+
  scale_color_gradientn(colours = tim.colors(25))+labs(x="Longitude",y="Latitude",color="Sea Surface")
r_sub

### Deep and Wide subsamples
m=100
dev.new()
ic=sample(1:nrow(sst10),5)
Dis=rdist(sst10[,-3])
inb=matrix(0,5,round(m/5))
for(i in 1:5)
{
  rk=rank(Dis[ic[i],],ties.method = "first")
  inb[i,]=which(rk>1 & rk<=round(m/5)+1)
}
inr=unique(as.vector(inb))
DaW=sst10[inr,]
DaW_sub=ggplot(DaW,aes(x=DaW[,1],y=DaW[,2],color=DaW[,3]))+geom_point()+theme_light()+
  scale_color_gradientn(colours = tim.colors(25))+labs(x="Longitude",y="Latitude",color="Sea Surface")+
  scale_x_continuous(limits =range(sst10$loc.x1))+scale_y_continuous(limits =range(sst10$loc.x2))
DaW_sub

### MaxPro
m=100
dev.new()
map=MaxProLHD(m,2)$Design
Map=map
Map[,1]=map[,1]*(max(sst10$loc.x1)-min(sst10$loc.x1))+min(sst10$loc.x1)
Map[,2]=map[,2]*(max(sst10$loc.x2)-min(sst10$loc.x2))+min(sst10$loc.x2)
Mdist=rdist(Map,sst10[,-3])
imap=rep(0,m)
for(i in 1:m)
{
  rkm=rank(Mdist[i,],ties.method = "first")
  imap[i]=which(rkm==1)
}
imap=unique(as.vector(imap))
MAP=sst10[imap,]
MAP_sub=ggplot(MAP,aes(x=MAP[,1],y=MAP[,2],color=MAP[,3]))+geom_point()+theme_light()+
  scale_color_gradientn(colours = tim.colors(25))+labs(x="Longitude",y="Latitude",color="Sea Surface")
MAP_sub
