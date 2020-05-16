#########        Treat Data        ########
geoSST10=as.geodata(Data0)
sst10=data.frame(loc.x1=geoSST10$coords[,1],loc.x2=geoSST10$coords[,2],y=geoSST10$data)
ilocmis=which(is.na(Data0$y))
locmis=cbind(Data0$loc.x1,Data0$loc.x2)[ilocmis,]


#########        Compare Various Subsamples (Figure 5 and Figure 6)       ########
### generate training set and test data, obtain parameter estimation and prediction and take them as true values
set.seed(123)
subi=sample(1:nrow(sst10),4000)
subdata=sst10[subi,]
testdata=sst10[-subi,]

GPres=likfit(as.geodata(subdata),ini.cov.pars = c(1,1),trend = "1st",
             limits = pars.limits(phi = c(1e-3,+Inf),sigmasq = c(1e-3,+Inf),nugget.rel = c(1e-3,+Inf)))
pred=krige.conv(as.geodata(subdata),locations = testdata[,-3],
                krige = krige.control(trend.d = "1st",trend.l = "1st",beta = GPres$beta,
                                      cov.pars = GPres$cov.pars,nugget = GPres$nugget))$predict
mse=mean((testdata$y-pred)^2)
#likfit: estimated model parameters:
#  beta0     beta1     beta2     tausq   sigmasq       phi 
#"51.6916" " 0.0074" " 0.9599" " 0.0025" " 2.5192" " 8.1006" 
#Practical Range with cor=0.05 for asymptotic range: 24.26714

#likfit: maximised log-likelihood = -1727

#[1] 0.05984328


### estimate parameters and predict with various subsamples 
Subsamp=function(daTa,m,testdata)
{
  para=matrix(0,3,8)     
  findi=function(x){idex=which(daTa$loc.x1==x[1] & daTa$loc.x2==x[2])}
  
  #Random
  ir=sample(1:nrow(daTa),m)
  R=daTa[ir,]
  RGPres=likfit(as.geodata(R),trend = "1st",ini.cov.pars = c(1,1),
                limits = pars.limits(phi = c(1e-3,+Inf),sigmasq = c(1e-3,+Inf),nugget.rel = c(1e-3,+Inf)))
  Rpred=krige.conv(as.geodata(R),locations =testdata[,-3],
                   krige=krige.control(trend.d = "1st",trend.l = "1st",beta = RGPres$beta,
                                       cov.pars = RGPres$cov.pars,nugget = RGPres$nugget))$predict
  para[1,-8]=c(RGPres$beta,RGPres$cov.pars,RGPres$phi/RGPres$sigmasq,RGPres$nugget)
  para[1,8]=mean((Rpred-testdata$y)^2)
  
  #Deep and Wide
  ic=sample(1:nrow(daTa),5)
  Dis=rdist(daTa[,-3])
  inb=matrix(0,5,round(m/5))
  for(i in 1:5)
  {
    rk=rank(Dis[ic[i],],ties.method = "first")
    inb[i,]=which(rk>1 & rk<=round(m/5)+1)
  }
  inr=unique(as.vector(inb))
  DaW=daTa[inr,]
  DaWGPres=likfit(as.geodata(DaW),trend = "1st",ini.cov.pars = c(1,1),
                  limits = pars.limits(phi = c(1e-3,+Inf),sigmasq = c(1e-3,+Inf),nugget.rel = c(1e-3,+Inf))) 
  DaWpred=krige.conv(as.geodata(DaW),locations =testdata[,-3],
                     krige=krige.control(trend.d = "1st",trend.l = "1st",beta = DaWGPres$beta,
                                         cov.pars = DaWGPres$cov.pars,nugget = DaWGPres$nugget))$predict
  para[2,-8]=c(DaWGPres$beta,DaWGPres$cov.pars,DaWGPres$phi/DaWGPres$sigmasq,DaWGPres$nugget)
  para[2,8]=mean((DaWpred-testdata$y)^2)
  
  #MaxPro
  map=MaxProLHD(m,2)$Design
  Map=map
  Map[,1]=map[,1]*(max(daTa$loc.x1)-min(daTa$loc.x1))+min(daTa$loc.x1)
  Map[,2]=map[,2]*(max(daTa$loc.x2)-min(daTa$loc.x2))+min(daTa$loc.x2)
  Mdist=rdist(Map,daTa[,-3])
  imap=rep(0,m)
  for(i in 1:m)
  {
    rkm=rank(Mdist[i,],ties.method = "first")
    imap[i]=which(rkm==1)
  }
  imap=unique(as.vector(imap))
  MAP=daTa[imap,]
  MAPGPres=likfit(as.geodata(MAP),trend = "1st",ini.cov.pars = c(1,1),
                  limits = pars.limits(phi = c(1e-3,+Inf),sigmasq = c(1e-3,+Inf),nugget.rel = c(1e-3,+Inf))) 
  MAPpred=krige.conv(as.geodata(MAP),locations =testdata[,-3],
                     krige=krige.control(trend.d = "1st",trend.l = "1st",beta = MAPGPres$beta,
                                         cov.pars = MAPGPres$cov.pars,nugget = MAPGPres$nugget))$predict
  para[3,-8]=c(MAPGPres$beta,MAPGPres$cov.pars,MAPGPres$phi/MAPGPres$sigmasq,MAPGPres$nugget)
  para[3,8]=mean((MAPpred-testdata$y)^2)
  
  return(para)
}

Subsamp_main=function(nseq)
{
  subsamp1=function(m){return(Subsamp(subdata,m,testdata))}
  out=sapply(nseq,subsamp1,simplify = FALSE)
  return(out)
}

cl= makeCluster(2)        #repeat 100 times, m=100, 200 and 300.
registerDoParallel(cl) 
result = foreach(i=1:100,
                 .combine=cbind,
                 .packages=c("geoR","MaxPro","fields")) %dopar% Subsamp_main(c(100,200,300))
stopCluster(cl)




