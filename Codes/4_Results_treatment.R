beta1_R=matrix(0,3,100)    # estimation of beta1 with m Random subsamples, m=100,200 and 300
beta1_DAW=matrix(0,3,100)
beta1_MAP=matrix(0,3,100)
beta2_R=matrix(0,3,100)
beta2_DAW=matrix(0,3,100)
beta2_MAP=matrix(0,3,100)
beta3_R=matrix(0,3,100)
beta3_DAW=matrix(0,3,100)
beta3_MAP=matrix(0,3,100)
ratio_R=matrix(0,3,100)
ratio_DAW=matrix(0,3,100)
ratio_MAP=matrix(0,3,100)
tausq_R=matrix(0,3,100)
tausq_DAW=matrix(0,3,100)
tausq_MAP=matrix(0,3,100)
pa_R=matrix(0,3,100)
pa_DAW=matrix(0,3,100)
pa_MAP=matrix(0,3,100)
for(i in 1:3)
{
  for(j in 1:100)
  {
    beta1_R[i,j]=result[i,j][[1]][1,1]
    beta1_DAW[i,j]=result[i,j][[1]][2,1]
    beta1_MAP[i,j]=result[i,j][[1]][3,1]
    beta2_R[i,j]=result[i,j][[1]][1,2]
    beta2_DAW[i,j]=result[i,j][[1]][2,2]
    beta2_MAP[i,j]=result[i,j][[1]][3,2]
    beta3_R[i,j]=result[i,j][[1]][1,3]
    beta3_DAW[i,j]=result[i,j][[1]][2,3]
    beta3_MAP[i,j]=result[i,j][[1]][3,3]
    ratio_R[i,j]=result[i,j][[1]][1,6]
    ratio_DAW[i,j]=result[i,j][[1]][2,6]
    ratio_MAP[i,j]=result[i,j][[1]][3,6]
    tausq_R[i,j]=result[i,j][[1]][1,7]
    tausq_DAW[i,j]=result[i,j][[1]][2,7]
    tausq_MAP[i,j]=result[i,j][[1]][3,7]
    pa_R[i,j]=result[i,j][[1]][1,8]
    pa_DAW[i,j]=result[i,j][[1]][2,8]
    pa_MAP[i,j]=result[i,j][[1]][3,8]
  }
}

# Result is a data frame, containing the estimation of beta1, beta2, beta3, ratio, tausq and 
# prediction accuracy under varios m and method.
Result=data.frame(beta1=c(as.vector(t(beta1_R)),as.vector(t(beta1_DAW)),as.vector(t(beta1_MAP))),
                  beta2=c(as.vector(t(beta2_R)),as.vector(t(beta2_DAW)),as.vector(t(beta2_MAP))),
                  beta3=c(as.vector(t(beta3_R)),as.vector(t(beta3_DAW)),as.vector(t(beta3_MAP))),
                  ratio=c(as.vector(t(ratio_R)),as.vector(t(ratio_DAW)),as.vector(t(ratio_MAP))),
                  tausq=c(as.vector(t(tausq_R)),as.vector(t(tausq_DAW)),as.vector(t(tausq_MAP))),
                  m=as.factor(rep(rep(c(100,200,300),each=100),times=3)),
                  method=as.factor(rep(c("R","DAW","MAP"),each=300)))

# Resultm is a data frame, containing MSE of various parameters, total MSE, and MSE of prediction accuracy
Resultm=data.frame(m=rep(c(100,200,300),times=3),
                   method=rep(c("R","DAW","MAP"),each=3))
beta1m=c(apply((beta1_R-GPres$beta[1])^2,1,mean),apply((beta1_DAW-GPres$beta[1])^2,1,mean),apply((beta1_MAP-GPres$beta[1])^2,1,mean))
Resultm$beta1=beta1m
beta2m=c(apply((beta2_R-GPres$beta[2])^2,1,mean),apply((beta2_DAW-GPres$beta[2])^2,1,mean),apply((beta2_MAP-GPres$beta[2])^2,1,mean))
Resultm$beta2=beta2m
beta3m=c(apply((beta3_R-GPres$beta[3])^2,1,mean),apply((beta3_DAW-GPres$beta[3])^2,1,mean),apply((beta3_MAP-GPres$beta[3])^2,1,mean))
Resultm$beta3=beta3m
ratiom=c(apply((ratio_R-GPres$phi/GPres$sigmasq)^2,1,mean),apply((ratio_DAW-GPres$phi/GPres$sigmasq)^2,1,mean),apply((ratio_MAP-GPres$phi/GPres$sigmasq)^2,1,mean))
Resultm$ratio=ratiom
tausqm=c(apply((tausq_R-GPres$nugget)^2,1,mean),apply((tausq_DAW-GPres$nugget)^2,1,mean),apply((tausq_MAP-GPres$nugget)^2,1,mean))
Resultm$tausq=tausqm
Resultm$total=Resultm$beta1+Resultm$beta2+Resultm$beta3+Resultm$ratio+Resultm$tausq # total MES
Resultm$pa=c(apply(pa_R,1,mean),apply(pa_DAW,1,mean),apply(pa_MAP,1,mean))



# Boxplot of beta_1 (Figure 5. top left panel)
dev.new()
beta1_pe=ggplot(Result,aes(x=m,y=beta1,fill=method))+geom_boxplot()+             
  scale_fill_brewer(palette="Pastel1")+                                         
  theme_light()+geom_hline(aes(yintercept=GPres$beta[1]),color="red")+                                  #theme and line
  labs(x = "subsample size m", y = expression(widehat(beta[1])),title = expression(beta[1]))+  
  theme(plot.title = element_text(hjust=0.5))+
  stat_summary(fun.y = mean,geom="point",col="white",position=position_dodge(width=0.76))                                 
beta1_pe

# Boxplot of beta_2
dev.new()
beta2_pe=ggplot(Result,aes(x=m,y=beta2,fill=method))+geom_boxplot()+scale_fill_brewer(palette="Pastel1")+
  theme_light()+labs(x="subsample size m",y = expression(widehat(beta[2])),title = expression(beta[2]))+
  theme(plot.title = element_text(hjust=0.5))+geom_hline(aes(yintercept=GPres$beta[2]),color="red")+
  stat_summary(fun.y = mean,geom="point",col="white",position=position_dodge(width=0.76))
beta2_pe

# Boxplot of beta_3
dev.new()
beta3_pe=ggplot(Result,aes(x=m,y=beta3,fill=method))+geom_boxplot()+scale_fill_brewer(palette="Pastel1")+
  theme_light()+labs(x="subsample size m",y = expression(widehat(beta[3])),title = expression(beta[3]))+
  theme(plot.title = element_text(hjust=0.5))+geom_hline(aes(yintercept=GPres$beta[3]),color="red")+
  stat_summary(fun.y = mean,geom="point",col="white",position=position_dodge(width=0.76))
beta3_pe

# Boxplot of ratio (Figure 5. top right panel)
dev.new()
ratio_pe=ggplot(Result,aes(x=m,y=ratio,fill=method))+geom_boxplot()+scale_fill_brewer(palette="Pastel1")+
  theme_light()+labs(x="subsample size m",title = expression(phi/sigma^2))+
  theme(plot.title = element_text(hjust=0.5))+geom_hline(aes(yintercept=GPres$phi/GPres$sigmasq),color="red")
ratio_pe

# Boxplot of nugget effect tausq (Figure 5. bottom left panel)
dev.new()
tausq_pe=ggplot(Result,aes(x=m,y=tausq,fill=method))+geom_boxplot()+scale_fill_brewer(palette="Pastel1")+
  theme_light()+labs(x="subsample size m",y=expression(log[10](widehat(tau^2))),title = expression(tau^2))+scale_y_log10()+
  theme(plot.title = element_text(hjust=0.5))+geom_hline(aes(yintercept=GPres$nugget),color="red")+
  stat_summary(fun.y = mean,geom="point",col="white",position=position_dodge(width=0.76))
tausq_pe

# Total MSEs (Figure 5. bottom right panel) 
Resultm$total=Resultm$beta1+Resultm$beta2+Resultm$beta3+Resultm$ratio+Resultm$tausq
total_pe=ggplot(Resultm,aes(x=m,y=total,color=method))+geom_line(size=2)+geom_point(size=3)+scale_fill_brewer(palette="Pastel1")+
  theme_light()+labs(x="subsample size m",y="parameter estimation accuracy",title = expression(total))+
  theme(plot.title = element_text(hjust=0.5))
total_pe

# Boxplot of prediction accuracy
Result$pa=c(as.vector(t(pa_R)),as.vector(t(pa_DAW)),as.vector(t(pa_MAP)))
pa_p=ggplot(Result,aes(x=m,y=pa,fill=method))+geom_boxplot()+scale_fill_brewer(palette="Pastel1")+
  theme_light()+labs(x="subsample size m",y=expression(log[10](mspe)),title = "prediction accuracy")+scale_y_log10()+
  theme(plot.title = element_text(hjust=0.5))+geom_hline(aes(yintercept=mse),color="red")+
  stat_summary(fun.y = mean,geom="point",col="white",position=position_dodge(width=0.76))
pa_p

# MSE of prediction accuracy (Figure 6)
dev.new()
pa_p=ggplot(Resultm,aes(x=m,y=pa,color=method))+geom_line(size=2)+geom_point(size=3)+scale_fill_brewer(palette="Pastel1")+
  theme_light()+labs(x="subsample size m",y="mspe",title="prediction accuracy")+
  theme(plot.title = element_text(hjust=0.5))+geom_hline(aes(yintercept=mse),size=1.5)
pa_p