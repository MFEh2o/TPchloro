##### SModel analyses and figure generation 
##### 2021-09-27

rm(list=ls())
library(deSolve)

##### load simulation output
load(file="smodel.RData")

library(wesanderson)


##### Generating Figure 2
#2a) I limitation colored
d10=output[output$Z==10,]

colors100=wes_palette('Zissou1', 100, type =  "continuous")
ilimcol=colors100[floor(99*d10$lightlim)+1]

plot(d10$TP,d10$chl, xlab=expression("TP (ug"~'L'^-1*')'), ylab=expression("algal biomass (ug chl"~'L'^-1*')'), xlim=c(0,500), ylim=c(0,200),type="p",pch=16,col=ilimcol)

# make a key for colors
image(1, seq(0.01,1,0.01), t(seq_along(seq(0.01,1,0.01))), col=colors100, axes=FALSE, xlab = '', ylab='', main="light limitation", cex.main=0.9)
axis(4)


#2b) fraction TP as SRP colored
srp2tpcol=wes_palette('Zissou1', 100, type =  "continuous")[floor(99*d10$fracSRP)+1]

plot(d10$TP,d10$chl, xlab=expression("TP (ug"~'L'^-1*')'), ylab=expression("algal biomass (ug chl"~'L'^-1*')'), xlim=c(0,500), ylim=c(0,200),col=srp2tpcol,type="p",pch=16)

# make a key for colors
image(1, seq(0.01,1,0.01), t(seq_along(seq(0.01,1,0.01))), col=colors100, axes=FALSE, xlab = '', ylab='', main="fraction TP as SRP", cex.main=0.9)
axis(4)


##### Generating Figure 3
#3a) Pin colored
colors33=wes_palette('Zissou1', 33, type =  "continuous")
uniqueColors33=unique(output$Pin)
pinColors=rep(colors33[1],length(output$Pin))
for(i in 2:length(uniqueColors33)){
  pinColors[output$Pin==uniqueColors33[i]]=colors33[i]
}
plot(output$TP,output$chl, xlab=expression("TP (ug"~'L'^-1*')'), ylab=expression("algal biomass (ug chl"~'L'^-1*')'), xlim=c(0,500), ylim=c(0,200),col=pinColors,type="p",pch=16)

# key for colors
image(1, unique(output$Pin)*1000, t(seq_along(unique(output$Pin))), col=colors33, axes=FALSE, xlab = '', ylab='', main=expression("Pin\n(ug"~'L'^-1*')'), cex.main=0.9,log="y")
axis(4)

#3b) HRT colored
colors100=wes_palette('Zissou1', 100, type =  "continuous")
uniqueColors100=unique(output$HRT)
hrtColors=rep(colors100[1],length(output$HRT))
for(i in 2:length(uniqueColors100)){
  hrtColors[output$HRT==uniqueColors100[i]]=colors100[i]
}
plot(output$TP,output$chl, xlab=expression("TP (ug"~'L'^-1*')'), ylab=expression("algal biomass (ug chl"~'L'^-1*')'), xlim=c(0,500), ylim=c(0,200),col=hrtColors,type="p",pch=16)

# key for colors
image(1, unique(output$HRT), t(seq_along(unique(output$HRT))), col=colors100, axes=FALSE, xlab = '', ylab='', main="HRT (days)", cex.main=0.9,log="y")
axis(4)

#3c) depth colored
colors4=wes_palette('Zissou1', 4, type =  "continuous")
depthColors=rep(colors4[1],length(output$TP))
depthColors[output$Z==5]=colors4[2]
depthColors[output$Z==10]=colors4[3]
depthColors[output$Z==20]=colors4[4]
plot(output$TP,output$chl, xlab=expression("TP (ug"~'L'^-1*')'), ylab=expression("algal biomass (ug chl"~'L'^-1*')'), xlim=c(0,500), ylim=c(0,200),col=depthColors,type="p",pch=16)
legend('topleft',paste("Z=",unique(output$Z),sep=""),pch=16,col=colors4,box.lty=0)

#3d) light limitation
ilimcol2=wes_palette('Zissou1', 100, type =  "continuous")[floor(99*output$lightlim)+1]
plot(output$TP,output$chl, xlab=expression("TP (ug"~'L'^-1*')'), ylab=expression("algal biomass (ug chl"~'L'^-1*')'), xlim=c(0,500), ylim=c(0,200),col=ilimcol2,type="p",pch=16)

# key for colors
image(1, seq(0.01,1,0.01), t(seq_along(seq(0.01,1,0.01))), col=colors100, axes=FALSE, xlab = '', ylab='', main="light limitation", cex.main=0.9)
axis(4)


##### Generating Figure 4
colors3=wes_palette('Zissou1', 3, type =  "continuous")

cpColors=rep(colors3[1],length(cpOut$TP))
cpColors[3301:6600]=colors3[2]
cpColors[6601:9900]=colors3[3]
plot(cpOut$TP,cpOut$chl, xlab=expression("TP (ug"~'L'^-1*')'), ylab=expression("algal biomass (ug chl"~'L'^-1*')'), xlim=c(0,500), ylim=c(0,200),type="p",pch=16,col=cpColors)
legend('topleft',paste("cp=",c(0.005,0.015,0.025),sep=""),pch=16,col=colors3,box.lty=0)

maColors=rep(colors3[1],length(maOut$TP))
maColors[3301:6600]=colors3[2]
maColors[6601:9900]=colors3[3]
plot(maOut$TP,maOut$chl, xlab=expression("TP (ug"~'L'^-1*')'), ylab=expression("algal biomass (ug chl"~'L'^-1*')'), xlim=c(0,500), ylim=c(0,200),type="p",pch=16,col=maColors)
legend('topleft',paste("ma=",c(0.002,0.003,0.005),sep=""),pch=16,col=colors3,box.lty=0)

haColors=rep(colors3[1],length(haOut$TP))
haColors[3301:6600]=colors3[2]
haColors[6601:9900]=colors3[3]
plot(haOut$TP,haOut$chl, xlab=expression("TP (ug"~'L'^-1*')'), ylab=expression("algal biomass (ug chl"~'L'^-1*')'), xlim=c(0,500), ylim=c(0,200),type="p",pch=16,col=haColors)
legend('topleft',paste("ha=",c(36,80,100),sep=""),pch=16,col=colors3,box.lty=0)


##### Generating Figure 5
plot(nla2$tp,nla2$chl, xlab=expression("TP (ug"~'L'^-1*')'), ylab=expression("algal biomass (ug chl"~'L'^-1*')'), xlim=c(0,500), ylim=c(0,250),type="p",pch=16)
points(NLAsim_output$TP,NLAsim_output$chl,pch=21,col='grey')
legend('topleft',c('observed','modeled'),pch=c(16,21),col=c('black','grey'),box.lty=0)

###removing simulations with unrealistically low chlorophyll
keep=NLAsim_output.d[NLAsim_output.d$chl >= 0.167,]

plot(nla2$tp,nla2$chl, xlab=expression("TP (ug"~'L'^-1*')'), ylab=expression("algal biomass (ug chl"~'L'^-1*')'), xlim=c(0,500), ylim=c(0,250),type="p",pch=16)
points(keep$TP,keep$chl,pch=21,col='grey')
legend('topleft',c('observed','modeled'),pch=c(16,21),col=c('black','grey'),box.lty=0)


##### Generating Figure S2
plot(output$Pin[output$Z==10]*1000,output$TP[output$Z==10],col=hrtColors[output$Z==10],xlab="Pin (ug L-1)",ylab="TP (ug L-1)")

image(1, unique(output$HRT), t(seq_along(unique(output$HRT))), col=colors100, axes=FALSE, xlab = '', ylab='', main="HRT days", cex.main=0.9,log="y")
axis(4)


##### Generating Figure S1
load("VollenweiderOptimization.RData")

## simulations of Vollenweider lakes with initial, literature-based parameters
plot(log(Vdata$TP), log((Vout[,5]+Vout[,3])*1000/Vout[,6]), xlab='log Vollenweider TP', ylab='log model TP', pch=16, xlim=c(-1,8.5), ylim=c(-1,8.5))
VfitInitial=lm(log((Vout[,5]+Vout[,3])*1000/Vout[,6]) ~ log(Vdata$TP))
abline(0,1, lwd=3)
abline(VfitInitial, untf=F, col='red',lwd=3)

## boxplot for parameter sensitivity analysis
boxplot((percentDev$pctChange*100)~percentDev$trt,las=2,xlab="",ylab="percent change in TP")
# apmax: higher apmax gives higher TP
# b: higher b gives lower TP
# r: higher r gives lower TP

# 1:1 plot between Vollenweider data and optimized simulations
plot(log(Vdata$TP), log((VoutOPT[,5]+VoutOPT[,3])*1000/VoutOPT[,6]), xlab='log Vollenweider TP', ylab='log model TP', pch=16, xlim=c(-1,8.5), ylim=c(-1,8.5))
VfitOPT=lm(log((VoutOPT[,5]+VoutOPT[,3])*1000/VoutOPT[,6]) ~ log(Vdata$TP))
abline(0,1, lwd=3)
abline(VfitOPT, untf=F, col='red',lwd=3)





##########################################
### Code used to create .Rdata objects ###
##########################################

# Some of this takes a long time to run!

##### model definition
Model<-function(t, y, parms) {
  # define state variables at time step
  C=y[1]
  A=y[2]
  P=y[3]
  S=y[4]
  
  # define parameters from arguments
  kbg=parms[1]
  kc=parms[2]
  ka=parms[3]
  I0=parms[4]
  zmax=parms[5]
  Cin=parms[6]
  d=parms[7]
  pa=parms[8]
  ha=parms[9]
  ma=parms[10]
  la=parms[11]
  r=parms[12]
  Pin=parms[13]
  apmax=parms[14]
  b=parms[15]
  Al=parms[16]
  zsed=parms[17]
  Qin=parms[18]
  a=parms[19]
  c=parms[20]
  Cp=parms[21]
  
  # intermediate equations
  V=Al*zmax ##m3
  Qout=Qin
  Vsed=Al*zsed ##m3
  kd=kbg + (kc*(C/V)) + (ka*(A/V)) ##1/m 
  Izmax=I0*exp(-kd*zmax) ##uE
  Pgrad=((S/Vsed)-(P/V)) ##concentration gradient between sediments and phosphorus
  ap=apmax*((exp(a + c*Pgrad)) / (1 + exp(a + c*Pgrad)))
  u=(pa/(kd*zmax))*log((ha+I0)/(ha+Izmax))*((P/V)/((P/V)+ma))
  
  # differential equations
  dC=(Qin*Cin) - ((C/V)*Qout) - d*C  # g C day-1
  dA=(u - la - (r/zmax) - (Qout/V))*A    # g C day-1
  dP=(Qin*Pin) - ((P/V)*Qout) - (u*A*Cp) + (la*A*Cp) + ap*S    # g P  day-1
  dS=(r/zmax)*A*Cp - ap*S - b*S  # mg P day-1
  
  # calculate pools, fluxes, etc to store for each time step
  #A in terms of P
  APhos=A*Cp
  
  # calculating light limitation
  LightLim=1-(1/(kd*zmax))*log((ha+I0)/(ha+Izmax))
  
  # calculating nutrient limitation
  NutLim=1-((P/V)/((P/V)+ma))
  
  # vector of deltas
  dY=c(dC=dC, dA=dA, dP=dP, dS=dS)
  
  return(list(dY, APhos, V, LightLim, NutLim, u))
}

##### Run simulations to equilibrium and store the output across Pin, HRT, and depth gradients
Pins=rep(exp(seq(-3.912023,-0.6931472,length.out=33)),400)  # g P m-3
Zs=rep(c(2,5,10,20),each=3300)
HRTs=rep(10^rep(seq(1,4,length.out=100),each=33),4)
SAs=rep(1e7,13200)
Qins=Zs*SAs/HRTs

x0=c(0, 0.005*10000*2, 0.005*10000*2, 1)
times=1:1000

output=data.frame(Qin=Qins,Pin=Pins,Z=Zs,SA=SAs,HRT=HRTs,TP=NA,biomass=NA,SRP=NA,lightlim=NA,plim=NA,u=NA)

for(i in 1:length(Qins)){
  print(i)
  pars=c(kbg=0, kc=0.42, ka=0.22, I0=600, zmax=Zs[i], Cin=0, d=0.01, pa=1, ha=80,
         ma=0.003, la=0.1, r=0.6478685, Pin=Pins[i], apmax=0.1745694, b=0.003058574, 
         Al=SAs[i], zsed=0.01, Qin=Qins[i], a=-1, c=10, Cp=0.015)
  out=ode(y=x0,times=times,func=Model,parms=pars)
  
  output$TP[i]=(out[nrow(out),4]+out[nrow(out),6])*1000/out[nrow(out),7] # mg P  m-3
  output$biomass[i]=out[nrow(out),3]/out[nrow(out),7] # g C m-3
  output$SRP[i]=out[nrow(out),4]*1000/out[nrow(out),7] # mg P m-3
  output$lightlim[i]=out[nrow(out),8]
  output$plim[i]=out[nrow(out),9]
  output$u[i]=out[nrow(out),10]
}

output$chl=output$biomass/60*1000  # mg chl m-3
output$fracSRP=output$SRP/output$TP


##### trait sensitivity simulations
traitPins=rep(exp(seq(-3.912023,-0.6931472,length.out=33)),100)  # g P m-3
traitZs=rep(10,each=3300)
traitHRTs=10^rep(seq(1,4,length.out=100),each=33)
traitSAs=rep(1e7,3300)
traitQins=traitZs*traitSAs/traitHRTs

x0=c(0, 0.005*10000*2, 0.005*10000*2, 1)
times=1:1000

trait_output=data.frame(Qin=rep(traitQins,9),
                        Pin=rep(traitPins,9),
                        Z=rep(traitZs,9),
                        SA=rep(traitSAs,9),
                        HRT=rep(traitHRTs,9),
                        TP=NA,
                        biomass=NA,
                        SRP=NA,
                        lightlim=NA,
                        plim=NA,
                        u=NA)

traits=data.frame(cp=c(c(0.005,0.015,0.025),rep(0.015,6)),ma=c(rep(0.003,3),c(0.002,0.003,0.005),rep(0.003,3)),ha=c(rep(80,6),c(36,80,100)))
for(j in 1:nrow(traits)){
  for(i in 1:length(traitQins)){
    print(c(j,i))
    pars=c(kbg=0, kc=0.42, ka=0.22, I0=600, zmax=traitZs[i], Cin=0, d=0.01, pa=1, ha=traits$ha[j],
           ma=traits$ma[j], la=0.1, r=0.6478685, Pin=traitPins[i], apmax=0.1745694, b=0.003058574, 
           Al=traitSAs[i], zsed=0.01, Qin=traitQins[i], a=-1, c=10, Cp=traits$cp[j])
    out=ode(y=x0,times=times,func=Model,parms=pars)
    
    trait_output$TP[((j-1)*3300+i)]=(out[nrow(out),4]+out[nrow(out),6])*1000/out[nrow(out),7] # mg P  m-3
    trait_output$biomass[((j-1)*3300+i)]=out[nrow(out),3]/out[nrow(out),7] # g C m-3
    trait_output$SRP[((j-1)*3300+i)]=out[nrow(out),4]*1000/out[nrow(out),7] # mg P m-3
    trait_output$lightlim[((j-1)*3300+i)]=out[nrow(out),8]
    trait_output$plim[((j-1)*3300+i)]=out[nrow(out),9]
    trait_output$u[((j-1)*3300+i)]=out[nrow(out),10]
  }
}
trait_output$chl=trait_output$biomass/60*1000  # mg chl m-3
trait_output$fracSRP=trait_output$SRP/trait_output$TP

cpOut=trait_output[1:9900,]
maOut=trait_output[9901:19800,]
haOut=trait_output[19801:29700,]


##### NLA qualitative comparison
nla=read.csv('nla12_keyvariables_data.csv', sep = ',', header = TRUE, stringsAsFactors = FALSE)

chl=aggregate(nla$CHLX_RESULT, by = list(nla$SITE_ID), FUN=mean,na.rm=TRUE)
tp=aggregate(nla$PTL_RESULT, by = list(nla$SITE_ID), FUN=mean,na.rm=TRUE)

nla=merge(chl, tp, by = 'Group.1', all=T)
colnames(nla)=c('site', 'chl', 'tp')

# remove sites with NA for chl
nla=nla[!is.na(nla$chl),]

# removing values outside of simulation range
nla2=nla[nla$tp <= 500,]

# removing a single algal biomass outlier
nla2=nla2[nla2$chl < 333,]

# setting up model run
x0=c(0, 0.005*10000*2, 0.005*10000*2, 1)
times=1:1000

N=1000
set.seed(N)

Qins=runif(N,min=1e3,max=4e6)  #m3 day-1
Pins=runif(N,min=0.01,max=0.5) # g m-3
Zs=runif(N,min=2,max=30)       # m
SAs=runif(N,min=1e6,max=2.5e8) # m2

NLAsim_output=data.frame(Qin=Qins,Pin=Pins,Z=Zs,SA=SAs,TP=NA,biomass=NA)

for(i in 1:length(Qins)){
  print(i)
  pars=c(kbg=0, kc=0.42, ka=0.22, I0=600, zmax=Zs[i], Cin=0, d=0.01, pa=1, ha=80,
         ma=0.003, la=0.1, r=0.6478685, Pin=Pins[i], apmax=0.1745694, b=0.003058574, 
         Al=SAs[i], zsed=0.01, Qin=Qins[i], a=-1, c=10, Cp=0.015)
  out=ode(y=x0,times=times,func=Model,parms=pars)
  NLAsim_output$TP[i]=(out[nrow(out),4]+out[nrow(out),6])*1000/out[nrow(out),7] # mg P  m-3
  NLAsim_output$biomass[i]=out[nrow(out),3]/out[nrow(out),7] # g C m-3
}
NLAsim_output$chl=NLAsim_output$biomass/60*1000  # mg chl m-3

##adding DOC
DOCs=runif(N, min=0, max=30)

NLAsim_output.d=data.frame(Qin=Qins,Pin=Pins,Z=Zs,SA=SAs,DOC=DOCs,TP=NA,biomass=NA, docLake=NA, lightlim=NA)

for(i in 1:length(Qins)){
  print(i)
  pars=c(kbg=0, kc=0.42, ka=0.22, I0=600, zmax=Zs[i], Cin=DOCs[i], d=0.01, pa=1, ha=80,
         ma=0.003, la=0.1, r=0.6478685, Pin=Pins[i], apmax=0.1745694, b=0.003058574, 
         Al=SAs[i], zsed=0.01, Qin=Qins[i], a=-1, c=10, Cp=0.015)
  out=ode(y=x0,times=times,func=Model,parms=pars)
  NLAsim_output.d$TP[i]=(out[nrow(out),4]+out[nrow(out),6])*1000/out[nrow(out),7] # mg P  m-3
  NLAsim_output.d$biomass[i]=out[nrow(out),3]/out[nrow(out),7] # g C m-3
  NLAsim_output.d$docLake[i]=out[nrow(out),2]/out[nrow(out),7] # g C m-3
  NLAsim_output.d$lightlim[i]=out[nrow(out), 8]
}
NLAsim_output.d$chl=NLAsim_output.d$biomass/60*1000  # mg chl m-3

########### write workspace image with simulations output
save.image(file="smodel.RData")



##### optimization with vollenweider data - this was run prior to all other simulations to optimize parameterization
##load in Vollenweider data
Vdata=read.csv('Vollenweider_lakes_raw.csv', header = TRUE, stringsAsFactors = FALSE)
Vdata=Vdata[!is.na(Vdata$uniqueID),]

##getting the data in the same units as Smodel
Vdata$sa=Vdata$surfaceArea_km2*10^6  # m2
Vdata$depth=Vdata$meanDepth_vol_SA_m # m
Vdata$discharge=Vdata$inflowRate_Q_m3_s*60*60*24 # m3 day-1
Vdata$hrt=Vdata$hrt_years*365 # days
Vdata$TP=Vdata$Tpout_mg_m3 # mg m-3
Vdata$volume=Vdata$volume_10.6m3*10^6  # m3
Vdata$Pin=Vdata$TPin_mg_m3/1000  # g m-3
Vdata$Pload=Vdata$loadingConstant_kgP_yr/365*1000  # g P day-1

##removing row 23...doesn't have a volume nor discharge
Vdata=Vdata[-23,]

## filling in some hrt or discharge using other hydrologic characteristics
#HRT
tofill=which(Vdata$hrt==0)
Vdata$hrt[tofill]=Vdata$volume[tofill]/Vdata$discharge[tofill] # a bit strange these all of HRT<2 days
#discharge
tofill=which(Vdata$discharge==0)
Vdata$discharge[tofill]=Vdata$volume[tofill]/Vdata$hrt[tofill]


## simulate model with initial parameterization from literature
##vectors defining initial states, parameters, and time over which to solve
x0=c(0, 0.005*10000*2, 0.005*10000*2, 1)
times=1:1000

##simulate model for all Vollenweider lakes
Vout=matrix(NA, nrow(Vdata), 9)

for (i in 1:nrow(Vdata)) {
  print(i)
  # Cin set to 0 because we don't know DOC loads
  pars=c(kbg=0, kc=0.42, ka=0.22, I0=600, zmax=Vdata$depth[i], Cin=0, d=0.01, pa=1, ha=80,
         ma=0.003, la=0.1, r=0.1, Pin=Vdata$Pin[i], apmax=0.1, b=0.001, 
         Al=Vdata$sa[i], zsed=0.01, Qin=Vdata$discharge[i] , a=-1, c=10, Cp=0.015)
  run=ode(y=x0,times=times,func=Model,parms=pars)
  Vout[i,]=run[nrow(run),-1]
}

colnames(Vout)=c("DOC", "AlgalBiomass", "SRP", "SedimentPhosphorus", "AlgalBiomassP","V", 'LightLim', 'NutLim', 'u')

BASE=Vout


## parameter sensitivity analysis
BASEpars=c(kbg=0, kc=0.42, ka=0.22, I0=600, zmax=2, Cin=0, d=0.01, pa=1, ha=80,
           ma=0.003, la=0.1, r=0.1, Pin=0.2, apmax=0.1, b=0.001, 
           Al=10000, zsed=0.01, Qin=55, a=-1, c=10, Cp=0.015)

##considering the following parameters: ka, pa, ha, ma, la, r, apmax, b, a, c, Cp 
sensitivityPars=matrix(0,22,2)
sensitivityPars[,1]=rep(c(3,8,9,10,11,12,14,15,19,20,21),2)
sensitivityPars[,2]=c(BASEpars[c(3,8,9,10,11,12,14,15,19,20,21)]*0.9,BASEpars[c(3,8,9,10,11,12,14,15,19,20,21)]*1.1)

sensitivity=array(0,dim=c(nrow(sensitivityPars),nrow(Vdata),9))

for(j in 1:nrow(sensitivityPars)){
  out=matrix(NA, nrow(Vdata), 9)
  
  for (i in 1:nrow(Vdata)) {
    print(c(j,i))
    pars=c(kbg=0, kc=0.42, ka=0.22, I0=600, zmax=Vdata$depth[i], Cin=0, d=0.01, pa=1, ha=80,
           ma=0.003, la=0.1, r=0.1, Pin=Vdata$Pin[i], apmax=0.1, b=0.001, 
           Al=Vdata$sa[i], zsed=0.01, Qin=Vdata$discharge[i], a=-1, c=10, Cp=0.015)
    pars[sensitivityPars[j,1]]=sensitivityPars[j,2]
    run=ode(y=x0,times=times,func=Model,parms=pars)
    out[i,]=run[nrow(run),-1]
  }
  
  sensitivity[j,,]=out
}

##calculate base and perturbed TPs
baseTP=(BASE[,5]+BASE[,3])*1000/BASE[,6]
sensTP=(sensitivity[,,5]+sensitivity[,,3])*1000/sensitivity[,,6]

##plot deviation from base sim -> most sensitive to apmax, b, and r, but only max of 5% change...
percentDev=data.frame(trt=rep(c(paste(names(BASEpars)[c(3,8,9,10,11,12,14,15,19,20,21)],"low",sep="_"),paste(names(BASEpars)[c(3,8,9,10,11,12,14,15,19,20,21)],"hi",sep="_")),each=nrow(Vdata)),
                      pctChange=rep(0,22*nrow(Vdata)))
for(i in 1:22){
  percentDev$pctChange[((i-1)*nrow(Vdata)+1):(nrow(Vdata)*i)]=(sensTP[i,]-baseTP)/baseTP
}


# monte carlo search on subset of parameters (apmax, b, and r)
# -ran monte carlo searches to optimize these because max. likelihood didn't work very well
# -monte carlo suggested some equifinality issues between (b  and r); this makes sense
# selected values for  apmax, b, and r based on having lakes simulate, a model vs. observed  with slope and itercept not  significantly different  from 1 and 0 and high R^2
# code for running  the monte carlo searches is in VollenweiderGridSearch_parallel.R and VollenweiderSubGridSearch_parallel.R - these take 8+ hours with 24 cores

# load output of first grid search
gs=read.csv("VollenweiderGridSearch.csv",header=TRUE)

plot(gs$apmax,gs$R2)
plot(gs$r,gs$R2)
plot(gs$b,gs$R2)

plot(gs$apmax,gs$intercept_pvalue)
plot(gs$r,gs$intercept_pvalue)
plot(gs$b,gs$intercept_pvalue)

plot(gs$apmax,gs$slope_pvalue)
plot(gs$r,gs$slope_pvalue)
plot(gs$b,gs$slope_pvalue)

gsnoNA=gs[gs$numNA==0,]
gsnoNAp05=gsnoNA[gsnoNA$intercept_pvalue>0.05,]
gsnoNAp05=gsnoNAp05[gsnoNAp05$slope_pvalue>0.05,]

gsnoNAp05[order(gsnoNAp05$R2),]

plot(gsnoNAp05$r,gsnoNAp05$b)
plot(gsnoNAp05$apmax,gsnoNAp05$r)
plot(gsnoNAp05$apmax,gsnoNAp05$b)


### try more refined search
gss=read.csv("VollenweiderSubGridSearch.csv",header=TRUE)

plot(gss$apmax,gss$R2)
plot(gss$r,gss$R2)
plot(gss$b,gss$R2)

plot(gss$apmax,gss$intercept_pvalue)
plot(gss$r,gss$intercept_pvalue)
plot(gss$b,gss$intercept_pvalue)

plot(gss$apmax,gss$slope_pvalue)
plot(gss$r,gss$slope_pvalue)
plot(gss$b,gss$slope_pvalue)

gssnoNA=gss[gss$numNA==0,]
gssnoNAp05=gssnoNA[gssnoNA$intercept_pvalue>0.05,]
gssnoNAp05=gssnoNAp05[gssnoNAp05$slope_pvalue>0.05,]

range(gssnoNAp05$R2)

gssnoNAp05R08=gssnoNAp05[gssnoNAp05$R2>0.8,]

tail(gssnoNAp05R08[order(gssnoNAp05R08$R2),])

plot(gssnoNAp05R08$r,gssnoNAp05R08$b)
plot(gssnoNAp05R08$apmax,gssnoNAp05R08$r)
plot(gssnoNAp05R08$apmax,gssnoNAp05R08$b)

colMeans(gssnoNAp05R08[,1:3])



### compare initial with optimized
# initial parameterization 
optComp=matrix(0,2,9)
colnames(optComp)=colnames(gssnoNAp05R08)

optComp[1,1:3]=c(0.1,0.1,0.001)

TPhat=(Vout[,5]+Vout[,3])*1000/Vout[,6]

modlm=lm(log(TPhat)~log(Vdata$TP))   
modlm_slopetest=lm(log(TPhat)~log(Vdata$TP),offset=1*log(Vdata$TP))

optComp[1,4]=sum(is.na(TPhat))
optComp[1,5:6]=summary(modlm)$coefficients[,1]
optComp[1,7]=summary(modlm)$coefficients[1,4]
optComp[1,8]=summary(modlm_slopetest)$coefficients[2,4]
optComp[1,9]=summary(modlm)$r.squared


# optimized parameterization 
##vectors defining initial states, parameters, and time over which to solve
x0=c(0, 0.005*10000*2, 0.005*10000*2, 1)
times=1:1000

##simulate model for all Vollenweider lakes
VoutOPT=matrix(NA, nrow(Vdata), 9)

for (i in 1:nrow(Vdata)) {
  print(i)
  # Cin set to 0 because we don't know DOC loads
  pars=c(kbg=0, kc=0.42, ka=0.22, I0=600, zmax=Vdata$depth[i], Cin=0, d=0.01, pa=1, ha=80,
         ma=0.003, la=0.1, r=0.6478685, Pin=Vdata$Pin[i], apmax=0.1745694, b=0.003058574, 
         Al=Vdata$sa[i], zsed=0.01, Qin=Vdata$discharge[i], a=-1, c=10, Cp=0.015)
  run=ode(y=x0,times=times,func=Model,parms=pars)
  VoutOPT[i,]=run[nrow(run),-1]
}

colnames(VoutOPT)=c("DOC", "AlgalBiomass", "SRP", "SedimentPhosphorus", "AlgalBiomassP","V", 'LightLim', 'NutLim', 'u')

optComp[2,1:3]=c(0.160420083,0.562745783,0.003869021)

TPhat=(VoutOPT[,5]+VoutOPT[,3])*1000/VoutOPT[,6]

modlm=lm(log(TPhat)~log(Vdata$TP))   
modlm_slopetest=lm(log(TPhat)~log(Vdata$TP),offset=1*log(Vdata$TP))

optComp[2,4]=sum(is.na(TPhat))
optComp[2,5:6]=summary(modlm)$coefficients[,1]
optComp[2,7]=summary(modlm)$coefficients[1,4]
optComp[2,8]=summary(modlm_slopetest)$coefficients[2,4]
optComp[2,9]=summary(modlm)$r.squared



save.image(file="VollenweiderOptimization.RData")
