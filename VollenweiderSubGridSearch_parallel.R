####  running optimization of smodel with vollenweider P loading data  in parallel environment
####  SEJ

require(deSolve,lib.loc="~/myRlibs")

##### wrapper function that defines model, loads Vollenweider data, and optimizes b
##### for a given apmax value; will be used with mclapply

wrapper<-function(x){

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


  store=numeric(9)
  
  ## store information on optimized parameters and performance relative to Vollenweider data
  store[1:3]=x

  # simulate and count number of NA lakes
  x0=c(0, 0.005*10000*2, 0.005*10000*2, 1)
  times=1:1000
  
  out=matrix(NA, nrow(Vdata), 9)
  
  for (i in 1:nrow(Vdata)) {
    pars=c(kbg=0, kc=0.42, ka=0.22, I0=600, zmax=Vdata$depth[i], Cin=0, d=0.01, pa=1, ha=80,
           ma=0.003, la=0.1, r=x[2], Pin=Vdata$Pin[i], apmax=x[1], b=x[3], 
           Al=Vdata$sa[i], zsed=0.01, Qin=Vdata$discharge[i], a=-1, c=10, Cp=0.015)

    run=ode(y=x0,times=times,func=Model,parms=pars)
    out[i,]=run[nrow(run),-1]
  }
  
  TPhat=(out[,5]+out[,3])*1000/out[,6]
  
  modlm=lm(log(TPhat)~log(Vdata$TP))   
  modlm_slopetest=lm(log(TPhat)~log(Vdata$TP),offset=1*log(Vdata$TP))
  
  store[4]=sum(is.na(TPhat))
  store[5:6]=summary(modlm)$coefficients[,1]
  store[7]=summary(modlm)$coefficients[1,4]
  store[8]=summary(modlm_slopetest)$coefficients[2,4]
  store[9]=summary(modlm)$r.squared

  return(store)
}


#### need a list with  apmax values
N=10000
params=list(c(runif(1,0.125,0.175),runif(1,0.45,0.65),runif(1,0.0025,0.0075)))
for(i in 2:N){
  params[[i]]=c(runif(1,0.125,0.175),runif(1,0.45,0.65),runif(1,0.0025,0.0075))
}

require(parallel)

output=mclapply(params,wrapper,mc.cores=24)

output.df=as.data.frame(do.call(rbind,output))
colnames(output.df)=c('apmax','r','b','numNA','intercept','slope','intercept_pvalue','slope_pvalue','R2')
  
write.csv(output.df,"VollenweiderSubGridSearch.csv",row.names=FALSE)


