setwd("~/Downloads/")

##  csv's downloaded from https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys
##  2022-03-29
wc=read.csv("nla2012_waterchem_wide.csv",header=TRUE)
si=read.csv("nla2012_wide_siteinfo_08232016.csv",header=TRUE)
kv=read.csv("nla12_keyvariables_data.csv",header=TRUE)
chla=read.csv("nla2012_chla_wide.csv",header=TRUE)

range(wc$PTL_RESULT)
range(kv$PTL_RESULT)
unique(wc$PTL_UNITS)

range(wc$DOC_RESULT)
unique(wc$DOC_UNITS)

range(si$AREA_HA)

range(kv$CHLX_RESULT,na.rm=TRUE)
unique(kv$CHLX_UNITS)
range(chla$CHLX_RESULT,na.rm=TRUE)
unique(chla$CHLX_UNITS)

range(phab$DEPTH_AT_STATION,na.rm=TRUE)
unique(phab$DEPTH_UNIT)

NLAdepth=phab$DEPTH_AT_STATION
NLAdepth[phab$DEPTH_UNIT=="ft"]=phab$DEPTH_AT_STATION[phab$DEPTH_UNIT=="ft"]*0.3048

sites=unique(kv$SITE_ID)

nla=data.frame(SITE_ID=sites,chla=NA,DOC=NA,TP=NA,depth=NA,area=NA)
for(i in 1:nrow(nla)){
  curKV=kv[kv$SITE_ID%in%nla$SITE_ID[i],]
  curWC=wc[wc$UID%in%curKV$UID,]
  curSI=si[si$UID%in%curKV$UID,]
  
  nla$chla[i]=mean(curKV$CHLX_RESULT,na.rm=TRUE)
  nla$DOC[i]=mean(curWC$DOC_RESULT,na.rm=TRUE)
  nla$TP[i]=mean(curKV$PTL_RESULT,na.rm=TRUE)
  nla$depth[i]=mean(curKV$INDEX_SITE_DEPTH,na.rm=TRUE)
  nla$area[i]=mean(curSI$AREA_HA,na.rm=TRUE)*1e4
}

write.csv(nla,"nla2012aggregated_2022-03-29.csv",row.names=FALSE)