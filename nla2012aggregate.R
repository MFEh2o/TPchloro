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


###### also generating file with data from NLA 2007 and NSRA 2013-14 used for distributions of forcings for NLA model comparison
NLAiso=read.csv("nla2007_isotopes_wide.csv",header=TRUE)
NLAiso$RT=NLAiso$RT*365  # days
NLAiso=NLAiso[NLAiso$RT>0,] # remove 5 with RT=0

NLAforcingDistributions=data.frame(origin=rep("nla2007_isotopes",nrow(NLAiso)),quantity=rep("RT_days",nrow(NLAiso)),value=NLAiso$RT)

EPAstream=read.csv("nrsa1314_widechem_04232019.csv",header=TRUE)
EPAstream=EPAstream[EPAstream$PTL_RESULT>0,]   # remove 5 measures that equal 0

NLAforcingDistributions=rbind(NLAforcingDistributions,data.frame(origin=rep("nrsa1314_chem",nrow(EPAstream)),
                                                                 quantity=rep("PTL_mgPm3",nrow(EPAstream)),
                                                                 value=EPAstream$PTL_RESULT))

NLAforcingDistributions=rbind(NLAforcingDistributions,data.frame(origin=rep("nrsa1314_chem",nrow(EPAstream)),
                                                                 quantity=rep("DOC_gCm3",nrow(EPAstream)),
                                                                 value=EPAstream$DOC_RESULT))
                              

write.csv(NLAforcingDistributions,"NLAfforcingDistributions.csv",row.names=FALSE)
