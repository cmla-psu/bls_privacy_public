require(tidyverse)

########### WAGE VALUES ##################

#adjust wage values to match max and minimum possible
adjust_wagevalues<-function(fitwagedf=wagesout,dfmaxmin=wagedf4_maxmin){
  maxmindf = dfmaxmin %>%select(geo4naics,minwage,maxwage)
  fitwagedf = fitwagedf%>%left_join(maxmindf,by="geo4naics")
  fitwagedf = fitwagedf%>%
    mutate("wage"=if_else(wage<minwage,minwage,wage))%>%
    mutate("wage"=if_else(wage>maxwage,maxwage,wage))
  fitwagedf%>%
    select(-c(minwage,maxwage))
}


#using QWI EarnBeg if available (in case qp1 not available)
wagesFromQWI<-function(df,empmat){
  wages=rowSums(empmat)*df$EarnBeg
  wages
}

#get wage from linear regression model
wagesFromModel<-function(df,modelwage=wagemodel){
  #fit wages using model
  predwage = predict(modelwage,df,se.fit=TRUE)
  #cat("Predicted fit for 29189_5259// is ",predwage$fit[df$geoindkey=="29189_5259//"],
  #    "and the standard erro is ",predwage$se.fit[df$geoindkey=="29189_5259//"])
  wagefit=(rnorm(length(df$emp),mean=predwage$fit,predwage$se.fit))^2
  
}


############ Get Wage Model Fit ##########

#fit a linear reg model to predict wage
get_wageModel = function(df=df4,empMatAdj=empMatA,ROAR=onROAR){
#dataframe to fit model
wagedf4<-cbind(df%>%select(-geoindkey),
               data.frame(empMatAdj)%>%
                 mutate_if(grepl("m",colnames(empMatAdj)),as.numeric))%>%
  filter(!is.na(qp1_nf) & qp1_nf!="D" )%>%filter(!is.na(wagediff))


wagefit<-lm(sqrt(wagediff)~wageCBP_missing6by4*m3emp+
              poly(m3emp,3)+state+
              sector,data=wagedf4)

#### Other Models Considered
# wagefit<-lm(sqrt(qp1)~poly(estnum,2)+m3emp*estnum+poly(m3emp,3)+state+
#               sector,data=wagedf4)
#wagefit2<-lm(sqrt(qp1)~poly(estnum,2)+emp*estnum+poly(m3emp,3)+state+
#              sector,#+m1emp*m1empFromModel+emp,
#            data=wagedf4)
#wagefit2<-lm(sqrt(qp1)~poly(estnum,2)+emp*estnum+poly(m3emp,3)+state+
#              sector,data=wagedf4.sub)
#wagefit<-lm(qp1~estnum*m1emp+poly(m1emp,2)+poly(m3emp,2)+state+sector,data=wagedf4) #seems overfitted
#wagefit<-lm(sqrt(qp1)~estnum+m1emp+m3emp+state+sector,data=wagedf4) #residual vs fitted not good
#wagefit<-lm(qp1~estnum+m1emp*m1empFromModel+m3emp+state+sector,data=wagedf4) #not better fit


#remove outliers and refit
### Outliers identified in plots
### par(mfrow=c(2,2))
### plot(wagefit)
wagedf4.sub<-wagedf4[-c(92405,95067,166023,16180,24236),]
wagefit.sub<-lm(sqrt(wagediff)~wageCBP_missing6by4*m3emp+
                  poly(m3emp,3)+state+
                  sector,data=wagedf4.sub)


if(ROAR==FALSE){
  summary(wagefit.sub)
  par(mfrow=c(2,2))
  plot(wagefit.sub)
  
  
  par(mfrow=c(1,2))
  plot(x=wagefit.sub$fitted,y=wagefit.sub$residuals)
  qqnorm(wagefit.sub$residuals)
  qqline(wagefit.sub$residuals)
}
wagefit.sub
}

####

#get wages at the NAICS4 level
get_wages4<-function(df4,empmat,wagemodel,useEarnQWI=F,count6digdf=count6dig,maxmindf=wage_maxmin){
  #If qp1 in CBP availiable, use this
  qp1Availiable <- df4$qp1_nf!="D"
  
  df4$wage = df4$qp1
  useModel <- !qp1Availiable
  
  if(useEarnQWI==T){
    print("Number which uses QWI:",end=" ")
    EarnBegAvailiable <- df4$sEarnBeg==1 #indicator that EarnBeg is available
    useQWI <- (EarnBegAvailiable)&(!qp1Availiable) #use EarnBeg if available and qp1 missing
    print(sum(useQWI))
    df4$wage[useQWI]=wagesFromQWI(df4[useQWI,],empmat[useQWI,])
    useModel <- (!EarnBegAvailiable)&(!qp1Availiable) #useModel if EarnBeg and qp1 unavailable
   }
  
  df4=cbind(df4,empmat)
  print("Number of cells which use Model:",end=" ")
  print(sum(useModel))
  df4$wage[useModel] <- wagesFromModel(df4[useModel,],wagemodel)
  df4$usewageModel<-ifelse(useModel==T,1,0) #indicator that model is used
  if(useEarnQWI==T){df4$usewageModel[useQWI]=2}
  df4 = adjust_wagevalues(fitwagedf=df4,
                          dfmaxmin =maxmindf) 
  df4$wage = round(df4$wage)
  df4
}


#get max wage value for NAICS4 level based on NAICS3 and NAISC sector values
get_wagemax<-function(codes4naics=wagedf4$geoindkey,fulldf=df){
  #initialize dataframe to be outputted
  outdf=data.frame("geoindkey"=codes4naics,"maxwage"=NA,
                   "geo3naics"=str_sub(codes4naics,start=1,end=-4),
                   "geo2naics"=str_sub(codes4naics,start=1,end=-5),
                   "geography"=str_sub(codes4naics,start=1,end=-8))
  
  #get max values for naics3 level (by county)
  tomergedf3 = fulldf%>%filter(grepl("_[0-9]{3}[^0-9]{3}",geoindkey))%>%
    mutate("geo3naics"=str_sub(geoindkey,start=1,end=-4),
           "maxwage3"=if_else(qp1_nf=="D",NA,qp1))%>%
    select(geo3naics,estnum,maxwage3)
  #join with initialized data.frame by naics3 code
  outdf = outdf%>%left_join(tomergedf3,by="geo3naics",suffix=c("","_naics3"))
  
  #sector codes for rows without naics3 wage value
  notmaxcodes = outdf$geo2naics[is.na(outdf$maxwage3)] 
  
  ### get maximum geo2naics for missing wages at naics3 level
  #data subset to sector level wage and establishment number values (by county)
  tomergedf2 = fulldf%>%
    mutate("geo2naics"=str_sub(geoindkey,start=1,end=-5))%>% #sector code
    filter(geo2naics %in% notmaxcodes)%>% #keep only codes missing wages at naics3
    filter(grepl("_[0-9]{2}[^0-9]{4}",geoindkey))%>% #keep only sector rows
    mutate("wage2"=if_else(qp1_nf=="D",NA,qp1))%>% #if qp1 missing, maxwage=NA
    select(geo2naics,estnum,wage2)
  
  #df of wages at naics3 level summed to the sector level (by county)
  tomergedf3 = tomergedf3%>% 
    mutate("geo2naics"=str_sub(geo3naics,start=1,end=-2))%>% #sector codes
    group_by(geo2naics)%>%
    summarize("summaxwage3"=sum(maxwage3,na.rm=T))%>%ungroup()
  
  #combine sum naics3 data and sector level data
  tomergedf2 = tomergedf2%>%left_join(tomergedf3,by="geo2naics")%>%
    #maxwage2 is remainder of sector wage by county unaccounted for 
    #(i.e. difference between known naics3 sum and known naics2)
    mutate("maxwage2"=wage2-summaxwage3)%>% 
    select(geo2naics,maxwage2,estnum,wage2)
  
  #if maxwage not filled in with naics3 value, fill with sector value (by county)
  outdf = outdf%>%left_join(tomergedf2,by="geo2naics",suffix=c("","_naics2"))%>%
    mutate("maxwage"=if_else(is.na(maxwage3),maxwage2,maxwage3))%>%
    select(-wage2)
  
  
  max_allind_allcounty=max(df$qp1[df$ind_level==4],na.rm=T) #max county level val
  notmaxcodes = outdf$geography[is.na(outdf$maxwage)] #geography codes w/out maxwage
  
  ### get max from county level totals if no max found from naics3 and sector totals
  #df of wages by county (across all industries)
  tomergedfall = fulldf%>% 
    mutate("geography"=str_sub(geoindkey,start=1,end=-8))%>%
    filter(geography %in% notmaxcodes)%>%
    filter(grepl("_------",geoindkey))%>%
    mutate("geography"=str_sub(geoindkey,start=1,end=-8),
           #wageall is county wage total if known,& max observed county total o.w.
           "wageall"=if_else(qp1_nf=="D",max_allind_allcounty,qp1))%>% 
    select(geography,estnum,wageall)
  #df of wages at sector level summed to the county level
  tomergedf2 = tomergedf2%>% 
    mutate("geography"=str_sub(geo2naics,start=1,end=-4))%>% #sector codes
    group_by(geography)%>%
    summarize("summaxwage2"=sum(wage2,na.rm=T))%>%ungroup()
  #combine sum sector data and county level data
  tomergedfall = tomergedfall%>%left_join(tomergedf2,by="geography")%>%
    #maxwage2 is difference between known sector sum and known county
    mutate("maxwageall"=wageall-summaxwage2)%>% 
    select(geography,maxwageall,estnum,wageall)
  
  outdf = outdf%>%
    left_join(tomergedfall,by="geography",suffix=c("","_allindustry"))%>%
    mutate("maxwage"=if_else(is.na(maxwage),maxwageall,maxwage))%>%
    select(geoindkey,maxwage)
  
  outdf
}

#get min wage value for NAICS4 level based on NAICS6 values
get_wagemin<-function(codes4naics=wagedf4$geoindkey,fulldf=df){
  
  tomerge6dig = get_codes_summary(dfin=fulldf,groupbydigits=4,levelgrouped=6)%>%
    mutate("minwage"=if_else(is.na(wageCBP_sum6by4),0,wageCBP_sum6by4),
           "geoindkey"=paste0(geo4naics,"//"))%>%
    select(geoindkey,minwage)
  tomerge6dig
  
}

#get max and min for NAICS4 level wage based on other aggregate levels
get_maxmindf = function(df4dig=df4,fulldf=df,empMatAdj=empMatA){
  
  wagedf4<-cbind(df4dig%>%select(-geoindkey),
                 data.frame(empMatAdj)%>%
                   mutate_if(grepl("m",colnames(empMatAdj)),as.numeric))
  maxwagedf = get_wagemax(codes4naics = wagedf4$geoindkey,fulldf=fulldf)
  minwagedf = get_wagemin(codes4naics = wagedf4$geoindkey,fulldf=fulldf)
  wagedf4_maxmin = wagedf4 %>% left_join(maxwagedf,by="geoindkey")%>%
    left_join(minwagedf,by="geoindkey")%>%
    mutate("minwage"=if_else(is.na(minwage),0,minwage))
  wagedf4_maxmin
}


#empMat = get_employmentCounts4(df4=df4,m2emp_noisecoef = 1.69,include_m1emp_indicator=T)

##look at first 6 and last 7 entries
#empMat[c(seq(1,6),seq(length(empMat[,1])-6,length(empMat[,1]))),]


#adjustdf=data.frame(empMat)%>%mutate_if(grepl("m",colnames(empMat)),as.numeric)

#adjustdf$geoindkey = df4$geoindkey
#adjustm1emp = adjust_countytotal_qwi(valdf=adjustdf,
#                                     sumdf = df%>%filter(industry=="------"))
#empMatA=empMat
#empMatA[,2]=adjustm1emp

