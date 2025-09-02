investigate_NAwage = function(df=dfsave){
  #get dataframe of 6 digits naics levels
  df6<- df%>%
    filter(grepl("_[0-9]{6}",geoindkey))%>%
    mutate("geo4naics"=str_sub(geoindkey,start=1,end=-3),
           "geo5naics"=str_sub(geoindkey,start=1,end=-2))%>%
    select(geoindkey,geo4naics,geo5naics,state,cnty,estnum,qp1,qp1_nf,emp)
  
  
  
  
  
  df4 = df[df$industry!="------",]%>% #exclude "Total Across Industries Level"
    filter(grepl("^[0-9]{4}[^0-9]{2}",industry))%>% #subset to NAISC4 Level
    mutate("sector" = as.factor(str_sub(industry,start=1,end=2)), #add sector factor
           "state"=as.factor(str_sub(geography,end=-4))) #make state a factor
  df4 =df4 %>%
    mutate("geo4naics"=str_sub(geoindkey,start=1,end=-3),
           "geo3naics"=str_sub(geoindkey,start=1,end=-4))%>%
    left_join(count6dig,by="geo4naics")%>%
    mutate("wagediff"=qp1-wageCBP_sum6by4)
  
  
  
  m1empfit = get_m1empModel(df=df4,ROAR=onROAR)
  
  
  empMat = get_employmentCounts4(df4=df4,m2emp_noisecoef = 1.69,include_m1emp_indicator=T)
  
  ##look at first 6 and last 7 entries
  #empMat[c(seq(1,6),seq(length(empMat[,1])-6,length(empMat[,1]))),]
  
  
  adjustdf=data.frame(empMat)%>%mutate_if(grepl("m",colnames(empMat)),as.numeric)
  
  #adjustdf$geoindkey = df4$geoindkey
  adjustm1emp = adjust_countytotal_qwi(valdf=adjustdf,
                                       sumdf = df%>%filter(industry=="------"))
  empMatA=empMat
  empMatA[,2]=adjustm1emp
  #save(empMatA,m1empfit,file=paste0(folderout,"EmpImputeCheckpoint.RData"))
  
  set.seed(1)
  
  wagefit.sub = get_wageModel(df=df4,empMatAdj=empMatA,ROAR=onROAR)
  
  set.seed(1)
  #combine into matrix
  empMatwage=cbind(as.numeric(empMatA[,2]),as.numeric(empMatA[,3]),as.numeric(empMatA[,4])) 
  colnames(empMatwage)=c("m1emp","m2emp","m3emp") #name columns
  
  wages_maxmin=get_maxmindf(df4dig=df4,fulldf=df,empMatAdj=empMatA)
  wagesout=get_wages4(df=df4,empmat=empMatwage,wagemodel=wagefit.sub,useEarnQWI = T,maxmindf=wages_maxmin) #impute wages
  #sum(is.na(wagesout$wage)) #No NA wages
  
  
    #repeat all prior code with df=dfsave
    df4imp<-wagesout%>%mutate("EmpScale"=if_else(emp/m3emp==Inf,1,emp/m3emp),
                              "geo4naics"=str_sub(geoindkey,start=1,end=-3))
    df4imp$m1empFromModel=as.numeric(empMatA[,5])
    
    ###### PROBLEM state 29, cnty 189, industry 5259 has 1 establishment 
    # and no values for any NAICS5 or NAICS6 levels
    print(df4imp%>%filter(is.na(wage)))
    
    ## Generate the last 2 digits of the industry code
    
    #first level which contains wage information is NAICS sector (industry==51---)
    count2dig_prob=get_codes_summary(groupbydigits = 2,levelgrouped = 4)%>%
      filter(geo2naics=="29189_52")  
    #sum without missing levels is  791,515, there are 2 naics4 levels missing qp1
    
    #Find the two missing codes
    dfcodesin29189_52 = df4%>%filter(grepl("29189_52[0-9]{2}//",geoindkey))
    print(dfcodesin29189_52%>%filter(qp1_nf=="D"))
    # two out of 9 codes have suppressed qp1 (wages from CBP): 29189_5211 and 29189_5259
    # However 29189_5211 has EarnBeg from QWI data.
    
    indicatorsprob = grepl("29189_52",df$geoindkey)
    
    print(
      wagesFromQWI(df=df%>%filter(geoindkey %in% c("29189_5211//","29189_5259//")),
                 empmat = empMatwage[empMatA[,1]%in% c("29189_5211//","29189_5259//"),]))
    #Using QWI, 29189_5211// has wage of 249.186, and 29189_5259// does not have EarnBeg value
    
    sectorlevprob = df%>%filter(grepl("29189_52----",geoindkey)) #naics sector qp1=791713
    print(sectorlevprob$qp1-count2dig_prob$wageCBP_sum4by2) #=198
    #the imputed wage for 29189_5211// is greater than the 
    #   difference between sector qp1 and the sum of known qp1 naics4 values.
    #   Therefore we force 29189_5259// to be 0
    print(df%>%filter(grepl("29[0-9]{3}_5259[0-9]{2}",geoindkey))%>%
      mutate("naics6"=str_sub(geoindkey,start=7,end=-1))%>%
      select(naics6)%>%table())
    #525990 is a more common NAICS6 code in stat 29 within 5259 NAICS4 codes
    
    print("Conclusion: set qp1=0,emp=1,estnum=1 for 29189_525990 in df")
  
}