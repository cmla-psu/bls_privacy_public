require(tidyverse)

############## Fitting Regression Model For m1emp ################
#set.seed(1)

get_m1empModel = function(df=df4,ROAR=onROAR){
subqwifull=df%>%
  filter(!is.na(sEmpEnd),sEmpEnd!=5,sEmp!=5,ind_level!="A")%>%
  mutate(EmpProp=Emp/EmpEnd) #for fitting model

m1empfit = lm(Emp~EmpEnd+estnum+sector+state,data=subqwifull[-12004,]) #entry 12004 is outlier

### Other Models Tried
# fit1 = lm(EmpProp~estnum+sector+state,
#           data=subqwifull[!is.na(subqwifull$EmpProp)&subqwifull$EmpProp!=Inf,]) #bad diagnostics
#fit1 =lm(sqrt(Emp)~EmpEnd+estnum+sector+state,data=subqwifull) #nonlinear residuals

if(ROAR==FALSE){
  summary(m1empfit)
  par(mfrow=c(1,2))
  plot(x=m1empfit$fitted,y=m1empfit$residuals)
  qqnorm(m1empfit$residuals)
  qqline(m1empfit$residuals)
}
m1empfit
}

## adjusts m1emp to agree with by county total Emp from QWI (i.e. industry="------")
adjust_countytotal_qwi = function(valdf,sumdf){
  sumdf = sumdf%>%
    mutate("stcnty"=str_replace_all(geoindkey,"_.*",""))%>% #get geography code
    select(stcnty,Emp,sEmp)
  
  HasSumIndic <- (sumdf$sEmp==1) #indicator that we have a Emp value
  
  groupeddf = valdf%>%
    mutate("stcnty"=str_replace_all(geoindkey,"_.*",""), #get geography code
           "m1empFromModel"=if_else(m1empFromModel>=1,"Model_or_EarnBeg","QWI_Emp"))%>%
    #filter rows that have Emp at industry="------"
    filter(stcnty %in% sumdf$stcnty[HasSumIndic])%>% 
    select(stcnty,m1emp,m1empFromModel)%>%
    #group by geography and whether the m1emp is from the regression model
    group_by(stcnty,m1empFromModel)%>% 
    summarize("summ1emp"=sum(m1emp),"CellCount"=n())%>% #get sum m1emp & #rows in group
    ungroup()%>%
    #pivot so there is a col for from regression model and from QWI data
    pivot_wider(names_from=m1empFromModel,values_from = c(summ1emp,CellCount))%>%
    mutate("QWI_Emp"=summ1emp_QWI,"Model_or_EarnBeg"=summ1emp_Model)%>% #change column names
    select(-c(summ1emp_QWI,summ1emp_Model,CellCount_QWI))
  
  
  mergedf = full_join(groupeddf, #combine with sumdf
                      sumdf[HasSumIndic,]%>%select(-sEmp),
                      by="stcnty")
  
  # get difference for each county between month 1 employment (across all industries) 
  # and sum of month 1 employments (not from regression model) at naics4 level
  # Emp comes from sumdf, QWI comes from sum naics4 Emp in valdf
  mergedf =mergedf%>%
    mutate("ModelTotal"= Emp-QWI) 
  
  
  valdf=valdf%>%
    mutate("stcnty"=str_replace_all(geoindkey,"_.*",""))%>% #geography code
    full_join(mergedf,by="stcnty",suffix = c("","_agg"))%>% #combine with mergeddf
    filter(!is.na(m1empFromModel))%>% #subset valdf row exists
    #if month 1 employment (Emp) summed over naics4 (from valdf) is 0 or NA
    ##     then proposed m1emp is valdf m1emp (from regression model or given in QWI) 
    ##        adjusted evenly to sum up to Emp in sumdf
    # otherwise
    ##      proposed m1emp is valdf m1emp adjusted proportional to 
    ##        the row's m1emp share of sum m1emp across county (sum from valdf m1emp)
    ##        (i.e. ratio of valdf m1emp over sum m1emp across county)
    mutate("ProposedM1emp"=if_else(QWI==0|is.na(QWI), 
                                   m1emp+(ModelTotal/CellCount_Model),
                                   m1emp+(ModelTotal*m1emp/QWI)))#,0)
  
  #update m1emp with proposed m1emp if... 
  ##      m1emp is from regression model and 
  ##      valdf has Emp for >=1 naics4 row in county group (not from regression model)
  valdf$m1emp[valdf$m1empFromModel==1 & !is.na(valdf$QWI)] = 
    valdf$ProposedM1emp[valdf$m1empFromModel==1 & !is.na(valdf$QWI)]
  valdf$m1emp[valdf$m1emp<0]=0 #force employment positive
  
  
  ### Checking function works
  # checkdf= valdf%>%select(m1emp,ProposedM1emp,stcnty,QWI,Emp,ModelTotal,Model)
  # print(head(checkdf))
  # print(summary(checkdf))
  
  
  round(valdf$m1emp,0) #return rounded m1emp
}


#check proposed value >= stable employment count if stable employment is available
# empvals are proposed values, stablevals are the stable values, 
# stableFlag=NA if missing values from QWI,=1 if EmpS available, !=1 if missing
check_EmpS <-function(empvals,stablevals,stableFlag){
  #if stable employment (EmpS) is not suppressed + fit value is >EmpS -> use fit value
  empfitokay <- is.na(stableFlag)| stableFlag!=1 | (stableFlag==1 & empvals>=stablevals)
  #otherwise use EmpS
  empvals[!empfitokay] = stablevals[!empfitokay]
  empvals
}

get_m1emp<- function(df,m1empmodel,rseed=NA,include_indicator=F){
  if(!is.na(rseed)){ #if random see supplied, set random seed
    set.seed(rseed)
  }
  m1emp = df$Emp # M1 emp is Begining of Q1 measure from QWI
  #If Emp is missing from Q1, use model and EmpS if availiable
  missm1indicator <- df$sEmp!=1 #indicator for begining of Q1 missing value in QWI
  missingsub = df[missm1indicator,] 
  predm1emp = predict(m1empmodel,missingsub,se.fit=TRUE)
  m1empfit=rnorm(sum(missm1indicator),mean=predm1emp$fit,predm1emp$se.fit)
  m1empfit[m1empfit<0] = 0 # force to be non-negative
  
  # ### Old code probably don't need/doesn't work
  # #if stable employment (EmpS) is not suppressed + fit value is >EmpS -> use fit value
  # m1empfitokay <- is.na(missingsub$sEmpS)| missingsub$sEmpS!=1 | (missingsub$sEmpS==1 & m1empfit>=missingsub$EmpS)
  # #otherwise use EmpS
  # m1empfit[!m1empfitokay] = missingsub$EmpS[!m1empfitokay]
  
  m1emp[missm1indicator]=check_EmpS(m1empfit,missingsub$EmpS,missingsub$sEmpS)
  output = round(m1emp,0)
  if(include_indicator==T){
    output=cbind(output,missm1indicator) #returns m1emp value, Flag for Imputed
  }
  output
}


#m2emp is used for finding wage eventually
get_m2emp = function(m1emp,m3emp,stabval,stabF,noisecoef=0.5,rseed=NA){
  if(!is.na(rseed)){ #if random see supplied, set random seed
    set.seed(rseed)
  }
  
  m2emp =rep(0,length(m1emp)) #initialize m2emp as 0
  
  #
  ### if m1emp and m3emp are zero, then m2emp is 0. Otherwise...
  #
  nonzeroindic = (m1emp>0)|(m3emp>0) #indicator that m1emp or m3emp is nonzero
  m1emp_nz=m1emp[nonzeroindic]
  m3emp_nz=m3emp[nonzeroindic]
  
  #noise variance is proportional to 
  # distance between employment values relative to the mean employment value
  # scaled by noisecoef
  noisesd = sqrt((noisecoef*2*abs(m1emp_nz-m3emp_nz))/(m1emp_nz+m3emp_nz))
  changeFromMid = rnorm(length(m1emp_nz),0,noisesd) #normal noise rvs
  m2emp_nz=m1emp_nz+((m3emp_nz-m1emp_nz)/2)+changeFromMid #middle of m1emp,m3emp + noise
  
  # #Check lengths are okay
  # cat("Length NZ",sum(nonzeroindic),
  #     "\n of m1emp=",length(m1emp_nz),
  #     "of m2emp=",length(m2emp_nz),
  #     "of m3emp=",length(m3emp_nz))
  
  #
  ###
  #
  m2emp[nonzeroindic] = m2emp_nz
  # force non-negative. If m1emp and m2emp !=0 then force m2emp=1
  m2emp[m2emp<0]=ifelse(m1emp[m2emp<0]==0|m3emp[m2emp<0]==0,0,1) 
  m2emp = check_EmpS(m2emp,stabval,stabF) #check if fits stability restriction
  round(m2emp,digits=0) #round to integer
}

##get m1emp, m2emp, and m3emp at county by 4-digit NAICS level
##m2emp noise coefficient is arbitrarily choosen currently
get_employmentCounts4<-function(df4,
                                m1emp_model=m1empfit,
                                m2emp_noisecoef=1,
                                rseed=NA,include_m1emp_indicator=F){
  if(!is.na(rseed)){ #if random seed supplied, set random seed
    set.seed(rseed)
  }
  #Indicator that QWI EmpEnd is availiable (i.e. not suppressed and QWI had match)
  qwiEmpEndAvailiable <- (!is.na(df4$sEmpEnd))&(df4$sEmpEnd==1) 
  
  #if QWI EmpEnd is not availiable then use Imputted CBP data
  df4$EmpEnd[!qwiEmpEndAvailiable]=df4$emp[!qwiEmpEndAvailiable]
  
  #get monthly employments
  if(include_m1emp_indicator==T){ 
    m1empAndFlag = get_m1emp(df=df4,m1empmodel=m1emp_model,include_indicator=T)
    m1emp = m1empAndFlag[,1]
    m1empFlag = m1empAndFlag[,2]
  }
  m3emp = df4$EmpEnd
  m2emp = get_m2emp(m1emp,m3emp,df4$EmpS,df4$sEmpS,noisecoef = m2emp_noisecoef)
  
  empMat = cbind(df4$geoindkey,m1emp,m2emp,m3emp,m1empFlag) #matrix monthly employments
  colnames(empMat)=c("geoindkey","m1emp","m2emp","m3emp","m1empFromModel")
  empMat
}

#empMat = get_employmentCounts4()
