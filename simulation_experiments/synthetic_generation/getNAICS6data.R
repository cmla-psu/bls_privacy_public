library(tidyverse)
library(MCMCprecision)

set.seed(1)
onROAR=TRUE
investigate_wageNA=F

folderout=paste0(getwd(),"/Get4and6NAICS_R_output/") #output folder

funcfolder = paste0(getwd(),"/NAICS6_Rfunctions/") #.R files with functions folder

# import functions from .R files
source(paste0(funcfolder,"WageFunctions.R"))
source(paste0(funcfolder,"EmploymentFunctions.R"))
source(paste0(funcfolder,"getAggLevelSummaries.R"))

####################### Dataframes Set Ups #######################
#read in combined data (from python)
df = read.csv(paste0(getwd(),"/PythonPreprocessOut/combineQWIandCBP.csv"),
              stringsAsFactors = FALSE)
df$year = 2016 #set year
df$quarter = 1 #set quarter
df = df[,2:length(colnames(df))] #remove column of index values

dfsave=df #save original copy of df

#if you want the justification for correcting "29189_525990" and "29189_5259//"
if(investigate_wageNA==T){ 
  source(paste0(funcfolder,"investigate_NAwage_problem.R"))
  investigate_wageNA(df=dfsave) #This will print the justification
}

#correcting "29189_5259//" and adding "29189_525990".  
##  b/c df has 29189_5259// entry but not any corresponding naics6 entries.
df = rbind(df, #add 29189_525990 to df
           data.frame("state"=29,"cnty"=198,"emp"=1,"geoindkey"="29189_525990",
                      "qp1_nf"="Impute","qp1"=0,"estnum"=2,"geo_level"="C",
                      "geography"=29189,"ind_level"=6,"industry"="525990",
                      "year"=2016,"quarter"=1,
                      "EarnBeg"=NA,"EarnHirAS"=NA,"Emp"=NA,"EmpEnd"=NA,"EmpS"=NA,
                      "sEarnBeg"=NA,"sEarnHirAS"=NA,"sEmp"=NA,"sEmpEnd"=NA,"sEmpS"=NA))
#force qp1=0 to be treated as unsuppressed since justification suggests wage=0 is reasonable
df$qp1_nf[grepl("29189_5259//",df$geoindkey)]="Impute" 


#get dataframe of 6 digits naics levels by county
df6<- df%>%
  filter(grepl("_[0-9]{6}",geoindkey))%>%
  mutate("geo4naics"=str_sub(geoindkey,start=1,end=-3),
         "geo5naics"=str_sub(geoindkey,start=1,end=-2))%>%
  select(geoindkey,geo4naics,geo5naics,state,cnty,estnum,qp1,qp1_nf,emp)


# get summary of naics6 values when grouped by naics4 value by county.
## this includes # of naics6 by county in each naics4 by county, 
## and # of naics6 by county missing qp1 values, 
## and sum of known naics6 by county values for each naics4 by county
count6dig=get_codes_summary(dfin=df,groupbydigits = 4,levelgrouped=6)


#get naics4 by county dataframe
df4 = df[df$industry!="------",]%>% #exclude "Total Across Industries Level"
  filter(grepl("^[0-9]{4}[^0-9]{2}",industry))%>% #subset to NAISC4 Level
  mutate("sector" = as.factor(str_sub(industry,start=1,end=2)), #add sector factor
         "state"=as.factor(str_sub(geography,end=-4)),
         "geo4naics"=str_sub(geoindkey,start=1,end=-3),
         "geo3naics"=str_sub(geoindkey,start=1,end=-4))%>%
  left_join(count6dig,by="geo4naics")%>% #join with summary df
  #difference in naics4 by county qp1 value and sum of known naics6 by county qp1 values
  mutate("wagediff"=qp1-wageCBP_sum6by4) 
#this difference was should be sum of the unknown qp1 once we impute them (or close to)

######################## Employment Counts #############################

#get model of Emp (to become m1emp when emp from CBP is missing) 
## based on EmpEnd (from QWI data), number of establishments, state, and naics sector
## if onROAR=F diagnostic plots and summary tables will be produced
m1empfit=get_m1empModel(df=df4,ROAR=onROAR)


#m2emp noise coefficient arbitrarily chosen
# to get m2emp between m1emp and m3emp (to be used in wage imputation)
empMat = get_employmentCounts4(df4=df4,m2emp_noisecoef = 1.69,include_m1emp_indicator=T)

#initialize dataframe of geoindkey, m1emp,m2emp, and m3emp 
## to adjust m1emp to be in agreement with county totals
adjustdf=data.frame(empMat)%>%mutate_if(grepl("m",colnames(empMat)),as.numeric)
adjustm1emp = adjust_countytotal_qwi(valdf=adjustdf,
                                     sumdf = df%>%filter(industry=="------"))
empMatA=empMat #initialize matrix array of adjusted employment counts
empMatA[,2]=adjustm1emp #replace existing m1emp with adjusted m1emp

## Optional: save checkpoint for imputed m1emp,m2emp,m3emp
#save(empMatA,m1empfit,file="EmpImputeCheckpoint.RData")


###################### Wages #########################
set.seed(1)
#load("EmpImputeCheckpoint.RData")


#get model to predict wage
wagefit.sub = get_wageModel(df=df4,empMatAdj = empMatA,ROAR=onROAR)

# get dataframe of the maximum and minimum wage values for each naics4 by county value
# based on naics3/2 by county and naics6 by county values
wage_maxmin = get_maxmindf(df4dig=df4,fulldf=df,empMatAdj = empMatA)


#combine employments into matrix
empMatwage=cbind(as.numeric(empMatA[,2]),as.numeric(empMatA[,3]),as.numeric(empMatA[,4])) 
colnames(empMatwage)=c("m1emp","m2emp","m3emp") #name columns

#get wages at naics4 by county level
wagesout=get_wages4(df=df4,empmat=empMatwage,wagemodel=wagefit.sub,useEarnQWI = T) #impute wages

# Optional Check for NA wages
#sum(is.na(wagesout$wage))


#combine geoindkey, employment counts, and m1empModel indicator 
##  with wages and wageModel indicator. 
##  add county by naics4 code (without slashes), 
##  add ratio of emp (from imputed CBP) over our imputed m3emp
empMatwageA<-cbind(empMatA,wagesout) 
colnames(empMatwageA)=c(colnames(empMatA),"wage","usewageModel")
df4imp<-wagesout%>%mutate("EmpScale"=if_else(emp/m3emp==Inf,1,emp/m3emp),
                          "geo4naics"=str_sub(geoindkey,start=1,end=-3))
df4imp$m1empFromModel=as.numeric(empMatA[,5])


## Optional save df4imp checkpoint
#save(df4imp,file="NAICS4imputeCheckpoint2.RData")


############################ Get NAICS6 By County Aggregates ###################
set.seed(1)
#load("NAICS4imputeCheckpoint2.RData")


#scale emp from CBP imputted data at NAICS 6-digit level to the value that corresponds
# to m3emp at 4-digit NAICS level found using QWI data
get_m3emp6_all <-function(df=df6,df4n=df4imp){
  sub4df = df4n%>%mutate("geo4naics"=str_sub(geoindkey,start=1,end=-3))%>%
    select(geo4naics,EmpScale) #data of just ratio and naics4 by county code

  dfout = df%>% #join naics6 by county with the sub4df above
    inner_join(sub4df,by="geo4naics")%>%
    mutate("m3emp"=round(emp/EmpScale)) #set m3emp to be scaled emp value
  #(see justification for inner_join rather than left_join in block below)
  
  dfout$m3emp[dfout$EmpScale==Inf]=0
  dfout$m3emp[is.na(dfout$EmpScale)]=dfout$emp[is.na(dfout$EmpScale)]
  dfout%>%select(-EmpScale)
}

justify_inner_in_m3emp6_all <- function(df=df6,df4n=df4imp){
  sub4df = df4n%>%mutate("geo4naics"=str_sub(geoindkey,start=1,end=-3))%>%
    select(geo4naics,EmpScale) #data of just ratio and naics4 by county code
  
  #when using left_join to make df6step1
  testdf = df%>% #join naics6 by county with the sub4df above
    left_join(sub4df,by="geo4naics")%>%
    mutate("m3emp"=round(emp/EmpScale)) 
  
  print(sum(is.na(testdf$Count6Codes)))
  print(glimpse(testdf))
  cat("\nThere are 11,5348 missing naics4 by county codes from df4imp",
      "that are in df6step1 out of 670,406 (i.e. 1.7%).",
      "Thus it is reasonable to cut the rows of df6step1",
      "to be those that match df4imp naics4 codes.")
  names(which.max(table(testdf$geo4naics)))
}


### OPTIONAL: To see inner_join justification
# justify_inner_in_m3emp6_all()


# get dirichlet parameters for splitting m1emp values
# these parameters are equal to the m3emp value for each naics6 by county code
dirichletparams_m1emp<-function(sub6=subdf6){
  #row1=sub6$estnum ## Could use establishment number as well?
  row2=sub6$m3emp
  if(sum(row2)==0){ #at least one parameter needs to be positive
    row2=rep(1,length(row2))
  }
  #rbind(row1,row2)
  row2
}



#get m1emp based on 4-digit m1emp and m3emp
get_m1emp6_per4 <-function(df=df6,df4n=df4imp,rseed=NA){
  if(!is.na(rseed)){set.seed(rseed)} #set seed if specified
  
  
  m1empparams=dirichletparams_m1emp(df) #get m1emp parameters
  #get proportions for each naics6 by county code based on m1emp parameters
  rprops=rdirichlet(1,m1empparams) 
  
  if(nrow(df)>1){ #if more than one naics6 x county w/in a specified naics4 x county
    # split the naics4 x county m1emp value proportional to those generated in rprops
    df = df%>%mutate("m1emp"=as.numeric(t(round(rprops*df4n$m1emp)))) 
  }else if(nrow(df)==1){ #if only one naics6 x county code w/in naics4 x county
    df = df%>%mutate("m1emp"=df4n$m1emp) #m1emp equal at naics6 & naics4 by county
  }
  df
}


# get proportions for naics6 x county share of wages w/in a naics4 x county
dirichletparams_wage<-function(sub6=subdf6){
  row1 = sub6$m3emp
  #row2 = sub6$estnum #Could use restnum?
  if(sum(row1)==0){
    row1=rep(1,length(row1))
  }
  #rbind(row1,row2)
  row1
}

# for a given 4-digit naics code get the remainder for wages between known qp1 at 
#6-digit level and wage from df4imp
#return remainder of wages, number of establishments
get_wage6_per4<-function(subdf6=df6,subdf4=df4imp,rseed=NA){
  if(!is.na(rseed)){set.seed(rseed)}
  
  #NAICS4 total wage minus sum of known NAICS6 total wage (by county)
  remain_wage = subdf4$wage-sum(subdf6$qp1,na.rm=T) 
  
  if(length(remain_wage)>0){ #if there is atleast one wage value
    if(remain_wage<0){ #if remainder of wages<0, we have a problem to investigate
      print("WARNING: remainders are negative!")
      cat("Remainders: wage",remain_wage," Codes:",paste(subdf6$geoindkey,collapse=","))
    }
  }
  
  subdf6 = subdf6 %>%mutate("wage"=qp1) #add wage column
  unknown_indic <- subdf6$qp1_nf=="D" #indicator that wages is missing from CBP
  
  if(nrow(subdf6[unknown_indic,])==1 ){ #if only missing one 6-digit aggregate
    subdf6$wage[unknown_indic]=remain_wage #wage is simply the remainder
  }else{ #otherwise use dirichlet distribution
    subdf6unknown=subdf6[unknown_indic,]
    rprop = rdirichlet(1,dirichletparams_wage(sub6=subdf6unknown))
    subdf6$wage[subdf6$qp1_nf=="D"]=round(remain_wage*rprop)
  }
  subdf6
  
}


# get naics6 by county for a specific naics4 by county
## NOTE: df6=df should already have m3emp adjusted by get_m3emp6_all()
get_6naics_per4<-function(naics4dig,df6=df,df4imp=df4n,rseed=NA){
  if(!is.na(rseed)){set.seed(rseed)}
  
  #subset dataframes to focus on one 4-digit naics by county code
  subdf6 = df6%>%filter(geo4naics==naics4dig)
  subdf4 = df4imp%>%filter(geo4naics==naics4dig)
  
  #get m1emp
  subdf6emp = get_m1emp6_per4(df=subdf6,df4n=subdf4,rseed=rseed)
  if(length(subdf6emp)==0){ #if no naics6 x county codes in naics4 x county
    subdf6wage=subdf6%>%mutate("m1emp"=NA,"m3emp"=NA,"wage"=NA) #set naics6 x county values =NA
  }else{
    #get wage
    subdf6wage = get_wage6_per4(subdf6=subdf6emp,subdf4=subdf4)
  }
  
  subdf6wage%>%select(-c(qp1,qp1_nf,emp,geo5naics))
  
}


# # #test function for various specified naics4 x county codes
# # #get m3emp for each naics6 by county code
# #df6step1 = get_m3emp6_all(df=df6,df4n=df4imp)
# #
# #df6step1[df6step1$geo4naics=="56045_8111",]
# #df4imp[df4imp$geo4naics=="56045_8111",]
# #tempdf = get_6naics_per4("56045_8111",df6=df6step1,df4imp=df4imp)
# #tempdf
# #tempdf = get_6naics_per4("1001_2372",df6=df6step1,df4imp=df4imp)
# #tempdf
# 
# df6step1[df6step1$geo4naics=="10001_3121",]
# df4imp[df4imp$geo4naics=="10001_3121",]
# tempdfnew = get_6naics_per4("10001_3121",df6=df6step1,df4imp=df4imp) 
# #takes 0.0135 on my laptop
# 
# #naics4 with most naics6 in it
# df6step1[df6step1$geo4naics=="6037_3339",]
# df4imp[df4imp$geo4naics=="6037_3339",]
# tempdfnew = get_6naics_per4("6037_3339",df6=df6step1,df4imp=df4imp)
# #takes 0.02124 on my laptop



#get naics6 x county level data from imputed naics4 x county data and 
# summary of naics6 x county grouped by naics4 x county
get_6naics_all<-function(df=df6,df4n=df4imp,codes4summary=count6dig,rseed=NA){
  if(!is.na(rseed)){set.seed(rseed)}
  
  timestart1=Sys.time() #get start time
  
  # codes which naics4 x county only contains 1 naics6 x county code
  codesNOTtoget = codes4summary$geo4naics[codes4summary$Count6Codes==1]
  
  # simplifying imputed naics4 x county for joining with naics6 x county codesNOTtoget
  df4forjoin =df4n%>%select(geo4naics,m1emp,m3emp,wage)
  # use naics4 x county values for the naics6 x county values if there is only 1
  #    naics6 x county code in naics4 x county aggregate
  df6_onecodeper4 = df%>%filter(geo4naics %in% codesNOTtoget)%>%
    inner_join(df4forjoin)%>%
    select(geoindkey,geo4naics,state,cnty,estnum,m1emp,m3emp,wage)
  
  # naics6 x county corresponding to naics4 x county codes containing >1 naics6 codes
  # adjust emp based on EmpScale to be m3emp
  df6_toget = get_m3emp6_all(df=df%>%filter(!(geo4naics %in% codesNOTtoget)),df4n=df4n)
  
  test4dig = unique(df6_toget$geo4naics) #naics4 x county codes in df6_toget
  
  # prints time that the steps of 
  ##   splitting the df6 into 1 code per naics4 and >1 code per naics4
  ##   and creating df6 data for the set of 1 code per naics4 takes 
  cat("Start of get_6naics_all execution time is:", Sys.time()-timestart1)
  
  # now to address naics6 x county in >1 code per naics4 subset
  df6_toget_imputed = 
    bind_rows( 
      parallel::mclapply( #for each
        test4dig,#naics4 x county codes in >1 code per naics4 subset
        function(x)get_6naics_per4(x,df6=df6_toget,df4imp=df4n) 
        ) #get naics6 x county rows (per naics4  x county code in test4dig)
      ) #stack the resulting dataframes (1 dataframe per naics4 x county code)
  
  #stack naics6 x county dataframes corresponding to 
  ##  1 code per naics4 subset and >1 code per naics4 subset
  bind_rows(df6_toget_imputed,df6_onecodeper4)
}

## combining all the above functions to take df4imp to become naics6 x county data
set.seed(3) #random seed
#get estnum,m1emp,m3emp, and wage for each naics6 x county code
naics6df=get_6naics_all(df=df6,df4n=df4imp,codes4summary = count6dig)
#subset to geoindkey,geonaics4,state,cnty,estnum,m1emp, m3emp, and wage columns
naics6df = naics6df%>%select(colnames(naics6df)[1:5],m1emp,m3emp,wage) 
# NOTE: m2emp will be imputed at establishment level based on m1emp and m3emp

#save results as .RData and .csv files
save(naics6df,file=paste0(folderout,"naics6postlapply.RData"))
write.csv(naics6df,paste0(folderout,"DataNAICS6.csv"))

## Optional: check head and check that there are no missing m1emp or wage values
#head(naics6df)
#naics6df%>%filter(is.na(wage)|is.na(m1emp))



