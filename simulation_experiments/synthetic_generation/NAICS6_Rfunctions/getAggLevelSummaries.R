require(tidyverse)
get_codes_summary<-function(dfin=df,groupbydigits=3,levelgrouped=4){
  
  #pattern to subset data to
  patterngrep=paste0("_[0-9]{",levelgrouped,"}[^0-9]{",6-levelgrouped,"}")
  if(levelgrouped==6){patterngrep="_[0-9]{6}"}
  
  #character to end substring on to get [county#]_[industry at group-by digits level]
  strendchr = -(6-groupbydigits)-1 
  #to label the type of grouping
  labelgroup=paste0(levelgrouped,"by",groupbydigits)
  
  df = 
    dfin %>% 
    filter(grepl(patterngrep,geoindkey))%>% #subset to levelgrouped rows
    mutate("geodignaics"=str_sub(geoindkey,start=1,end=strendchr))%>% #get groupby vals
    select(geoindkey,geodignaics,state,cnty,estnum,qp1,qp1_nf) #select relevant cols
  #print(df%>%filter(grepl("29189_52",geodignaics)))
  count6dig = df%>%
    group_by(geodignaics)%>% #group by specified level
    summarize("CountCodes"=n(), #count of rows in group
              "wageCBP"=sum(qp1,na.rm=T), #sum available wage in group
              "wageCBP_missing"=sum(qp1_nf=="D"))%>% #count rows w/out wage val in group
    mutate("grouplevels"=paste0("group",labelgroup)) #label group-by & what level grouped
  #print(colnames(count6dig))
  #print(count6dig%>%filter(grepl("29189_525",geodignaics)))
  if(levelgrouped!=6){ #if we are not grouping 6 digit NAICS
    #replace sum available wage with NA when all rows are missing values
    count6dig$wageCBP[count6dig$wageCBP_missing==count6dig$CountCodes]=NA 
  }
  
  #change column names to reflect group-by and grouped levels
  colnames(count6dig)=
    c(paste0("geo",groupbydigits,"naics"),
      paste0("Count",levelgrouped,"Codes"),
      paste0("wageCBP_sum",labelgroup),
      paste0("wageCBP_missing",labelgroup),"grouplevels")
  
  count6dig #return summary data.frame
}



get_4codes_summary<-function(df=df6){
  count6dig = df%>%group_by(geo4naics)%>%
    summarize("Count6Codes"=n(),"Count5Codes"=n_distinct(geo5naics),
              "empCBP"=sum(emp,na.rm = T),
              "wageCBP"=sum(qp1,na.rm=T),"wageCBP_missing"=sum(qp1_nf=="D"))
}

