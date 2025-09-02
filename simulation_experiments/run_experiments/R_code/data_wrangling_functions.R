
### pivot comparison file to be long
pivot_agg_file=function(sanagg=NULL,state.prefix=NULL,
                        comp.suffix,metrics=c("dif","rel","abs"),
                        param.colname="parameter",
                        param.translator=NULL,
                        basepath=NULL,
                        filename.comp.suffix=NULL,
                        aggfile=F,
                        clipprob=F,
                        budgets=F,baseline=NULL,
                        config.seperated.names=c("rep","mech","metric"),
                        flipsign=T,
                        adjust.rel=NULL){
  if((param.colname%in%config.seperated.names)==F){
    config.seperated.names=c(param.colname,config.seperated.names)
  }
  if(is.null(sanagg)==T){
    sanagg=file_name_changes(fin=paste0(basepath,"/compare_data/",filename.comp.suffix),
                             st.pref=state.prefix,comp.suff=filename.comp.suffix,aggfile=aggfile)
    sanagg=sanagg[sanagg$own_code==5,grepl("agglvl|fips|industry|qtrly|month3",colnames(sanagg))]
  }
  if(sum(colnames(sanagg)=="own_code")>0){
    sanagg=sanagg[sanagg$own_code==5,]
  }
  if(length(metrics)<4){
    sanagg=sanagg[,grepl(paste0("agglvl|fips|industry|estabs|",paste0("_",metrics,collapse="|")),colnames(sanagg))]
  }

  if(clipprob==T){
    colnames(sanagg)[grepl("sqrt",colnames(sanagg))]=gsub("_sqrt","_NA_sqrt",colnames(sanagg)[grepl("sqrt",colnames(sanagg))])
  }else if(budgets==T){
    colnames(sanagg)=gsub("budget","budget_",colnames(sanagg))
  }

  sanagg$agggroup=paste0(sanagg$area_fips,"X",sanagg$industry_code)
  basecol.indic=grepl("agglvl|fips|industry|estabs|agggroup",colnames(sanagg))
  wage.indic=grepl("total_qtrly_wages",colnames(sanagg))
  emp.indic=grepl("month3_emplvl",colnames(sanagg))

  if(is.null(baseline)==F){
    colnames(sanagg)=gsub(baseline,paste0(baseline,"_base"),colnames(sanagg))
    pivotcols=paste0(paste0(comp.suffix,collapse="|"),"|",paste0(baseline,collapse="|"))
    pivotnamepref=NULL
  }else{
    pivotcols=comp.suffix
    pivotnamepref=comp.suffix
  }
  wagedf=sanagg[,basecol.indic|wage.indic]
  empdf=sanagg[,basecol.indic|emp.indic]
  colnames(wagedf)=gsub("total_qtrly_wages_","",colnames(wagedf))
  colnames(empdf)=gsub("month3_emplvl_","",colnames(empdf))



  wagedf_long=tidyr::pivot_longer(wagedf,
                                  cols=colnames(wagedf)[grepl(pivotcols,colnames(wagedf))],
                                  names_to = config.seperated.names,
                                  names_prefix=pivotnamepref,names_sep="_",
                                  values_to = "value")

  wagedf_long$variable="wage"

  empdf_long=tidyr::pivot_longer(empdf,
                                 cols=colnames(empdf)[grepl(pivotcols,colnames(empdf))],
                                 names_to = config.seperated.names,
                                 names_prefix=pivotnamepref,names_sep="_",
                                 values_to = "value")
  empdf_long$variable="emp"
  #print(str(empdf_long))
  if(is.null(param.translator)==F){
    wagedf_long[,param.colname]=sapply(unlist(unname(wagedf_long[,param.colname])),function(x)param.translator(x))
    empdf_long[,param.colname]=sapply(unlist(unname(empdf_long[,param.colname])),function(x)param.translator(x))
  }
  #print(str(empdf_long))

  combinedf=dplyr::bind_rows(empdf_long,wagedf_long)
  if("rep"%in%colnames(combinedf)){
    combinedf$rep=gsub("rep","",combinedf$rep)
  }
  if(flipsign==T){
  if(("dif" %in% metrics)|("rdif" %in% metrics)){
    combinedf$value[grepl("dif",combinedf$metric)]=(-1)*combinedf$value[grepl("dif",combinedf$metric)]
  }
  }

  return(combinedf)
}

## get vector of string names based on agglvl_codes
agglvl_code_labeller=function(aggcodes){
  firstdigit=as.numeric(aggcodes)%/%10
  seconddigit=as.numeric(aggcodes)%%10
  agglabels=ifelse(firstdigit==5,"State","County by")
  industrylevels=paste0("NAICS-",seconddigit-2)
  speciali=seconddigit<5
  industrylevels[speciali]=ifelse(seconddigit[speciali]<3,ifelse(seconddigit[speciali]==1,"Total","Domain"),
                                  ifelse(seconddigit[speciali]==3,"Supersector","Sector"))
  agglabels=paste(agglabels,industrylevels)
  agglabels[agglabels=="County by Total"]="County Total"
  agglabels=factor(agglabels,levels=c("State Total","State Domain","State Supersector","State Sector",paste0("State NAICS-",seq(3,6)),
                                      "County Total","County by Domain","County by Supersector","County by Sector",paste0("County by NAICS-",seq(3,6))))
  return(agglabels)
}

nestab_cut_labeller=function(cutlvls,input_estab_breaks=NULL,estab_breaks=NULL,possible_estab_breaks=NULL,use.quantile=F,ndig.enotation=1){
  override.use.quantile=(sum((input_estab_breaks<=0)&(input_estab_breaks)>=1)==0)&(is.null(input_estab_breaks)==F)
  use.quantile=(use.quantile==T)|(override.use.quantile==T)
  if(use.quantile==T){
    input_estab_breaks=input_estab_breaks[(input_estab_breaks>0)&(input_estab_breaks<1)]

    if(length(estab_breaks)!=length(possible_estab_breaks)){
      dup.indic=base::duplicated(possible_estab_breaks,fromLast=T)
      end_estab_breaks=input_estab_breaks[!dup.indic]
    }else{
      end_estab_breaks=input_estab_breaks[(input_estab_breaks>0)&(input_estab_breaks<1)]
    }


    if(length(end_estab_breaks)==0){
      qbreaks="(100%)"#confdf.sub$estab_bin=paste0("n Estabs in [",min(confdf.sub$qtrly_estabs),", ",max(confdf.sub$qtrly_estabs),"]; (100% Range of n)")
    }else{
      if(length(end_estab_breaks)==1){
        qbreaks=c(paste0('; (Bottom ',round(end_estab_breaks[1]*100,0), "%)"),
                  paste0("; (Top ",100*round(1-end_estab_breaks[1],0),'%)'))
      }else if(length(estab_breaks)==2){
        qbreaks=c(paste0('; (Bottom ',100*round(end_estab_breaks[1],0), "%)"),
                  paste0("; (Middle ",100*round(end_estab_breaks[2]-end_estab_breaks[1],2),"%)"),
                  paste0("; (Top ",100*round(1-end_estab_breaks[2],2),'%)'))
      }else{
        qbreaks=c(paste0('; (Bottom ',round(end_estab_breaks[1]*100,0), "%)"),
                  #paste0("; (From ",100*round(end_estab_breaks[1],2),"% to ",100*round(end_estab_breaks[2],2),"%)"),
                  paste0("; (From ",100*round(end_estab_breaks[seq(1,length(end_estab_breaks)-1)],2),"% to ",100*round(end_estab_breaks[seq(2,length(end_estab_breaks))],2),"%)"),
                  paste0("; (Top ",100*round(1-end_estab_breaks[length(end_estab_breaks)],2),'%)'))
      }
    }
  }else{
    qbreaks=NULL
  }

  cutvals=unique(unlist(strsplit(gsub("\\(|\\)|\\[|\\]","",cutlvls),",")))
  eindic=grepl("e",cutvals)
  elvls=sapply(strsplit(cutvals[eindic],"e"),function(x)paste0(round(as.numeric(x[1]),ndig.enotation),"e",x[2]))
  cutvals[eindic]=elvls
  firstbracket=substr(cutlvls,1,1)
  lastbracket=sapply(cutlvls,function(x)substr(x,start=nchar(x),stop=nchar(x)))
  return(paste0("n Estabs in ",firstbracket,cutvals[seq(1,length(cutvals)-1)],",",cutvals[seq(2,length(cutvals))],lastbracket," ",qbreaks))
}


#get value for top k establishments or top pper percent of establishments
top_k_group=function(data,k=NA,pper=NA,ratio=F,fillNA=F){
  stopifnot(is.na(k)==FALSE|is.na(pper)==FALSE)
  if(is.na(pper)==F){
    k=round(length(data)*pper,0)
    k=ifelse(k==0,1,k)
  }
  #if(((k==length(data))&(fillNA==F))|
  if((k==length(data))&(ratio==T)){
    return(NA)
  }else{
    sortdf=sort(data,decreasing=T)
    topk=sum(sortdf[seq(1,k)])
    if(ratio==T){
      nottop=sum(sortdf[-seq(1,k)])
      return(topk/nottop)
    }else{
      if(sum(data)==0){
        return(1)
      }else{
        return(topk/sum(data))
      }
    }
  }
}

sum_not_max=function(data){
  if(length(data)==1){
    return(0)
  }else{
    return(sum(data,na.rm=T)-max(data,na.rm=T))
  }
}

compare_reps_topk=function(sanagg,confdf,agglvl_code,ks.and.ppers=NULL,state.prefix=state.prefixes,
                           estab_breaks=NULL,metric_keep=c("abs","rel"),add.conf.stats=NULL){
  if(is.null(estab_breaks)==T){
    bynestabs=F
    bynestab=F
  }else{
    bynestabs=T
    bynestab=T
  estab_breaks=sort(estab_breaks)
  input_estab_breaks=estab_breaks#stopifnot(is.null(k)==FALSE|is.null(pper)==FALSE)
  }
  statecode=as.numeric(base::substr(state.prefix,3,4))
  statelvl=ifelse((as.numeric(agglvl_code)<69)&(as.numeric(agglvl_code)>49),T,F)
  lastdig=as.numeric(agglvl_code)%%10
  indlvl_from_lastdig=c("total","domain","supersector","sector","naics3","naics4","naics5","naics")
  industrylvl=indlvl_from_lastdig[lastdig]
  if(industrylvl=="domain"){
    confdf$domain=base::substr(as.character(confdf$supersector),1,3)
  }

  if(sum(colnames(sanagg)=="own_code")>0){
    sanagg=sanagg[sanagg$own_code==5,]
  }
  sanagg=sanagg[,grepl("agglvl|fips|industry|estabs",colnames(sanagg))|grepl("equalwage",colnames(sanagg))|colnames(sanagg)%in%c("total_qtrly_wages","month3_emplvl")]
  #sanagg$wagediff=sanagg$total_qtrly_wages_equalwage_clip_rel-sanagg$total_qtrly_wages_equalwage_sqrt_rel
  #sanagg$empdiff=sanagg$month3_emplvl_equalwage_clip_rel-sanagg$month3_emplvl_equalwage_sqrt_rel
  #sanagg$wageratio=sanagg$total_qtrly_wages_equalwage_clip_rel/sanagg$total_qtrly_wages_equalwage_sqrt_rel
  #sanagg$empratio=sanagg$month3_emplvl_equalwage_clip_rel/sanagg$month3_emplvl_equalwage_sqrt_rel
  sanagg$cnty=gsub(paste0("^",statecode),"",as.character(sanagg$area_fips))

  possible_metrics=c("_dif","_rdif","_abs","_rel")
  remove_metrics=possible_metrics[(!(possible_metrics%in%metric_keep))|(!(possible_metrics%in%paste0("_",metric_keep)))]
  agglvldf=sanagg[sanagg$agglvl_code==agglvl_code,ifelse(length(remove_metrics)==0,rep(T,ncol(sanagg)),!grepl(paste0(remove_metrics,collapse="|"),colnames(sanagg)))]

  statetotal.indic=F
  if(statelvl==F){
    ###print("county level")
    if(industrylvl=="total"){
      ###print("county total")
      agglvldf$agggroup=agglvldf$cnty
      confdf$agggroup=paste0(as.numeric(confdf$cnty))
      legend_label=paste("County")
    }else{
      ###print("county by industry code")
      agglvldf$agggroup=paste(agglvldf$cnty,"X",agglvldf$industry_code)
      industrycol=confdf[,industrylvl]
      confdf$agggroup=paste(as.numeric(confdf$cnty),"X",unlist(industrycol))
      legend_label=paste("County by ",stringr::str_to_title(stringr::str_to_lower((industrylvl))))
    }
  }else if(industrylvl=="total"){
    ###print("state total")
    statetotal.indic=T
    agglvldf$agggroup=base::substr(state.prefix,1,2)
    confdf$agggroup=base::substr(state.prefix,1,2)
    legend_label="State Total"
  }else{
    ###print("by industry level")
    agglvldf$agggroup=as.character(agglvldf$industry_code)
    confdf$agggroup=as.character(unlist(confdf[,industrylvl]))
    legend_label=stringr::str_to_title(stringr::str_to_lower(industrylvl))
  }


  ###print("here 3")
  if(statetotal.indic==F){
    confdf.sub=dplyr::group_by(confdf,agggroup)
  }else{
    confdf.sub=confdf
  }
  ###print(colnames(confdf.sub))

  for(r in seq(1,nrow(ks.and.ppers))){
    k=ks.and.ppers$k[r]
    pper=ks.and.ppers$p[r]
    r_colname=ifelse(is.na(pper)==T,paste0("prop_of_top",k),paste0("prop_of_top_",pper*100,"percent"))
    if(r==1){
      if(statetotal.indic==F){
        confdf.sub.temp=dplyr::summarise(confdf.sub,"wage_topkpercent"=top_k_group(wage,k=k,pper=pper,ratio=F),"month3_emplvl_topkpercent"=top_k_group(m3emp,k=k,pper=pper,ratio=F),"ngroups"=n())
      }else{
        confdf.sub.temp=data.frame("agggroup"=confdf.sub$agggroup[1],"wage_topkpercent"=top_k_group(confdf.sub$wage,k=k,pper=pper,ratio=F),"month3_emplvl_topkpercent"=top_k_group(confdf.sub$m3emp,k=k,pper=pper,ratio=F),"ngroups"=1)
      }
      renamecols=colnames(confdf.sub.temp)
      renamecols[grepl("topkpercent",renamecols)]=paste0(c("wage_","month3_emplvl_"),r_colname)
      colnames(confdf.sub.temp)=renamecols
      internal.confdf.sub=confdf.sub.temp
    }else{
      if(statetotal.indic==F){
        confdf.sub.temp=dplyr::summarise(confdf.sub,"wage_topkpercent"=top_k_group(wage,k=k,pper=pper,ratio=F),"month3_emplvl_topkpercent"=top_k_group(m3emp,k=k,pper=pper,ratio=F))
      }else{
        confdf.sub.temp=data.frame("agggroup"=confdf.sub$agggroup[1],"wage_topkpercent"=top_k_group(confdf.sub$wage,k=k,pper=pper,ratio=F),"month3_emplvl_topkpercent"=top_k_group(confdf.sub$m3emp,k=k,pper=pper,ratio=F))
      }
      renamecols=colnames(confdf.sub.temp)
      renamecols[grepl("topkpercent",renamecols)]=paste0(c("wage_","month3_emplvl_"),r_colname)
      colnames(confdf.sub.temp)=renamecols
      internal.confdf.sub=dplyr::inner_join(internal.confdf.sub,confdf.sub.temp,by="agggroup")

    }
    #confdf.sub=confdf%>%group_by(agggroup)%>%summarize("wage_topkpercent"=top_k_group(wage,k=k,pper=pper,ratio=F),"month3_emplvl_topkpercent"=top_k_group(m3emp,k=k,pper=pper,ratio=F))
    #confdf.sub=dplyr::inner_join(confdf.sub,confdf.sub.temp,by="agggroup")
    #renamecols=colnames(confdf.sub)
    #renamecols[grepl("topkpercent",renamecols)]=paste0(c("wage_","month3_emplvl"),r_colname)
    #colnames(confdf.sub)=renamecols
  }

  if(is.null(add.conf.stats)==FALSE){
    if(statetotal.indic==F){
      confdf.sub=dplyr::group_by(confdf,agggroup)
    }else{
      confdf.sub=confdf
    }
   if(sum(c("max","sumnotmax")%in%add.conf.stats)==2){
     if(statetotal.indic==F){
       confdf.sub.temp=dplyr::summarise(confdf.sub,
                                        "wage_maxestab"=max(wage,na.rm=T),
                                        "month3_emplvl_maxestab"=max(m3emp,na.rm=T),
                                        "wage_sumnotmax"=sum(wage,na.rm=T)-max(wage,na.rm=T),
                                        "month3_emplvl_sumnotmax"=sum(m3emp,na.rm=T)-max(m3emp,na.rm=T))
     }else{
       confdf.sub.temp=data.frame("agggroup"=confdf.sub$agggroup[1],
                                  "wage_maxestab"=max(confdf.sub$wage,na.rm=T),
                                  "month3_emplvl_maxestab"=max(confdf.sub$m3emp,na.rm=T),
                                  "wage_sumnotmax"=sum(confdf.sub$wage,na.rm=T)-max(confdf.sub$wage,na.rm=T),
                                  "month3_emplvl_sumnotmax"=sum(confdf.sub$m3emp,na.rm=T)-max(confdf.sub$m3emp,na.rm=T))
     }
   }else if("max"%in%add.conf.stats){
     if(statetotal.indic==F){
       confdf.sub.temp=dplyr::summarise(confdf.sub,
                                        "wage_maxestab"=max(wage,na.rm=T),
                                        "month3_emplvl_maxestab"=max(m3emp,na.rm=T))
     }else{
       confdf.sub.temp=data.frame("agggroup"=confdf.sub$agggroup[1],
                                  "wage_maxestab"=max(confdf.sub$wage,na.rm=T),
                                  "month3_emplvl_maxestab"=max(confdf.sub$m3emp,na.rm=T))
     }
   }else if("sumnotmax"%in%add.conf.stats){
     if(statetotal.indic==F){
       confdf.sub.temp=dplyr::summarise(confdf.sub,"wage_sumnotmax"=sum(wage,na.rm=T)-max(wage,na.rm=T),
                                        "month3_emplvl_sumnotmax"=sum(m3emp,na.rm=T)-max(m3emp,na.rm=T))
     }else{
       confdf.sub.temp=data.frame("agggroup"=confdf.sub$agggroup[1],
                                  "wage_sumnotmax"=sum(confdf.sub$wage,na.rm=T)-max(confdf.sub$wage,na.rm=T),
                                  "month3_emplvl_sumnotmax"=sum(confdf.sub$m3emp,na.rm=T)-max(confdf.sub$m3emp,na.rm=T))
     }
   }else{
     confdf.sub.temp=NULL
     warning(paste0("Only supported add.conf.stats inputs are 'max' and/or 'sumnotmax'. Input was ignored: ",paste0(add.conf.stats,collapse=", ")))
   }
     internal.confdf.sub=dplyr::inner_join(internal.confdf.sub,confdf.sub.temp,by="agggroup")
   }
  confdf.sub=dplyr::inner_join(internal.confdf.sub,agglvldf,by="agggroup")

  if(bynestab==TRUE){
    if(is.null(estab_breaks)==TRUE){
      ##print("is.null(estab_breaks==T)")
      #confdf.sub=confdf.sub[confdf.sub$qtrly_estabs>k,]
      possible_estab_breaks=round(quantile(confdf.sub$qtrly_estabs,probs=c(1/3,2/3)),0)
      estab_breaks=unique(possible_estab_breaks)
      ##print(paste("estab_breaks=",paste0(estab_breaks,collapse=", "),"| possible_estab_breaks=",paste0(possible_estab_breaks,collapse=", "),"| input_estab_breaks=",paste0(input_estab_breaks,collapse=", ")))
      confdf.sub$estab_bin=cut(confdf.sub$qtrly_estabs,
                               breaks=unique(c(0,estab_breaks,max(confdf.sub$qtrly_estabs)+1)),
                               dig.lab=3)
      levels(confdf.sub$estab_bin)=nestab_cut_labeller(levels(confdf.sub$estab_bin),
                                                       input_estab_breaks=input_estab_breaks,
                                                       estab_breaks=estab_breaks,
                                                       possible_estab_breaks=possible_estab_breaks,use.quantile=T)
    }else if(sum((estab_breaks<=1)&(estab_breaks>=0))==length(estab_breaks)){
      ##print("else if estab_breaks in [0,1]")
      estab_breaks=sort(estab_breaks[(estab_breaks>0)&(estab_breaks<1)])

      possible_estab_breaks=round(quantile(confdf.sub$qtrly_estabs,probs=estab_breaks),0)
      estab_breaks=unique(possible_estab_breaks)
      ##print(paste("estab_breaks=",paste0(estab_breaks,collapse=", "),"| possible_estab_breaks=",paste0(possible_estab_breaks,collapse=", "),"| input_estab_breaks=",paste0(input_estab_breaks,collapse=", ")))

      confdf.sub$estab_bin=cut(confdf.sub$qtrly_estabs,breaks=c(0,estab_breaks,max(confdf.sub$qtrly_estabs)+1),dig.lab=3)
      levels(confdf.sub$estab_bin)=nestab_cut_labeller(levels(confdf.sub$estab_bin),
                                                       input_estab_breaks=input_estab_breaks,
                                                       estab_breaks=estab_breaks,
                                                       possible_estab_breaks=possible_estab_breaks,use.quantile=T)
    }else{
      ##print("else (estab_breaks not NUll, and not between 0 and 1)")
      possible_estab_breaks=sort(estab_breaks)
      confdf.sub$estab_bin=cut(confdf.sub$qtrly_estabs,breaks=unique(c(0,estab_breaks,max(confdf.sub$qtrly_estabs)+1)),dig.lab = 3)
      ##print(paste("estab_breaks=",paste0(estab_breaks,collapse=", "),"| possible_estab_breaks=",paste0(possible_estab_breaks,collapse=", "),"| input_estab_breaks=",paste0(input_estab_breaks,collapse=", ")))

      levels(confdf.sub$estab_bin)=nestab_cut_labeller(levels(confdf.sub$estab_bin),use.quantile=F)
    }
  }else{
    ##print("else not quantile")
    confdf.sub$estab_bin=paste0("n Estabs in [",min(confdf.sub$qtrly_estabs),", ",max(confdf.sub$qtrly_estabs),"]; (100%)")
  }

  #View(confdf.sub)
  return(list(confdf.sub,legend_label))
}



compare_sqrt_clip_topk=function(sanagg,confdf,agglvl_code,k=NA,pper=NA,state.prefix=state.prefixes,metrics=c("rdif","rel","dif","abs"),
                                basesize=8,add.theme=NULL,add.legend.theme=NULL,bynestab=F,estab_breaks=NULL,newversion=F,
                                add.max.sumnotmax=F,only1=F){
  if((agglvl_code<52)|((agglvl_code>59)&(agglvl_code<71))|(agglvl_code>79)){
    warning(paste0("agglvl_code:  ",agglvl_code," not supported. Returning NULL"))
    return(list("confdf.sub"=NULL,"legend_label"=NULL))
  }else{
  estab_breaks=sort(estab_breaks)
  input_estab_breaks=estab_breaks
  stopifnot(is.na(k)==FALSE|is.na(pper)==FALSE)
  statecode=as.numeric(base::substr(state.prefix,3,4))
  statelvl=ifelse((as.numeric(agglvl_code)<69)&(as.numeric(agglvl_code)>49),T,F)
  lastdig=as.numeric(agglvl_code)%%10
  indlvl_from_lastdig=c("total","domain","supersector","sector","naics3","naics4","naics5","naics")
  industrylvl=indlvl_from_lastdig[lastdig]
  if(industrylvl=="domain"){
    confdf$domain=base::substr(as.character(confdf$supersector),1,3)
  }

  if(sum(colnames(sanagg)=="own_code")>0){
    sanagg=sanagg[sanagg$own_code==5,]
  }
  if(only1==T){
    sanagg=sanagg[,grepl("agglvl|fips|industry|estabs",colnames(sanagg))|grepl("equalwages1_",colnames(sanagg))|colnames(sanagg)%in%c("total_qtrly_wages","month3_emplvl")]

  }else{
    sanagg=sanagg[,grepl("agglvl|fips|industry|estabs",colnames(sanagg))|grepl("equalwage",colnames(sanagg))|colnames(sanagg)%in%c("total_qtrly_wages","month3_emplvl")]
  }
   sanagg$cnty=gsub(paste0("^",statecode),"",as.character(sanagg$area_fips))

  if(newversion==T){
  for(met in metrics){
    if(met%in%c("rel","abs")){
    for(var in c("total_qtrly_wages_equalwage","month3_emplvl_equalwage")){
      if(only1==T){
        var=paste0(var,"s1")
      }
      shortvar=ifelse(grepl("month3",var)==T,"emp","wage")
      sanagg[,paste0(shortvar,"_",met,"_mechdiff")]=unlist(unname(sanagg[,paste0(var,"_clip_",met)]))-unlist(unname(sanagg[,paste0(var,"_sqrt_",met)]))
      sanagg[,paste0(shortvar,"_",met,"_mechratio")]=unlist(unname(sanagg[,paste0(var,"_clip_",met)]))-unlist(unname(sanagg[,paste0(var,"_sqrt_",met)]))
    }
    }
    agglvldf=sanagg[sanagg$agglvl_code==agglvl_code,grepl(paste0("agglvl|fips|industry|estabs|",paste0("_",metrics,collapse="|")),colnames(sanagg))|(colnames(sanagg)%in%c("total_qtrly_wages","month3_emplvl"))]

  }
  }else{
  sanagg$wagediff=sanagg$total_qtrly_wages_equalwage_clip_rel-sanagg$total_qtrly_wages_equalwage_sqrt_rel
  sanagg$empdiff=sanagg$month3_emplvl_equalwage_clip_rel-sanagg$month3_emplvl_equalwage_sqrt_rel
  sanagg$wageratio=sanagg$total_qtrly_wages_equalwage_clip_rel/sanagg$total_qtrly_wages_equalwage_sqrt_rel
  sanagg$empratio=sanagg$month3_emplvl_equalwage_clip_rel/sanagg$month3_emplvl_equalwage_sqrt_rel

  agglvldf=sanagg[sanagg$agglvl_code==agglvl_code,!grepl("_dif|_rdif",colnames(sanagg))]
  }



  if(statelvl==F){
    if(industrylvl=="total"){
      agglvldf$agggroup=agglvldf$cnty
      confdf$agggroup=paste0(as.numeric(confdf$cnty))
      legend_label=paste("County")
    }else{
      agglvldf$agggroup=paste(agglvldf$cnty,"X",agglvldf$industry_code)
      industrycol=confdf[,industrylvl]
      confdf$agggroup=paste(as.numeric(confdf$cnty),"X",unlist(industrycol))
      legend_label=paste("County by ",stringr::str_to_title(stringr::str_to_lower(industrylvl)))
    }
  }else{
    if(industrylvl=="total"){
      agglvldf$agggroup=agglvldf$cnty
      confdf$agggroup=paste0(as.numeric(confdf$cnty))
      legend_label=paste("County")
    }else{
    agglvldf$agggroup=as.character(agglvldf$industry_code)
    confdf$agggroup=as.character(unlist(confdf[,industrylvl]))
    legend_label=stringr::str_to_title(stringr::str_to_lower(industrylvl))
    }
  }
  ###print("here 3")
   print(head(confdf))
  confdf.sub=confdf%>%group_by(agggroup)%>%summarize("wage_topkpercent"=top_k_group(wage,k=k,pper=pper,ratio=F),"month3_emplvl_topkpercent"=top_k_group(m3emp,k=k,pper=pper,ratio=F))

  if(add.max.sumnotmax==T){
    confdf.sub=confdf%>%group_by(agggroup)%>%summarize("wage_max"=max(wage,na.rm=T),
                                                       "month3_emplvl_max"=max(m3emp,na.rm=T),
                                                       "wage_sumnotmax"=sum_not_max(wage),
                                                       "month3_emplvl_max"=sum_not_max(m3emp))

  }
  if(newversion==F){
    confdf.sub=confdf.sub%>%inner_join(agglvldf[,!grepl("_abs",colnames(agglvldf))],by="agggroup")
  }else{
  confdf.sub=confdf.sub%>%inner_join(agglvldf,by="agggroup")
  }

  if(bynestab==TRUE){
    if(is.null(estab_breaks)==TRUE){
      use.quant=T
      #confdf.sub=confdf.sub[confdf.sub$qtrly_estabs>k,]
      possible_estab_breaks=round(quantile(confdf.sub$qtrly_estabs,probs=c(1/3,2/3)),0)
      estab_breaks=unique(estab_breaks)
      confdf.sub$estab_bin=cut(confdf.sub$qtrly_estabs,breaks=c(0,estab_breaks,max(confdf.sub$qtrly_estabs)+1),dig.lab=3)
      #levels(confdf.sub$estab_bin)=paste0(nestab_cut_labeller(levels(confdf.sub$estab_bin),c("; (Bottom 33%)","; (Middle 33%)", "; (Top 34%)"))
    }else if(sum((estab_breaks<=1)&(estab_breaks>=0))==length(estab_breaks)){
      use.quant=T
      estab_breaks=sort(estab_breaks)
      estab_breaks=estab_breaks[(estab_breaks>0)&(estab_breaks<1)]

      possible_estab_breaks=round(quantile(confdf.sub$qtrly_estabs,probs=estab_breaks),0)#,max(confdf.sub$qtrly_estabs)+1)
      estab_breaks=unique(possible_estab_breaks)

      confdf.sub$estab_bin=cut(confdf.sub$qtrly_estabs,
                               breaks=unique(c(0,estab_breaks,max(confdf.sub$qtrly_estabs)+1)),
                               dig.lab=3)

      levels(confdf.sub$estab_bin)=nestab_cut_labeller(levels(confdf.sub$estab_bin),
                                                       input_estab_breaks=input_estab_breaks,
                                                       estab_breaks=estab_breaks,
                                                       possible_estab_breaks=possible_estab_breaks,use.quantile=T)
    }else{
      estab_breaks=sort(estab_breaks)
      confdf.sub$estab_bin=cut(confdf.sub$qtrly_estabs,
                               breaks=unique(c(0,estab_breaks,max(confdf.sub$qtrly_estabs)+1)),dig.lab = 3)
      levels(confdf.sub$estab_bin)=nestab_cut_labeller(levels(confdf.sub$estab_bin),use.quantile=F)
    }
  }else{
    confdf.sub$estab_bin=paste0("n Estabs in [",min(confdf.sub$qtrly_estabs),", ",max(confdf.sub$qtrly_estabs),"]; (100%)")
  }

  #View(confdf.sub)
  return(list(confdf.sub,legend_label))
  }
}

## handles the inconsistency of the file names for rhode island
file_name_changes=function(fin=folderin,st.pref=state.prefix,comp.suff=compare.suffix,aggfile=F,quietly=T){
  wprep_fname=paste0(st.pref,"_",comp.suff,ifelse(aggfile==T,"_agg_prep.csv","_prep.csv"))
  wprep_path=paste0(fin,"/",wprep_fname)
  wprep_path2=paste0(fin,"/",comp.suff,"/",wprep_fname)
  if(file.exists(wprep_path)==T){
    pathfilename=wprep_path
  }else if(file.exists(paste0(fin,"/",gsub("_prep.csv",".csv",wprep_fname)))==T){
    pathfilename=paste0(fin,"/",gsub("_prep.csv",".csv",wprep_fname))
  }else if(file.exists(wprep_path2)==T){
    pathfilename=wprep_path2
  }else if(file.exists(paste0(fin,"/",comp.suff,"/",gsub("_prep.csv",".csv",wprep_fname)))==T){
    pathfilename=paste0(fin,"/",comp.suff,"/",gsub("_prep.csv",".csv",wprep_fname))
  }else{
    stop(paste0("Cannot find comparison files in ",fin,"\nNeither ",wprep_fname," or ",gsub("_prep.csv",".csv",wprep_fname)," exist."))
  }
  if(quietly==F){
    print(paste("Reading data from",pathfilename))
  }
  return(read.csv(pathfilename))
}


##edit labels in aggcodes and removes the aggcodes we are not interested in
## aggcodes is the dataframe of aggregate codes if it is NULL, aggcodefolder and aggcodefname will be used to read it in
## onlybyowner=TRUE removes any queries that are not "by ownership sector" (focuses on privately owned establishments)
## bysizeclass=FALSE removes any queries that are "by size class"
## keepcodes is a numeric vector of codes that overrides onlybyowner to keep additionally specified codes
## aggcodefolder and addcodefname give the location and file name of the aggcode csv to read in if aggcodes=NULL
## newfname is the new file name to save the relabeled agg codes to
## returns data.frame of "agglvl_code" and "agglvl" where the first is the numeric code and the second is the new label.
aggcode_relabel=function(aggcodes=NULL,
                         onlybyowner=T,
                         bysizeclass=F,
                         keepcodes=NULL,#c(50,70),
                         aggcodefolder=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/R_tables_plots"),
                         aggcodefname="agg_level_titles.csv",
                         newfname="agg_level_titles_processed.csv"){

  if(is.null(aggcodes)==TRUE){
    aggcodes=read.csv(paste0(aggcodefolder,"/",aggcodefname))
  }
  aggcodes$agglvl_title=gsub(",","",aggcodes$agglvl_title)

  #remove msa, csa, and microsa queries as well as government owned establishment queries
  msa.csa.indic=grepl("MSAs|CSAs|MicroSA",aggcodes$agglvl_title,ignore.case = F)
  government.indic=grepl("Total Government",aggcodes$agglvl_title,ignore.case = F)
  all.indics=(!msa.csa.indic)&(!government.indic)
  if(onlybyowner==T){
    all.indics=(all.indics)&(grepl("by ownership sector",aggcodes$agglvl_title))
    badcodes=
      new_aggcodes=aggcodes[grepl("by ownership sector",aggcodes$agglvl_title),]
  }
  if(bysizeclass==F){
    all.indics=(all.indics)&(!grepl("by establishment size class",aggcodes$agglvl_title))
  }
  new_aggcodes=aggcodes[all.indics,]
  if(onlybyowner==T){ #shortens label to remove the -- by ownership sector
    new_title=gsub(" -- by ownership sector","",new_aggcodes$agglvl_title)
    if(length(keepcodes)>0){ #if keeping any codes add them back in
      new_aggcodes=dplyr::bind_rows(new_aggcodes,aggcodes[aggcodes$agglvl_code%in%c(keepcodes),])
      new_title=c(new_title,aggcodes$agglvl_title[aggcodes$agglvl_code%in%c(keepcodes)])
    }
  }else{
    new_title=new_aggcodes$agglvl_title
  }
  new_aggcodes$agglvl=sapply(new_title,trimws)


  new_aggcodes=dplyr::arrange(new_aggcodes,agglvl_code)
  write.csv(new_aggcodes,file=paste0(aggcodefolder,"/",newfname))
  return(new_aggcodes[,c("agglvl_code","agglvl")])
}

longdf_reps_topk=function(data){
  basecols=c("agglvl_code","agggroup","state","cnty","industry_code","estab_bin","qtrly_estabs","ngroups")

  #confdf
  colnames(data)=gsub("equalwages","rep",colnames(data))
  wagesdf=data[,c(basecols,colnames(data)[grepl("wage",colnames(data))])]
  wagesdf$variable="wages"
  colnames(wagesdf)=gsub("total_qtrly_wages","orig_val",colnames(wagesdf))
  colnames(wagesdf)=gsub("orig_val_|wage_","",colnames(wagesdf))

  empdf=data[,c(basecols,colnames(data)[grepl("emplvl",colnames(data))])]
  empdf$variable="employment"
  ###print(colnames(empdf))
  colnames(empdf)=gsub("month3_emplvl","orig_val",colnames(empdf))
  ###print(colnames(empdf))
  colnames(empdf)=gsub("orig_val_","",colnames(empdf))
  ###print(colnames(empdf))

  stackdf=dplyr::bind_rows(wagesdf,empdf)
  #print(colnames(stackdf))
  notmetric=!grepl("dif|abs|rel", colnames(stackdf))
  colnames(stackdf)[grepl("_sqrt|_clip", colnames(stackdf))&notmetric]=paste0(colnames(stackdf)[grepl("_sqrt|_clip", colnames(stackdf))&notmetric],"_sanvalue")
  #colnames(stackdf)[grepl("rep[0-9]*_clip", colnames(stackdf))]=paste0(colnames(stackdf)[grepl("rep[0-9]*_clip", colnames(stackdf))],"_sanvalue")

  #print(colnames(stackdf))

  longdf=tidyr::pivot_longer(stackdf,cols=colnames(stackdf)[grepl("rep",colnames(stackdf))],names_to=c("repetition","mechanism","metric_measure"),
                             names_prefix="rep",names_sep="_",values_to = "metric_value")
  longdf=tidyr::pivot_wider(longdf,names_from=metric_measure,values_from=metric_value)
  #colnames(longdf)
  longdf$mechanism=ifelse(longdf$mechanism=="sqrt","sqrt","pnc")
  return(longdf)

}


## create cell identifier with area_fips X industry_code,
## where industry_code=10 if not grouped by industry, and
## area_fips is the 2 digit state, followed by 3-digit county with leading zeros
measurement_agggroup=function(data,aggcode,st.pref=state.prefix){
  stnum=base::substr(st.pref,3,4)
  if(aggcode>69){
    cnty3dig=sprintf("%03d",as.numeric(unlist(data$cnty)))
    data$area_fips=as.numeric(paste0(stnum,cnty3dig))
  }else{
    data$area_fips=as.numeric(paste0(stnum,"000"))
  }

  if(aggcode%%10==7){
    data$industry_code=as.numeric(unlist(data$naics5))
  }else if(aggcode%%10==1){
    data$industry_code=10
  }else{
    stop("Only 51,57,71,77 agg codes are currently supported")
  }
  data$agggroup=paste0(data$area_fips," X ",data$industry_code)

  return(data)
}

combine_measure_files_reps=function(state.prefix,compare.suffix=comp.suffix,configuration.suffix=config.suffix,
                                    folderin=paste0(basepath,"/measurements/",comp.suffix),
                                    nreps=34,include.identity=F){
  measdf=data.frame()
  identdf=data.frame()

  for(i in seq(1,nreps)){
    folderstart=paste0(folderin,"/",state.prefix,"__",configuration.suffix,i)
    ## for clip mech
    clipfolder=paste0(folderstart,"_clip__protected")
    queryfiles=list.files(clipfolder)
    queryfiles=queryfiles[!grepl("Identity",queryfiles)]
    sqrtfolder=paste0(folderstart,"_sqrt__protected")
    for(query in queryfiles){
      queryname=gsub("By","",gsub("[[:punct:]]","",substr(query,start=7,stop=nchar(query)-4)))
      aggcode=ifelse(grepl("Sum",queryname),51,
                     ifelse(grepl("NAICS",queryname),
                            ifelse(grepl("County",queryname),77,57),71))
      clipcells=read.csv(paste0(clipfolder,"/",query))
      clipcells$mechanism="pnc"

      sqrtcells=read.csv(paste0(sqrtfolder,"/",query))
      sqrtcells$m3emp=(sqrtcells$m3emp_sqrt)^2
      sqrtcells$wage=(sqrtcells$wage_sqrt)^2
      sqrtcells$mechanism="sqrt"

      cellsdf=dplyr::bind_rows(sqrtcells[,!(colnames(sqrtcells)%in%c("wage_sqrt","m3emp_sqrt"))],clipcells)
      cellsdf=cellsdf[,grepl("m3emp|wage|cnty|naics|mechanism",colnames(cellsdf),ignore.case=T)]
      cellsdf$agglvl_code=aggcode
      cellsdf$state=substr(state.prefix,start=1,stop=4)
      cellsdf$replicate=i
      cellsdf=measurement_agggroup(cellsdf,aggcode,state.prefix)
      measdf=dplyr::bind_rows(measdf,cellsdf)
    }
    if(include.identity==T){
      clipcells=read.csv(paste0(clipfolder,"/nmf_0_Identity.csv"))
      clipcells=clipcells[,grepl("m3emp|wage|cnty|naics",colnames(clipcells),ignore.case=T)]
      clipcells$mechanism="pnc"

      sqrtcells=read.csv(paste0(sqrtfolder,"/nmf_0_Identity.csv"))
      sqrtcells=sqrtcells[,grepl("m3emp|wage|cnty|naics",colnames(sqrtcells),ignore.case=T)]
      sqrtcells$m3emp=(sqrtcells$m3emp_sqrt)^2
      sqrtcells$wage=(sqrtcells$wage_sqrt)^2
      sqrtcells$mechanism="sqrt"

      cellsdf=dplyr::bind_rows(sqrtcells,clipcells)
      cellsdf$agglvl_code=aggcode
      cellsdf$state=substr(state.prefix,start=1,stop=4)
      cellsdf$replicate=i
      identdf=dplyr::bind_rows(identdf,cellsdf)
    }
  }
  if(include.identity==T){
    return(list(identdf,measdf))
  }else{
    return(measdf)
  }
}

equalwage_orig_data_info=function(state.prefixes,aggcodes.pper,ks.and.ppers=trytopk,
                                  compare.suffix=comp.suffix,basepath.str=basepath,override.agggroups=T){
  pperdf=data.frame()
  if(is.null(ks.and.ppers)==T){
    ks.and.ppers=data.frame("k"=c(1,2),"p"=c(NA,NA))
  }
  #read in data
  for(st.pref in state.prefixes){
    nj.indic=grepl("nj34",st.pref)
    confdf=read.csv(paste0(basepath.str,"/EstablishmentLevelData/",base::substr(st.pref,1,4),"/",st.pref,".csv"))
    if(nj.indic==T){
      sanagg.fname=paste0(basepath.str,"/compare_data/",compare.suffix,"/",st.pref,"_",compare.suffix,"_prep.csv")
    }else{
      sanagg.fname=paste0(basepath.str,"/compare_data/",compare.suffix,"/",st.pref,"_",compare.suffix,".csv")
    }
    sanagg=read.csv(sanagg.fname)
    sanagg=sanagg[sanagg$own_code==5,grepl("agglvl|fips|industry|qtrly|month3",colnames(sanagg))]
    sanagg=sanagg[,grepl("agglvl|fips|industry|estabs",colnames(sanagg))|grepl("equalwage",colnames(sanagg))|colnames(sanagg)%in%c("total_qtrly_wages","month3_emplvl")]

    for(aggc in aggcodes.pper){ #for each agglvl code
      outtemp=compare_reps_topk(sanagg,confdf,agglvl_code=aggc,ks.and.ppers=ks.and.ppers,
                                state.prefix=st.pref,
                                estab_breaks=NULL,
                                add.conf.stats=c("max","sumnotmax"))
      tempdf=outtemp[[1]]
      tempdf$state=base::substr(st.pref,1,4)
      pperdf=dplyr::bind_rows(pperdf,tempdf)
    }
    #stop()
  }
  subpper=pperdf[,(colnames(pperdf)%in%c("month3_emplvl","total_qtrly_wages"))|
                   (grepl("prop_of|agggroup|ngroups|state|maxestab|sumnotmax|agglvl|area_fips|industry_code|qtrly_estabs",colnames(pperdf)))]
  ##fix area_fips codes
  area_fips_str=as.character(subpper$area_fips)
  problem_fipsI=ifelse(nchar(area_fips_str)!=5,T,F)
  nprobs=sum(problem_fipsI)
  if(nprobs!=nrow(subpper)){
    problem_fips=area_fips_str[problem_fipsI]
    statenum=substr(problem_fips,1,2)
    cnty=as.numeric(substr(problem_fips,start=rep(3,nprobs),nchar(problem_fips)))
    subpper$area_fips[problem_fipsI]=as.numeric(paste0(statenum,sprintf("%03d",cnty)))
  }
  if(override.agggroups==T){
    subpper$agggroup=paste0(subpper$area_fips," X ",subpper$industry_code)
  }
  return(subpper)
}

fix_fips=function(fips,statenum="34"){
  cnty=substr(as.character(fips),start=nchar(as.character(statenum))+1,stop=nchar(as.character(fips)))
  cnty3dig=sprintf("%03d",as.numeric(cnty))
  area_fips=as.numeric(paste0(statenum,cnty3dig))
  return(area_fips)
}

