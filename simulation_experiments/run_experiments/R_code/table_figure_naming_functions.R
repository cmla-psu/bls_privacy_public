source(paste0(rprojroot::find_rstudio_root_file(),"/comparison_config_functions.R"))
source(paste0(rprojroot::find_rstudio_root_file(),"/table_functions.R"))

#read config.yaml to get parameter values in a data.frame
config_values=function(config.nm,
                       config.folder=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/configs")){

  config.full=paste0(config.folder,"/",config.nm)
  if(grepl(".yaml",config.full)==FALSE){
    config.full=paste0(config.full,".yaml")
  }
  if(file.exists(config.full)==FALSE){
    flist=list.files(path=config.folder,pattern=config.nm,recursive=T)
    if(length(flist)>1){
      warning(paste("More than 1 file with that config name:",paste0(gsub(config.folder,"",flist),collapse=", ")))
      config.full=paste0(config.folder,"/",unlist(flist[1]))
    }else{
      config.full=paste0(config.folder,"/",unlist(flist))
    }
  }
  params=yaml::read_yaml(config.full,readLines.warn = F)
  privdist=params$privacy_distances
  ident.query=params$queries$identity
  group.queries=params$queries$groupby
  group.queries.names=sapply(group.queries,function(x)x$name)

  gq_muk=function(qname,gqueries=group.queries,gqnames=group.queries.names){

    altqname.indic=(gsub("-","",qname)%in%gqnames)
    if(altqname.indic==TRUE){
      qname=gsub("-","",qname)
    }
    if(qname%in%gqnames){
      query=gqueries[[which(gqnames==qname)]]
      return(sqrt(sum(unlist(query$mu)^2)))
    }else{
      return(NA)
    }
  }

  gq_muk_ew=function(qname,gqueries=group.queries,gqnames=group.queries.names,emp=T){
    altqname.indic=(gsub("-","",qname)%in%gqnames)
    if(altqname.indic==TRUE){
      qname=gsub("-","",qname)
    }
    if(qname%in%gqnames){
      query=gqueries[[which(gqnames==qname)]]
      mus=query$mu
      if(emp==T){
        return(mus$m3emp)
      }else{
        return(mus$wage)
      }
    }else{
      return(NA)
    }
  }

  total_mu=function(gqueries=group.queries,iquery=ident.query){
    ident.mu2=sum(unlist(iquery)^2,na.rm=T)
    gmus=unlist(lapply(gqueries,function(x)unlist(x$mu)))
    g.mu2=sum(gmus^2,na.rm=T)
    return(sqrt(g.mu2+ident.mu2))
  }

  config.vals=data.frame(configName=gsub(".yaml","",config.nm),
                         mu=total_mu(),
                         privdist_emp=privdist$m3emp,
                         privdist_wage=privdist$wage,
                         confparam=NA,
                         identmuk=sqrt(sum(unlist(ident.query)^2)),
                         summuk=gq_muk("Sum Query"),
                         n5muk=gq_muk("By NAICS-5"),
                         cntymuk=gq_muk("By County"),
                         cntyn5muk=gq_muk("By County/NAICS-5"),
                         supsecmuk=gq_muk("By Supersector"),
                         cntysupsecmuk=gq_muk("By County/Supersector"),
                         identmuk_emp=ident.query$m3emp,
                         summuk_emp=gq_muk_ew("Sum Query"),
                         n5muk_emp=gq_muk_ew("By NAICS-5"),
                         cntymuk_emp=gq_muk_ew("By County"),
                         cntyn5muk_emp=gq_muk_ew("By County/NAICS-5"),
                         supsecmuk_emp=gq_muk_ew("By Supersector"),
                         cntysupsecmuk_emp=gq_muk_ew("By County/Supersector"),
                         identmuk_wage=ident.query$wage,
                         summuk_wage=gq_muk_ew("Sum Query",emp=F),
                         n5muk_wage=gq_muk_ew("By NAICS-5",emp=F),
                         cntymuk_wage=gq_muk_ew("By County",emp=F),
                         cntyn5muk_wage=gq_muk_ew("By County/NAICS-5",emp=F),
                         supsecmuk_wage=gq_muk_ew("By Supersector",emp=F),
                         cntysupsecmuk_wage=gq_muk_ew("By County/Supersector",emp=F))
  if(grepl("SQRT",params$accountant)==FALSE){
    config.vals$confparam=params$clipping_prob
  }

  return(config.vals)
}
#config_values("blsvals_clip")

########
tabfignames_maker=function(comp.configs,compname,config.vals,mudigits=1,sqrtclip=T){
  comp.configs=comp.configs[(comp.configs!="blsvals_sqrt")&(comp.configs!="blsvals_clip")]
  sub.config=config.vals[config.vals$configName%in%comp.configs,,drop=F]
  cstem=unique(gsub("_sqrt|_clip","",comp.configs))
  cstemnames=paste0(rep(cstem,each=2),c("_sqrt","_clip"))
  nstem=length(cstem)

  tabstem=rep(c("\\sqroot ","\\shortpnc "),nstem)
  figstem=rep(c("sqrt ","pnc "),nstem)
  if(length(comp.configs)>1){
    sub.config=sub.config[match(comp.configs,sub.config$configName),,drop=F]
  }
  if(grepl("clipprob|clip_prob",compname)==TRUE){
    tabfignames=data.frame(configName=c(comp.configs),
                           compareName=rep(compname,length(comp.configs)),
                           longtable=paste0("\\shortpnc $\\confparam=",sub.config$confparam,"$"),
                           shorttable=paste0("\\shortpnc $\\confparam=",sub.config$confparam,"$"),
                           longfig=paste0("pnc confidence=",sub.config$confparam),
                           shortfig=paste0("pnc conf=",sub.config$confparam))
  }else if(grepl("budget",compname)==TRUE){
    budgets=sub.config$mu[match(cstemnames,sub.config$configName)]
    tabfignames=data.frame(configName=cstemnames,
                           compareName=rep(compname,2*nstem),
                           longtable=paste0(tabstem," $\\mu=",round(budgets,mudigits),"$"),
                           shorttable=paste0(tabstem," $\\mu=",round(budgets,mudigits),"$"),
                           longfig=paste0(figstem," mu=",round(budgets,mudigits)),
                           shortfig=paste0(figstem, " mu=",round(budgets,mudigits)))
  }else if(grepl("querychoice|querytype|querynumber",compname)==TRUE){
    if(grepl("querynumber",compname)==TRUE){
      cstem=gsub("51n","51",cstem)
      qnum=T
    }else{
      qnum=F
    }
    tempout=naics_titler(cstem,qnum=qnum)
    long=rep(unlist(tempout[[1]]),each=2)
    short=rep(unlist(tempout[[2]]),each=2)
    tabfignames=data.frame(configName=cstemnames,
                           compareName=rep(compname,2*nstem),
                           longtable=paste(tabstem,long),
                           shorttable=paste(tabstem,short),
                           longfig=paste(figstem,long),
                           shortfig=paste(figstem,short))
  }else if(grepl("wageAllocation|wage_share",compname)==TRUE){
    equal=grepl("equal",cstem)
    mult=gsub("[^0-9]*","",cstem[!equal])
    long=rep(ifelse(equal,"Emp Equal Wage Share",paste0(mult,"x Wage Share")),each=2)
    shorttab=rep(ifelse(equal,"=W\\%",paste0("W\\%x",mult)),each=2)
    shortfig=rep(ifelse(equal,"=W%",paste0("W%x",mult)),each=2)
    tabfignames=data.frame(configName=cstemnames,
                           compareName=rep(compname,2*nstem),
                           longtable=paste(tabstem,long),
                           shorttable=paste(tabstem,shorttab),
                           longfig=paste(figstem,long),
                           shortfig=paste(figstem,shortfig))

  }else if(grepl("queryAllocation|ident_share",compname)==TRUE){

    add.value=ifelse(grepl("half|p5",cstem,ignore.case = T),0.5,0)
    add.value[grepl("quart|p25",cstem)]=0.25
    add.value[grepl("p75",cstem)]=0.75
    tempcstem=gsub("p5|p25|p75","",cstem)
    multiplier=as.numeric(gsub("[^0-9]*","",tempcstem))+add.value

    state=grepl("sum",cstem,ignore.case = T)
    noti=grepl("NotIdent",cstem,ignore.case = T)
    long=paste0(multiplier,ifelse(noti,"x Group Queries","x Identity"))
    long[state]=paste0(multiplier[state],"x State Total")
    short=paste0(multiplier,ifelse(noti,"xGroups","xI"))
    short[state]=paste0(multiplier[state],"xState")
    long=rep(long,each=2)
    short=rep(short,each=2)
    tabfignames=data.frame(configName=cstemnames,
                           compareName=rep(compname,2*nstem),
                           longtable=paste(tabstem,long),
                           shorttable=paste(tabstem,short),
                           longfig=paste(figstem,long),
                           shortfig=paste(figstem,short))
    }else if(sqrtclip==TRUE){
    newstem=rep(unique(gsub("blsvals_|_clip|_sqrt","",comp.configs)),each=2)
    newcstemnames=paste0(newstem,c("_sqrt","_clip"))

    tabfignames=data.frame(configName=newcstemnames,
                           compareName=rep(compname,2*nstem),
                           longtable=paste(tabstem,newstem),
                           shorttable=paste(tabstem,newstem),
                           longfig=paste(figstem,newstem),
                           shortfig=paste(figstem,newstem))
  }else{
    newstem=gsub("blsvals_","",comp.configs)
    tabfignames=data.frame(configName=newstem,
                           compareName=rep(compname,length(newstem)),
                           longtable=newstem,
                           shorttable=newstem,
                           longfig=newstem,
                           shortfig=newstem)
  }
  return(tabfignames)
}



################################
table_config_namer=function(folderin=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/compare_data"),
                            folderout=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/R_tables_plots"),
                            state.prefix="nj34_qbp_2016_1",
                            csv.fname="table_config_names.csv",
                            overwrite=T,
                            functionfilepath=paste0(rprojroot::find_rstudio_root_file(),"/comparison_config_functions.R"),
                            mudigits=1){

  csv.full=paste0(folderout,"/",csv.fname)
  if((file.exists(csv.full)==FALSE)|(overwrite==TRUE)){ #if making dataframe
    if(is.null(functionfilepath)==FALSE){
    source(functionfilepath)
    }

    #comparison stems
    compare.st=comparison_stems(folderin=folderin,state.prefixes = state.prefix)$compare.suffixes
    configs_per_compare=function(c.sfx,fin=folderin,st.prefix=state.prefix){
      st.compares=file_name_changes(fin=fin,st.pref=st.prefix,comp.suff=c.sfx,aggfile=T)
      config_extractor(cnames=colnames(st.compares))
    }
    config.list=lapply(compare.st,configs_per_compare)

    config.vals=dplyr::bind_rows(lapply(unique(unlist(config.list)),config_values))

    tabfignames=data.frame(configName=c("blsvals_sqrt","blsvals_clip"),
                           compareName=c("base","base"),
                        longtable=c("\\sqroot base","\\shortpnc base"),
                        shorttable=c("\\sqroot base","\\shortpnc base"),
                        longfig=c("sqrt base","pnc base"),
                        shortfig=c("sqrt base","pnc base"))
    #othertabfigs
    if(length(config.list)>1){
    othertabfigs=dplyr::bind_rows(lapply(seq(1,length(config.list)),
                                         function(i)tabfignames_maker(comp.configs=config.list[[i]],
                                                                      compname=compare.st[i],
                                                                      config.vals=config.vals,
                                                                      mudigits=mudigits)))
    tabfignames=dplyr::bind_rows(list(tabfignames,othertabfigs))
    tabnamer=dplyr::right_join(tabfignames,config.vals,by="configName")
    }else if(length(config.list)==1){
      othertabfigs=tabfignames_maker(comp.configs=config.list[[1]],
                                     compname=compare.st[1],
                                     config.vals=config.vals,
                                     mudigits=mudigits)
      tabfignames=dplyr::bind_rows(list(tabfignames,othertabfigs))
      tabnamer=dplyr::right_join(tabfignames,config.vals,by="configName")
    }else{
      othertabfigs=NULL
      tabnamer=tabfignames
    }


    #if some of the configs are missing
    missingconfigs=tabnamer$configName[is.na(tabnamer$compareName)]
    if(length(missingconfigs)>0){
      missing.list=NULL
      message("Some configs not handled by tabfigname_maker() function originally")
      for(i in seq(1,length(config.list))){
        missing.indic=(missingconfigs%in%config.list[[i]])
        if(sum(missing.indic)>0){
          temptabfig=tabfignames_maker(missingconfigs[missing.indic],compare.st[i],config.vals=config.vals,mudigits=mudigits,sqrtclip = F)
          missing.list=c(missing.list,list(temptabfig))
        }
      }
      add.tabfignames=dplyr::bind_rows(missing.list)
      tabnamer=dplyr::right_join(add.tabfignames,config.vals,by="configName")
    }


    write.csv(tabnamer,file=csv.full)
  }else{
    tabnamer=read.csv(csv.full)
  }
  return(tabnamer)
}

#temp=table_config_namer(csv.fname="table_config_names_new.csv")
#head(temp)


naics_titler=function(cstem,qnum=F){
  cstem=gsub("query|queries","",cstem)
  newnaics=grepl("_[0-9]{2}",cstem)&(!grepl("_[0-9]{3}",cstem))
  ### sub function ###
  newnaics_one_query=function(dd){
    county=grepl("7[0-9]{1}",dd)
    dd=gsub("n","",dd)
    naicsdig=as.numeric(substring(dd,nchar(dd),nchar(dd)))-2
    if(naicsdig<=0){
      longtitle=ifelse(county,"County","State")
      shorttitle=ifelse(county,"C","State")
    }else{
    if(naicsdig<=2){
      naicslongtitle=ifelse(naicsdig==1,"Supersector",ifelse(naicsdig==2,"Sector",""))
      naicsshorttitle=ifelse(naicsdig==1,"SS",ifelse(naicsdig==2,"S",""))
    }else{
      naicslongtitle=paste0("NAICS",naicsdig)
      naicsshorttitle=paste0("N",naicsdig)
    }
    longtitle=paste0(ifelse(county,"County x ",""),naicslongtitle)
    shorttitle=paste0(ifelse(county,"Cx",""),naicsshorttitle)
    }
    return(c(longtitle,shorttitle))
  }
  #####
  newnaics_one_config=function(whichnaics,qnum=F){
    whichnaics=unlist(whichnaics)[unlist(whichnaics)!=""]

    if(length(whichnaics)==1){
      tempout=newnaics_one_query(unlist(whichnaics))
      stitles=tempout[2]
      ltitles=paste0(tempout[1]," Only")
    }else{
      tempout=sapply(whichnaics,newnaics_one_query)
      tempout=tempout[,!grepl("State",tempout[1,]),drop=F]
      stitles=paste0(tempout[2,],collapse="+")
      nltitles=length(tempout[1,])
      if(nltitles==2){
        ltitles=paste0(tempout[1,],collapse=" and ")
      }else{
        ltitles=paste0(paste0(tempout[1,seq(1,nltitles-1)],collapse=", "),", and ",tempout[1,nltitles])
      }

    }
    if(qnum==T){
      num_queries=length(whichnaics)
      if(num_queries==2){
        q2_county=(sum(grepl("71",whichnaics))>0)
        stitles=paste0("2",ifelse(q2_county,"c","n"))
      }else{
        stitles=as.character(num_queries)
      }
    }
    return(c(ltitles,stitles))
  }
  ####
  if(sum(newnaics)>0){
    stitles=rep(NA,length(newnaics))
    ltitles=rep(NA,length(newnaics))
    whichnaics=strsplit(cstem,"_")
    if(length(newnaics)==1){
      if(qnum==F){
      tempout=newnaics_one_config(whichnaics,qnum=qnum)
      stitles=tempout[2]
      ltitles=tempout[1]
      }
    }else{ #length(newnaics)>1
      tempout=sapply(whichnaics,function(x)newnaics_one_config(x,qnum=qnum))
      stitles=tempout[2,]
      ltitles=tempout[1,]
    }
    }else{ #not newnaics
      supsecbyc=grepl("_CSS|73",cstem)
      supsec=grepl("_SS_|53",cstem)
      ltitles=paste(ifelse(supsec,"Supersector","NAICS5"),ifelse(supsecbyc,"+ County x Supersector","+ County x NAICS5"))
      stitles=paste(ifelse(supsec,"SS","N5"),ifelse(supsecbyc,"+ CxSS","+ CxN5"))
      # sec=grepl("_S_|54",cstem)
      # secbyc=grepl("_CS_|74",cstem)

    }
  return(list(unlist(ltitles),unlist(stitles)))
}
#naics_titler(c("queries_71_77","queries_71_77_52_57"))
  #oldnaics=grepl("_N[0-9]{1}",cstem)
  #oldnaicsbyc=grepl("_CN[0-9]{1}",cstem)
  #naics=grepl("_N|75|76|77|78")
  #naicsbyc=grepl("_CN|55|56|57|58")
#temp=table_config_namer()

