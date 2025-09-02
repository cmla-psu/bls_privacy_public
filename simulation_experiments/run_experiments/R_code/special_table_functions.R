### Ident Share Stack Acct Type

stacked_by_acct_table_rel_err=function(folderin=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/compare_data"),
                       folderout=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/R_tables_plots"),
                       st.compares=NULL,
                       queries.in.create=c("State Total","State NAICS 5-digit",
                                           "County Total","County NAICS 5-digit"),
                       queries.in.create.codes=NULL,
                       aggstats=NULL,
                       aggcodes=NULL,
                       keepaggcodes=NULL,
                       removeaggcodes=c(50,70),
                       compare.suffix="ident_share",
                       state.prefix="nj34_qbp_2016_1",
                       ndigits=2,
                       rndigits=2,
                       andigits=0,
                       tab.name.csv=NULL,
                       short.config.names=T){

  combineAggLevelCode=T
  configorder="byacct"
  n_estab_stats=NULL
  new.folderout=paste0(folderout,"/",compare.suffix)
  if(file.exists(new.folderout)==FALSE){
    dir.create(new.folderout)
  }

  if(is.null(st.compares)==TRUE){
    st.compares=read.csv(paste0(folderin,"/",state.prefix,"_",compare.suffix,"_agg_prep.csv"))
  }

  if(is.null(aggstats)==TRUE){
    aggstats=get_aggcode_info(aggcodes=aggcodes,
                              st.data=NULL,
                              folderin=folderin,
                              compare.suffix=compare.suffix,
                              state.prefix=state.prefix,
                              write.tex=paste0(folderout,"/agg_estnum_summary_",state.prefix,".tex"))

  }
  if(is.null(tab.name.csv)==FALSE){
    source(paste0(rprojroot::find_rstudio_root_file(),"/table_figure_naming_functions.R"))
    tabnamer=table_config_namer(folderin=folderin,folderout=folderout,state.prefix=state.prefix,
                                csv.fname=tab.name.csv,overwrite=F,functionfilepath=NULL,mudigits=1)
  }

  folderout=new.folderout

  #remove 1st row (it has with mean and median strings)
  #subset to get agglvl_code, month3_emplvl and total_qtrly_wages
  sub.st=st.compares[-1,grepl("agglvl|month3|wage",colnames(st.compares))]
  sub.st=sub.st[,!grepl("_dif|_rdif",colnames(sub.st))]
  cnames=colnames(sub.st)
  sub.st[,cnames]=lapply(sub.st[,cnames],as.numeric)
  if(is.null(keepaggcodes)==FALSE){
    sub.st=sub.st[sub.st$agglvl_code%in%keepaggcodes,]
  }else if(is.null(removeaggcodes)==FALSE){
    sub.st=sub.st[!(sub.st$agglvl_code%in%removeaggcodes),]
  }

  #extract the names of the configurations
  config.nms=config_extractor(cnames=colnames(sub.st),order=configorder)

  if(is.null(tab.name.csv)==FALSE){
    config.cname.str=tabnamer$shorttable[match(config.nms,tabnamer$configName)]
  }else{
    config.cname.str=config.nms
  }
  nconfigs=length(config.nms)/2
  config.nms.half=gsub("_sqrt|_clip","",config.nms[seq(1,nconfigs)])
  config.cname.str=gsub("\\\\sqroot|\\\\shortpnc","",config.cname.str[seq(1,nconfigs)])


  caption_maker=function(st.prefix=state.prefix,
                         split.we=T,
                         split.ar=T,
                         measure="abs",
                         comp.suffix=compare.suffix,
                         starsadd=starsadded,
                         conf.var="emp",
                         configorder="byacct"){
    if(split.we==T){
      variable=ifelse(grepl("emp",conf.var),"employment","quarterly wages")
      var.lab=ifelse(grepl("emp",conf.var),"_emp_","_wages_")
    }else{
      variable="monthly employment and quarterly wages"
      var.lab="_"
    }
    if(split.ar==T){
      if(grepl("abs",measure)==T){
        measure.str="Average absolute error"
        measure.lab="abs_err"
      }else{
        measure.str="Median relative error"
        measure.lab="rel_err"
      }
    }else{
      measure.str="Average absolute error and median relative error"
      measure.lab="err"
    }
    state.str=paste("synthetic",ifelse(grepl("nj34",st.prefix),"New Jersey","Rhode Island"))
    state.lab=ifelse(grepl("nj34",st.prefix),"nj34","ri44")
    if(grepl("querynumber",comp.suffix)==T){
      experiment=paste("We compare configurations with varying number of group-by queries.",
                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."),
                       "For the column labeled `1', only the state total (51) is used in creating the PPM. For the configurations with two group-by queries, `2c' indicates the state and county queries (51 and 71) were used while `2n' indicates the state and NAICS5 queries (51 and 57) were used. The column `3' adds the County by NAICS5 (77) and `4' adds Supersector (53). The column `5' further adds and finally column `6' adds County by Supersector (57).",
                       "The identity share of the privacy budget remains fixed. The share of the remaining budget is split between the group-by queries  based on the number of groups in the query with higher allocation going to queries with more groups.")
    }else if(grepl("querychoice|querytype",comp.suffix)==T){
      experiment=paste("We compare configurations with a fixed number of group-by queries, while varying what those group-by queries are.",
                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."),
                       "All configurations still use State (51) and County (71) queries. The NAICS5 query is denoted 'N5'. The Supersector query is denoted 'SS'. The county by supersector and county by NAICS5 queries are denoted 'CxSS' and CxN5' respectively.")
    }else if(grepl("ident",comp.suffix)==T){
      experiment=paste("We multiply the share of the privacy budget that the identity query is allocated and compare the resulting PPM.",
                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."))
    }else if(grepl("wageAllocation",comp.suffix)){
      experiment=paste("We compare configurations which alter the share of the query-level budgets allocated to wages. In the baseline parameters, less than 10\\% is allocated to wages. 'W\\%x2' indicates the wage share is doubled and '=W\\%' indicated the wage share is equal to each of the three employment budget allocations. Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort.",
                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."))
    }else if(grepl("budget",comp.suffix)==T){
      experiment=paste("We compare sanitized data across several total privacy budgets.",
                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."))
    }else if(grepl("queryAllocation",comp.suffix)){
      experiment=paste("We compare configurations which alter the query-level budget allocation.",

                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."),
                       "We denoted 1.5 times the baseline identity share as '1.5xI'. Increasing the allocation to the State aggregate query is denoted '3xState'. '1.25xGroups' inicates the share of the group-by queries is multiplied by 1.25.")
    }else if(grepl("clip_pro|clippro",comp.suffix)==T){
      experiment=paste("We compare sanitized data across several confidence parameters ($\\confparam$) for the \\pncmech, labeled \\pncshort. The baselines confidence parameter is $\\confparam=0.01$.",
                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."))
    }else{
      experiment=
        ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort.")
    }

    captionlines=paste0("\\caption{",measure.str," of ",variable,ifelse(split.ar==T," is "," are "),
                        "summarized across the cells of various aggregate levels for ",state.str,". ",experiment,
                        ifelse(starsadd==T," Rows marked with * indicate the aggregate level was used in the sanitization process.}","}"))
    labellines=paste0("\\label{tab:",measure.lab,var.lab,gsub("blsvals_","",comp.suffix),"_",ifelse(grepl("nj34",st.prefix),"nj}","ri}"))
    return(c(captionlines,labellines,"\\end{table}"))
  }

  sub.st=dplyr::left_join(sub.st,aggstats,by=dplyr::join_by(agglvl_code))
  if(is.null(queries.in.create.codes)==FALSE){
    sub.st$usedagg=ifelse(sub.st$agglvl_code %in% c(queries.in.create.codes),"*","")
  }else if(is.null(queries.in.create)==FALSE){
    sub.st$usedagg=ifelse(sub.st$agglvl %in% c(queries.in.create),"*","")
  }else{
    sub.st$usedagg="?"
  }



  #colnames(nj.compares)=c("agglvl_code",clip.nm,sqrt.nm)



      sub.st$newcode=paste0(sub.st$usedagg,sub.st$agglvl_code," ",sub.st$agglvl)
      sub.st$newcode=gsub("-digit","",sub.st$newcode)
      sub.st$newcode[!grepl("Total",sub.st$newcode)]=gsub("County |County by","County X",gsub("State |State by","State X",gsub("NAICS ","NAICS",sub.st$newcode[!grepl("Total",sub.st$newcode)])))
      sub.st$newcode=gsub("NAICSSector","Sector",sub.st$newcode)
      cname.order.base=c("newcode",unname(unlist(n_estab_stats)))
      base.cname.str=c("Aggregate Level",names(n_estab_stats))
    starsadded=T



  cname.order.emp=c(paste0("month3_emplvl_",config.nms.half))
  cname.order.wages=c(paste0("total_qtrly_wages_",config.nms.half))

  #base.cname.str=c("","Code","Aggregate Level",names(n_estab_stats))
  base.col.num=ifelse(combineAggLevelCode==TRUE,1,2)+length(n_estab_stats)
  n_estab.col.num=0 #antiquated




  #cname.order.base=c("newcode","agglvl",unname(unlist(n_estab_stats)))
  #base.cname.str=c("Code","Aggregate Level",names(n_estab_stats))



  sub.st.sqrt.rel=sub.st[,c(cname.order.base,paste0(cname.order.emp,"_sqrt_rel"),paste0(cname.order.wages,"_sqrt_rel"))]
  n.sqrt.rel=nrow(sub.st.sqrt.rel)
  sub.st.clip.rel=sub.st[,c(cname.order.base,paste0(cname.order.emp,"_clip_rel"),paste0(cname.order.wages,"_clip_rel"))]
  n.clip.rel=nrow(sub.st.clip.rel)
  sub.st.sqrt.abs=sub.st[,c(cname.order.base,paste0(cname.order.emp,"_sqrt_abs"),paste0(cname.order.wages,"_sqrt_abs"))]
  n.sqrt.abs=nrow(sub.st.sqrt.abs)
  sub.st.clip.abs=sub.st[,c(cname.order.base,paste0(cname.order.emp,"_clip_abs"),paste0(cname.order.wages,"_clip_abs"))]
  n.clip.abs=nrow(sub.st.clip.abs)
  new.cnames=c(cname.order.base,cname.order.emp,cname.order.wages)

  colnames(sub.st.sqrt.rel)=new.cnames
  colnames(sub.st.sqrt.abs)=new.cnames
  colnames(sub.st.clip.rel)=new.cnames
  colnames(sub.st.clip.abs)=new.cnames
  sub.st.rel=dplyr::bind_rows(sub.st.sqrt.rel,sub.st.clip.rel)
  sub.st.abs=dplyr::bind_rows(sub.st.sqrt.abs,sub.st.clip.abs)

  colnames(sub.st.rel)=new.cnames
  colnames(sub.st.abs)=new.cnames
  table.list=NULL


      #Relative Error for Employment
      rel.tab.emp=knitr::kable(sub.st.rel[,c(cname.order.base,cname.order.emp)],col.names = c(base.cname.str,config.cname.str),
                               digits=rndigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))

      rel.tab.emp=kableExtra::add_header_above(rel.tab.emp,c(" "=base.col.num,"Employment"=length(config.nms)),escape=F)
      rel.tab.emp=kableExtra::pack_rows(rel.tab.emp,"\\\\sqrtmech",1,n.sqrt.rel,escape=F)
      rel.tab.emp=kableExtra::pack_rows(rel.tab.emp,"\\\\pncmech",n.sqrt.rel+1,n.sqrt.rel+n.clip.rel,escape=F)

      table.list=c(table.list,list("rel_errsummary_emp"=rel.tab.emp))

      file.name=paste0(folderout,"/",state.prefix,"_stacked_emp_rel_error_summary__",compare.suffix,".tex")
      print(file.name)
      writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
      CON=file(file.name,"a")
      writeLines(rel.tab.emp,CON)
      writeLines(caption_maker(measure="rel",conf.var="emp",configorder=configorder),CON)
      close(CON)

      #Absolute Error for Employment
      abs.tab.emp=knitr::kable(sub.st.abs[,c(cname.order.base,cname.order.emp)],col.names = c(base.cname.str,config.cname.str),
                               digits=andigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))

      abs.tab.emp=kableExtra::add_header_above(abs.tab.emp,c(" "=base.col.num,"Employment"=length(config.nms)),escape=F)
      abs.tab.emp=kableExtra::pack_rows(abs.tab.emp,"\\\\sqrtmech",1,n.sqrt.abs,escape=F)
      abs.tab.emp=kableExtra::pack_rows(abs.tab.emp,"\\\\pncmech",n.sqrt.abs+1,n.sqrt.abs+n.clip.abs,escape=F)



      table.list=c(table.list,list("abs_errsummary_emp"=abs.tab.emp))

      file.name=paste0(folderout,"/",state.prefix,"_stacked_emp_abs_error_summary__",compare.suffix,".tex")
      print(file.name)
      writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
      CON=file(file.name,"a")
      writeLines(abs.tab.emp,CON)
      writeLines(caption_maker(measure="abs",conf.var="emp",configorder=configorder),CON)
      close(CON)

      #Relative Error for Wages
      rel.tab.w=knitr::kable(sub.st.rel[,c(cname.order.base,cname.order.wages)],col.names = c(base.cname.str,config.cname.str),
                             digits=rndigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))

      rel.tab.w=kableExtra::add_header_above(rel.tab.w,c(" "=base.col.num,"Wages"=length(config.nms)),escape=F)
      rel.tab.w=kableExtra::pack_rows(rel.tab.w,"\\\\sqrtmech",1,n.sqrt.rel,escape=F)
      rel.tab.w=kableExtra::pack_rows(rel.tab.w,"\\\\pncmech",n.sqrt.rel+1,n.sqrt.rel+n.clip.rel,escape=F)


      table.list=c(table.list,list("rel_errsummary_wages"=rel.tab.w))

      file.name=paste0(folderout,"/",state.prefix,"_stacked_wages_rel_error_summary__",compare.suffix,".tex")
      print(file.name)
      writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
      CON=file(file.name,"a")
      writeLines(rel.tab.w,CON)
      writeLines(caption_maker(measure="rel",conf.var="wage",configorder=configorder),CON)
      #c(paste0("\\caption{Median relative error for wages across aggregate levels for synthetic ",
      #                    ifelse(grepl("nj34",state.prefix),"New Jersey","Rhode Island"),". Rows marked with * indicate the aggregate level was used in the sanitization process.}"),
      #             paste0("\\label{tab:relerr_wages_",gsub("blsvals_","",compare.suffix),"_",ifelse(grepl("nj34",state.prefix),"nj}","ri}")),
      #             "\\end{table}"),CON)
      close(CON)

      #Absolute Error for Wages
      abs.tab.w=knitr::kable(sub.st.abs[,c(cname.order.base,cname.order.wages)],col.names = c(base.cname.str,config.cname.str),
                             digits=andigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))

      abs.tab.w=kableExtra::add_header_above(abs.tab.w,c(" "=base.col.num,"Wages"=length(config.nms)),escape=F)
      abs.tab.w=kableExtra::pack_rows(abs.tab.w,"\\\\sqrtmech",1,n.sqrt.abs,escape=F)
      abs.tab.w=kableExtra::pack_rows(abs.tab.w,"\\\\pncmech",n.sqrt.abs+1,n.sqrt.abs+n.clip.abs,escape=F)


      table.list=c(table.list,list("abs_errsummary_wages"=abs.tab.w))

      file.name=paste0(folderout,"/",state.prefix,"_stacked_wages_abs_error_summary__",compare.suffix,".tex")
      print(file.name)
      writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
      CON=file(file.name,"a")
      writeLines(abs.tab.w,CON)
      writeLines(caption_maker(measure="abs",conf.var="wages",configorder=configorder),CON)
      close(CON)

  return(list("error.data"=list("rel.err.df"=sub.st.rel,"abs.err.df"=sub.st.abs),"aggstats"=aggstats,"table.list"=table.list))
}



special_wage_rel_err=function(folderin=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/compare_data"),
                              folderout=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/R_tables_plots"),
                                       st.compares=NULL,
                                       queries.in.create=c("State Total","State NAICS 5-digit",
                                                           "County Total","County NAICS 5-digit"),
                                       queries.in.create.codes=NULL,
                                       aggstats=NULL,
                                       aggcodes=NULL,
                                       keepaggcodes=NULL,
                                       removeaggcodes=c(50,70),
                              n_estab_stats=NULL,
                                       state.prefix="nj34_qbp_2016_1",
                                       ndigits=2,
                                       rndigits=2,
                                       andigits=0,
                                       tab.name.csv=NULL,
                                       short.config.names=T){

  combineAggLevelCode=T
  configorder="byacct"
  compare.suffix="wage_share"
  compare.suffix2="blsvals_wageAllocation"

  new.folderout=paste0(folderout,"/",compare.suffix)
  if(file.exists(new.folderout)==FALSE){
    dir.create(new.folderout)
  }

  if(is.null(st.compares)==TRUE){
    st.compares=read.csv(paste0(folderin,"/",state.prefix,"_",compare.suffix,"_agg_prep.csv"))
    st.compares2=read.csv(paste0(folderin,"/",state.prefix,"_",compare.suffix2,"_agg_prep.csv"))
    st.compares2=st.compares2[,grepl("agglvl|equalwage",colnames(st.compares2))]
    st.compares=dplyr::left_join(st.compares,st.compares2,by=dplyr::join_by(agglvl_code))
  }

  if(is.null(aggstats)==TRUE){
    aggstats=get_aggcode_info(aggcodes=aggcodes,
                              st.data=NULL,
                              folderin=folderin,
                              compare.suffix=compare.suffix,
                              state.prefix=state.prefix,
                              write.tex=paste0(folderout,"/agg_estnum_summary_",state.prefix,".tex"))

  }
  if(is.null(tab.name.csv)==FALSE){
    source(paste0(rprojroot::find_rstudio_root_file(),"/table_figure_naming_functions.R"))
    tabnamer=table_config_namer(folderin=folderin,folderout=folderout,state.prefix=state.prefix,
                                csv.fname=tab.name.csv,overwrite=F,functionfilepath=NULL,mudigits=1)
  }

  folderout=new.folderout

  #remove 1st row (it has with mean and median strings)
  #subset to get agglvl_code, month3_emplvl and total_qtrly_wages
  sub.st=st.compares[-1,grepl("agglvl|month3|wage",colnames(st.compares))]
  sub.st=sub.st[,!grepl("_dif|_rdif",colnames(sub.st))]
  cnames=colnames(sub.st)
  sub.st[,cnames]=lapply(sub.st[,cnames],as.numeric)
  if(is.null(keepaggcodes)==FALSE){
    sub.st=sub.st[sub.st$agglvl_code%in%keepaggcodes,]
  }else if(is.null(removeaggcodes)==FALSE){
    sub.st=sub.st[!(sub.st$agglvl_code%in%removeaggcodes),]
  }

  #extract the names of the configurations
  config.nms=config_extractor(cnames=colnames(sub.st),order=configorder)

  if(is.null(tab.name.csv)==FALSE){
    config.cname.str=tabnamer$shorttable[match(config.nms,tabnamer$configName)]
  }else{
    config.cname.str=config.nms
  }
  #nconfigs=length(config.nms)/2
  #config.nms.half=gsub("_sqrt|_clip","",config.nms[seq(1,nconfigs)])
  #config.cname.str=gsub("////sqroot|////shortpnc","",config.cname.str[seq(1,nconfigs)])


  caption_maker=function(st.prefix=state.prefix,
                         split.we=T,
                         split.ar=T,
                         measure="abs",
                         comp.suffix=compare.suffix,
                         starsadd=starsadded,
                         conf.var="emp",
                         configorder="byacct"){
    if(split.we==T){
      variable=ifelse(grepl("emp",conf.var),"employment","quarterly wages")
      var.lab=ifelse(grepl("emp",conf.var),"_emp_","_wages_")
    }else{
      variable="monthly employment and quarterly wages"
      var.lab="_"
    }
    if(split.ar==T){
      if(grepl("abs",measure)==T){
        measure.str="Average absolute error"
        measure.lab="abs_err"
      }else{
        measure.str="Median relative error"
        measure.lab="rel_err"
      }
    }else{
      measure.str="Average absolute error and median relative error"
      measure.lab="err"
    }
    state.str=paste("synthetic",ifelse(grepl("nj34",st.prefix),"New Jersey","Rhode Island"))
    state.lab=ifelse(grepl("nj34",st.prefix),"nj34","ri44")
    if(grepl("querynumber",comp.suffix)==T){
      experiment=paste("We compare configurations with varying number of group-by queries.",
                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."),
                       "For the column labeled `1', only the state total (51) is used in creating the PPM. For the configurations with two group-by queries, `2c' indicates the state and county queries (51 and 71) were used while `2n' indicates the state and NAICS5 queries (51 and 57) were used. The column `3' adds the County by NAICS5 (77) and `4' adds Supersector (53). The column `5' further adds and finally column `6' adds County by Supersector (57).",
                       "The identity share of the privacy budget remains fixed. The share of the remaining budget is split between the group-by queries  based on the number of groups in the query with higher allocation going to queries with more groups.")
    }else if(grepl("querychoice|querytype",comp.suffix)==T){
      experiment=paste("We compare configurations with a fixed number of group-by queries, while varying what those group-by queries are.",
                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."),
                       "All configurations still use State (51) and County (71) queries. The NAICS5 query is denoted 'N5'. The Supersector query is denoted 'SS'. The county by supersector and county by NAICS5 queries are denoted 'CxSS' and CxN5' respectively.")
    }else if(grepl("ident",comp.suffix)==T){
      experiment=paste("We multiply the share of the privacy budget that the identity query is allocated and compare the resulting PPM.",
                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."))
    }else if(grepl("wageAllocation|wage_share",comp.suffix)){
      experiment=paste("We compare configurations which alter the share of the query-level budgets allocated to wages. In the baseline parameters, less than 10\\% is allocated to wages. 'W\\%x2' indicates the wage share is doubled and '=W\\%' indicated the wage share is equal to each of the three employment budget allocations. Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort.",
                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."))
    }else if(grepl("budget",comp.suffix)==T){
      experiment=paste("We compare sanitized data across several total privacy budgets.",
                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."))
    }else if(grepl("queryAllocation",comp.suffix)){
      experiment=paste("We compare configurations which alter the query-level budget allocation.",

                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."),
                       "We denoted 1.5 times the baseline identity share as '1.5xI'. Increasing the allocation to the State aggregate query is denoted '3xState'. '1.25xGroups' inicates the share of the group-by queries is multiplied by 1.25.")
    }else if(grepl("clip_pro|clippro",comp.suffix)==T){
      experiment=paste("We compare sanitized data across several confidence parameters ($\\confparam$) for the \\pncmech, labeled \\pncshort. The baselines confidence parameter is $\\confparam=0.01$.",
                       ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort."))
    }else{
      experiment=
        ifelse(configorder=="byacct","","Configurations using the \\sqrtmech are labeled with a \\sqroot, and those using a \\pncmech are labeled with a \\pncshort.")
    }

    captionlines=paste0("\\caption{",measure.str," of ",variable,ifelse(split.ar==T," is "," are "),
                        "summarized across the cells of various aggregate levels for ",state.str,". ",experiment,
                        ifelse(starsadd==T," Rows marked with * indicate the aggregate level was used in the sanitization process.}","}"))
    labellines=paste0("\\label{tab:",measure.lab,var.lab,gsub("blsvals_","",comp.suffix),"_",ifelse(grepl("nj34",st.prefix),"nj}","ri}"))
    return(c(captionlines,labellines,"\\end{table}"))
  }

  sub.st=dplyr::left_join(sub.st,aggstats,by=dplyr::join_by(agglvl_code))
  if(is.null(queries.in.create.codes)==FALSE){
    sub.st$usedagg=ifelse(sub.st$agglvl_code %in% c(queries.in.create.codes),"*","")
  }else if(is.null(queries.in.create)==FALSE){
    sub.st$usedagg=ifelse(sub.st$agglvl %in% c(queries.in.create),"*","")
  }else{
    sub.st$usedagg="?"
  }




  #colnames(nj.compares)=c("agglvl_code",clip.nm,sqrt.nm)



  sub.st$newcode=paste0(sub.st$usedagg,sub.st$agglvl_code," ",sub.st$agglvl)
  sub.st$newcode=gsub("-digit","",sub.st$newcode)
  sub.st$newcode[!grepl("Total",sub.st$newcode)]=gsub("County |County by","County X",gsub("State |State by","State X",gsub("NAICS ","NAICS",sub.st$newcode[!grepl("Total",sub.st$newcode)])))
  sub.st$newcode=gsub("NAICSSector","Sector",sub.st$newcode)
  cname.order.base=c("newcode",unname(unlist(n_estab_stats)))
  base.cname.str=c("Aggregate Level",names(n_estab_stats))
  starsadded=T



  cname.order.emp=c(paste0("month3_emplvl_",config.nms))
  cname.order.wages=c(paste0("total_qtrly_wages_",config.nms))

  #base.cname.str=c("","Code","Aggregate Level",names(n_estab_stats))
  base.col.num=ifelse(combineAggLevelCode==TRUE,1,2)+length(n_estab_stats)
  n_estab.col.num=0 #antiquated




  #cname.order.base=c("newcode","agglvl",unname(unlist(n_estab_stats)))
  #base.cname.str=c("Code","Aggregate Level",names(n_estab_stats))


  sub.st.rel=sub.st[,c(cname.order.base,paste0(cname.order.emp,"_rel"),paste0(cname.order.wages,"_rel"))]
  n.rel=nrow(sub.st.rel)
  sub.st.abs=sub.st[,c(cname.order.base,paste0(cname.order.emp,"_abs"),paste0(cname.order.wages,"_abs"))]
  n.abs=nrow(sub.st.abs)

  new.cnames=c(cname.order.base,cname.order.emp,cname.order.wages)
  colnames(sub.st.rel)=new.cnames
  colnames(sub.st.abs)=new.cnames

  #sub.stack.rel=dplyr::bind_rows(sub.st.sqrt.rel,sub.st.clip.rel)
  #sub.st.abs=dplyr::bind_rows(sub.st.sqrt.abs,sub.st.clip.abs)

  config.cname.str=gsub("\\\\sqroot|\\\\shortpnc","",config.cname.str)

  table.list=NULL


  #Relative Error for Employment
  rel.tab.emp=knitr::kable(sub.st.rel[,c(cname.order.base,cname.order.emp)],col.names = c(base.cname.str,config.cname.str),
                           digits=rndigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))
    rel.tab.emp=kableExtra::add_header_above(rel.tab.emp,c(" "=base.col.num,
                                                           "\\\\sqrtmech"=length(config.nms)/2,
                                                           "\\\\pncmech"=length(config.nms)/2),escape=F)

  rel.tab.emp=kableExtra::add_header_above(rel.tab.emp,c(" "=base.col.num,"Employment"=length(config.nms)),escape=F)

  table.list=c(table.list,list("rel_errsummary_emp"=rel.tab.emp))

  file.name=paste0(folderout,"/",state.prefix,"_special_emp_rel_error_summary__",compare.suffix,".tex")
  print(file.name)
  writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
  CON=file(file.name,"a")
  writeLines(rel.tab.emp,CON)
  writeLines(caption_maker(measure="rel",conf.var="emp",configorder=configorder),CON)
  close(CON)

  #Absolute Error for Employment
  abs.tab.emp=knitr::kable(sub.st.abs[,c(cname.order.base,cname.order.emp)],col.names = c(base.cname.str,config.cname.str),
                           digits=andigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))

  abs.tab.emp=kableExtra::add_header_above(abs.tab.emp,c(" "=base.col.num,
                                                         "\\\\sqrtmech"=length(config.nms)/2,
                                                         "\\\\pncmech"=length(config.nms)/2),escape=F)
  abs.tab.emp=kableExtra::add_header_above(abs.tab.emp,c(" "=base.col.num,"Employment"=length(config.nms)),escape=F)


  table.list=c(table.list,list("abs_errsummary_emp"=abs.tab.emp))

  file.name=paste0(folderout,"/",state.prefix,"_special_emp_abs_error_summary__",compare.suffix,".tex")
  print(file.name)
  writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
  CON=file(file.name,"a")
  writeLines(abs.tab.emp,CON)
  writeLines(caption_maker(measure="abs",conf.var="emp",configorder=configorder),CON)
  close(CON)

  #Relative Error for Wages
  rel.tab.w=knitr::kable(sub.st.rel[,c(cname.order.base,cname.order.wages)],col.names = c(base.cname.str,config.cname.str),
                         digits=rndigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))

  rel.tab.w=kableExtra::add_header_above(rel.tab.w,c(" "=base.col.num,
                                                         "\\\\sqrtmech"=length(config.nms)/2,
                                                         "\\\\pncmech"=length(config.nms)/2),escape=F)

  rel.tab.w=kableExtra::add_header_above(rel.tab.w,c(" "=base.col.num,"Wages"=length(config.nms)),escape=F)


  table.list=c(table.list,list("rel_errsummary_wages"=rel.tab.w))

  file.name=paste0(folderout,"/",state.prefix,"_special_wages_rel_error_summary__",compare.suffix,".tex")
  print(file.name)
  writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
  CON=file(file.name,"a")
  writeLines(rel.tab.w,CON)
  writeLines(caption_maker(measure="rel",conf.var="wage",configorder=configorder),CON)
  #c(paste0("\\caption{Median relative error for wages across aggregate levels for synthetic ",
  #                    ifelse(grepl("nj34",state.prefix),"New Jersey","Rhode Island"),". Rows marked with * indicate the aggregate level was used in the sanitization process.}"),
  #             paste0("\\label{tab:relerr_wages_",gsub("blsvals_","",compare.suffix),"_",ifelse(grepl("nj34",state.prefix),"nj}","ri}")),
  #             "\\end{table}"),CON)
  close(CON)

  #Absolute Error for Wages
  abs.tab.w=knitr::kable(sub.st.abs[,c(cname.order.base,cname.order.wages)],col.names = c(base.cname.str,config.cname.str),
                         digits=andigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))

  abs.tab.w=kableExtra::add_header_above(abs.tab.w,c(" "=base.col.num,
                                                         "\\\\sqrtmech"=length(config.nms)/2,
                                                         "\\\\pncmech"=length(config.nms)/2),escape=F)

  abs.tab.w=kableExtra::add_header_above(abs.tab.w,c(" "=base.col.num,"Wages"=length(config.nms)),escape=F)

  table.list=c(table.list,list("abs_errsummary_wages"=abs.tab.w))

  file.name=paste0(folderout,"/",state.prefix,"_special_wages_abs_error_summary__",compare.suffix,".tex")
  print(file.name)
  writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
  CON=file(file.name,"a")
  writeLines(abs.tab.w,CON)
  writeLines(caption_maker(measure="abs",conf.var="wages",configorder=configorder),CON)
  close(CON)

  return(list("error.data"=list("rel.err.df"=sub.st.rel,"abs.err.df"=sub.st.abs),"aggstats"=aggstats,"table.list"=table.list))
}
