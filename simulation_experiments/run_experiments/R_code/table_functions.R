

source(paste0(rprojroot::find_rstudio_root_file(),"/comparison_config_functions.R"))
source(paste0(rprojroot::find_rstudio_root_file(),"/data_wrangling_functions.R"))


## get the min, median, mean, std.dev, max of the number of establishments in a cell
## and the number of cells for each aggegate level
### aggcodes is a dataframe of "agglvl_code" and "agg_lvl" columns which gives the name for each aggregate level code
### st.data is the state data (not aggregated),
### folderin is the folder where the st.data can be found if st.data=NULL
### compare.suffix is the config file suffix
### state.prefix is state abbreviation code _ qbp _ year _1
### write.tex is file name and path to write the latex table to. If NULL, no latex table is made.
### overwrite is logical to indicate if latex table should be overwritten if it exists.
##
### returns dataframe with agglvl_code, agglvl label, number of groups in agglvl,
###     and statistics on number of establishments in groups
###
## FROM dplyr NEED group_by(), summarize(), n(), if_else(), left_join()
## FROM knitr NEED kable(),
## FROM kableExtra NEED add_header_above()
###
get_aggcode_info=function(aggcodes=NULL,
                          st.data=NULL,
                          folderin=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/compare_data"),
                          compare.suffix="blsvals_accttype_clipprob",
                          state.prefix="nj34_qbp_2016_1",
                          write.tex=NULL,overwrite=F,
                          empwage.summarystats=F,one.table=F,tabdigits=0,quietly=F){

  if(is.null(aggcodes)==TRUE){
    aggcodes=aggcode_relabel()
  }else if(sum(colnames(aggcodes)=="agglvl")==0){
    aggcodes=aggcode_relabel(aggcodes=aggcodes)
  }else{
    aggcodes=aggcodes[,c("agglvl_code","agglvl")]
  }

  if(is.null(st.data)==TRUE){
    st.data=file_name_changes(fin=folderin,st.pref=state.prefix,comp.suff=compare.suffix,aggfile=F,quietly=T)
  }


  #summarize by agglvl_codes to get number of groups in agglvl, and statistics on number of establishments in group
  nest.sum.st.data=dplyr::summarize(dplyr::group_by(st.data,agglvl_code),
                               "num_groups"=dplyr::n(),
                               "min_n_estabs"=min(qtrly_estabs),
                               "med_n_estabs"=median(qtrly_estabs),
                               "avg_n_estabs"=mean(qtrly_estabs),
                               "sd_n_estabs"=dplyr::if_else(dplyr::n()>1,sqrt(var(qtrly_estabs)),NA),
                               "max_n_estabs"=max(qtrly_estabs))
  nest.sum.st.data=dplyr::left_join(nest.sum.st.data,aggcodes,by=dplyr::join_by(agglvl_code)) #join with agglvl labels
  nest.sum.st.data=nest.sum.st.data[,c("agglvl","agglvl_code","num_groups",paste0(c("min","med","max","avg","sd"),"_n_estabs"))]

  if(empwage.summarystats==F){

    texcaption=paste0("\\caption{Number of cells and establishments for each aggregate level code for the Synthetic",
                      ifelse(grepl("nj34",state.prefix),"New Jersey","Rhode Island"),".}")

    if(is.null(write.tex)==FALSE){
      if((file.exists(write.tex)==FALSE)|(overwrite==TRUE)){
        nest.sum.st.data=nest.sum.st.data[,c("agglvl","agglvl_code","num_groups",paste0(c("min","med","max","avg","sd"),"_n_estabs"))]
        write.csv(nest.sum.st.data,file=gsub(".tex",".csv",write.tex))

        tab=knitr::kable(nest.sum.st.data,col.names=c("Aggregate Level","Code","Cell Count","Min","Median","Max","Mean","Std.Dev."),
                         digits=tabdigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))
        tab=kableExtra::add_header_above(tab,c(" "=2," "=1,"Establishments in Cell Summary"=4),italic=T,escape=F)

        writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),write.tex)
        CON=file(write.tex,"a")
        writeLines(tab,CON)
        writeLines(c(texcaption,
                     paste0("\\label{tab:agg_estnum_summary",ifelse(grepl("nj34",state.prefix),"nj}","ri}")),
                     "\\end{table}"),CON)
        close(CON)
      }
    }
    return(nest.sum.st.data)
  }else{ #empwages.summarystats==T


    emp.sum.st.data=dplyr::summarize(dplyr::group_by(st.data,agglvl_code),
                                 "num_groups"=dplyr::n(),
                                 "min"=min(month3_emplvl),
                                 "med"=median(month3_emplvl),
                                 "avg"=mean(month3_emplvl),
                                 "sd"=dplyr::if_else(dplyr::n()>1,sqrt(var(month3_emplvl)),NA),
                                 "max"=max(month3_emplvl))
    wage.sum.st.data=dplyr::summarize(dplyr::group_by(st.data,agglvl_code),
                                     "num_groups"=dplyr::n(),
                                     "min"=min(total_qtrly_wages),
                                     "med"=median(total_qtrly_wages),
                                     "avg"=mean(total_qtrly_wages),
                                     "sd"=dplyr::if_else(dplyr::n()>1,sqrt(var(total_qtrly_wages)),NA),
                                     "max"=max(total_qtrly_wages))
    emp.sum.st.data=dplyr::left_join(emp.sum.st.data,aggcodes,by=dplyr::join_by(agglvl_code)) #join with agglvl labels
    emp.sum.st.data=emp.sum.st.data[,c("agglvl","agglvl_code","num_groups","min","med","max","avg","sd")]
    wage.sum.st.data=dplyr::left_join(wage.sum.st.data,aggcodes,by=dplyr::join_by(agglvl_code)) #join with agglvl labels
    wage.sum.st.data=wage.sum.st.data[,c("agglvl","agglvl_code","num_groups","min","med","max","avg","sd")]

    data.list=list("num_estabs"=nest.sum.st.data,"employment"=emp.sum.st.data,
                   "wages"=wage.sum.st.data)

      if(one.table==T){
        colnames(nest.sum.st.data)=gsub("_n_estabs","",colnames(nest.sum.st.data))

        texcaption=paste0("\\caption{Summary statistics for number of establishments, employment count, and quarterly wages for each aggregate level code for the Synthetic",
                          ifelse(grepl("nj34",state.prefix),"New Jersey","Rhode Island"),".}")

        nest.sum.st.data$variable="Establishment Count"
        emp.sum.st.data$variable="Employment"
        wage.sum.st.data$variable="Wages"

        sum.st.data=dplyr::bind_rows(nest.sum.st.data,emp.sum.st.data,wage.sum.st.data)
        sum.st.data$variable=factor(sum.st.data$variable,levels=c("Establishment Count","Employment","Wages"))
        sum.st.data=dplyr::arrange(sum.st.data,agglvl_code)
        sum.st.data=sum.st.data[,c("agglvl","agglvl_code","num_groups","min","med","max","avg","sd","variable")]
        tab=knitr::kable(sum.st.data[,c("variable","min","med","max","avg","sd")],col.names=c("Variable","Min","Median","Max","Mean","Std.Dev."),
                         digits=tabdigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))
        counter=1
        for(code in unique(nest.sum.st.data$agglvl_code)){
          codedf=nest.sum.st.data[nest.sum.st.data$agglvl_code==code,,drop=F]
          packlabel=paste0(codedf$agglvl[1],"(",code,") has ",codedf$num_groups[1]," cells")
          tab=kableExtra::pack_rows(tab,packlabel,start=counter,end=counter+2)
          counter=counter+3
        }
        if(is.null(write.tex)==FALSE){
          newtex_stem=gsub(".tex|.csv","",write.tex)
          write.tex=paste0(newtex_stem,"_all.tex")
          if((file.exists(write.tex)==FALSE)|(overwrite==TRUE)){

            sum.st.data=sum.st.data[,c("agglvl","agglvl_code","num_groups","min","med","max","avg","sd","variable")]
            write.csv(sum.st.data,file=gsub(".tex",".csv",write.tex))

            #write file
            writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),write.tex)
            CON=file(write.tex,"a")
            writeLines(tab,CON)
            writeLines(c(texcaption,
                         paste0("\\label{tab:agg_estnum_summary",ifelse(grepl("nj34",state.prefix),"nj","ri"),"_all}"),
                         "\\end{table}"),CON)
            close(CON)
          }
        }
        return(c(data.list,list("combined"=sum.st.data)))
      }else{ #saving each table
        if(is.null(write.tex)==FALSE){
          newtex_stem=gsub(".tex|.csv","",write.tex)
          headers.vec=c("Number of Establishments","Employment","Wages")
          label.vec=c("nestab","emp","wages")
          for(i in seq(1,length(data.list))){
            texcaption=paste0("\\caption{",headers.vec[i]," summary statistics for each aggregate level code for the Synthetic",
                          ifelse(grepl("nj34",state.prefix),"New Jersey","Rhode Island"),".}")
            write.tex=paste0(newtex_stem,"_",label.vec[i],".tex")
            if((file.exists(write.tex)==FALSE)|(overwrite==TRUE)){
            sum.st.data=data.list[[i]]
            sum.st.data=sum.st.data[,c("agglvl","agglvl_code","num_groups","min","med","max","avg","sd")]
            write.csv(sum.st.data,file=gsub(".tex",".csv",write.tex))
            tab=knitr::kable(sum.st.data,col.names=c("Aggregate Level","Code","Cell Count","Min","Median","Max","Mean","Std.Dev."),
                             digits=tabdigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))

            addheader.vec=c(2,1,4)
            names(addheader.vec)=c(" "," ",header.vec[i])
            inputlist=list("kabel_input"=tab,"header"=add.headervec,"italic"=T,"escape"=F)

            tab=do.call(kableExtra::add_header_above,args=inputlist)
            writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),write.tex)
            CON=file(write.tex,"a")
            writeLines(tab,CON)
            writeLines(c(paste0(texcaption,"}"),
                         paste0("\\label{tab:agg_estnum_summary",ifelse(grepl("nj34",state.prefix),"nj","ri"),"_",label.vec[i],"}"),
                         "\\end{table}"),CON)
            close(CON)
            }
          }
        }
        return(data.list)
      }
    } #end if empwages.summarystats==T
} #end function


#nj.summary.agglvls=get_aggcode_info()



#####################################

table_rel_err=function(folderin=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/compare_data"),
                       folderout=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/R_tables_plots"),
                       st.compares=NULL,
                       queries.in.create=c("State Total","State NAICS 5-digit",
                                           "County Total","County NAICS 5-digit"),
                       queries.in.create.codes=NULL,
                       aggstats=NULL,
                       aggcodes=NULL,
                       keepaggcodes=NULL,
                       removeaggcodes=c(50,70),
                       n_estab_stats=list("N"="num_groups","Mean (s.d.) n"="avg_sd_n_estabs","Median n"="med_n_estabs"),
                       compare.suffix="blsvals_accttype_clipprob",
                       state.prefix="nj34_qbp_2016_1",
                       ndigits=2,
                       rndigits=2,
                       andigits=0,
                       split.wage.emp=F,
                       split.abs.rel=T,
                       tab.name.csv=NULL,
                       short.config.names=T,
                       combineAggLevelCode=T,
                       configorder="byacct",
                       quietly=F){

  new.folderout=paste0(folderout,"/",compare.suffix)
  if(file.exists(new.folderout)==FALSE){
    dir.create(new.folderout)
  }

  if(is.null(st.compares)==TRUE){
    st.compares=file_name_changes(fin=folderin,st.pref=state.prefix,comp.suff=compare.suffix,aggfile=T,quietly=T)#read.csv(paste0(folderin,"/",state.prefix,"_",compare.suffix,"_agg_prep.csv"))
  }

  if(is.null(aggstats)==TRUE){
    aggstats=get_aggcode_info(aggcodes=aggcodes,
                              st.data=NULL,
                              folderin=folderin,
                              compare.suffix=compare.suffix,
                              state.prefix=state.prefix,
                              write.tex=paste0(folderout,"/agg_estnum_summary_",state.prefix,".tex"),
                              quietly=quietly)

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
  if(grepl("querynumber",compare.suffix)){
    config.nms=config.nms[!grepl("blsvals_sqrt|blsvals_clip",config.nms)] #remove base
  }
  if(is.null(tab.name.csv)==FALSE){
    config.cname.str=tabnamer$shorttable[match(config.nms,tabnamer$configName)]
  }else{
    config.cname.str=config.nms
  }

  caption_maker=function(st.prefix=state.prefix,
                         split.we=split.wage.emp,
                         split.ar=split.abs.rel,
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

  sub.st$avg_sd_n_estabs=paste0(round(sub.st$avg_n_estabs,ndigits)," (",round(sub.st$sd_n_estabs,ndigits),")")
  sub.st$avg_sd_n_estabs=gsub(" \\(NA\\)","",sub.st$avg_sd_n_estabs)


  #colnames(nj.compares)=c("agglvl_code",clip.nm,sqrt.nm)


  if(grepl("querynumber|querytype|querychoice",compare.suffix)==TRUE){
    if(combineAggLevelCode==T){
      sub.st$newcode=paste0(sub.st$agglvl_code," ",sub.st$agglvl)
      sub.st$newcode=gsub("-digit","",sub.st$newcode)
      sub.st$newcode[!grepl("Total",sub.st$newcode)]=gsub("County |County by","County X",gsub("State |State by","State X",gsub("NAICS "," NAICS",sub.st$newcode[!grepl("Total",sub.st$newcode)])))
      sub.st$newcode=gsub("NAICSSector"," Sector",sub.st$newcode)
      cname.order.base=c("newcode",unname(unlist(n_estab_stats)))
      base.cname.str=c("Aggregate Level",names(n_estab_stats))
    }else{
      sub.st$newcode=paste0(sub.st$agglvl_code)
      cname.order.base=c("newcode","agglvl",unname(unlist(n_estab_stats)))
      base.cname.str=c("Code","Aggregate Level",names(n_estab_stats))
    }
    starsadded=F
  }else{
    if(combineAggLevelCode==T){
      sub.st$newcode=paste0(sub.st$usedagg,sub.st$agglvl_code," ",sub.st$agglvl)
      sub.st$newcode=gsub("-digit","",sub.st$newcode)
      sub.st$newcode[!grepl("Total",sub.st$newcode)]=gsub("County |County by","County X",gsub("State |State by","State X",gsub("NAICS ","NAICS",sub.st$newcode[!grepl("Total",sub.st$newcode)])))
      sub.st$newcode=gsub("NAICSSector","Sector",sub.st$newcode)
      cname.order.base=c("newcode",unname(unlist(n_estab_stats)))
      base.cname.str=c("Aggregate Level",names(n_estab_stats))
    }else{
    sub.st$newcode=paste0(sub.st$usedagg,sub.st$agglvl_code)
    cname.order.base=c("newcode",unname(unlist(n_estab_stats)))
    base.cname.str=c("Code","Aggregate Level",names(n_estab_stats))
    }
    starsadded=T
  }



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

  sub.stack=dplyr::bind_rows(sub.st.rel,sub.st.abs)

  if(configorder=="byacct"){
    config.cname.str=gsub("\\\\shortpnc|\\\\sqroot","",config.cname.str)
  }
  table.list=NULL

  if(split.wage.emp==FALSE){
    if(split.abs.rel==FALSE){

      tab=knitr::kable(sub.stack,col.names = c(base.cname.str,config.cname.str,config.cname.str),
                       digits=ndigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))
      if((configorder=="byacct")&(!grepl("accttype",compare.suffix))){
        tab=kableExtra::add_header_above(tab,c(" "=base.col.num,
                                               "\\\\sqrtmech"=length(config.nms)/2,
                                               "\\\\pncmech"=length(config.nms)/2,
                                               "\\\\sqrtmech"=length(config.nms)/2,
                                               "\\\\pncmech"=length(config.nms)/2),escape=F)

      }

      tab=kableExtra::add_header_above(tab,c(" "=base.col.num,"Employment"=length(config.nms),"Wages"=length(config.nms)),escape=F)
      tab=kableExtra::pack_rows(tab,"Median Relative Error",1,n.rel)
      tab=kableExtra::pack_rows(tab,"Mean Absolute Error",n.rel+1,n.rel+n.abs)

      table.list=c(table.list,list("abs_rel_errsummary_emp_wage"=tab))
      file.name=paste0(folderout,"/",state.prefix,"_emp_wage_abs_rel_error_summary__",compare.suffix,".tex")
      #print(file.name)
      writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
      CON=file(file.name,"a")
      writeLines(tab,CON)
      writeLines(caption_maker(configorder=configorder),CON)
      #c(paste0("\\caption{Median relative error and mean absolute error across aggregate levels for synthetic ",
      #                    ifelse(grepl("nj34",state.prefix),"New Jersey","Rhode Island"),
      #                    ifelse(starsadded==TRUE,". Rows marked with * indicate the aggregate level was used in the sanitization process.}",".}")),
      #             paste0("\\label{tab:err_",gsub("blsvals_","",compare.suffix),"_",ifelse(grepl("nj34",state.prefix),"nj}","ri}")),
      #             "\\end{table}"),CON)
      close(CON)
    }else{
      #relative error
      reltab=knitr::kable(sub.st.rel,col.names = c(base.cname.str,config.cname.str,config.cname.str),
                          digits=rndigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))
      if((configorder=="byacct")&(!grepl("accttype",compare.suffix))){
        reltab=kableExtra::add_header_above(reltab,c(" "=base.col.num,
                                               "\\sqrtmech"=length(config.nms)/2,
                                               "\\pncmech"=length(config.nms)/2,
                                               "\\sqrtmech"=length(config.nms)/2,
                                               "\\pncmech"=length(config.nms)/2),escape=F)

      }
      reltab=kableExtra::add_header_above(reltab,c(" "=base.col.num,"Employment"=length(config.nms),"Wages"=length(config.nms)),escape=F)

      table.list=c(table.list,list("rel_errsummary_emp_wage"=reltab))
      relfile.name=paste0(folderout,"/",state.prefix,"_emp_wage_rel_error_summary__",compare.suffix,".tex")
      #print(relfile.name)
      writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),relfile.name)
      CON=file(relfile.name,"a")
      writeLines(reltab,CON)
      writeLines(caption_maker(measure="rel",configorder=configorder),CON)
      #c(paste0("\\caption{Median relative error across aggregate levels for synthetic ",
      #                    ifelse(grepl("nj34",state.prefix),"New Jersey","Rhode Island"),
      #                    ifelse(starsadded==TRUE,". Rows marked with * indicate the aggregate level was used in the sanitization process.}",".}")),
      #             paste0("\\label{tab:relerr_",gsub("blsvals_","",compare.suffix),"_",ifelse(grepl("nj34",state.prefix),"nj}","ri}")),
      #             "\\end{table}"),CON)
      close(CON)

      #absolute error
      abstab=knitr::kable(sub.st.abs,col.names = c(base.cname.str,config.cname.str,config.cname.str),
                          digits=andigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))
      if((configorder=="byacct")&(!grepl("accttype",compare.suffix))){
        abstab=kableExtra::add_header_above(abstab,c(" "=base.col.num,
                                               "\\\\sqrtmech"=length(config.nms)/2,
                                               "\\\\pncmech"=length(config.nms)/2,
                                               "\\\\sqrtmech"=length(config.nms)/2,
                                               "\\\\pncmech"=length(config.nms)/2),escape=F)

      }
      abstab=kableExtra::add_header_above(abstab,c(" "=base.col.num,"Employment"=length(config.nms),"Wages"=length(config.nms)),escape=F)


      table.list=c(table.list,list("abs_errsummary_emp_wage"=abstab))

      absfile.name=paste0(folderout,"/",state.prefix,"_emp_wage_abs_error_summary__",compare.suffix,".tex")
      #print(absfile.name)
      writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),absfile.name)
      CON=file(absfile.name,"a")
      writeLines(abstab,CON)
      writeLines(caption_maker(measure="abs",configorder=configorder),CON)
      #c(paste0("\\caption{Mean Absolute error across aggregate levels for synthetic ",
      #                    ifelse(grepl("nj34",state.prefix),"New Jersey","Rhode Island"),
      #                    ifelse(starsadded==TRUE,". Rows marked with * indicate the aggregate level was used in the sanitization process.}",".}")),
      #             paste0("\\label{tab:abserr_",gsub("blsvals_","",compare.suffix),"_",ifelse(grepl("nj34",state.prefix),"nj}","ri}")),
      #             "\\end{table}"),CON)
      close(CON)


    }
  }else{
    if(split.abs.rel==FALSE){
      tab.emp=knitr::kable(sub.stack[,c(cname.order.base,cname.order.emp)],col.names = c(base.cname.str,config.cname.str),
                           digits=ndigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))
      if((configorder=="byacct")&(!grepl("accttype",compare.suffix))){
        tab.emp=kableExtra::add_header_above(tab.emp,c(" "=base.col.num,
                                               "\\\\sqrtmech"=length(config.nms)/2,
                                               "\\\\pncmech"=length(config.nms)/2),escape=F)

      }
      tab.emp=kableExtra::add_header_above(tab.emp,c(" "=base.col.num,"Employment"=length(config.nms)),escape=F)
      tab.emp=kableExtra::pack_rows(tab.emp,"Median Relative Error",1,n.rel)
      tab.emp=kableExtra::pack_rows(tab.emp,"Mean Absolute Error",n.rel+1,n.rel+n.abs)

      table.list=c(table.list,list("abs_rel_errsummary_emp"=tab.emp))

      file.name=paste0(folderout,"/",state.prefix,"_emp_abs_rel_error_summary__",compare.suffix,".tex")
      #print(file.name)
      writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
      CON=file(file.name,"a")
      writeLines(tab.emp,CON)
      writeLines(caption_maker(conf.var="emp",configorder=configorder),CON)
      #c(paste0("\\caption{Median relative error and mean absolute error for employment across aggregate levels for synthetic ",
      #                    ifelse(grepl("nj34",state.prefix),"New Jersey","Rhode Island"),
      #                    ifelse(starsadded==TRUE,". Rows marked with * indicate the aggregate level was used in the sanitization process.}",".}")),
      #             paste0("\\label{tab:err_emp_",gsub("blsvals_","",compare.suffix),"_",ifelse(grepl("nj34",state.prefix),"nj}","ri}")),
      #             "\\end{table}"),CON)
      close(CON)

      tab.w=knitr::kable(sub.stack[,c(cname.order.base,cname.order.wages)],col.names = c(base.cname.str,config.cname.str),
                         digits=ndigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))
      if((configorder=="byacct")&(!grepl("accttype",compare.suffix))){
        tab.w=kableExtra::add_header_above(tab.w,c(" "=base.col.num,
                                               "\\\\sqrtmech"=length(config.nms)/2,
                                               "\\\\pncmech"=length(config.nms)/2),escape=F)

      }
      tab.w=kableExtra::add_header_above(tab.w,c(" "=base.col.num,"Wages"=length(config.nms)),escape=F)
      tab.w=kableExtra::pack_rows(tab.w,"Median Relative Error",1,n.rel)
      tab.w=kableExtra::pack_rows(tab.w,"Mean Absolute Error",n.rel+1,n.rel+n.abs)

      table.list=c(table.list,list("abs_rel_errsummary_wages"=tab.w))

      file.name=paste0(folderout,"/",state.prefix,"_wages_error_summary__",compare.suffix,".tex")
      writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
      CON=file(file.name,"a")
      writeLines(tab.w,CON)
      writeLines(caption_maker(conf.var="wage",configorder=configorder),CON)
      # c(paste0("\\caption{Median relative error and mean absolute error for quarterly wages across aggregate levels for synthetic ",
      #                     ifelse(grepl("nj34",state.prefix),"New Jersey","Rhode Island"),". Rows marked with * indicate the aggregate level was used in the sanitization process.}"),
      #              paste0("\\label{tab:err_wages_",gsub("blsvals_","",compare.suffix),"_",ifelse(grepl("nj34",state.prefix),"nj}","ri}")),
      #              "\\end{table}"),CON)
      close(CON)
    }else{
      #Relative Error for Employment
      rel.tab.emp=knitr::kable(sub.st.rel[,c(cname.order.base,cname.order.emp)],col.names = c(base.cname.str,config.cname.str),
                               digits=rndigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))
      if((configorder=="byacct")&(!grepl("accttype",compare.suffix))){
        rel.tab.emp=kableExtra::add_header_above(rel.tab.emp,c(" "=base.col.num,
                                               "\\\\sqrtmech"=length(config.nms)/2,
                                               "\\\\pncmech"=length(config.nms)/2),escape=F)

      }
      rel.tab.emp=kableExtra::add_header_above(rel.tab.emp,c(" "=base.col.num,"Employment"=length(config.nms)),escape=F)


      table.list=c(table.list,list("rel_errsummary_emp"=rel.tab.emp))

      file.name=paste0(folderout,"/",state.prefix,"_emp_rel_error_summary__",compare.suffix,".tex")
      #print(file.name)
      writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
      CON=file(file.name,"a")
      writeLines(rel.tab.emp,CON)
      writeLines(caption_maker(measure="rel",conf.var="emp",configorder=configorder),CON)
      close(CON)

      #Absolute Error for Employment
      abs.tab.emp=knitr::kable(sub.st.abs[,c(cname.order.base,cname.order.emp)],col.names = c(base.cname.str,config.cname.str),
                               digits=andigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))
      if((configorder=="byacct")&(!grepl("accttype",compare.suffix))){
        abs.tab.emp=kableExtra::add_header_above(abs.tab.emp,c(" "=base.col.num,
                                               "\\\\sqrtmech"=length(config.nms)/2,
                                               "\\\\pncmech"=length(config.nms)/2),escape=F)

      }
      abs.tab.emp=kableExtra::add_header_above(abs.tab.emp,c(" "=base.col.num,"Employment"=length(config.nms)),escape=F)


      table.list=c(table.list,list("abs_errsummary_emp"=abs.tab.emp))

      file.name=paste0(folderout,"/",state.prefix,"_emp_abs_error_summary__",compare.suffix,".tex")
      #print(file.name)
      writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
      CON=file(file.name,"a")
      writeLines(abs.tab.emp,CON)
      writeLines(caption_maker(measure="abs",conf.var="emp",configorder=configorder),CON)
      close(CON)

      #Relative Error for Wages
      rel.tab.w=knitr::kable(sub.st.rel[,c(cname.order.base,cname.order.wages)],col.names = c(base.cname.str,config.cname.str),
                             digits=rndigits,format="latex",booktabs=T,escape=F,linesep=c(rep("",7),"\\addlinespace"))
      if((configorder=="byacct")&(!grepl("accttype",compare.suffix))){
        rel.tab.w=kableExtra::add_header_above(rel.tab.w,c(" "=base.col.num,
                                               "\\\\sqrtmech"=length(config.nms)/2,
                                               "\\\\pncmech"=length(config.nms)/2),escape=F)

      }
      rel.tab.w=kableExtra::add_header_above(rel.tab.w,c(" "=base.col.num,"Wages"=length(config.nms)),escape=F)


      table.list=c(table.list,list("rel_errsummary_wages"=rel.tab.w))

      file.name=paste0(folderout,"/",state.prefix,"_wages_rel_error_summary__",compare.suffix,".tex")
      #print(file.name)
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
      if((configorder=="byacct")&(!grepl("accttype",compare.suffix))){
        abs.tab.w=kableExtra::add_header_above(abs.tab.w,c(" "=base.col.num,
                                               "\\\\sqrtmech"=length(config.nms)/2,
                                               "\\\\pncmech"=length(config.nms)/2),escape=F)

      }
      abs.tab.w=kableExtra::add_header_above(abs.tab.w,c(" "=base.col.num,"Wages"=length(config.nms)),escape=F)


      table.list=c(table.list,list("abs_errsummary_wages"=abs.tab.w))

      file.name=paste0(folderout,"/",state.prefix,"_wages_abs_error_summary__",compare.suffix,".tex")
      #print(file.name)
      writeLines(c("\\begin{table}[!htb]","\\centering","\\small"),file.name)
      CON=file(file.name,"a")
      writeLines(abs.tab.w,CON)
      writeLines(caption_maker(measure="abs",conf.var="wages",configorder=configorder),CON)
      close(CON)

    }
  }
  return(list("error.data"=list("rel.err.df"=sub.st.rel,"abs.err.df"=sub.st.abs),"aggstats"=aggstats,"table.list"=table.list))

}



fixed_query_rel_err=function(basefolder=basepath,
                             comparefolder=NULL,
                             Rtabsfolder=NULL,
                             state.prefixes=c("nj34_qbp_2016_1"),
                             removeaggcodes=c(50,70),
                             queries.in.create=c("State Total","State NAICS 5-digit",
                                                 "County Total","County NAICS 5-digit"),
                             n_estab_stats=list("N"="num_groups","Mean (s.d.) n"="avg_sd_n_estabs","Median n"="med_n_estabs"),
                             ndigits=2,rndigits=2,andigits=0,split.wage.emp=T,split.abs.rel=F,savename="fixed_query_err_tables.Rda",
                             tab.name.csv=NULL,quietly=F){
  if(is.null(comparefolder)==T){
    comparefolder=paste0(basefolder,"/compare_data")
  }
  if(is.null(Rtabsfolder)==T){
    Rtabsfolder=paste0(basefolder,"/R_tables_plots")
  }
  files.info=comparison_stems(folderin=comparefolder,state.prefixes =state.prefixes)
  compare.suffixes=files.info[[1]]
  state.prefixes=files.info[[2]]
  aggcodes=aggcode_relabel(aggcodes=NULL,
                           onlybyowner=T,
                           bysizeclass=F,
                           keepcodes=NULL,
                           aggcodefolder=Rtabsfolder,
                           aggcodefname="agg_level_titles.csv",
                           newfname="agg_level_titles_processed.csv")
  states.aggstats=NULL
  for(s.pref in c(state.prefixes)){
    print(paste("Making tables for",s.pref," for comparisons: ",paste0(compare.suffixes,collapse=", ")))
    if(length(compare.suffixes)>1){
      aggstats=get_aggcode_info(aggcodes=aggcodes,
                                st.data=NULL,
                                folderin=comparefolder,
                                compare.suffix=compare.suffixes[1],
                                state.prefix=s.pref,
                                write.tex=paste0(Rtabsfolder,"/agg_estnum_summary_",s.pref,".tex"),quietly=quietly)
      oldnames=names(states.aggstats)
      states.aggstats=c(states.aggstats,list(aggstats))
      names(states.aggstats)=c(oldnames,s.pref)
      err.dfs=lapply(compare.suffixes,
                     function(c.suf)table_rel_err(folderin=comparefolder,
                                                  folderout=Rtabsfolder,
                                                  st.compares=NULL,
                                                  queries.in.create=queries.in.create,
                                                  queries.in.create.codes=NULL,
                                                  aggstats=aggstats,
                                                  aggcodes=aggcodes,
                                                  keepaggcodes=NULL,
                                                  removeaggcodes=removeaggcodes,
                                                  n_estab_stats=n_estab_stats,
                                                  compare.suffix=c.suf,
                                                  state.prefix=s.pref,
                                                  ndigits=ndigits,
                                                  rndigits=rndigits,
                                                  andigits=andigits,
                                                  split.wage.emp=split.wage.emp,
                                                  split.abs.rel=split.abs.rel,
                                                  tab.name.csv = tab.name.csv,quietly=quietly))
    }else{
      aggstats=get_aggcode_info(aggcodes=aggcodes,st.data=NULL,
                                folderin=comparefolder,
                                compare.suffix=compare.suffixes,
                                state.prefix=s.pref,
                                write.tex=paste0(Rtabsfolder,"/agg_estnum_summary_",s.pref,".tex"),quietly=quietly)
      err.dfs=table_rel_err(folderin=comparefolder,
                            folderout=Rtabsfolder,
                            st.compares=NULL,
                            queries.in.create=queries.in.create,
                            queries.in.create.codes=NULL,
                            aggstats=aggstats,
                            aggcodes=aggcodes,
                            keepaggcodes=NULL,
                            removeaggcodes=removeaggcodes,
                            n_estab_stats=n_estab_stats,
                            compare.suffix=compare.suffixes,
                            state.prefix=s.pref,
                            ndigits=ndigits,
                            rndigits=rndigits,
                            andigits=andigits,
                            split.wage.emp=split.wage.emp,
                            split.abs.rel=split.abs.rel,
                            tab.name.csv = tab.name.csv,quietly=quietly)
    }
  }
  save(err.dfs,aggcodes,states.aggstats,file=paste0(Rtabsfolder,"/",savename))
  return(err.dfs)
}

