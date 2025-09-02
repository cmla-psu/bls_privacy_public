library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)


source(paste0(rprojroot::find_rstudio_root_file(),"/data_wrangling_functions.R"))


compare_sqrt_clip_topk_plots=function(sanagg,confdf,agglvl_code,k=NA,pper=NA,state.prefix=state.prefixes,
                                      basesize=8,add.theme=NULL,add.legend.theme=NULL,bynestab=F,estab_breaks=NULL,min_nestabs=NA,
                                      colpalette=c("blue","orange"),combine_emp_wages=F,topksubtitle=F){
  out=compare_sqrt_clip_topk(sanagg = sanagg,confdf=confdf,agglvl_code = agglvl_code,k=k,pper=pper,state.prefix = state.prefix,
                             bynestab=bynestab,estab_breaks=estab_breaks)
  confdf.sub=out[[1]]
  legend_label=out[[2]]
  if(is.na(min_nestabs)==FALSE){
    confdf.sub=confdf.sub[confdf.sub$qtrly_estabs>min_nestabs,]
  }
  if(topksubtitle==T){
  if(is.na(pper)==T){
    title_add=paste0("Percent of Cell Total From Largest ",k," Values")
  }else{
    title_add=paste0("Percent of Cell Total From Largest ",pper*100,"% Establishments")
  }
  }else{
    title_add=NULL
  }

  plt_list=list()

  if(combine_emp_wages==T){
    if(is.na(min_nestabs)==FALSE){
      legend_label=paste0(legend_label," (>",min_nestabs," Estabs in Group)")
    }
    if(bynestab==TRUE){
      overtitle=paste0(legend_label," by Number of Establishments in Group")
    }else{
       overtitle=legend_label
    }

    wagedf=confdf.sub[(is.na(confdf.sub$wage_topkpercent)==FALSE),c("wage_topkpercent","total_qtrly_wages","wagediff","estab_bin","qtrly_estabs","wageratio")]
    colnames(wagedf)=c("topkpercent","orig_val","diff","estab_bin","qtrly_estabs","ratio")
    wagedf$val_type="wages"
    empdf=confdf.sub[(is.na(confdf.sub$month3_emplvl_topkpercent)==FALSE),c("month3_emplvl_topkpercent","month3_emplvl","empdiff","estab_bin","qtrly_estabs","empratio")]
    colnames(empdf)=c("topkpercent","orig_val","diff","estab_bin","qtrly_estabs","ratio")
    empdf$val_type="employment"

    plotdf=dplyr::bind_rows(empdf,wagedf)


    endplots=ggplot(plotdf,aes(x=orig_val,y=diff))+
      geom_point(aes(col=val_type),shape=20,alpha=0.5)+
      geom_hline(aes(yintercept=0),linetype="dashed")+
      theme_bw(base_size = basesize)+
      labs(x="Original Group Value",y="pnc-sqrt Rel. Err.",col="Value Type")+
      ggtitle(overtitle)+scale_color_manual(values=colpalette)


    diffplots=ggplot(plotdf)+
      geom_point(aes(x=topkpercent,y=diff,col=val_type),shape=20,alpha=0.5)+
      geom_hline(aes(yintercept=0),linetype="dashed")+
      theme_bw(base_size = basesize)+#theme(legend.position = "none")+
      labs(x="% Value of Top Estabs",y="pnc-sqrt Relative Error",col="Value Type")+
      ggtitle(overtitle,subtitle=title_add)+scale_color_manual(values=colpalette)


    nestabplots=ggplot(plotdf,aes(x=qtrly_estabs,y=diff))+
      geom_point(aes(col=val_type),shape=20,alpha=0.5)+
      geom_hline(aes(yintercept=0),linetype="dashed")+
      theme_bw(base_size = basesize)+
      labs(x="Number of Estabs",y="pnc-sqrt Rel. Err.",col="Value Type")+
      ggtitle(overtitle)+scale_color_manual(values=colpalette)

    ratioplots=ggplot(plotdf)+
      geom_point(aes(x=topkpercent,y=ratio,col=val_type),shape=20,alpha=0.5)+
      geom_hline(aes(yintercept=1),linetype="dashed")+
      theme_bw(base_size = basesize)+#theme(legend.position = "none")+
      labs(x="% Value of Top Estabs",y="pnc Rel. Err./ sqrt Rel. Err.",col="Value Type")+
      ggtitle(overtitle,subtitle=title_add)+scale_color_manual(values=colpalette)

    ratioplotsne=ggplot(plotdf)+
      geom_point(aes(x=qtrly_estabs,y=ratio,col=val_type),shape=20,alpha=0.5)+
      geom_hline(aes(yintercept=1),linetype="dashed")+
      theme_bw(base_size = basesize)+#theme(legend.position = "none")+
      labs(x="Number of Estabs",y="pnc Rel. Err./ sqrt Rel. Err.",col="Value Type")+
      ggtitle(overtitle,subtitle=title_add)+scale_color_manual(values=colpalette)

    if(is.null(add.legend.theme)==FALSE){
      diffplots=diffplots+add.legend.theme
      nestabplots=nestabplots+add.legend.theme
      endplots=endplots+add.legend.theme
      ratioplots=ratioplots+add.legend.theme
      ratioplotsne=ratioplotsne+add.legend.theme
    }

    if(bynestab==T){
      endplots= endplots+facet_wrap(~estab_bin,scales="free")
      diffplots=diffplots+facet_wrap(~estab_bin,scales="free")
      nestabplots=nestabplots+facet_wrap(~estab_bin,scales="free")
      ratioplots=ratioplots+facet_wrap(~estab_bin,scales="free")
      ratioplotsne=ratioplotsne+facet_wrap(~estab_bin,scales="free")
    }
    if(is.null(add.theme)==FALSE){
      endplots= endplots+add.theme
      diffplots=diffplots+add.theme
      nestabplots=nestabplots+add.theme
      ratioplots=ratioplots+add.theme
      ratioplotsne=ratioplotsne+add.theme
    }

  }else{


  wage_plot=ggplot(confdf.sub[!is.na(confdf.sub$wage_topkpercent),],aes(x=total_qtrly_wages,y=wagediff))+
    geom_point(shape=20)+
    geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
    #geom_smooth()+
    theme_bw(base_size = basesize)+
    ggtitle(paste("Wages"))+
    labs(x="Original Group Wages",y="pnc-sqrt Rel. Err.")#,col="Mechanism",fill="Mechanism")+

  if(bynestab==T){
    wage_plot=wage_plot+facet_wrap(~estab_bin,scales="free")
  }

  emp_plot=ggplot(confdf.sub[!is.na(confdf.sub$month3_emplvl_topkpercent),],aes(x=month3_emplvl,y=empdiff))+
    geom_point(shape=20)+
    geom_hline(aes(yintercept=0),col="red",linetype="dashed")+

    theme_bw(base_size = basesize)+
    ggtitle(paste("Employment"))+
    labs(x="Original Group Employment",y="pnc-sqrt Rel. Err.")#,col="Mechanism",fill="Mechanism")+
  #scale_color_manual(values=colpalette[c(2,4)])+scale_fill_manual(values=colpalette[c(1,3)])

  #emp_plot=emp_plot+theme(legend.position = "none")

  wagediff_plot=ggplot(confdf.sub[!is.na(confdf.sub$wage_topkpercent),])+
    geom_point(aes(x=wage_topkpercent,y=wagediff),shape=20)+
    geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
    #geom_smooth(aes(x=wage_topkpercent,y=wagediff),se=F)+
    theme_bw(base_size = basesize)+#theme(legend.position = "none")+
    labs(x="% Wages of Top Estabs",y="pnc-sqrt Relative Error")+
    ggtitle(paste("Wages:"),subtitle=title_add)#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")

  empdiff_plot=ggplot(confdf.sub[!is.na(confdf.sub$month3_emplvl_topkpercent),])+
    geom_point(aes(x=month3_emplvl_topkpercent,y=empdiff),shape=20)+
    geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
    #geom_smooth(aes(x=month3_emplvl_topkpercent,y=empdiff),se=F)+
    theme_bw(base_size = basesize)+#theme(legend.position = "none")+
    labs(x="% Employment of Top Estabs",y="pnc-sqrt Relative Error")+
    ggtitle(paste("Employment:"),subtitle=title_add)#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")

  wagenestab_plot=ggplot(confdf.sub[!is.na(confdf.sub$wage_topkpercent),],aes(x=qtrly_estabs,y=wagediff))+
    geom_point(shape=20)+
    geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
    #geom_smooth()+
    theme_bw(base_size = basesize)+#theme(legend.position = "none")+
    labs(x="Number of Estabs.",y="pnc-sqrt Rel. Err.")+
    ggtitle(paste("Wages"))#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")

  empnestab_plot=ggplot(confdf.sub,aes(x=qtrly_estabs,y=empdiff))+
    geom_point(shape=20)+
    geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
    #geom_smooth()+
    theme_bw(base_size = basesize)+#theme(legend.position = "none")+
    labs(x="Number of Estabs",y="pnc-sqrt Rel. Err.")+
    ggtitle(paste("Employment"))#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")

  # wagenestab_plot=ggplot(confdf.sub[!is.na(confdf.sub$wage_topkpercent),],aes(x=qtrly_estabs,y=wagediff))+
  #   geom_point(shape=20)+
  #   geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
  #   #geom_smooth()+
  #   theme_bw(base_size = basesize)+#theme(legend.position = "none")+
  #   labs(x="Number of Estabs.",y="pnc-sqrt Rel. Err.")+
  #   ggtitle(paste("Wages"))#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")
  #
  # empnestab_plot=ggplot(confdf.sub,aes(x=qtrly_estabs,y=empdiff))+
  #   geom_point(shape=20)+
  #   geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
  #   #geom_smooth()+
  #   theme_bw(base_size = basesize)+#theme(legend.position = "none")+
  #   labs(x="Number of Estabs",y="pnc-sqrt Rel. Err.")+
  #   ggtitle(paste("Employment"))#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")

  wageratio_plot=ggplot(confdf.sub[!is.na(confdf.sub$wage_topkpercent),])+
    geom_point(aes(x=wage_topkpercent,y=wageratio),shape=20)+
    geom_hline(aes(yintercept=1),col="red",linetype="dashed")+
    #geom_smooth(aes(x=wage_topkpercent,y=wagediff),se=F)+
    theme_bw(base_size = basesize)+#theme(legend.position = "none")+
    labs(x="% Wages of Top Estabs",y="pnc-sqrt Relative Error")+
    ggtitle(paste("Wages:"),subtitle=title_add)#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")

  empratio_plot=ggplot(confdf.sub[!is.na(confdf.sub$month3_emplvl_topkpercent),])+
    geom_point(aes(x=month3_emplvl_topkpercent,y=empratio),shape=20)+
    geom_hline(aes(yintercept=1),col="red",linetype="dashed")+
    #geom_smooth(aes(x=month3_emplvl_topkpercent,y=empdiff),se=F)+
    theme_bw(base_size = basesize)+#theme(legend.position = "none")+
    labs(x="% Employment of Top Estabs",y="pnc Rel. Err./ sqrt Rel. Err.")+
    ggtitle(paste("Employment:"),subtitle=title_add)#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")

  if(bynestab==T){
    emp_plot= emp_plot+facet_wrap(~estab_bin,scales="free")
    empdiff_plot=empdiff_plot+facet_wrap(~estab_bin,scales="free")
    wagediff_plot=wagediff_plot+facet_wrap(~estab_bin,scales="free")
    wagenestab_plot=wagenestab_plot+facet_wrap(~estab_bin,scales="free")
    empnestab_plot=empnestab_plot+facet_wrap(~estab_bin,scales="free")
    empratio_plot=empratio_plot+facet_wrap(~estab_bin,scales="free")
    wageratio_plot=wageratio_plot+facet_wrap(~estab_bin,scales="free")
  }
  if(is.null(add.theme)==FALSE){
    emp_plot= emp_plot+add.theme
    empdiff_plot=empdiff_plot+add.theme
    wage_plot= wage_plot+add.theme
    wagediff_plot=wagediff_plot+add.theme
    wagenestab_plot=wagenestab_plot+add.theme
    empnestab_plot=empnestab_plot+add.theme
    empratio_plot=empratio_plot+add.theme
    wageratio_plot=wageratio_plot+add.theme
  }

  if(is.na(min_nestabs)==FALSE){
    legend_label=paste0(legend_label," (>",min_nestabs," Estabs in Group)")
  }
  if(bynestab==TRUE){
    overtitle=cowplot::ggdraw()+cowplot::draw_label(paste0(legend_label," by Number of Establishments in Group"),size=basesize*1.3)


    endplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wage_plot,emp_plot,ncol=1),ncol=1,rel_heights = c(.1,2))

    diffplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wagediff_plot,empdiff_plot,ncol=1),ncol=1,rel_heights = c(.1,2))

    nestabplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wagenestab_plot,empnestab_plot,ncol=1),ncol=1,rel_heights = c(.1,2))

    ratioplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wageratio_plot,empratio_plot,ncol=1),ncol=1,rel_heights = c(.1,2))

    ratioplotsne=NA
  }else{
    overtitle=cowplot::ggdraw()+cowplot::draw_label(paste0(legend_label),size=basesize*1.3)

    endplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wage_plot,emp_plot,ncol=2,rel_widths = c(1,1)),ncol=1,rel_heights = c(0.1,1))

    diffplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wagediff_plot,empdiff_plot,ncol=2,rel_widths = c(1,1)),ncol=1,rel_heights = c(0.1,1))

    nestabplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wagenestab_plot,empnestab_plot,ncol=2,rel_widths = c(1,1)),ncol=1,rel_heights = c(0.1,1))
    ratioplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wageratio_plot,empratio_plot,ncol=2,rel_widths = c(1,1)),ncol=1,rel_heights = c(0.1,1))
    ratioplotsne=NA
  }
  }

  plt_list[[1]]=endplots
  plt_list[[2]]=diffplots
  plt_list[[3]]=nestabplots
  plt_list[[4]]=ratioplots
  plt_list[[5]]=ratioplotsne
  return(plt_list)
  #return(list("plot_list"=plot_list,"legend_list"=legend_list,"data"=confdf.sub))
}


find_groups=function(sanagg,agglvl_code,ntails=20,state.prefix=state.prefixes,set_max_relerr=NULL){
  statecode=as.numeric(base::substr(state.prefix,3,4))
  statelvl=ifelse((as.numeric(agglvl_code)<69)&(as.numeric(agglvl_code)>49),T,F)
  lastdig=as.numeric(agglvl_code)%%10
  indlvl_from_lastdig=c("total","domain","supersector","sector","naics3","naics4","naics5","naics")
  industrylvl=indlvl_from_lastdig[lastdig]

  sanagg=sanagg[,grepl("agglvl|fips|industry|qtrly|month3",colnames(sanagg))]
  sanagg=sanagg[,grepl("agglvl|fips|industry|estabs",colnames(sanagg))|grepl("equalwage",colnames(sanagg))|colnames(sanagg)%in%c("total_qtrly_wages","month3_emplvl")]

  sanagg$wagediff=ifelse(sanagg$total_qtrly_wages_equalwage_clip_rel-sanagg$total_qtrly_wages_equalwage_sqrt_rel>0,"pnc>sqrt","pnc<sqrt")
  sanagg$empdiff=ifelse(sanagg$month3_emplvl_equalwage_clip_rel-sanagg$month3_emplvl_equalwage_sqrt_rel>0,"pnc>sqrt","pnc<sqrt")
  if(is.null(set_max_relerr)==FALSE){
    sanagg=sanagg[(sanagg$month3_emplvl_equalwage_clip_rel<set_max_relerr)&(sanagg$total_qtrly_wages_equalwage_clip_rel<set_max_relerr)&(sanagg$month3_emplvl_equalwage_sqrt_rel<set_max_relerr)&(sanagg$total_qtrly_wages_equalwage_sqrt_rel<set_max_relerr),]
  }

  agglvldf=sanagg[sanagg$agglvl_code==agglvl_code,!grepl("_dif|_rdif",colnames(sanagg))]
  agglvldf$month3_emplvl_rel_clip_m_sqrt=agglvldf$month3_emplvl_equalwage_clip_rel-agglvldf$month3_emplvl_equalwage_sqrt_rel
  agglvldf$wages_rel_clip_m_sqrt=agglvldf$total_qtrly_wages_equalwage_clip_rel-agglvldf$total_qtrly_wages_equalwage_sqrt_rel

  agglvldf=agglvldf%>%arrange(wages_rel_clip_m_sqrt)
  #tails_wage=agglvldf[c(seq(1,ntails),seq(nrow(agglvldf)-ntails,nrow(agglvldf))),]
  #agglvldf=agglvldf%>%arrange(month3_emplvl_rel_clip_m_sqrt)
  #tails_emp=agglvldf[c(seq(1,ntails),seq(nrow(agglvldf)-ntails,nrow(agglvldf))),]
  ###print("here 2")
  if(statelvl==F){
    if(industrylvl=="total"){
      agglvldf$agggroup=paste0(as.numeric(base::substr(agglvldf$area_fips,ifelse(statecode>10,3,2),nchar(agglvldf$area_fips[1]))))
    }else{
      agglvldf$agggroup=paste(as.numeric(base::substr(agglvldf$area_fips,ifelse(statecode>10,3,2),nchar(agglvldf$area_fips[1]))),"X",agglvldf$industry_code)
    }
  }else{
    agglvldf$agggroup=as.character(agglvldf$industry_code)
  }
  ###print("here 3")
  #agglvldf$empwageflip=ifelse(agglvldf$wagediff!=agglvldf$empdiff,T,F)
  #flipdf=agglvldf[agglvldf$wagediff!=agglvldf$empdiff,]
  flipdf=agglvldf
  ###print(nrow(flipdf))
  flipdf=flipdf%>%arrange(wages_rel_clip_m_sqrt)%>%mutate("wage_rank"=seq(1,nrow(flipdf)))
  flipdf=flipdf%>%arrange(month3_emplvl_rel_clip_m_sqrt)%>%mutate("combo_rank"=abs(wage_rank-seq(1,nrow(flipdf))))
  flipdf=flipdf%>%arrange(combo_rank)%>%tail(ntails)
  return(flipdf)
}

compare_sqrt_clip_plots=function(sanagg,confdf,agglvl_code,ntails,state.prefix=state.prefixes,add.theme=NULL,add.legend.theme=NULL,basesize=8,nbins=15,max_rel_error=NULL){

  statecode=as.numeric(base::substr(state.prefix,3,4))
  statelvl=ifelse((as.numeric(agglvl_code)<69)&(as.numeric(agglvl_code)>49),T,F)
  lastdig=as.numeric(agglvl_code)%%10
  indlvl_from_lastdig=c("total","domain","supersector","sector","naics3","naics4","naics5","naics")
  industrylvl=indlvl_from_lastdig[lastdig]
  if(industrylvl=="domain"){
    confdf$domain=base::substr(as.character(confdf$supersector),1,3)
  }



  ###print("here 1")
  subdf=find_groups(sanagg,agglvl_code,ntails,state.prefix,set_max_relerr=max_rel_error)
  # sanagg=sanagg[sanagg$own_code==5,grepl("agglvl|fips|industry|qtrly|month3",colnames(sanagg))]
  # sanagg=sanagg[,grepl("agglvl|fips|industry|estabs",colnames(sanagg))|grepl("equalwage",colnames(sanagg))|colnames(sanagg)%in%c("total_qtrly_wages","month3_emplvl")]
  # sanagg$wagediff=ifelse(sanagg$total_qtrly_wages_equalwage_clip_rel-sanagg$total_qtrly_wages_equalwage_sqrt_rel>0,"pnc>sqrt","pnc<sqrt")
  # sanagg$empdiff=ifelse(sanagg$month3_emplvl_equalwage_clip_rel-sanagg$month3_emplvl_equalwage_sqrt_rel>0,"pnc>sqrt","pnc<sqrt")
  #
  # agglvldf=sanagg[sanagg$agglvl_code==agglvl_code,!grepl("_dif|_rdif",colnames(sanagg))]
  # agglvldf$month3_emplvl_rel_clip_m_sqrt=agglvldf$month3_emplvl_equalwage_clip_rel-agglvldf$month3_emplvl_equalwage_sqrt_rel
  # agglvldf$wages_rel_clip_m_sqrt=agglvldf$total_qtrly_wages_equalwage_clip_rel-agglvldf$total_qtrly_wages_equalwage_sqrt_rel
  #
  # agglvldf=agglvldf%>%arrange(wages_rel_clip_m_sqrt)
  # tails_wage=agglvldf[c(seq(1,ntails),seq(nrow(agglvldf)-ntails,nrow(agglvldf))),]
  # agglvldf=agglvldf%>%arrange(month3_emplvl_rel_clip_m_sqrt)
  # tails_emp=agglvldf[c(seq(1,ntails),seq(nrow(agglvldf)-ntails,nrow(agglvldf))),]

  ###print("here 2")
  if(statelvl==F){
    if(industrylvl=="total"){
      #tails_wage$agggroup=paste0(as.numeric(base::substr(tails_wage$area_fips,ifelse(statecode>10,3,2),nchar(tails_wage$area_fips[1]))))
      #tails_emp$agggroup=paste0(as.numeric(base::substr(tails_emp$area_fips,ifelse(statecode>10,3,2),nchar(tails_emp$area_fips[1]))))
      confdf$agggroup=paste0(as.numeric(confdf$cnty))
      legend_label=paste("County")
    }else{
      #tails_wage$agggroup=paste(as.numeric(base::substr(tails_wage$area_fips,ifelse(statecode>10,3,2),nchar(tails_wage$area_fips[1]))),"X",tails_wage$industry_code)
      #tails_emp$agggroup=paste(as.numeric(base::substr(tails_emp$area_fips,ifelse(statecode>10,3,2),nchar(tails_emp$area_fips[1]))),"X",tails_emp$industry_code)
      industrycol=confdf[,industrylvl]
      confdf$agggroup=paste(as.numeric(confdf$cnty),"X",unlist(industrycol))
      legend_label=paste("County X ",stringr::str_to_title(stringr::str_to_lower(industrylvl)))
    }
  }else{
    #tails_wage$agggroup=as.character(tails_wage$industry_code)
    #tails_emp$agggroup=as.character(tails_emp$industry_code)
    confdf$agggroup=as.character(unlist(confdf[,industrylvl]))
    legend_label=stringr::str_to_title(stringr::str_to_lower(industrylvl))
  }
  ###print("here 3")


  #group_overlap=tails_wage$agggroup[tails_wage$agggroup%in%tails_emp$agggroup]
  #subdf=tails_emp[tails_emp$agggroup%in%group_overlap,]
  confdf.sub=confdf[confdf$agggroup%in%subdf$agggroup,]
  confdf.sub=confdf.sub%>%inner_join(subdf[,!grepl("_abs",colnames(subdf))],by="agggroup")
  #confdf.sub$wagediff=ifelse(confdf.sub$wages_rel_clip_m_sqrt>0,"pnc>sqrt","pnc<sqrt")
  #confdf.sub$empdiff=ifelse(confdf.sub$month3_emplvl_rel_clip_m_sqrt>0,"pnc>sqrt","pnc<sqrt")


  legend_list=list()
  wagediff_plot=ggplot(confdf.sub,aes(x=total_qtrly_wages,col=agggroup,fill=agggroup))+
    #geom_histogram(alpha=0.01,position="dodge",bins=nbins)+
    geom_density(alpha=0.01)+
    theme_bw(base_size = basesize)+facet_wrap(~wagediff)+
    labs(x="Original Establishment Wages",col=legend_label,fill=legend_label)+
    ggtitle("Wages")#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")
  if(is.null(add.legend.theme)==FALSE){
    wagediff_plot_legend=wagediff_plot+add.legend.theme
    legend_list1=cowplot::get_legend(wagediff_plot_legend)
  }else{
    legend_list1=cowplot::get_legend(wagediff_plot)
  }


  empdiff_plot=ggplot(confdf.sub,aes(x=month3_emplvl,col=agggroup,fill=agggroup))+
    #geom_histogram(alpha=0.01,position="dodge",bins=nbins)+
    geom_density(alpha=0.01)+
    theme_bw(base_size = basesize)+facet_wrap(~empdiff)+
    labs(x="Original Establishment Employment",col=legend_label,fill=legend_label)+theme(legend.position="none")+
    ggtitle("Employment")#subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")

  nestab_emp_plot=ggplot()+
    geom_point(aes(x=subdf$qtrly_estabs,y=subdf$month3_emplvl_equalwage_clip_rel,col=as.factor(subdf$agggroup),shape="pnc"))+
    geom_point(aes(x=subdf$qtrly_estabs,y=subdf$month3_emplvl_equalwage_sqrt_rel,col=as.factor(subdf$agggroup),shape="sqrt"))+
    labs(x="Number of Establishment in Group",y="Relative Error",shape="Mechanism",col=legend_label)+theme_bw(base_size = basesize)+
    ggtitle("Employment")+#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")
    guides(shape=guide_legend(ncol=2))

  nestab_emp_plot_test=ggplot(data=subdf,aes(x=qtrly_estabs,y=month3_emplvl_equalwage_clip_rel,yend=month3_emplvl_equalwage_sqrt_rel,col=empdiff))+
    geom_segment(position="jitter")+
    labs(x="Number of Establishment in Group",y="Relative Error",col="Relative Error Inequality")+theme_bw(base_size = basesize)+
    ggtitle("Employment")#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")

  nestab_wage_plot_test=ggplot(data=subdf,aes(x=qtrly_estabs,y=total_qtrly_wages_equalwage_clip_rel,yend=total_qtrly_wages_equalwage_sqrt_rel,col=wagediff))+
    geom_segment(position="jitter")+
    labs(x="Number of Establishment in Group",y="Relative Error",col="Relative Error Inequality")+
    theme_bw(base_size = basesize)+theme(legend.position = "none")+
    ggtitle("Wages")#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")

  if(is.null(add.legend.theme)==FALSE){
    nestab_emp_plot_legend=nestab_emp_plot+add.legend.theme
    legend_list2=cowplot::get_legend(nestab_emp_plot_legend)
    nestab_emp_plot_test_legend=nestab_emp_plot_test+add.legend.theme
    legend_list3=cowplot::get_legend(nestab_emp_plot_test_legend)

  }else{
    legend_list2=cowplot::get_legend(nestab_emp_plot)
    legend_list3=cowplot::get_legend(nestab_emp_plot_test)
  }

  nestab_wage_plot=ggplot()+geom_point(aes(x=subdf$qtrly_estabs,y=subdf$total_qtrly_wages_equalwage_clip_rel,col=as.factor(subdf$agggroup),shape="pnc"))+
    geom_point(aes(x=subdf$qtrly_estabs,y=subdf$total_qtrly_wages_equalwage_sqrt_rel,col=as.factor(subdf$agggroup),shape="sqrt"))+
    labs(x="Number of Establishment in NAICS5 Group",y="Relative Error",shape="Mechanism",col=legend_label)+
    theme_bw(base_size = basesize)+theme(legend.position="none")+
    ggtitle("Wages")#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")

  wagediff_plot=wagediff_plot+theme(legend.position = "none")
  nestab_emp_plot=nestab_emp_plot+theme(legend.position = "none")
  nestab_emp_plot_test=nestab_emp_plot_test+theme(legend.position = "none")
  if(is.null(add.theme)==FALSE){
    nestab_emp_plot= nestab_emp_plot+add.theme
    empdiff_plot=empdiff_plot+add.theme
    nestab_wage_plot= nestab_wage_plot+add.theme
    nestab_wage_plot_test= nestab_wage_plot_test+add.theme
    nestab_emp_plot_test= nestab_emp_plot_test+add.theme
    wagediff_plot=wagediff_plot+add.theme
  }
  diff_plots=cowplot::plot_grid(cowplot::plot_grid(wagediff_plot,empdiff_plot,ncol=1),legend_list1,ncol=2,rel_widths = c(1,0.3))
  nestab_plots=cowplot::plot_grid(cowplot::plot_grid(nestab_wage_plot,nestab_emp_plot,ncol=1),legend_list2,ncol=2,rel_widths = c(1,0.3))
  test_plots=cowplot::plot_grid(cowplot::plot_grid(nestab_wage_plot_test,nestab_emp_plot_test,ncol=1),legend_list3,ncol=2,rel_widths = c(1,0.3))
  plot_list=list()
  plot_list[[1]]=diff_plots
  #plot_list[[2]]=empdiff_plot
  plot_list[[2]]=nestab_plots
  plot_list[[3]]=test_plots
  #plot_list[[4]]=nestab_emp_plot
  return(plot_list)
  #return(list("plot_list"=plot_list,"legend_list"=legend_list,"data"=confdf.sub))
}


err_plot=function(agglvl,longdf=qdata_long,
                  x="qtrly_estabs",variable="wage",relative=T,
                  agglvl.df=nj.summary.agglvls,
                  additional.theme=NULL,
                  col.lab="Budget",
                  base.size=8,pt.sz=1,fill.alpha=0.15,linew=0.5,log.transform=F,facet=T){
  agglvl_name=agglvl.df$agglvl[agglvl.df$agglvl_code==agglvl]
  metric=ifelse(relative==T,"Relative","Absolute")
  subdf=longdf[(longdf$agglvl_code==agglvl)&grepl(variable,longdf$variable,ignore.case = T)&grepl(metric,longdf$metric,ignore.case=T),]

  if(grepl("estab",x,ignore.case = T)==T){
    x=subdf$qtrly_estabs
  }else{
    if(grepl("wage",variable,ignore.case=T)==T){
      x=subdf$conf_wage_entropy
    }else{
      x=subdf$conf_m3emp_entropy
    }
  }
  if(facet==T){
    colvar=subdf$mechanism
  }else{
    colvar=subdf$shortfig
  }
  plotdf=data.frame("x"=as.numeric(x),"y"=subdf$value,"col"=colvar,"fill"=colvar,facet=subdf$configStem)



  tempwageplot=ggplot2::ggplot(data=plotdf,ggplot2::aes(x=x,y=y,col=col,fill=fill))+
    ggplot2::geom_point(size=pt.sz,shape=20)+
    ggplot2::geom_smooth(alpha=fill.alpha,linewidth=linew)+
    ggplot2::labs(x=ifelse(grepl("estab",x),"Number of Establishments","Confidential Data Entropy"),
                  y=ifelse(metric=="Relative","Relative Absolute Error","Absolute Error"),
                  col=col.lab,fill=col.lab)+
    ggplot2::ggtitle(paste0(agglvl_name," Query"))+
    ggplot2::theme_minimal(base_size=base.size)
  if(log.transform==T){
    tempwageplot=tempwageplot+ggplot2::scale_y_continuous(trans="pseudo_log")#+labs(y="log10 Relative Absolute Error")
  }
  if(is.null(additional.theme)==FALSE){
    tempwageplot=tempwageplot+additional.theme
  }
  if(facet==TRUE){
    tempwageplot=tempwageplot+ggplot2::facet_wrap(~facet)
  }
  return(tempwageplot)
}

emp_wage_legend=function(basesize=8,add.theme=NULL,add.legend.theme=NULL,
                         colpalette=c("blue","orange"),pt.shape=20,pt.alpha=0.5){
  df=data.frame("x"=seq(1,100), "y"=seq(201,300),"faketype"=rep(c("employment","wages"),50))
  temp=ggplot2::ggplot(data=df,ggplot2::aes(x=x,y=y,col=faketype))+
    ggplot2::geom_point(shape=pt.shape,alpha=pt.alpha)+
    ggplot2::labs(col="Value Type")+
    ggplot2::theme_bw(base_size = basesize)+
    scale_color_manual(values=colpalette)
  if(is.null(add.theme)==F){
    temp=temp+add.theme
  }
  if(is.null(add.legend.theme)==F){
    temp=temp+add.legend.theme
  }
  return(temp)
}

mechanism_legend=function(basesize=8,add.theme=NULL,add.legend.theme=NULL,
                         colpalette=c("blue","orange"),pt.shape=20,pt.alpha=1){
  df=data.frame("x"=seq(1,100), "y"=seq(201,300),"faketype"=rep(c("pnc","sqrt"),50))

  temp=ggplot2::ggplot(data=df,ggplot2::aes(x=x,y=y,col=faketype))+
    ggplot2::geom_point(shape=pt.shape,alpha=pt.alpha)+
    ggplot2::labs(col="Mechanism")+
    ggplot2::theme_bw(base_size = basesize)+
    scale_color_manual(values=colpalette)

  if(is.null(add.theme)==F){
    temp=temp+add.theme
  }
  if(is.null(add.legend.theme)==F){
    temp=temp+add.legend.theme
  }
  return(temp)
}

state_legend=function(basesize=8,add.theme=NULL,add.legend.theme=NULL,pt.shape=20){
  df=data.frame("x"=seq(1,100), "y"=seq(201,300),"faketype"=rep(c("NJ","RI"),50))

  temp=ggplot2::ggplot(data=df,ggplot2::aes(x=x,y=y,shape=faketype))+
    ggplot2::geom_point()+
    ggplot2::labs(shape="State")+
    ggplot2::theme_bw(base_size = basesize)

  if(is.null(add.theme)==F){
    temp=temp+add.theme
  }
  if(is.null(add.legend.theme)==F){
    temp=temp+add.legend.theme
  }
  return(temp)
}

handle_addtheme=function(add.theme,plt){
  if((is.list(add.theme))&(length(add.theme)>1)){
    #ggproto.indic=sapply(add.theme,ggplot2::is_ggproto)
    #if(sum(ggproto.indic)>0){
    for(item in add.theme){
      #if(ggplot2::is_ggproto(item)==T){
      #  print(item)
        plt=plt+item
      #}else{
      #  plt=plt+item
      #}
    }
    #}else{

  }else{
    #if(ggplot2::is_ggproto(add.theme)==T){
      plt=plt+add.theme
    #}else{
    #  plt=plt+add.theme
    #}
  }
  return(plt)
}



equalwage_rep_plot=function(data,xvar,yvar,colvar=NULL,aggcode,xvarlab=NULL,yvarlab=NULL,plot.title.add=NULL,aggcode_str=NULL,
                            variable="both",state="all",facetcol=NULL,
                            add.theme=NULL,add.legend.theme=NULL,colpalette=c("blue","orange"),
                            basesize=8,facet.scales="fixed",override.plot.title=NULL,pt.alpha=0.2){
  title.null<-is.null(plot.title.add)==TRUE


  data=data[data$agglvl_code%in%c(aggcode),]
  data$reformatVariable=ifelse(grepl("wages",data$variable),"wages","employment")


  if(variable=="both"){
    bothvar=T
    var_str="Wages and Employment"

  }else{
    bothvar=F
    data=data[data$variable==variable,]

    if(grepl("wage",variable)==TRUE){
        var_str="Wages"
      }else{
    var_str="Employment"
  }
  }
  plot.title=var_str

  if(state=="all"){
    twostates=T
    state_str="Synthetic NJ and RI"
  }else{
    twostates=F
    data=data[data$state==state,]
    state_str=stringr::str_to_upper(state)
  }
  plot.title=paste(state_str,plot.title)

  if(length(aggcode)==1){
    if(is.null(aggcode_str)==T){
      aggcode_str=agglvl_code_labeller(aggcode)
    }
      plot.title=paste0(plot.title,": ",aggcode_str)
  }else{
    if(is.null(aggcode_str)==T){
      data$aggcode_label=agglvl_code_labeller(data$agglvl_code)
    }else{
      aggnamer=list(aggcode)
      names(aggnamer)=aggcode_str
      data$aggcode_label=unlist(unname(sapply(data$agglvl_code,function(x)names(aggnamer[aggnamer==x]))))
    }
  }


  plot.title=paste(plot.title,plot.title.add)

  if(is.null(override.plot.title)==F){
    if(is.function(override.plot.title)==T){
      plot.title=override.plot.title(
        list("aggcode_str"=aggcode_str,"state_str"=state_str,
             "var_str"=var_str,"plot.title.add"=plot.title.add))
    }else{
      plot.title=override.plot.title
    }
  }

  #filling in null variable labels
  if(is.null(xvarlab)==T){
    xcname=xvar#colnames(data[,xvar,drop=F])
    if(grepl("prop_of_top",xcname)==T){
      xvarlab="Proportion From Top"
      xcname=gsub("prop_of_top|_","",xcname)
      if(grepl("percent",xcname)==T){
        xvarlab=paste0(xvarlab," ",gsub("percent","",xcname),"% Establishments")
      }else{
        xvarlab=paste0(xvarlab," k=",xcname," Establishments")
      }
    }else if(grepl("estabs",xcname)==T){
      xvarlab="# Establishments"
    }else if(grepl("ngroup",xcname)==T){
      xvarlab="# Groups in Query"
    }else if(grepl("orig",xcname)==T){
      xvarlab="Original Value"
    }else{
      xvarlab=xcname
    }
  }

  if(is.null(yvarlab)==T){
    ycname=colnames(data[,yvar,drop=F])
    ###print(ycname)
    if(grepl("sanvalue",ycname)==T){
      yvarlab="Sanitized Value"
    }else if(yvar=="abs"){
      yvarlab="Absolute Difference"
    }else if(yvar=="rel"){
      yvarlab="Abs. Relative Difference"
    }else if(yvar=="dif"){
      yvarlab="Difference"#"Sanitized-Original"
    }else if(yvar=="orig_val"){
      yvarlab="Original Value"
    }else if(yvar=="rdif"){
      yvarlab="Relative Difference"#"(Sanitized-Original)/Original"
    }else{
      yvarlab=yvar
    }
  }

  #preparing data for plotting
  # if((xvar%in%c("dif","rdif","abs","rel"))|((yvar%in%c("dif","rdif","abs","rel")))){
  #   data=data[(data$metric==xvar)|(data$metric==yvar),]
  #   xvar=ifelse(xvar%in%c("dif","rdif","abs","rel"),"metric_value",xvar)
  #     yvar=ifelse(yvar%in%c("dif","rdif","abs","rel"),"metric_value",yvar)
  #   }

  plot.data=data.frame("x"=unlist(data[,xvar]),"y"=unlist(data[,yvar]),"col"=NA,"shape"=NA)
  if(is.null(colvar)==F){
    usecol=T
    plot.data$col=unlist(data[,colvar])
  }else{
    usecol=F
  }

  if(is.null(facetcol)==F){
    usefacet=T
    if(facetcol=="agglvl_code"){
      plot.data$facet=as.factor(unlist(data$aggcode_label))
      facetcol="aggcode_label"
    }else{
    plot.data$facet=as.factor(unlist(data[,facetcol]))
    facetcol=colnames(data[,facetcol,drop=F])
    }
  }else{
    usefacet=F
    facetcol="none"
  }
  if((twostates==TRUE)&(facetcol!="state")){
    useshape=T
    if((bothvar==TRUE)&(facetcol!="variable")){
      plot.data$shape=as.factor(paste(stringr::str_to_upper(base::substr(data$state,1,2)),data$reformatVariable))
    }else{
      plot.data$shape=as.factor(stringr::str_to_upper(base::substr(data$state,1,2)))
    }
  }else if((bothvar==TRUE)&(facetcol!="variable")){
    useshape=T
    plot.data$shape=as.factor(data$reformatVariable)
  }else{
    useshape=F
  }



  na.x=sum(is.na(plot.data$x))
  if(na.x>0){
    warning(paste("There are",na.x," NA values in x axis variable that are removed"))
    plot.data=plot.data[!is.na(plot.data$x),]
  }

  na.y=sum(is.na(plot.data$y))
           if(na.y>0){
             warning(paste("There are",na.y," NA values in y axis variable that are removed"))
             plot.data=plot.data[!is.na(plot.data$y),]
           }

  if(usecol==T){
    if(useshape==T){
      plt=ggplot2::ggplot(data=plot.data,ggplot2::aes(x=x,y=y,col=col,shape=shape))
    }else{
      plt=ggplot2::ggplot(data=plot.data,ggplot2::aes(x=x,y=y,col=col))
    }
  }else{
    plt=ggplot2::ggplot(data=plot.data,ggplot2::aes(x=x,y=y))
  }

  size.scale=(log(length(unique(plot.data$x)),base=3))^(4/9)
  ###print("here")
  plt=plt+ggplot2::geom_point(position="jitter",size=1.3/size.scale,alpha=pt.alpha)+
    ggplot2::labs(x=xvarlab,y=yvarlab)+
    ggplot2::theme_bw(base_size = basesize)#+
    #add.theme+add.legend.theme


  plt=handle_addtheme(add.legend.theme,plt)

  plt=handle_addtheme(add.theme,plt)


  plt=plt+ggplot2::ggtitle(plot.title)

  if(usecol==T){
    plt=plt+ggplot2::labs(col="Mechanism")+ggplot2::scale_color_manual(values=colpalette)
  }
  if(useshape==T){
    plt=plt+ggplot2::labs(shape="State")
  }


  if(usefacet==T){
    #print("usefacet")
    plt=plt+ggplot2::facet_wrap(~facet,scales=facet.scales)
  }


  ###print(plt)

  return(plt)
}


equalwage_rep_boxplots=function(data,contvar,catvar="mechanism",aggcodes,contvarlab=NULL,catvarlab=NULL,plot.title.add=NULL,aggcode_strs=NULL,
                                variable="both",state="all",facetcol=NULL,
                            add.theme=NULL,add.legend.theme=NULL,colpalette=c("blue","orange"),
                            basesize=8,facet.scales="fixed",by_numestab=F,override.title=NULL){
  title.null<-is.null(plot.title.add)==TRUE

  data=data[data$agglvl_code%in%c(aggcodes),]
  data$reformatVariable=ifelse(grepl("wages",data$variable),"wages","employment")

  if(variable=="both"){
    bothvar=T
    plot.title=ifelse(is.null(override.title)==T,"Wages and Employment",override.title)

  }else{
    bothvar=F
    data=data[data$variable==variable,]

    if(grepl("wage",variable)==TRUE){
      plot.title=ifelse(is.null(override.title)==T,"Wages",override.title)
    }else{
      plot.title=ifelse(is.null(override.title)==T,"Employment",override.title)
    }
  }

  if(state=="all"){
    twostates=T
    plot.title=ifelse(is.null(override.title)==T,paste("Synthetic NJ and RI",plot.title),override.title)
  }else{
    twostates=F
    data=data[data$state==state,]
    plot.title=ifelse(is.null(override.title)==T,paste("Synthetic",stringr::str_to_upper(state),plot.title),override.title)
  }

  if(by_numestab==T){
    plot.title=ifelse(is.null(override.title)==T,paste(plot.title,"By Number of Establishments in a Cell"),override.title)
  }

  #preparing data for plotting
  plot.data=data.frame("contvar"=unlist(data[,contvar]),"catvar"=unlist(data[,catvar]),"state"=data$state)
  if(variable=="both"){
    plot.data$variable=data$variable
  }
  if(twostates==T){
    plot.data$state_lab=as.factor(ifelse(grepl("nj",plot.data$state),"Syn-NJ","Syn-RI"))
  }

  if(by_numestab==T){
    plot.data$Bin=gsub("Bottom|Top|From","",gsub(" to ","-",gsub("\\)","",gsub("[^;]*; \\(","",data$estab_bin))),ignore.case=T)

  }

  if(catvar=="agglvl_code"){
    usefacet=F
    plot.data=plot.data[plot.data$catvar%in%c(aggcodes),]
    if(is.null(aggcode_strs)==FALSE){
      recodelevels_list=aggcode_strs
      names(recodelevels_list)=aggcodes
      plot.data$Query=dplyr::recode(plot.data$catvar,!!!recodelevels_list)
    }
  }else if(length(aggcodes)>1){
    usefacet=T
    plot.data$Query=as.factor(as.character(data$agglvl_code))
    if(is.null(aggcode_strs)==F){
      recodelevels_list=aggcode_strs
      names(recodelevels_list)=aggcodes
      plot.data$Query=dplyr::recode(plot.data$Query,!!!recodelevels_list)
    }
  }else{
    if((state=="all")&(variable=="both")){
      usefacet=T
    }else{
    usefacet=F
    plot.data=plot.data[plot.data$catvar==aggcodes,]
    }
    if(is.null(override.title)==T){
    if((is.null(aggcode_strs)==F)){
      plot.title=paste0(plot.title,": ",paste0(aggcode_strs,collapse=", "))
    }else{
      plot.title=paste0(plot.title,": ",paste0(aggcodes,collapse=", "))
    }
    }
    plot.data$Query=plot.data$agglvl_codelab
  }

  if(variable=="both"){
    usefacet=T
  }



  #filling in null variable labels
  if(is.null(contvarlab)==T){
    xcname=contvar#colnames(data[,xvar,drop=F])
    if(grepl("prop_of_top",xcname)==T){
      xvarlab="Proportion From Top"
      xcname=gsub("prop_of_top|_","",xcname)
      if(grepl("percent",xcname)==T){
        xvarlab=paste0(xvarlab," ",gsub("percent","",xcname),"% Establishments")
      }else{
        xvarlab=paste0(xvarlab," k=",xcname," Establishments")
      }
    }else if(grepl("estabs",xcname)==T){
      xvarlab="# Establishments"
    }else if(grepl("ngroup",xcname)==T){
      xvarlab="# Groups in Query"
    }else if(grepl("orig",xcname)==T){
      xvarlab="Original Value"
    }else if(grepl("sanvalue",xcname)==T){
      xvarlab="Sanitized Value"
    }else if(contvar=="abs"){
      xvarlab="Absolute Difference"
    }else if(grepl("rel",contvar)){
      xvarlab="Abs. Relative Difference"
    }else if(contvar=="dif"){
      xvarlab="Difference"#"Sanitized-Original"
    }else if(contvar=="orig_val"){
      xvarlab="Original Value"
    }else if(contvar=="rdif"){
      xvarlab="Relative Difference"#(Sanitized-Original)/Original"
    }else{
      xvarlab=xcname
    }
    contvarlab=xvarlab
  }

  if(is.null(catvarlab)==T){
    ycname=colnames(data[,catvar,drop=F])
    ###print(ycname)
    #mechanism,state, stays the same
    if(catvar=="agglvl_code"){
      catvarlab="Query"
    }else if(catvar=="estab_bin"){
        catvarlab="Establishment Count Bins"
    }else{
        catvarlab=stringr::str_to_title(stringr::str_to_lower(catvar))
    }
  }

  colvarlab=catvarlab
  if(by_numestab==T){
    catvarlab="Binned Number of Establishments"
  }

  plot.title=paste(plot.title, plot.title.add)

  na.x=sum(is.na(plot.data$contvar))
  if(na.x>0){
    warning(paste("There are",na.x," NA values in continuous variable that are removed"))
    plot.data=plot.data[!is.na(plot.data$contvar),]
  }




  if(by_numestab==F){
    plt=ggplot2::ggplot(data=plot.data,ggplot2::aes(x=catvar,y=contvar,col=catvar))
  }else{
    plt=ggplot2::ggplot(data=plot.data,ggplot2::aes(x=Bin,y=contvar,col=catvar))
  }

  ###print("here")
  plt=plt+ggplot2::geom_boxplot()+
    ggplot2::labs(x=catvarlab,y=contvarlab,col=colvarlab)+
    ggplot2::theme_bw(base_size = basesize)+
    add.theme+add.legend.theme+
    ggplot2::ggtitle(plot.title)+scale_color_manual(values=colpalette)

  nquery=length(aggcodes)


  if(usefacet==T){ #if adding a facet
    if(variable=="both"){ #if using wages and employment
      if(nquery==1){
         if(state=="all"){

           plt=plt+ggplot2::facet_grid(rows=vars(variable),cols=vars(state_lab),scales="free")

         }
      }else if((nquery<=5)){ #if number of queries is <5 (queries become columns)
        if(facet.scales!="free"){ #if facets are not free, use facet_grid
          ##change facet.scales input accordingly
        if(facet.scales=="free_aggcode"){
          facet.scales="free_y"
        }else if(facet.scales=="free_variable"){
          facet.scales="free_x"
        }
        ###print(facet.scales)
        plt=plt+ggplot2::facet_grid(rows=vars(variable),cols=vars(Query),scales=facet.scales)
        }else{ #end facet.scales=="free" (use facet_wrap)
          plt=plt+ggplot2::facet_wrap(vars(variable,Query),nrow=2,scales=facet.scales)
        }
      }else{ #end nquery<=5 (now variable will be columns)
        if(facet.scales!="free"){ #if facet.scales not free, use facet_grid
          ##change facet.scales accordingly
        if(facet.scales=="free_aggcode"){
          facet.scales="free_x"
        }else if(facet.scales=="free_variable"){
          facet.scales="free_y"
        }
        ###print(facet.scales)
        plt=plt+ggplot2::facet_grid(rows=vars(Query),cols=vars(variable),scales=facet.scales)
        }else{ #end facet.scales!="free" (use facet_wrap with variable as columns)
          plt=plt+ggplot2::facet_wrap(vars(Query,variable),ncol=ifelse(nquery>6,4,2),scales=facet.scales)
        }
      }
    }else{ #end variable=="both" (now use facet_wrap)
      if(facet.scales=="free_aggcode"){
        facet.scales="free"
      }
      plt=plt+ggplot2::facet_wrap(~Query,scales=facet.scales)
    }
  } #end usefacet==T
  ###print(plt)

  return(plt)
}

original_distributions_plot=function(st.data=NULL,
                                     folderin=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/EstablishmentLevelData"),
                                     folderout=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/R_tables_plots"),
                                     state.prefixes=c("nj34_qbp_2016_1","ri44_qbp_2016_1"),
                                     add.theme=NULL,add.legend.theme=NULL,basesize=8,nbins=23,
                                     state.colpalette=c("skyblue2","darkblue"),
                                     fname=NULL,#"original_distribution_plots.png",
                                     plot.scale=1.2,plot.width=9.5,plot.height=3.5,justwages=F,overwrite.title=NULL,exclude.zeros=F){
  if(is.null(st.data)==TRUE){
    for(st.pref in c(state.prefixes)){
      temp.data=read.csv(paste0(folderin,"/",substr(st.pref,1,4),"/",st.pref,".csv"))
      temp.data$state=substr(st.pref,1,2)
      st.data=dplyr::bind_rows(st.data,temp.data)
    }
  }

  if(length(state.prefixes)>1){
    st.data$state=as.factor(stringr::str_to_upper(st.data$state))
  }

  if(exclude.zeros==T){
    empindic=st.data$m3emp>0
    wageindic=st.data$wage>0
  }else{
    empindic=rep(T,nrow(st.data))
    wageindic=empindic
  }

 empplot=ggplot2::ggplot(st.data[empindic,],ggplot2::aes(x=m3emp,fill=state))+
   ggplot2::geom_histogram(bins=nbins,alpha=0.73,position="identity")+
   ggplot2::labs(x="Employment",fill="State")+
   ggplot2::theme_bw(base_size = basesize)
 empplot=handle_addtheme(add.theme,empplot)
 empplot=handle_addtheme(add.legend.theme,empplot)


 wageplot=ggplot2::ggplot(st.data[wageindic,],ggplot2::aes(x=wage,fill=state))+
   ggplot2::geom_histogram(bins=nbins,alpha=0.73,position="identity")+
   ggplot2::labs(x="Wages",fill="State")+
   ggplot2::theme_bw(base_size = basesize)
 wageplot=handle_addtheme(add.theme,wageplot)
 wageplot=handle_addtheme(add.legend.theme,wageplot)

 #print("Adding Title and Ajusting plots...")

 if(length(state.prefixes)>1){
   if(justwages==F){
     if(exclude.zeros==F){
   overtitle=cowplot::ggdraw()+
     cowplot::draw_label(ifelse(is.null(overwrite.title)==T,
                                "Distribution of Synthetic Employment and Wages",
                                overwrite.title),size=basesize*1.5)
     }else{
       overtitle=cowplot::ggdraw()+
         cowplot::draw_label(ifelse(is.null(overwrite.title)==T,
                                    "Distribution of Synthetic Employment and Wages (Zeros Excluded)",
                                    overwrite.title),size=basesize*1.5)

   }
   empplot=empplot+ggplot2::scale_fill_manual(values=state.colpalette)
   wageplot=wageplot+ggplot2::scale_fill_manual(values=state.colpalette)+
     ggplot2::theme(legend.position = "none")
   statelegend=cowplot::get_legend(empplot)
   empplot=empplot+ggplot2::theme(legend.position = "none")
   combineplot=cowplot::plot_grid(empplot,wageplot,statelegend,ncol=3,rel_widths=c(1,1,0.2))
   }else{ #wages and employment
     if(exclude.zeros==F){
     overtitle=cowplot::ggdraw()+
       cowplot::draw_label(ifelse(is.null(overwrite.title)==T,"Distribution of Synthetic Wages",overwrite.title),
                           size=basesize*1.5)
     }else{
       overtitle=cowplot::ggdraw()+
         cowplot::draw_label(ifelse(is.null(overwrite.title)==T,"Distribution of Synthetic Wages (Zeros Excluded)",overwrite.title),
                             size=basesize*1.5)
     }
     combineplot=wageplot+ggplot2::scale_fill_manual(values=state.colpalette)
}
 }else{ #if only one state prefix
   if(justwages==F){
     if(exclude.zeros==F){
   overtitle=cowplot::ggdraw()+
     cowplot::draw_label(ifelse(is.null(overwrite.title)==T,
                                paste0("Distribution of Employment and Wages for Synthetic ",
                                ifelse(grepl("nj34",state.prefixes)==T,"New Jersey","Rhode Island")),
                                overwrite.title),
                                                   size=basesize*1.5)
     }else{ #wages and employment
       overtitle=cowplot::ggdraw()+
         cowplot::draw_label(ifelse(is.null(overwrite.title)==T,
                                    paste0("Distribution of Employment and Wages for Synthetic ",
                                           ifelse(grepl("nj34",state.prefixes)==T,"New Jersey","Rhode Island")," (Zeros Excluded)"),
                                    overwrite.title),
                             size=basesize*1.5)

     }
     #print("Changing colors")
   empplot=empplot+ggplot2::theme(legend.position = "none")+
     ggplot2::scale_fill_manual(values="grey45")
   wageplot=wageplot+ggplot2::theme(legend.position = "none")+
     ggplot2::scale_fill_manual(values="grey45")
   combineplot=cowplot::plot_grid(empplot,wageplot,ncol=2)
   }else{
     if(exclude.zeros==F){
     overtitle=cowplot::ggdraw()+
       cowplot::draw_label(ifelse(is.null(overwrite.title)==T,"Distribution of Synthetic Wages",overwrite.title),
                           size=basesize*1.5)
     }else{
       overtitle=cowplot::ggdraw()+
         cowplot::draw_label(ifelse(is.null(overwrite.title)==T,"Distribution of Synthetic Wages (Zeros Excluded)",overwrite.title),
                             size=basesize*1.5)
     }
     combineplot=wageplot+ggplot2::scale_fill_manual(values="grey45")+
       ggplot2::theme(legend.position = "none")
   }
 }

 #print("Adding title")
 finalplot=cowplot::plot_grid(overtitle,combineplot,ncol=1, rel_heights = c(0.1,1))
 if(is.null(fname)==F){
   #print("Saving")
 ggsave(paste0(folderout,"/",fname),
        finalplot,
        scale=plot.scale,width=plot.width,height=plot.height,units="in",dpi=520,bg="white")
 }
 #print("Done")
 return(finalplot)
}


cell_distributions_boxplot=function(sandata=NULL,comp.suffix=NULL,aggcodes=71,
                                     folderin=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/compare_data"),
                                     folderout=paste0(gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),"/R_tables_plots"),
                                     state.prefixes=c("nj34_qbp_2016_1","ri44_qbp_2016_1"),
                                    xcol="state",contcol="rdif",fillcol=NULL,facetcol=NULL,
                                    xlab="State",contlab="Relative Difference",filllab=NULL,facetlab=NULL,
                                    xcol.numeric=F,
                                     add.theme=NULL,add.legend.theme=NULL,basesize=8,
                                     colpalette=c("skyblue2","darkblue"),
                                     fname.stem=NULL,#"original_distribution_plots.png",
                                     plot.scale=1.2,plot.width=9.5,plot.height=3.5,
                                    title.addon=NULL,include.outliers=F,pivot_agg_file_args=NULL,
                                    facet.scales="free",facet.ncol=NULL,ftype="png"){
  if(is.null(sandata)==TRUE){
    for(st.pref in state.prefixes){
      temp.data=file_name_changes(fin=paste0(folderin,"/",comp.suffix),
                                  st.pref=st.pref,comp.suff=compare.suffix,aggfile=F,quietly=F)
      temp.data$state=substr(st.pref,1,2)
      if(is.null(pivot_agg_file_args)==F){
        temp.data=do.call(pivot_agg_file,args=pivot_agg_file_args)
      }else{
      temp.data=pivot_agg_file(sanagg=temp.data,state.prefix=st.pref,comp.suffix=comp.suffix,metrics=c(contcol),
                            param.colname="parameter",param.translator=NULL,basepath=gsub("/[^/]*$","",rprojroot::find_rstudio_root_file()),
                            filename.comp.suffix=comp.suffix,
                            clipprob=F,budgets=F)

      }
      temp.data$state=st.pref
      sandata=dplyr::bind_rows(sandata,temp.data)
    }
  }

  if(length(state.prefixes)>1){
    sandata$state=as.factor(stringr::str_to_upper(sandata$state))
  }

  fill.x.match=F
  if(is.null(fillcol)==T){
    fillcol=as.factor(xcol)
    filllab=xlab
    fill.x.match=T
  }else if(fillcol==xcol){
    fill.x.match=T
  }


  sandata=sandata[sandata$agglvl_code%in%c(aggcodes),]
  plotdf=data.frame("ycol"=unlist(unname(sandata[,contcol])),"xcol"=unlist(unname(sandata[,xcol])),"fillcol"=as.factor(unlist(unname(sandata[,fillcol]))))

  if(xcol.numeric==T){
    plotdf$xcol=factor(plotdf$xcol,levels=as.character(sort(unique(plotdf$xcol))))
  }
  if(is.null(facetcol)==F){
    plotdf$facetcol=as.factor(unlist(unname(sandata[,facetcol])))
  }
  #print(str(plotdf))
  #print(table(plotdf$fillcol))
  if(fill.x.match==T){

  bplot=ggplot2::ggplot(plotdf,ggplot2::aes(y=ycol,x=xcol,fill=fillcol,group=interaction(xcol,fillcol)))+
    ggplot2::geom_boxplot(outliers=include.outliers,outlier.size = 0.5)+
    ggplot2::labs(x=xlab,fill=filllab,y=contlab)+
    ggplot2::theme_bw(base_size = basesize)+
    add.legend.theme+
    ggplot2::scale_fill_manual(values=colpalette)
  }else{
    bplot=ggplot2::ggplot(plotdf,ggplot2::aes(y=ycol,x=xcol,fill=fillcol))+
      ggplot2::geom_boxplot(outliers=include.outliers,outlier.size=0.5)+
      ggplot2::labs(x=xlab,fill=filllab,y=contlab)+
      ggplot2::theme_bw(base_size = basesize)+
      add.legend.theme+
      ggplot2::scale_fill_manual(values=colpalette)

  }
  if(is.null(add.theme)==F){
    bplot=handle_addtheme(add.theme,bplot)
  }


  if(is.null(facetcol)==F){
    #title.addon=paste("by",facetlab,title.addon)
    bplot=bplot+facet_wrap(~plotdf$facetcol,scales=facet.scales,ncol=facet.ncol)
  }
  if(is.null(title.addon)==F){
    bplot=bplot+ggplot2::ggtitle(title.addon)
  }
  if(is.null(add.theme)==F){
    bplot=handle_addtheme(add.theme,bplot)
  }
   if(is.null(fname.stem)==F){
        ggsave(paste0(folderout,"/",fname.stem,".",ftype),
           bplot,
           scale=plot.scale,width=plot.width,height=plot.height,units="in",dpi=760,bg="white")
  }
  return(bplot)
}

iter_cell_distribution=function(rdif_data,dif_data,abs_data=NULL,rel_data=NULL,aggcodes_plot,plotvariants,
                                xcol,xcollab,
                                state.prefix="nj34",folderin,folderRtabs,
                                mechcolpalette,
                                add.legend.theme=NULL,add.theme=NULL,facet.scales="free",add.file.suffix=NULL,ftype="pdf"){
  naggs=length(aggcodes_plot)
  #print(naggs)
  plt.scale.b=ifelse((naggs==3)|(naggs>4),0.73,0.83)
  plt.h.b=ifelse(naggs>=4,6.5,3.5)
  #print(plt.h.b)
  plt.w.b=ifelse(naggs==4,6.5,ifelse((naggs==3)|(naggs>4),8.5,4.3))
  if(naggs==1){
    plt.h.b=plt.h.b*2/3
    plt.w.b=plt.w.b*2
  }
  for(rw in seq(1,nrow(plotvariants))){
    variant=plotvariants[rw,]
    var=unlist(unname(variant[1]))
    plotmet=unlist(unname(variant[2]))
    plotouttrans=unlist(unname(variant[3]))
    title.str=ifelse(var=="emp","Employment","Wages")
    if(grepl("keep",plotouttrans)==T){
      outlier.indic=T
      plottheme=NULL
    }else{
      outlier.indic=F
    }
    if(grepl("plog",plotouttrans)==T){
      plottheme=ggplot2::scale_y_continuous(trans="pseudo_log")
    }else if(grepl("log10",plotouttrans)==T){
      plottheme=ggplot2::scale_y_log10()
    }else{
      plottheme=NULL
    }
    if(plotmet=="reldiff"){
      plotdata=rdif_data#[isreps_rdif$variable==var,]
      plotlab="Relative Difference"#"(Sanitized-Original)/Original"
    }else if(plotmet=="diff"){
      plotdata=dif_data#[isreps_dif$variable==var,]
      plotlab="Difference"#"Sanitized-Original"
    }else if(plotmet=="rel"){
      plotdata=rel_data#[isreps_dif$variable==var,]
      plotlab="Absolute Relative Difference"#"Sanitized-Original"
    }else if(plotmet=="abs"){
      plotdata=abs_data#[isreps_dif$variable==var,]
      plotlab="Absolute Difference"#"Sanitized-Original"
    }

    if(naggs>1){
      plotdata=plotdata[grepl(var,plotdata$variable),]
      if(naggs>=4){
        plotfname=paste0(substr(state.prefix,1,4),"_",var,"_",plotmet,"_boxplots_",xcol,"s_",plotouttrans,"_",naggs,"aggs")
      }else{
        plotfname=paste0(substr(state.prefix,1,4),"_",var,"_",plotmet,"_boxplots_",xcol,"s_",plotouttrans)
      }
      facetcol="agglvl_codelab"
    }else{
      plt.h.b=2
      plt.w.b=5
      plotdata=plotdata[plotdata$agglvl_code==unname(unlist(aggcodes_plot)),]
      plotdata$VariableLab=ifelse(grepl("wage",plotdata$variable),"Wages","Employment")
      plotfname=paste0(substr(state.prefix,1,4),"_bothvar_only",unlist(unname(aggcodes_plot)),"_",plotmet,"_boxplots_",xcol,"s_",plotouttrans)
      facetcol="VariableLab"
      title.str=stringr::str_to_title(stringr::str_to_lower(plotdata$agglvl_codelab[1]))
    }
    #print(paste("naggs=",naggs,", facetcol=",facetcol,", n facet values=",length(unique(unlist(plotdata[,facetcol])))))
    if(naggs>=4){
      facetncol=2
      temp.w=plt.w.b
      plt.w.b=plt.h.b
      plt.h.b=temp.w+0.7
    }else{
      facetncol=NULL
    }


    #relative difference plots
    cell_distributions_boxplot(sandata=plotdata,#breps_rdif_plot[breps_rdif_plot$variable==var,],
                               aggcodes=aggcodes_plot,
                               folderin=folderin,
                               folderout = paste0(folderRtabs,"/",xcol,"_reps"),
                               state.prefixes=state.prefix,
                               xcol=xcol,contcol = "value",fillcol="mech",facetcol = facetcol,
                               xlab=xcollab,contlab=plotlab,#"(Sanitized-Original)/Original",
                               filllab="Mechanism",facetlab="Aggregation Level Code",
                               xcol.numeric=T,
                               add.theme=list(plottheme,add.theme),#add.theme.plog,
                               add.legend.theme=add.legend.theme,
                               basesize=8,
                               colpalette=mechcolpalette,
                               fname.stem=paste0(plotfname,add.file.suffix),#paste0(substr(nj.state.prefix,1,4),"_",var,"_reldiff_boxplots_budgets_removeoutliers_plog"),
                               plot.scale=plt.scale.b,plot.width=plt.w.b,plot.height=plt.h.b,
                               title.addon=paste("Synthetic New Jersey",title.str),
                               include.outliers=outlier.indic,pivot_agg_file_args=NULL,
                               facet.ncol=facetncol,facet.scales=facet.scales,ftype=ftype)
  }
}

# compare_sqrt_clip_topk_plots_simple=function(sanagg,confdf,agglvl_codes,metric="rdif",k=NA,pper=NA,state.prefix=state.prefix,
#                                       basesize=8,add.theme=NULL,add.legend.theme=NULL,
#                                       colpalette=colorBlindness::Blue2DarkOrange12Steps[c(1,10)],
#                                       combine_emp_wages=F,topksubtitle=F){
#   out=compare_sqrt_clip_topk(sanagg = sanagg,confdf=confdf,agglvl_code = agglvl_code,k=k,pper=pper,
#                              state.prefix = state.prefix,
#                              metrics=metric,
#                              bynestab=F,
#                              newversion=T)
#   confdf.sub=out[[1]]
#   legend_label=out[[2]]
#
#   if(topksubtitle==T){
#     if(is.na(pper)==T){
#       title_add=paste0("Percent of Cell Total From Largest ",k," Values")
#     }else{
#       title_add=paste0("Percent of Cell Total From Largest ",pper*100,"% Establishments")
#     }
#   }else{
#     title_add=NULL
#   }
#
#   plt_list=list()
#
#   confdf.long=pivot_agg_file(sanagg=confdf.sub,state.prefix=state.prefix,
#                              comp.suffix="equalwages",metrics=metric,
#                           param.colname="comp.suff",param.translator=NULL,basepath=NULL,filename.comp.suffix=NULL,aggfile=F,
#                           clipprob=F,budgets=F,baseline=NULL,
#                           config.seperated.names=c("comp.suff","mech","metric"))
#   print(head(confdf.long))
#   stop()
#
#     wagedf=confdf.sub[(is.na(confdf.sub$wage_topkpercent)==FALSE),c("wage_topkpercent","total_qtrly_wages","wagediff","estab_bin","qtrly_estabs","wageratio")]
#     colnames(wagedf)=c("topkpercent","orig_val","diff","estab_bin","qtrly_estabs","ratio")
#     wagedf$val_type="wages"
#     empdf=confdf.sub[(is.na(confdf.sub$month3_emplvl_topkpercent)==FALSE),c("month3_emplvl_topkpercent","month3_emplvl","empdiff","estab_bin","qtrly_estabs","empratio")]
#     colnames(empdf)=c("topkpercent","orig_val","diff","estab_bin","qtrly_estabs","ratio")
#     empdf$val_type="employment"
#
#     plotdf=dplyr::bind_rows(empdf,wagedf)
#
#
#     endplots=ggplot(plotdf,aes(x=orig_val,y=diff))+
#       geom_point(aes(col=val_type),shape=20,alpha=0.5)+
#       geom_hline(aes(yintercept=0),linetype="dashed")+
#       theme_bw(base_size = basesize)+
#       labs(x="Original Group Value",y="pnc-sqrt Rel. Err.",col="Value Type")+
#       ggtitle(overtitle)+scale_color_manual(values=colpalette)
#
#
#     diffplots=ggplot(plotdf)+
#       geom_point(aes(x=topkpercent,y=diff,col=val_type),shape=20,alpha=0.5)+
#       geom_hline(aes(yintercept=0),linetype="dashed")+
#       theme_bw(base_size = basesize)+#theme(legend.position = "none")+
#       labs(x="% Value of Top Estabs",y="pnc-sqrt Relative Error",col="Value Type")+
#       ggtitle(overtitle,subtitle=title_add)+scale_color_manual(values=colpalette)
#
#
#     nestabplots=ggplot(plotdf,aes(x=qtrly_estabs,y=diff))+
#       geom_point(aes(col=val_type),shape=20,alpha=0.5)+
#       geom_hline(aes(yintercept=0),linetype="dashed")+
#       theme_bw(base_size = basesize)+
#       labs(x="Number of Estabs",y="pnc-sqrt Rel. Err.",col="Value Type")+
#       ggtitle(overtitle)+scale_color_manual(values=colpalette)
#
#     ratioplots=ggplot(plotdf)+
#       geom_point(aes(x=topkpercent,y=ratio,col=val_type),shape=20,alpha=0.5)+
#       geom_hline(aes(yintercept=1),linetype="dashed")+
#       theme_bw(base_size = basesize)+#theme(legend.position = "none")+
#       labs(x="% Value of Top Estabs",y="pnc Rel. Err./ sqrt Rel. Err.",col="Value Type")+
#       ggtitle(overtitle,subtitle=title_add)+scale_color_manual(values=colpalette)
#
#     ratioplotsne=ggplot(plotdf)+
#       geom_point(aes(x=qtrly_estabs,y=ratio,col=val_type),shape=20,alpha=0.5)+
#       geom_hline(aes(yintercept=1),linetype="dashed")+
#       theme_bw(base_size = basesize)+#theme(legend.position = "none")+
#       labs(x="Number of Estabs",y="pnc Rel. Err./ sqrt Rel. Err.",col="Value Type")+
#       ggtitle(overtitle,subtitle=title_add)+scale_color_manual(values=colpalette)
#
#     if(is.null(add.legend.theme)==FALSE){
#       diffplots=diffplots+add.legend.theme
#       nestabplots=nestabplots+add.legend.theme
#       endplots=endplots+add.legend.theme
#       ratioplots=ratioplots+add.legend.theme
#       ratioplotsne=ratioplotsne+add.legend.theme
#     }
#
#     if(bynestab==T){
#       endplots= endplots+facet_wrap(~estab_bin,scales="free")
#       diffplots=diffplots+facet_wrap(~estab_bin,scales="free")
#       nestabplots=nestabplots+facet_wrap(~estab_bin,scales="free")
#       ratioplots=ratioplots+facet_wrap(~estab_bin,scales="free")
#       ratioplotsne=ratioplotsne+facet_wrap(~estab_bin,scales="free")
#     }
#     if(is.null(add.theme)==FALSE){
#       endplots= endplots+add.theme
#       diffplots=diffplots+add.theme
#       nestabplots=nestabplots+add.theme
#       ratioplots=ratioplots+add.theme
#       ratioplotsne=ratioplotsne+add.theme
#     }
#
#   }else{
#
#
#     wage_plot=ggplot(confdf.sub[!is.na(confdf.sub$wage_topkpercent),],aes(x=total_qtrly_wages,y=wagediff))+
#       geom_point(shape=20)+
#       geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
#       #geom_smooth()+
#       theme_bw(base_size = basesize)+
#       ggtitle(paste("Wages"))+
#       labs(x="Original Group Wages",y="pnc-sqrt Rel. Err.")#,col="Mechanism",fill="Mechanism")+
#
#     if(bynestab==T){
#       wage_plot=wage_plot+facet_wrap(~estab_bin,scales="free")
#     }
#
#     emp_plot=ggplot(confdf.sub[!is.na(confdf.sub$month3_emplvl_topkpercent),],aes(x=month3_emplvl,y=empdiff))+
#       geom_point(shape=20)+
#       geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
#
#       theme_bw(base_size = basesize)+
#       ggtitle(paste("Employment"))+
#       labs(x="Original Group Employment",y="pnc-sqrt Rel. Err.")#,col="Mechanism",fill="Mechanism")+
#     #scale_color_manual(values=colpalette[c(2,4)])+scale_fill_manual(values=colpalette[c(1,3)])
#
#     #emp_plot=emp_plot+theme(legend.position = "none")
#
#     wagediff_plot=ggplot(confdf.sub[!is.na(confdf.sub$wage_topkpercent),])+
#       geom_point(aes(x=wage_topkpercent,y=wagediff),shape=20)+
#       geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
#       #geom_smooth(aes(x=wage_topkpercent,y=wagediff),se=F)+
#       theme_bw(base_size = basesize)+#theme(legend.position = "none")+
#       labs(x="% Wages of Top Estabs",y="pnc-sqrt Relative Error")+
#       ggtitle(paste("Wages:"),subtitle=title_add)#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")
#
#     empdiff_plot=ggplot(confdf.sub[!is.na(confdf.sub$month3_emplvl_topkpercent),])+
#       geom_point(aes(x=month3_emplvl_topkpercent,y=empdiff),shape=20)+
#       geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
#       #geom_smooth(aes(x=month3_emplvl_topkpercent,y=empdiff),se=F)+
#       theme_bw(base_size = basesize)+#theme(legend.position = "none")+
#       labs(x="% Employment of Top Estabs",y="pnc-sqrt Relative Error")+
#       ggtitle(paste("Employment:"),subtitle=title_add)#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")
#
#     wagenestab_plot=ggplot(confdf.sub[!is.na(confdf.sub$wage_topkpercent),],aes(x=qtrly_estabs,y=wagediff))+
#       geom_point(shape=20)+
#       geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
#       #geom_smooth()+
#       theme_bw(base_size = basesize)+#theme(legend.position = "none")+
#       labs(x="Number of Estabs.",y="pnc-sqrt Rel. Err.")+
#       ggtitle(paste("Wages"))#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")
#
#     empnestab_plot=ggplot(confdf.sub,aes(x=qtrly_estabs,y=empdiff))+
#       geom_point(shape=20)+
#       geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
#       #geom_smooth()+
#       theme_bw(base_size = basesize)+#theme(legend.position = "none")+
#       labs(x="Number of Estabs",y="pnc-sqrt Rel. Err.")+
#       ggtitle(paste("Employment"))#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")
#
#     # wagenestab_plot=ggplot(confdf.sub[!is.na(confdf.sub$wage_topkpercent),],aes(x=qtrly_estabs,y=wagediff))+
#     #   geom_point(shape=20)+
#     #   geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
#     #   #geom_smooth()+
#     #   theme_bw(base_size = basesize)+#theme(legend.position = "none")+
#     #   labs(x="Number of Estabs.",y="pnc-sqrt Rel. Err.")+
#     #   ggtitle(paste("Wages"))#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")
#     #
#     # empnestab_plot=ggplot(confdf.sub,aes(x=qtrly_estabs,y=empdiff))+
#     #   geom_point(shape=20)+
#     #   geom_hline(aes(yintercept=0),col="red",linetype="dashed")+
#     #   #geom_smooth()+
#     #   theme_bw(base_size = basesize)+#theme(legend.position = "none")+
#     #   labs(x="Number of Estabs",y="pnc-sqrt Rel. Err.")+
#     #   ggtitle(paste("Employment"))#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")
#
#     wageratio_plot=ggplot(confdf.sub[!is.na(confdf.sub$wage_topkpercent),])+
#       geom_point(aes(x=wage_topkpercent,y=wageratio),shape=20)+
#       geom_hline(aes(yintercept=1),col="red",linetype="dashed")+
#       #geom_smooth(aes(x=wage_topkpercent,y=wagediff),se=F)+
#       theme_bw(base_size = basesize)+#theme(legend.position = "none")+
#       labs(x="% Wages of Top Estabs",y="pnc-sqrt Relative Error")+
#       ggtitle(paste("Wages:"),subtitle=title_add)#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")
#
#     empratio_plot=ggplot(confdf.sub[!is.na(confdf.sub$month3_emplvl_topkpercent),])+
#       geom_point(aes(x=month3_emplvl_topkpercent,y=empratio),shape=20)+
#       geom_hline(aes(yintercept=1),col="red",linetype="dashed")+
#       #geom_smooth(aes(x=month3_emplvl_topkpercent,y=empdiff),se=F)+
#       theme_bw(base_size = basesize)+#theme(legend.position = "none")+
#       labs(x="% Employment of Top Estabs",y="pnc Rel. Err./ sqrt Rel. Err.")+
#       ggtitle(paste("Employment:"),subtitle=title_add)#,subtitle="Groups with Highest and Lowest Differences in Relative Error Between Mechanisms")
#
#     if(bynestab==T){
#       emp_plot= emp_plot+facet_wrap(~estab_bin,scales="free")
#       empdiff_plot=empdiff_plot+facet_wrap(~estab_bin,scales="free")
#       wagediff_plot=wagediff_plot+facet_wrap(~estab_bin,scales="free")
#       wagenestab_plot=wagenestab_plot+facet_wrap(~estab_bin,scales="free")
#       empnestab_plot=empnestab_plot+facet_wrap(~estab_bin,scales="free")
#       empratio_plot=empratio_plot+facet_wrap(~estab_bin,scales="free")
#       wageratio_plot=wageratio_plot+facet_wrap(~estab_bin,scales="free")
#     }
#     if(is.null(add.theme)==FALSE){
#       emp_plot= emp_plot+add.theme
#       empdiff_plot=empdiff_plot+add.theme
#       wage_plot= wage_plot+add.theme
#       wagediff_plot=wagediff_plot+add.theme
#       wagenestab_plot=wagenestab_plot+add.theme
#       empnestab_plot=empnestab_plot+add.theme
#       empratio_plot=empratio_plot+add.theme
#       wageratio_plot=wageratio_plot+add.theme
#     }
#
#     if(is.na(min_nestabs)==FALSE){
#       legend_label=paste0(legend_label," (>",min_nestabs," Estabs in Group)")
#     }
#     if(bynestab==TRUE){
#       overtitle=cowplot::ggdraw()+cowplot::draw_label(paste0(legend_label," by Number of Establishments in Group"),size=basesize*1.3)
#
#
#       endplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wage_plot,emp_plot,ncol=1),ncol=1,rel_heights = c(.1,2))
#
#       diffplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wagediff_plot,empdiff_plot,ncol=1),ncol=1,rel_heights = c(.1,2))
#
#       nestabplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wagenestab_plot,empnestab_plot,ncol=1),ncol=1,rel_heights = c(.1,2))
#
#       ratioplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wageratio_plot,empratio_plot,ncol=1),ncol=1,rel_heights = c(.1,2))
#
#       ratioplotsne=NA
#     }else{
#       overtitle=cowplot::ggdraw()+cowplot::draw_label(paste0(legend_label),size=basesize*1.3)
#
#       endplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wage_plot,emp_plot,ncol=2,rel_widths = c(1,1)),ncol=1,rel_heights = c(0.1,1))
#
#       diffplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wagediff_plot,empdiff_plot,ncol=2,rel_widths = c(1,1)),ncol=1,rel_heights = c(0.1,1))
#
#       nestabplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wagenestab_plot,empnestab_plot,ncol=2,rel_widths = c(1,1)),ncol=1,rel_heights = c(0.1,1))
#       ratioplots=cowplot::plot_grid(overtitle,cowplot::plot_grid(wageratio_plot,empratio_plot,ncol=2,rel_widths = c(1,1)),ncol=1,rel_heights = c(0.1,1))
#       ratioplotsne=NA
#     }
#   }
#
#   plt_list[[1]]=endplots
#   plt_list[[2]]=diffplots
#   plt_list[[3]]=nestabplots
#   plt_list[[4]]=ratioplots
#   plt_list[[5]]=ratioplotsne
#   return(plt_list)
#   #return(list("plot_list"=plot_list,"legend_list"=legend_list,"data"=confdf.sub))
# }
#
#


lm_diagnostics_plots=function(fit,qq.linecolor="grey17",residfit.linecolor="blue",
                              basesize=8,include.title=T,
                              add.qq.theme=NULL,add.residfit.theme=NULL){
  fitdf=data.frame("fitted"=fit$fitted.values,"residuals"=fit$residuals)

  residfit=ggplot2::ggplot(fitdf,ggplot2::aes(y=residuals,x=fitted))+
    ggplot2::geom_hline(ggplot2::aes(yintercept=0),color=residfit.linecolor)+
    ggplot2::geom_point()+ggplot2::geom_smooth(se=F)+
    ggplot2::labs(x="Fitted",y="Residual")+
    ggplot2::ggtitle("Fitted vs. Residuals")+
    ggplot2::theme_bw(base_size = basesize)
  if(is.null(add.residfit.theme)==F){
    residfit=handle_addtheme(add.residfit.theme,residfit)
  }

  sampquant=quantile(fitdf$residuals,c(0.25,0.75))
  normquant=stats::qnorm(c(0.25,0.75))
  lineslope=base::diff(sampquant)/base::diff(normquant)
  qqplt=ggplot2::ggplot(fitdf,ggplot2::aes(sample=residuals))+
    ggplot2::stat_qq()+
    ggplot2::geom_abline(slope=lineslope,intercept=sampquant[1]-(lineslope*normquant[1]),color=qq.linecolor)+
    ggplot2::labs(x="Theoretical Quantiles",y="Sample Quantiles")+
    ggplot2::ggtitle("Normal Q-Q Plot")+
    ggplot2::theme_bw(base_size = basesize)
  if(is.null(add.qq.theme)==F){
    qqplt=handle_addtheme(add.qq.theme,qqplt)
  }
  plt=cowplot::plot_grid(residfit,qqplt,ncol=2)

  if(include.title==T){
    ##Get Overtitle
    modellab=as.character(lmfit1$call)[2]
    lablen=nchar(modellab)
    lablen.ns=nchar(gsub(" ","",modellab))
    if(lablen.ns>70){
      vecsplt=base::unlist(base::strsplit(modellab," "))
      wordsct=length(vecsplt)
      if(lablen.ns<120){
        labvec=c(paste(vecsplt[seq(1,base::ceiling(wordsct/2))],collapse=" "),
                 paste(vecsplt[seq(base::ceiling(wordsct/2)+1,wordsct)],collapse=" "))
        nmod=2
      }else if(lablen.ns<170){
        firstcut=base::ceiling(wordsct/3)
        secondcut=base::ceiling((wordsct-firstcut)/2)
        labvec=c(paste(vecsplt[seq(1,firstcut)],collapse=" "),
                 paste(vecsplt[seq(firstcut+1,secondcut)],collapse=" "),
                 paste(vecsplt[seq(secondcut+1,wordsct)],collapse=" "))
        nmod=3
      }else{
        firstcut=base::ceiling(wordsct/4)
        secondcut=base::ceiling((wordsct-firstcut)/3)
        thirdcut=base::ceiling((wordsct-secondcut)/2)
        labvec=c(paste(vecsplt[seq(1,firstcut)],collapse=" "),
                 paste(vecsplt[seq(firstcut+1,secondcut)],collapse=" "),
                 paste(vecsplt[seq(secondcut+1,thirdcut)],collapse=" "),
                 paste(vecsplt[seq(thirdcut+1,wordsct)],collapse=" "))
        nmod=4
      }
      #nmod=lablen.ns%/%35
      #nremain=lablen.ns%%35
      #if(nremain>17){
      #  nmod=nmod+1
      #}
      # vecsplt=base::unlist(base::strsplit(modellab," "))
      # goallen=base::ceiling(lablen/nmod)
      # labvec=NULL
      # templab=NULL
      # print(vecsplt)
      # print(goallen)
      # for(i in seq(1,length(vecsplt))){
      #   possiblelab=paste(templab,vecsplt[i])
      #   if(nchar(possiblelab)<goallen){
      #     templab=possiblelab
      #   }else{
      #     labvec=c(labvec,templab)
      #     templab=""
      #   }
      # }
      #print(labvec)
      overtitle=cowplot::ggdraw(xlim=c(0,4),ylim=c(0,nmod+1))+
        cowplot::draw_text(text=labvec,x=2,y=seq(1,nmod),size=(basesize*4)/nmod)
    }else{
      nmod=1
      if(lablen>70){
        modellab=gsub(" ","",modellab)
      }else{
        modellab=modellab
        nmod=1
      }
      overtitle=cowplot::ggdraw()+
        cowplot::draw_label(modellab,size=basesize*1.5)
    }
    plt=cowplot::plot_grid(overtitle,plt,ncol=1,rel_heights = c(0.2*nmod,1))
  }
  return(plt)
}

