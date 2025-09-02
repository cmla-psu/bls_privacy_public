## FROM dplyr NEED group_by(), summarize(), n(), if_else(), left_join(), bind_cols(), bind_rows()
## FROM yaml NEED read_yaml()

r_basepath = rprojroot::find_rstudio_root_file()
main.start=proc.time()

basepath=gsub("/[^/]*$","",r_basepath) #run Mechanism folder
folderin=paste0(basepath,"/compare_data") #folder of comparison outputs
folderRtabs=paste0(basepath,"/R_tables_plots") #folder for tables and plots out

source(paste0(r_basepath,"/comparison_config_functions.R"))
source(paste0(r_basepath,"/table_functions.R"))
source(paste0(r_basepath,"/special_table_functions.R"))
source(paste0(r_basepath,"/table_figure_naming_functions.R"))
source(paste0(r_basepath,"/ploting_functions.R"))
source(paste0(r_basepath,"/data_wrangling_functions.R"))

nj.state.prefix=c("nj34_qbp_2016_1")
ri.state.prefix=c("ri44_qbp_2016_1")
state.prefixes=c(nj.state.prefix,ri.state.prefix)

aggcodes_plot=c(55,57,71)
add.theme.plog=ggplot2::scale_y_continuous(trans="pseudo_log")

add.theme.none=NULL
add.legend.theme=ggplot2::theme(legend.position="bottom")
add.legend.theme.bottom=add.legend.theme
add.legend.theme.none=ggplot2::theme(legend.position = "none")
mechcolpalette=colorBlindness::Blue2Orange12Steps[c(2,10)]
bynestabs=F

plt.scale.b=0.87
plt.h.b=3.2
plt.w.b=6.5



###########################################
############## Changing Overall Budget
##############################################
# budget_translate=function(x){
#   gsub("0p","0.",x)
# }
budget_translate=NULL
####### Relative Difference
breps_rdif=NULL
breps_dif=NULL
breps_rel=NULL
breps_abs=NULL
for(pnum in c(1,2,3)){
  breps_p=pivot_agg_file(sanagg=NULL,state.prefix=nj.state.prefix,
                         comp.suffix="budget",
                         metrics=c("rdif","dif"),
                         param.colname="budget",
                         param.translator=budget_translate,
                         basepath=basepath,filename.comp.suffix=paste0("budgets_reps_p",pnum),
                         clipprob=F,budgets=T,
                         config.seperated.names=c("configstem","budget","rep","mech","metric"),
                         baseline="blsvals",flipsign=T)

  breps_p$agglvl_codelab=agglvl_code_labeller(breps_p$agglvl_code)
  breps_p$mech[breps_p$mech=="clip"]="pnc"
  breps_p$budget=as.character(breps_p$budget)
  breps_rdif=dplyr::bind_rows(breps_rdif,
                              breps_p[(breps_p$metric=="rdif")&(!is.na(breps_p$value)),])
  breps_dif=dplyr::bind_rows(breps_dif,
                             breps_p[(breps_p$metric=="dif")&(!is.na(breps_p$value)),])

}
str(breps_dif)


breps_rdif$rel=abs(breps_rdif$value)
breps_dif$abs=abs(breps_dif$value)
breps_rel=breps_rdif
breps_rel$value=breps_rel$rel
print(table(breps_dif$budget,useNA = "always"))
print(table(breps_rdif$budget,useNA = "always"))

breps_dif$budget=as.factor(round(as.numeric(gsub("2p","2.",gsub("0p","0.",as.character(breps_dif$budget)))),1))
breps_rdif$budget=as.factor(round(as.numeric(gsub("2p","2.",(gsub("0p","0.",as.character(breps_rdif$budget))))),1))

breps_dif$area_fips[breps_dif$agglvl_code>69]=sapply(breps_dif$area_fips[breps_dif$agglvl_code>69],fix_fips)
breps_dif$agggroup=paste(breps_dif$area_fips,"X",breps_dif$industry_code)
breps_rdif$area_fips[breps_rdif$agglvl_code>69]=sapply(breps_rdif$area_fips[breps_rdif$agglvl_code>69],fix_fips)
breps_rdif$agggroup=paste(breps_rdif$area_fips,"X",breps_rdif$industry_code)



all.aggcodes=c(51,52,53,54,55,56,57,58,71,72,73,74,75,76,77,78)
origdata=equalwage_orig_data_info(state.prefixes=nj.state.prefix,aggcodes.pper=all.aggcodes,ks.and.ppers=NULL,
                                  compare.suffix="equalwage_rep",basepath.str=basepath,override.agggroups=T)
breps_combine=dplyr::left_join(breps_dif,origdata[origdata$state=="nj34",c("agggroup","month3_emplvl","total_qtrly_wages")],"agggroup")
dffilt=breps_combine$agggroup[(breps_combine$month3_emplvl>0)&(breps_combine$total_qtrly_wages>0)]
breps_combine=breps_combine[breps_combine$agggroup%in%dffilt,]
breps_rdif=breps_rdif[breps_rdif$agggroup%in%dffilt,]

breps_combine$origvalue=breps_combine$month3_emplvl
breps_combine$origvalue[breps_combine$variable=="wage"]=breps_combine$total_qtrly_wages[breps_combine$variable=="wage"]
breps_combine$distparam=ifelse(breps_combine$variable=="wage",50,0.5)
breps_combine$privalloc=ifelse(breps_combine$variable=="wage",0.281*0.015,0.281*(1-0.15)/3)
breps_combine$new_rel=abs(breps_combine$value)/(1+breps_combine$origvalue)


numeric.cols=colnames(breps_rdif)[sapply(breps_rdif,is.numeric)]
breps_rdif_avg=dplyr::summarize_at(group_by(breps_rdif,agggroup,variable,configstem,agglvl_codelab,mech,budget),numeric.cols,mean,na.rm=T)
breps_rdif_med=dplyr::summarize_at(group_by(breps_rdif,agggroup,variable,configstem,agglvl_codelab,mech,budget),numeric.cols,median,na.rm=T)

breps_rdif_sd=dplyr::summarize_at(group_by(breps_rdif,agggroup,variable,configstem,agglvl_codelab,mech,budget),numeric.cols,var,na.rm=T)

numeric.cols=colnames(breps_combine)[sapply(breps_combine,is.numeric)]
breps_dif_avg=dplyr::summarize_at(group_by(breps_combine,agggroup,variable,configstem,agglvl_codelab,mech,budget),numeric.cols,mean,na.rm=T)
breps_dif_med=dplyr::summarize_at(group_by(breps_combine,agggroup,variable,configstem,agglvl_codelab,mech,budget),numeric.cols,median,na.rm=T)
breps_dif_sd=dplyr::summarize_at(group_by(breps_combine,agggroup,variable,configstem,agglvl_codelab,mech,budget),numeric.cols,var,na.rm=T)

breps_rdif_plot=dplyr::ungroup(breps_rdif_avg[,colnames(breps_rdif_avg)!="rel"])#,baseline_comp[,colnames(baseline_comp)%in%colnames(breps_rdif)])
breps_dif_plot=dplyr::ungroup(breps_dif_avg[,colnames(breps_dif_avg)!="abs"])#,baseline_comp[,colnames(baseline_comp)%in%colnames(breps_dif)])
breps_rel_plot=dplyr::ungroup(breps_dif_avg[,colnames(breps_dif_avg)!="value"])#,baseline_comp[,colnames(baseline_comp)%in%colnames(breps_rdif)])
breps_rel_plot$value=breps_rel_plot$new_rel
breps_rel_plot$metric="rel"
breps_abs_plot=dplyr::ungroup(breps_dif_avg[,colnames(breps_dif_avg)!="value"])#,baseline_comp[,colnames(baseline_comp)%in%colnames(breps_dif)])
breps_abs_plot$value=breps_abs_plot$abs
breps_abs_plot$metric="abs"

breps_rdif_plot_med=dplyr::ungroup(breps_rdif_med[,colnames(breps_rdif_med)!="rel"])#,baseline_comp[,colnames(baseline_comp)%in%colnames(breps_rdif)])
breps_dif_plot_med=dplyr::ungroup(breps_dif_avg[,colnames(breps_dif_med)!="abs"])#,baseline_comp[,colnames(baseline_comp)%in%colnames(breps_dif)])
breps_rel_plot_med=dplyr::ungroup(breps_dif_avg[,colnames(breps_dif_med)!="value"])#,baseline_comp[,colnames(baseline_comp)%in%colnames(breps_rdif)])
breps_rel_plot_med$value=breps_rel_plot_med$new_rel
breps_rel_plot_med$metric="rel"
breps_abs_plot_med=dplyr::ungroup(breps_dif_med[,colnames(breps_dif_med)!="value"])#,baseline_comp[,colnames(baseline_comp)%in%colnames(breps_dif)])
breps_abs_plot_med$value=breps_abs_plot_med$abs
breps_abs_plot_med$metric="abs"


source(paste0(r_basepath,"/ploting_functions.R"))
plotvariables=c("emp","wage")
plotmetrics=c("rel")#c("reldiff","diff")
transformations.outliers=c("keepoutliers_notrans")#c("removeoutliers_notrans","keepoutliers_notrans","keepoutliers_log10")#c("removeoutliers_plog","removeoutliers_notrans","keepoutliers_notrans")
plotvariants=expand.grid(plotvariables,plotmetrics,transformations.outliers)
aggcodes_plot=c(55,57,71)
plt.scale.b=0.63
plt.w.b=5.7
plt.h.b=3.2
iter_cell_distribution(rdif_data=breps_rdif_plot,dif_data=breps_dif_plot,
                       rel_data=breps_rel_plot, abs_data=breps_abs_plot,
                       aggcodes_plot=aggcodes_plot,
                       plotvariants=plotvariants,
                       xcol="budget","Privacy Budget",
                       state.prefix="nj34",folderin=folderin,folderRtabs=folderRtabs,
                       mechcolpalette=mechcolpalette,
                       add.legend.theme=add.legend.theme,add.theme=NULL,
                       facet.scale="fixed",add.file.suffix="_cellavg")

iter_cell_distribution(rdif_data=breps_rdif_plot_med,dif_data=breps_dif_plot_med,
                       rel_data=breps_rel_plot_med, abs_data=breps_abs_plot_med,
                       aggcodes_plot=aggcodes_plot,
                       plotvariants=plotvariants,
                       xcol="budget","Privacy Budget",
                       state.prefix="nj34",folderin=folderin,folderRtabs=folderRtabs,
                       mechcolpalette=mechcolpalette,
                       add.legend.theme=add.legend.theme,add.theme=NULL,
                       facet.scale="fixed",add.file.suffix="_cellmedian")

plt.scale.b=0.63
plt.w.b=5.7
aggcodes_plot=c(56,57,58)
iter_cell_distribution(rdif_data=breps_rdif_plot,dif_data=breps_dif_plot,
                       rel_data=breps_rel_plot, abs_data=breps_abs_plot,
                       aggcodes_plot=aggcodes_plot,
                       plotvariants=plotvariants,
                       xcol="budget",xcollab="Privacy Budget",
                       state.prefix="nj34",folderin=folderin,folderRtabs=folderRtabs,
                       mechcolpalette=mechcolpalette,
                       add.legend.theme=add.legend.theme,add.theme=NULL,
                       facet.scale="fixed",add.file.suffix="_altagglvls_cellavg")

transformations.outliers=c("removeoutliers_notrans","keepoutliers_notrans")#c("removeoutliers_plog","removeoutliers_notrans","keepoutliers_notrans")
plotvariants=expand.grid(plotvariables,plotmetrics,transformations.outliers)
aggcodes_plot=c(55,57,71)
plt.scale.b=0.63
plt.w.b=5.7
plt.h.b=3.2
iter_cell_distribution(rdif_data=breps_rdif_plot,dif_data=breps_dif_plot,
                       rel_data=breps_rel_plot, abs_data=breps_abs_plot,
                       aggcodes_plot=aggcodes_plot,
                       plotvariants=plotvariants,
                       xcol="budget","Privacy Budget",
                       state.prefix="nj34",folderin=folderin,folderRtabs=folderRtabs,
                       mechcolpalette=mechcolpalette,
                       add.legend.theme=add.legend.theme,add.theme=NULL,
                       facet.scale="free",add.file.suffix="_freescales_cellavg")
plt.scale.b=0.87
plt.w.b=6.5
plt.h.b=1.2
aggcodes_plot=c(51)
transformations.outliers=c("keepoutliers_notrans")
breps_notavg_rel=breps_combine
breps_notavg_rel$value=breps_notavg_rel$new_rel
breps_notavg_rel$metric="rel"
breps_notavg_abs=breps_combine
breps_notavg_abs$value=breps_notavg_rel$abs
breps_notavg_abs$metric="abs"
iter_cell_distribution(rdif_data=breps_rdif,
                       dif_data=breps_combine,
                       rel_data=breps_notavg_rel,
                       abs_data=breps_notavg_abs,
                       aggcodes_plot=aggcodes_plot,
                       plotvariants=plotvariants,
                       xcol="budget","Privacy Budget",
                       state.prefix="nj34",folderin=folderin,folderRtabs=folderRtabs,
                       mechcolpalette=mechcolpalette,
                       add.legend.theme=ggplot2::theme(legend.position="right"),add.theme=NULL,facet.scale="free")

iter_cell_distribution(rdif_data=breps_rdif[breps_rdif$budget!="0.5",],
                       dif_data=breps_combine[breps_combine$budget!="0.5",],
                       rel_data=breps_notavg_rel[breps_notavg_rel$budget!="0.5",],
                       abs_data=breps_notavg_abs[breps_notavg_abs$budget!="0.5",],
                       aggcodes_plot=aggcodes_plot,
                       plotvariants=plotvariants,
                       xcol="budget","Privacy Budget",
                       state.prefix="nj34",folderin=folderin,folderRtabs=folderRtabs,
                       mechcolpalette=mechcolpalette,
                       add.legend.theme=ggplot2::theme(legend.position="right"),add.theme=NULL,facet.scale="free",
                       add.file.suffix="_nohalfbudget")

breps_abs_plot$variable_lab=ifelse(breps_abs_plot$variable=="emp","Employment","Wages")
ggplot(breps_abs_plot[breps_abs_plot$agglvl_code%in%c(71),],aes(x=origvalue,y=new_rel,col=mech))+
  geom_point(size=1,shape=20,alpha=0.5)+
  geom_smooth(se=F)+
  labs(x="Original Value",y="Absolute Relative Difference",col=" ")+
  ggtitle("Synthetic New Jersey by County")+
  theme_bw(base_size=8)+
  facet_wrap(vars(variable_lab),scales="free")

absorig3_emp=ggplot(breps_abs_plot[(breps_abs_plot$agglvl_code%in%c(55,57,71))&(breps_abs_plot$budget==2.3)&(breps_abs_plot$variable=="emp"),],
                    aes(x=origvalue,y=new_rel,col=mech))+
  geom_point(size=1,shape=20,alpha=0.75)+
  #geom_smooth(se=F)+
  labs(x="Original Value",y="Absolute Relative Difference",col="Mechanism")+
  ggtitle("Synthetic New Jersey Employment")+
  theme_bw(base_size=8)+
  theme(legend.position = "bottom",
        axis.text.x = ggplot2::element_text(angle=-40,hjust=0,vjust=0.5),
        plot.margin=margin(10,20,0,10))+
  scale_y_log10()+
  scale_color_manual(values=mechcolpalette)+
  facet_wrap(vars(agglvl_codelab),scales="free_x")

ggsave(paste0(folderRtabs,"/budget_reps/abs_orig_baseprivbudget_emp_3aggs.pdf"),
       absorig3_emp,
       scale=0.73,width=3.7,height=8,units="in",dpi=760,bg="white")

absorig3_wage=ggplot(breps_abs_plot[(breps_abs_plot$agglvl_code%in%c(55,57,71))&(breps_abs_plot$budget==2.3)&(breps_abs_plot$variable!="emp"),],
                     aes(x=origvalue,y=new_rel,col=mech))+
  geom_point(size=1,shape=20,alpha=0.75)+
  #geom_smooth(se=F)+
  labs(x="Original Value",y="Absolute Relative Difference",col="Mechanism")+
  ggtitle("Synthetic New Jersey Wages")+
  theme_bw(base_size=8)+
  theme(legend.position = "bottom",
        axis.text.x = ggplot2::element_text(angle=-40,hjust=0,vjust=0.5),
        plot.margin=margin(10,20,0,10))+
  scale_y_log10()+
  scale_color_manual(values=mechcolpalette)+
  facet_wrap(vars(agglvl_codelab),scales="free_x")

ggsave(paste0(folderRtabs,"/budget_reps/abs_orig_baseprivbudget_wages_3aggs.pdf"),
       absorig3_wage,
       scale=0.73,width=3.7,height=8,units="in",dpi=760,bg="white")

tempdf=breps_abs_plot
#aggcodes_plot=c(55,57,71,77)
aggcodes_plot=c(55,57,71)

naggs=length(aggcodes_plot)
plt.scale.b=ifelse((naggs==3)|(naggs>4),0.73,0.83)
plt.h.b=ifelse(naggs>=4,6.5,3.5)
plt.w.b=ifelse(naggs==4,6.5,ifelse((naggs==3)|(naggs>4),8.5,4.3))
for(varemp in c("Employment","Wages")){
  for(metric in c("Absolute Relative Difference","Absolute Difference")){
    title.str=paste0("Synthetic New Jersey ",varemp,": ",metric)
    if(varemp=="Employment"){
      data.filter=(breps_abs_plot$agglvl_code%in%aggcodes_plot)&(breps_abs_plot$budget==2.3)&(breps_abs_plot$variable=="emp")
    }else{
      data.filter=(breps_abs_plot$agglvl_code%in%aggcodes_plot)&(breps_abs_plot$budget==2.3)&(breps_abs_plot$variable!="emp")
    }
    if(metric=="Absolute Difference"){
      filestr=paste0("abs_orig_baseprivbudget_",ifelse(varemp=="Wages","wages","emp"),"_",length(aggcodes_plot),"aggs.pdf")
      tempdf$yval=tempdf$abs
    }else{
      filestr=paste0("rel_orig_baseprivbudget_",ifelse(varemp=="Wages","wages","emp"),"_",length(aggcodes_plot),"aggs.pdf")
      tempdf$yval=tempdf$new_rel
    }
    plt=ggplot(tempdf[data.filter,],
               aes(x=origvalue,y=yval,col=mech))+
      geom_point(size=1,shape=20,alpha=0.75)+
      #geom_smooth(se=F)+
      labs(x="Original Value",y=paste("Mean",metric),col="Mechanism")+
      ggtitle(title.str)+
      theme_bw(base_size=8)+
      theme(legend.position = "bottom",
            axis.text.x = ggplot2::element_text(angle=-40,hjust=0,vjust=0.5),
            plot.margin=margin(10,20,0,10))
    if(metric!="Absolute Difference"){
      plt=plt+scale_y_log10()+scale_color_manual(values=mechcolpalette)+
        facet_wrap(vars(agglvl_codelab),scales="free")
    }else{
      plt=plt+
        scale_color_manual(values=mechcolpalette)+
        facet_wrap(vars(agglvl_codelab),scales="free")
    }
    ggsave(plot=plt,filename=paste0(folderRtabs,"/budget_reps/",filestr),
           scale=plt.scale.b,width=plt.w.b,height=plt.h.b,units="in",dpi=520,bg="white")
  }
}

