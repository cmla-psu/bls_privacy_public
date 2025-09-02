library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)

#investigate why =W% doesn't mean = performance of wage and employment
r_basepath = rprojroot::find_rstudio_root_file()
main.start=proc.time()

basepath=gsub("/[^/]*$","",r_basepath) #run Mechanism folder
folderin=paste0(basepath,"/compare_data") #folder of comparison outputs
folderRtabs=paste0(basepath,"/R_tables_plots") #folder for tables and plots out

source(paste0(r_basepath,"/comparison_config_functions.R"))
source(paste0(r_basepath,"/table_functions.R"))
source(paste0(r_basepath,"/table_figure_naming_functions.R"))
source(paste0(r_basepath,"/ploting_functions.R"))
source(paste0(r_basepath,"/data_wrangling_functions.R"))


nj.state.prefixes=c("nj34_qbp_2016_1")
ri.state.prefixes=c("ri44_qbp_2016_1")
state.prefixes=c(nj.state.prefixes,ri.state.prefixes)

comp.suffix="equalwage_rep"
namer.csv.fname="table_config_names.csv"
label.digits=2
removeaggcodes=c(50,70)
check_agglvls=c("55","71","75","77")


bynestab=T
estab_breaks=c(0,0.25,0.5,0.75,1)
min_nestabs=NA
add.theme=NULL
add.legend.theme=NULL
basesize=5

pper.possible=c(0.01,0.05,0.1,0.2,0.25,0.3) #consider the pper parameters
k.possible=c(1,2,5,10,15,20,30) #consider these k parameters
trytopk=data.frame(p=c(pper.possible,rep(NA,length(k.possible))),
                   k=c(rep(NA,length(pper.possible)),k.possible))
aggcodes.pper=c(51,52,53,54,55,56,57,58,71,72,73,74,75,76,77,78)

pperdf=data.frame()
nest_pperdf=data.frame()
#read in data
for(st.pref in state.prefixes){
  nj.indic=grepl("nj34",st.pref)
  confdf=read.csv(paste0(basepath,"/EstablishmentLevelData/",base::substr(st.pref,1,4),"/",st.pref,".csv"))

  if(nj.indic==T){
    sanagg.fname=paste0(basepath,"/compare_data/",comp.suffix,"/",st.pref,"_",comp.suffix,"_prep.csv")
  }else{
    sanagg.fname=paste0(basepath,"/compare_data/",comp.suffix,"/",st.pref,"_",comp.suffix,".csv")
  }
  sanagg=read.csv(sanagg.fname)
  sanagg=sanagg[sanagg$own_code==5,grepl("agglvl|fips|industry|qtrly|month3",colnames(sanagg))]
  sanagg=sanagg[,grepl("agglvl|fips|industry|estabs",colnames(sanagg))|grepl("equalwage",colnames(sanagg))|colnames(sanagg)%in%c("total_qtrly_wages","month3_emplvl")]

  for(aggc in aggcodes.pper){ #for each agglvl code
    outtemp=compare_reps_topk(sanagg,confdf,agglvl_code=aggc,ks.and.ppers=trytopk,
                              state.prefix=st.pref,
                              estab_breaks=NULL,
                              add.conf.stats=c("max","sumnotmax"))
    tempdf=outtemp[[1]]
    tempdf$state=base::substr(st.pref,1,2)
    #print(colnames(tempdf))
    pperdf=dplyr::bind_rows(pperdf,tempdf)
    if(bynestab==T){
      outtemp=compare_reps_topk(sanagg,confdf,agglvl_code=aggc,ks.and.ppers=trytopk,
                                state.prefix=st.pref,
                                estab_breaks=estab_breaks,
                                add.conf.stats=c("max","sumnotmax"))
      nest_tempdf=outtemp[[1]]
      nest_tempdf$state=base::substr(st.pref,1,2)
      nest_pperdf=dplyr::bind_rows(nest_pperdf,nest_tempdf)
    }

  }

}

longdf=longdf_reps_topk(pperdf)

if(bynestab==T){
  nestdf=longdf_reps_topk(nest_pperdf)

}

longdf$agglvl_codelab=agglvl_code_labeller(longdf$agglvl_code) #agglvl code string labels

longdf$dif=longdf$sanvalue-longdf$orig_val #difference (signed)
longdf$rdif=(longdf$sanvalue-longdf$orig_val)/longdf$orig_val #relative difference (signed)
longdf$abs=abs(longdf$dif) #absolute error
longdf$new_rel=longdf$abs/(longdf$orig_val+1) #absolute relative difference

#synthetic data quirk, where some establishment ended up with 0 employees and 0 wages (remove them)
dffilt2=intersect(longdf$agggroup[(longdf$orig_val==0)&(longdf$variable=="wages")],
                  longdf$agggroup[(longdf$orig_val==0)&(longdf$variable=="employment")])
longdf=longdf[(!(longdf$agggroup%in%dffilt2))&(longdf$repetition!="35"),] #replicate 35 is missing for RI

selectcols=c('agglvl_code','agggroup','state','cnty','industry_code',
             'qtrly_estabs','ngroups','maxestab','sumnotmax','orig_val',
             'variable','mechanism','sanvalue','abs','dif','rdif','rel',
             'agglvl_codelab','new_rel')
numeric.cols=colnames(longdf[,selectcols])[sapply(longdf[,selectcols],is.numeric)]
longdf_avg=dplyr::summarise_at(
  dplyr::group_by(longdf[,selectcols],variable,mechanism,agglvl_codelab,agggroup,state,cnty),
  numeric.cols,mean,na.rm=T)


#Plotting inputs
basesize=10
colpalette=colorBlindness::Blue2DarkOrange12Steps[c(2,10)]




## Boxplots of relative and absolute error
cntyplot=equalwage_rep_boxplots(data=longdf_avg,contvar="new_rel",catvar="mechanism",
                                aggcodes=c(71),
                                aggcode_strs = c("By County"),
                                variable="both",state="all",add.theme=NULL,
                                add.legend.theme=ggplot2::theme(legend.position="none"),
                                colpalette=colpalette,basesize=basesize,
                                facet.scales="free",override.title="By County")
ggsave(paste0(basepath,"/R_tables_plots/",comp.suffix,"/bothstate_rel_cnty_reps.pdf"),
       cntyplot,
       scale=0.73,width=6.5,height=5.5,units="in",dpi=740,bg="white")

n3plot=equalwage_rep_boxplots(data=longdf_avg,contvar="new_rel",catvar="mechanism",
                              aggcodes=c(55),
                              aggcode_strs = c("By NAICS-3"),
                              variable="both",state="all",add.theme=NULL,
                              add.legend.theme=ggplot2::theme(legend.position="none"),
                              colpalette=colpalette,basesize=basesize,
                              facet.scales="free",override.title="By NAICS-3")
ggsave(paste0(basepath,"/R_tables_plots/",comp.suffix,"/bothstate_rel_naics3_reps.pdf"),
       n3plot,
       scale=0.73,width=6.5,height=5.5,units="in",dpi=740,bg="white")

n5plot=equalwage_rep_boxplots(data=longdf_avg,contvar="new_rel",catvar="mechanism",
                              aggcodes=c(57),
                              aggcode_strs = c("By NAICS-5"),
                              variable="both",state="all",add.theme=NULL,
                              add.legend.theme=ggplot2::theme(legend.position="none"),
                              colpalette=colpalette,basesize=basesize,
                              facet.scales="free",override.title="By NAICS-5")
ggsave(paste0(basepath,"/R_tables_plots/",comp.suffix,"/bothstate_rel_naics5_reps.pdf"),
       n5plot,
       scale=0.73,width=6.5,height=5.5,units="in",dpi=740,bg="white")

stplot=equalwage_rep_boxplots(data=longdf,contvar="new_rel",catvar="mechanism",
                              aggcodes=c(51),
                              aggcode_strs = c("State Total"),
                              variable="both",state="all",add.theme=NULL,
                              add.legend.theme=ggplot2::theme(legend.position="none"),
                              colpalette=colpalette,basesize=basesize,
                              facet.scales="free",override.title="State Total")
ggsave(paste0(basepath,"/R_tables_plots/",comp.suffix,"/bothstate_rel_statetotal_reps.pdf"),
       stplot,
       scale=0.73,width=6.5,height=5.5,units="in",dpi=740,bg="white")


cntyplota=equalwage_rep_boxplots(data=longdf_avg,contvar="abs",catvar="mechanism",
                                 aggcodes=c(71),
                                 aggcode_strs = c("By County"),
                                 variable="both",state="all",add.theme=NULL,
                                 add.legend.theme=ggplot2::theme(legend.position="none"),
                                 colpalette=colpalette,basesize=basesize,
                                 facet.scales="free",override.title="By County")
ggsave(paste0(basepath,"/R_tables_plots/",comp.suffix,"/bothstate_abs_cnty_reps.pdf"),
       cntyplota,
       scale=0.73,width=6.5,height=5.5,units="in",dpi=740,bg="white")

n3plota=equalwage_rep_boxplots(data=longdf_avg,contvar="abs",catvar="mechanism",
                               aggcodes=c(55),
                               aggcode_strs = c("By NAICS-3"),
                               variable="both",state="all",add.theme=NULL,
                               add.legend.theme=ggplot2::theme(legend.position="none"),
                               colpalette=colpalette,basesize=basesize,
                               facet.scales="free",override.title="By NAICS-3")
ggsave(paste0(basepath,"/R_tables_plots/",comp.suffix,"/bothstate_abs_naics3_reps.pdf"),
       n3plota,
       scale=0.73,width=6.5,height=5.5,units="in",dpi=740,bg="white")

n5plota=equalwage_rep_boxplots(data=longdf_avg,contvar="abs",catvar="mechanism",
                               aggcodes=c(57),
                               aggcode_strs = c("By NAICS-5"),
                               variable="both",state="all",add.theme=NULL,
                               add.legend.theme=ggplot2::theme(legend.position="none"),
                               colpalette=colpalette,basesize=basesize,
                               facet.scales="free",override.title="By NAICS-5")
ggsave(paste0(basepath,"/R_tables_plots/",comp.suffix,"/bothstate_abs_naics5_reps.pdf"),
       n5plota,
       scale=0.73,width=6.5,height=5.5,units="in",dpi=740,bg="white")

stplota=equalwage_rep_boxplots(data=longdf,contvar="abs",catvar="mechanism",
                               aggcodes=c(51),
                               aggcode_strs = c("State Total"),
                               variable="both",state="all",add.theme=NULL,
                               add.legend.theme=ggplot2::theme(legend.position="none"),
                               colpalette=colpalette,basesize=basesize,
                               facet.scales="free",override.title="State Total")
ggsave(paste0(basepath,"/R_tables_plots/",comp.suffix,"/bothstate_abs_statetotal_reps.pdf"),
       stplota,
       scale=0.73,width=6.5,height=5.5,units="in",dpi=740,bg="white")


equalwage_rep_boxplots(data=longdf,contvar="dif",catvar="mechanism",
                       aggcodes=c(51,57,71,77),
                       aggcode_strs = c("State Totals","By NAICS-5","By County","By County X NAICS-5"),
                       variable="both",state="ri",add.theme=NULL,
                       add.legend.theme=ggplot2::theme(legend.position="none"),
                       colpalette=colpalette,basesize=basesize,
                       facet.scales="free")

equalwage_rep_boxplots(data=longdf,contvar="rdif",catvar="mechanism",
                       aggcodes=c(51,57,71,77),
                       aggcode_strs = c("State Totals","By NAICS-5","By County","By County X NAICS-5"),
                       variable="both",state="nj",add.theme=NULL,
                       add.legend.theme=ggplot2::theme(legend.position="none"),
                       colpalette=colpalette,basesize=basesize,
                       facet.scales="free")

equalwage_rep_boxplots(data=nestdf,contvar="rdif",catvar="mechanism",
                       aggcodes=c(51,57,71,77),
                       aggcode_strs = c("State Totals","By NAICS-5","By County","By County X NAICS-5"),
                       variable="both",state="nj",add.theme=NULL,#add.legend.theme=ggplot2::theme(legend.position="none"),
                       colpalette=colpalette,basesize=basesize,
                       facet.scales="free",by_numestab=T)
####################################



