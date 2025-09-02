## FROM dplyr NEED group_by(), summarize(), n(), if_else(), left_join(), bind_cols(), bind_rows()
## FROM knitr NEED kable(),
## FROM kableExtra NEED add_header_above(), and pack_rows()
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

nj.state.prefixes=c("nj34_qbp_2016_1")
ri.state.prefixes=c("ri44_qbp_2016_1")
state.prefixes=c(nj.state.prefixes,ri.state.prefixes)
#state.prefixes=c("nj34_qbp_2016_1")
namer.csv.fname="table_config_names.csv"
mudigits=1
removeaggcodes=c(50,70)
queries.in.create=c("State Total","State NAICS 5-digit",
                    "County Total","County NAICS 5-digit")
n_estab_stats=NULL
split.wage.emp=T
split.abs.rel=T

for(state.prefix in state.prefixes){
  tabnamer=table_config_namer(folderin=folderin,
                              folderout=folderRtabs,
                              state.prefix=state.prefix,
                              csv.fname=namer.csv.fname,
                              overwrite=T,
                              functionfilepath=NULL,
                              mudigits=mudigits)


  summary.agglvls=get_aggcode_info(aggcodes=NULL,
                                   st.data=NULL,
                                   folderin=folderin,
                                   compare.suffix="budgets",
                                   state.prefix=state.prefix,
                                   write.tex=paste0(folderRtabs,"/budgets_agg_estnum_summary_",state.prefix,".tex"),
                                   overwrite=T,empwage.summarystats=T,one.table=T,quietly=T)
  print("summary budgets done")
}

original_distributions_plot(st.data=NULL,
                            folderin=paste0(basepath,"/EstablishmentLevelData"),
                            folderout=paste0(basepath,"/R_tables_plots"),
                            state.prefixes=state.prefixes,
                            add.theme=ggplot2::scale_y_continuous(trans="pseudo_log"),
                            add.legend.theme=NULL,
                            basesize=8,nbins=49,
                            state.colpalette=c("grey45","darkblue"),
                            fname="original_distribution_plots_pseudolog.png",
                            plot.scale=0.83,plot.width=9,plot.height=3.5)

original_distributions_plot(st.data=NULL,
                            folderin=paste0(basepath,"/EstablishmentLevelData"),
                            folderout=paste0(basepath,"/R_tables_plots"),
                            state.prefixes=nj.state.prefixes,
                            add.theme=ggplot2::scale_y_continuous(trans="pseudo_log"),
                            add.legend.theme=ggplot2::theme(legend.position = "none"),
                            basesize=8,nbins=49,
                            state.colpalette=c("grey45"),
                            fname="nj34_original_distribution_plots_pseudolog.png",
                            plot.scale=0.83,plot.width=9,plot.height=3.5)

original_distributions_plot(st.data=NULL,
                            folderin=paste0(basepath,"/EstablishmentLevelData"),
                            folderout=paste0(basepath,"/R_tables_plots"),
                            state.prefixes=state.prefixes,
                            add.theme=NULL,
                            add.legend.theme=NULL,
                            basesize=8,nbins=49,
                            state.colpalette=c("grey45","darkblue"),
                            fname="original_distribution_plots.png",
                            plot.scale=0.83,plot.width=9,plot.height=3.5)

original_distributions_plot(st.data=NULL,
                            folderin=paste0(basepath,"/EstablishmentLevelData"),
                            folderout=paste0(basepath,"/R_tables_plots"),
                            state.prefixes=state.prefixes,
                            add.theme=ggplot2::scale_y_continuous(trans="pseudo_log"),
                            add.legend.theme=NULL,
                            basesize=8,nbins=15,
                            state.colpalette=c("grey45","darkblue"),
                            fname="original_distribution_plots_nozeros_pseudolog.png",
                            plot.scale=0.83,plot.width=9,plot.height=3.5,exclude.zeros=T)

###############
tabnamer=table_config_namer(folderin=folderin,
                            folderout=folderRtabs,
                            state.prefix=state.prefixes[1],
                            csv.fname=namer.csv.fname,
                            overwrite=T,
                            functionfilepath=NULL,
                            mudigits=mudigits)

nj.summary.agglvls=get_aggcode_info(aggcodes=NULL,
                                    st.data=NULL,
                                    folderin=folderin,
                                    compare.suffix="blsvals_accttype_clipprob",
                                    state.prefix="nj34_qbp_2016_1",
                                    write.tex=paste0(folderRtabs,"/agg_estnum_summary_nj34_qbp_2016_1.tex"),overwrite=T)
ri.summary.agglvls=get_aggcode_info(aggcodes=NULL,
                                    st.data=NULL,
                                    folderin=folderin,
                                    compare.suffix="blsvals_accttype_clipprob",
                                    state.prefix="ri44_qbp_2016_1",
                                    write.tex=paste0(folderRtabs,"/agg_estnum_summary_ri44_qbp_2016_1.tex"),overwrite=T)

