library(ggplot2)
library(cowplot)
library(RColorBrewer)

my_folder="~/GitHub/bls_privacy_public/simulation_experiments/compare_frameworks_composition/"

########### Preprocess Data
#read in data
#tnfndf=read.csv(paste0(my_folder,"scaling_tn_fn_data_low_power.csv"))
tnfndf2=read.csv(paste0(my_folder,"scaling_tn_fn_data.csv"))
tnfndf3=read.csv(paste0(my_folder,"scaling_tn_fn_data_low_power_low_numq.csv"))

#prepare tnfndf2 to be combined with tnfndf
tnfndf2$power=round(1-tnfndf2$fn,2)
tnfndf2$significance=round(1-tnfndf2$tn,2)

#combine round values
tnfn_full=rbind(tnfndf2,tnfndf3)
#tnfn_full=rbind(tnfn_full,tnfndf3)
tnfn_full$power=round(tnfn_full$power,2)
tnfn_full$significance=round(tnfn_full$significance,2)

#pivot longer so privacy frame is a column and all variances in a column
tnfn_long=tidyr::pivot_longer(tnfn_full,cols=c("var_gdp","var_pure","var_zcdp"),
                              names_prefix="var_",values_to="var",names_to="priv_frame")
#possible variance transformations
tnfn_long$var_log=log(tnfn_long$var,base=10)
tnfn_long$var_sqrt=sqrt(tnfn_long$var)
tnfn_long$var_sqrt_log=log(tnfn_long$var_sqrt)

#format priv_frame labels
tnfn_long$priv_frame=toupper(tnfn_long$priv_frame)
tnfn_long$priv_frame[tnfn_long$priv_frame=="PURE"]="Pure DP"
tnfn_long$priv_frame[tnfn_long$priv_frame=="ZCDP"]="zCDP"


#BACK TO tnfn_full to get ratio of zCDP or pure DP over GDP
# get ratio of variance over gdp variance
tnfn_full$ratio_pure_gdp=tnfn_full$var_pure/tnfn_full$var_gdp
tnfn_full$ratio_zcdp_gdp=tnfn_full$var_zcdp/tnfn_full$var_gdp
#pivot longer so privacy frame is a column and all variances in a column
tnfn_ratio=tidyr::pivot_longer(tnfn_full[,c("tn","fn","numq","power","significance","ratio_pure_gdp","ratio_zcdp_gdp")],
                               cols=c("ratio_pure_gdp","ratio_zcdp_gdp"),
                              names_prefix="ratio_",values_to="ratio",names_to="over_gdp")

#add transformations and format over_gdp label
tnfn_ratio$over_gdp[tnfn_ratio$over_gdp=="zcdp_gdp"]="zCDP over GDP"
tnfn_ratio$over_gdp[tnfn_ratio$over_gdp=="pure_gdp"]="Pure DP over GDP"
tnfn_ratio$ratio_log=log(tnfn_ratio$ratio,base=10)
tnfn_ratio$ratio_sqrt=sqrt(tnfn_ratio$ratio)

####################################
#### Functions for Plots ###########
####################################
### Several functions share inputs as defined below:
# data: the data frame to use to plot. This should have numq column
# numq_vals: vector of the number of queries to plot from numq column in data
# power_max: maximum power in plot
# savefolder: string for folder location of 'combined_plots' and 'individual_plots' subfolders
# filesuffix: suffix on the file name for the saved plot
### All three functions save the plots, and return a list which is (in order):
## the combined plot, the data to make the plot, and the list of individual plots

## In cont_heat_map and discrete_heat_map...
# fillvar: string of the name of the column in data whose value is represented in the color of the heatmap
# filllab: string to use as label on legend of heat map
# facetvar: string, name of column in data whose categorical value is used as a facet


## makes a heatmap with continuous gradient color scale
## Additional inputs:
# symmetric: logical if color key should be symmetric about symmetric_equal
# symmetric_equal: value that indicates fillvar is equal to the GDP at the same power, significance level (used for ratio fillvars)
# symmetric_min: minimum value for the color scale
cont_heat_map=function(data,numq_vals,fillvar,filllab,facetvar,power_max=0.4,
                       symmetric=FALSE,symmetric_equal=1,symmetric_min=0,
                       savefolder=my_folder,filesuffix=".pdf"){
  nfacet=length(unique(data[,facetvar]))
  ##Heat plots
  for(nq in numq_vals){ #number of queries
    if(nq==1){
      idx=1
      heatplotlist=list()
    }
    plotdf=data[(data$numq==nq)&(data$power<power_max),]
    cnames=colnames(plotdf)
    cnames[cnames==fillvar]="fillvar"
    cnames[cnames==facetvar]="facetvar"
    colnames(plotdf)=cnames

    if(symmetric==TRUE){
      #prepare legend breaks and labels to include break at equals
      minratio_log=(min(plotdf$fillvar)%/%0.1)*0.1
      maxratio_log=(max(plotdf$fillvar)%/%0.1)*0.1
      if(minratio_log>symmetric_equal){
        minratio_log=symmetric_equal
      }
      if(is.na(symmetric_min)==TRUE){
        symmetric_min=minratio_log
      }
      legend_breaks=round(seq(symmetric_min,maxratio_log,length.out=5),2)
      if(symmetric_equal %in% legend_breaks){
        legend_labels=legend_breaks
      }else{
        legend_breaks=sort(c(legend_breaks,symmetric_equal))
        legend_labels=legend_breaks
      }
      legend_labels[length(legend_labels)]=paste(maxratio_log,"(GDP Var smaller)")

      legend_labels[1]=as.character(symmetric_min)
      legend_labels[(legend_labels==as.character(symmetric_equal))|(legend_labels==symmetric_equal)]=paste0(symmetric_equal," (GDP Var equal)")
    }
    ptitle=ifelse(nq==1,"1 Query",paste(nq," Queries"))
    q1=ggplot(plotdf,
              aes(x=power,y=significance,fill=fillvar))+
      geom_tile()+theme_bw(base_size = 8)+
      labs(x="Power",y="Significance",fill=filllab)+
      ggtitle(ptitle)
    if(symmetric==FALSE){
      q1=q1+scale_fill_gradientn(colors=brewer.pal(9,"YlOrRd"))+
      facet_grid(~facetvar)
    }else{
      q1=q1+
        scale_fill_gradient2(low="orange",mid="grey",high="blue",midpoint=symmetric_equal,#colors=brewer.pal(9,"RdYlBu"),
                             breaks=legend_breaks,
                             labels=legend_labels,
                             limits=c(min(plotdf$fillvar)-0.05,max(plotdf$fillvar)+0.05))+
        facet_grid(~facetvar)

    }
    ggsave(paste0(savefolder,"individual_plots/heatmaps_",fillvar,"_",facetvar,"_numq",nq,"_",ifelse(symmetric==TRUE,"symmetric",""),filesuffix),
           plot=q1,width=4+nfacet,height=3,units="in",dpi=500)

    heatplotlist[[idx]]=q1
    idx=idx+1
  }
  combineheatplot=plot_grid(plotlist=heatplotlist,ncol=1)
  ggsave(paste0(savefolder,"combined_plots/heatmaps_",fillvar,"_",facetvar,"_",paste0(numq_vals,collapse="_"),ifelse(symmetric==TRUE,"symmetric",""),filesuffix),
         plot=combineheatplot,width=4+nfacet,height=(2*length(numq_vals))+0.5,
         units="in",dpi=500)
  return(list(combineheatplot,plotdf,heatplotlist))
}
############################

## makes a heatmap with discrete gradient color scale (for ratios)
## Additional inputs:
# cuts: vector of values to cut fillvar into discrete sections.
# neg_cutlabs: labels for discrete sections where the other frameworks are better than the GDP
# pos_cutlabs: labels for discrete sections where the GDP is better than other frameworks
discrete_heat_map=function(data,numq_vals,fillvar,filllab,facetvar="over_gdp",
                           cuts=c(seq(0,4,0.5),12),
                           neg_cutslabs=c('<0.5(GDP Noisier)', '0.5-1'),
                           pos_cutslabs=c('1-1.5','1.5-2', '2-2.5','2.5-3','3-3.5','3.5-4','>4(GDP Less Noise)'),
                           power_max=0.4,savefolder=my_folder,filesuffix=".pdf"){
  #preprocess data
  plotdf=data[(data$numq%in%numq_vals)&(data$power<power_max),]
  cnames=colnames(plotdf)
  cnames[cnames==fillvar]="fillvar"
  cnames[cnames==facetvar]="facetvar"
  colnames(plotdf)=cnames
  nfacet=length(unique(plotdf$facetvar))

  #make discrete sections
  plotdf$discrete_ratio=cut(plotdf$fillvar, breaks=cuts,
                            labels=c(neg_cutslabs,pos_cutslabs),
                            include.lowest=TRUE)

  #get color scale

  #when other framework is better than
  nneg=length(neg_cutslabs)
  if(nneg<3){
    negcolor_pal=brewer.pal(4,'YlOrRd')
    negcolor=c(negcolor_pal[4],negcolor_pal[2])
    negcolor=negcolor[seq(1,nneg)]
    #}else if(nneg<9){
    #  negcolor_pal=brewer.pal(1+nneg,'YlOrRd')
    #  negcolor=rev(negcolor_pal[-1])
  }else{
    negcolor=rev(brewer.pal(nneg,'YlOrRd'))
  }

  npos=length(pos_cutslabs)
  #if(npos<9){
  #  poscolor_pal=brewer.pal(1+npos,'PuBuGn')
  #  poscolor=poscolor_pal[-1]
  if(npos<3){
    poscolor_pal=brewer.pal(4,'PuBuGn')
    poscolor=poscolor_pal[1+seq(1,npos)]
  }else if(npos<10){
    poscolor=brewer.pal(npos,"PuBuGn")
  }else if(npos<13){
    greypal=rev(brewer.pal(6,"Greys"))
    poscolor=c(brewer.pal(9,"PuBuGn"),
               rev(greypal[1+seq(1,npos-9)]))
  }else{
    poscolor=c(brewer.pal(9,"PuBuGn"),
               rev(brewer.pal(npos-9,"Greys")))
  }
  cpalette=c(negcolor,poscolor)
  discrete_labs=c(neg_cutslabs,pos_cutslabs)

  for(nq in numq_vals){ #number of queries
    if(nq==1){ #initialize
      idx=1
      ratioplotlist=list()
    }
    ptitle=ifelse(nq==1,"1 Query",paste(nq," Queries"))

    subplotdf=plotdf[(plotdf$numq==nq),]
    unique_scales=unique(subplotdf$discrete_ratio)
    plt_cpal=cpalette[discrete_labs %in% unique_scales]
    plt_labs=discrete_labs[discrete_labs %in% unique_scales]

    #plot
    q1=ggplot(subplotdf,
              aes(x=power,y=significance,fill=discrete_ratio))+
      geom_tile()+theme_minimal(base_size = 10)+
      labs(x="Power",y="Significance",fill=filllab)+
      ggtitle(ptitle)+
      scale_fill_manual(values=plt_cpal,
                        labels=plt_labs)+
      facet_grid(~facetvar)
    ggsave(paste0(savefolder,"individual_plots/heatmaps_discrete_",fillvar,"_",facetvar,"_numq",nq,filesuffix),
           plot=q1,width=4+nfacet,height=3.5,units="in",dpi=500)
    if(idx==1){
      leg=cowplot::get_legend(q1)
    }
    q1=q1+theme(legend.position = "none")
    ratioplotlist[[idx]]=q1
    idx=idx+1
  }
  combineratioplot=plot_grid(plotlist=ratioplotlist,ncol=1)
  combineratioplot=plot_grid(combineratioplot,leg,rel_widths = c(1,0.3),ncol=2)

  ggsave(paste0(savefolder,"combined_plots/heatmaps_discrete_",fillvar,"_",facetvar,"_",paste0(numq_vals,collapse="_"),filesuffix),
         plot=combineratioplot,width=5.5+nfacet,height=1+(2*length(numq_vals)),units="in",dpi=500)
  return(list(combineratioplot,plotdf,ratioplotlist))
}
###################################

## line plot over power by significance and number of queries
## Additional inputs:
# signif_list: vector of alphas to plot from significance column of data
# yvar, ylab: string column name of y variable and string for y-axis label respectively
# colvar, collab: string of column name of colors of lines and label for legend respectively
# logscale: logical to indicate is the y-axis should have a log10 transformation
power_line_plot=function(data,signif_list,numq_vals,
                         yvar="var",ylab="Variance",colvar="priv_frame",collab="",
                         logscale=TRUE,max_power=0.5,savefolder=my_folder,filesuffix=".pdf"){
  plotdf=data[(data$numq%in%numq_vals)&(data$power<max_power),]
  cnames=colnames(plotdf)
  cnames[cnames==yvar]="yvar"
  cnames[cnames==colvar]="colvar"
  colnames(plotdf)=cnames
  plotdf$significance=round(plotdf$significance,2)

  for(nq in numq_vals){ #for number of queries
    if(nq==query_list[1]){
      masterplotter=list() #initialize master plot list
      masteridx=1
    }
    ptitle=ifelse(nq==1,"1 Query",paste(nq," Queries"))
    for(alpha in signif_list){ #for each alpha
      if(alpha==signif_list[1]){ #initialize numq==nq plotlist
        idx=1
        plotlist=list()
      }
      subplotdf=plotdf[(plotdf$significance==alpha)&(plotdf$numq==nq),]
      temp=ggplot(subplotdf,aes(x=power,y=yvar,color=colvar))+
        geom_smooth(se=F)#geom_point()
      if(logscale==TRUE){
        temp=temp+scale_y_log10()
      }
      temp=temp+
        theme_bw(base_size=8)+
        scale_color_brewer(palette="Dark2")+
        labs(x="Power",y=ylab,color=collab)+
        ggtitle(paste0("Significance=",round(alpha,2)))
      if(collab==""){
        temp=temp+theme(legend.title=element_blank())
      }
      pltleg=cowplot::get_legend(temp)
      temp=temp+theme(legend.position = "none")
      plotlist[[idx]]=temp

      temp=temp+ggtitle(paste0("Significance=",round(alpha,2),", ",ptitle))+
        theme(plot.title = element_text(size=9))
      masterplotter[[masteridx]]=temp
      idx=idx+1
      masteridx=masteridx+1
    }
    combineline=plot_grid(plot_grid(plotlist = plotlist,ncol=3),pltleg,ncol=2,rel_widths = c(1,0.25))
    ggsave(paste0(savefolder,"individual_plots/",yvar,"_",colvar,"_over_power_numq",nq,"_",paste0("0p",round(signif_list,2)*100,collapse="_"),filesuffix),
           plot=combineline,width=0.5+(2*length(signif_list)),height=2.5,units="in",dpi=500)
  }
  fullcombine=plot_grid(plot_grid(plotlist=masterplotter,ncol=3,rel_widths=c(1,1,1)),pltleg,ncol=2,rel_widths = c(1.5,0.25))
  ggsave(paste0(savefolder,"combined_plots/",yvar,"_",colvar,"_over_power_numq",paste0(numq_vals,collapse="_"),"_",paste0("0p",round(signif_list,2)*100,collapse="_"),filesuffix),
         plot=fullcombine,width=1.5+(2.5*length(signif_list)),height=1.5+(2.5*length(numq_vals)),
         units="in",dpi=500)
  return(list(fullcombine,plotdf,masterplotter))
}

#############################

##Heat plots of log standard deviation
temp1=cont_heat_map(tnfn_long,numq_vals=c(1,2,5),
                    fillvar="var_sqrt_log",filllab="log10 Standard Deviation",
                    facetvar="priv_frame",
                    power_max=0.4,
                    symmetric=FALSE,symmetric_equal=1,symmetric_min=0,savefolder=my_folder)


## variance ratio without symmetric color scale
temp1=cont_heat_map(tnfn_ratio,numq_vals=c(1,2,5),
                    fillvar="ratio",filllab="Variance Ratio",
                    facetvar="over_gdp",
                    power_max=0.4,
                    symmetric=FALSE,symmetric_equal=1,symmetric_min=0,savefolder=my_folder)

##Variance ratio with symmetric color scale
temp1=cont_heat_map(tnfn_ratio,numq_vals=c(1,2,5),
                    fillvar="ratio",filllab="Variance Ratio",
                    facetvar="over_gdp",
                    power_max=0.4,
                    symmetric=TRUE,symmetric_equal=1,symmetric_min=0,savefolder=my_folder)

#same as above but with log10 ratio
temp1=cont_heat_map(tnfn_ratio,numq_vals=c(1,2,5),
                    fillvar="ratio_log",filllab="log10 Variance Ratio",
                    facetvar="over_gdp",
                    power_max=0.4,
                    symmetric=FALSE,symmetric_equal=0,symmetric_min=NA,savefolder=my_folder)

temp1=cont_heat_map(tnfn_ratio,numq_vals=c(1,2,5),
                    fillvar="ratio_log",filllab="log10 Variance Ratio",
                    facetvar="over_gdp",
                    power_max=0.4,
                    symmetric=TRUE,symmetric_equal=0,symmetric_min=NA,savefolder=my_folder)



## discrete heatmaps with 0.5 length discrete scales, numq 1,2,3
temp1=discrete_heat_map(data=tnfn_ratio,numq_vals=c(1,2,3),
                        fillvar="ratio",filllab="Variance Ratio",
                        facetvar="over_gdp",cuts=c(seq(0,4,0.5),12),
                        neg_cutslabs=c('<0.5 (GDP Var larger)', '0.5-1'),
                        pos_cutslabs=c('1-1.5','1.5-2', '2-2.5','2.5-3','3-3.5','3.5-4','>4 (GDP Var smaller)'),
                        power_max=0.4,savefolder=my_folder,filesuffix=".pdf")
#same as above but with 1,3,5 number of queries
temp1=discrete_heat_map(data=tnfn_ratio,numq_vals=c(1,2,3),
                        fillvar="ratio",filllab="Variance Ratio",
                        facetvar="over_gdp",cuts=c(seq(0,4,1/3),12),
                        neg_cutslabs=c('<0.33 (GDP Var larger)', '0.33-66','0.66-1'),
                        pos_cutslabs=c('1-1.33','1.33-1.66','1.66-2', '2-2.33','2.33-2.66','2.66-3','3-3.33','3.33-3.66','3.66-4','>4 (GDP Var smaller)'),
                        power_max=0.4,savefolder=my_folder,filesuffix="_finer.pdf")

#same as two above but with 1/3 scale for discrete sections
temp1=discrete_heat_map(data=tnfn_ratio,numq_vals=c(1,3,5),
                        fillvar="ratio",filllab="Variance Ratio",
                        facetvar="over_gdp",cuts=c(seq(0,4,0.5),12),
                        neg_cutslabs=c('<0.5(GDP Noiser)', '0.5-1'),
                        pos_cutslabs=c('1-1.5','1.5-2', '2-2.5','2.5-3','3-3.5','3.5-4','>4(GDP Less Noise)'),
                        power_max=0.4,savefolder=my_folder,filesuffix="_bs10.pdf")
temp1=discrete_heat_map(data=tnfn_ratio,numq_vals=c(1,3,5),
                        fillvar="ratio",filllab="Variance Ratio",
                        facetvar="over_gdp",cuts=c(seq(0,4,1/3),12),
                        neg_cutslabs=c('<0.33 (GDP Var larger)', '0.33-66','0.66-1'),
                        pos_cutslabs=c('1-1.33','1.33-1.66','1.66-2', '2-2.33','2.33-2.66','2.66-3','3-3.33','3.33-3.66','3.66-4','>4 (GDP Var smaller)'),
                        power_max=0.4,savefolder=my_folder,filesuffix="_finer.pdf")

#line plot of log10 var over power by significance level and number of queries
temp=power_line_plot(data=tnfn_long,signif_list=c(0.01,0.05,0.1),
                     numq_vals=c(1,2,3,5),yvar="var",ylab="Variance",colvar="priv_frame",collab="",
                         logscale=TRUE,savefolder=my_folder,filesuffix=".pdf")

