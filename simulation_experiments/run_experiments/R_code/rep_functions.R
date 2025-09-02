between_rep_sd=function(data){
  data$agg_groups=paste0(data$agglvl_code,"_",data$area_fips,"_",data$industry_code)
  grouped_data=dplyr::group_by(data, agglvl_code, agg_groups,mech,variable)
  summarize_data=dplyr::summarise(grouped_data,"between_rep_sd"=sqrt(var(value,na.rm=T)),
                                  "bewteen_rep_median"=median(value,na.rm=T))
  grouped_summary=dplyr::group_by(dplyr::ungroup(summarize_data),agglvl_code, mech, variable)
  between_rep_sd_dist=dplyr::summarise(grouped_summary,
                                       "min_between_rep_sd"=min(between_rep_sd),
                                       "mean_between_rep_sd"=mean(between_rep_sd),
                                       "median_between_rep_sd"=median(between_rep_sd),
                                       "max_between_rep_sd"=max(between_rep_sd),
                                       "min_between_rep_median"=min(between_rep_median),
                                       "mean_between_rep_median"=mean(between_rep_median),
                                       "median_between_rep_median"=median(between_rep_median),
                                       "max_between_rep_median"=max(between_rep_median))
  return(list(between_rep_sd_dist,summarize_data))
}

within_rep_sd=function(data){
  #data$agg_groups=paste0(data$agglvl_code,"_",data$area_fips,"_",data$industy_code)
  grouped_data=dplyr::group_by(data, agglvl_code,mech,variable,rep)
  summarize_data=dplyr::summarise(grouped_data,"within_rep_sd"=sqrt(var(value,na.rm=T)),
                                  "within_rep_median"=median(value,na.rm=T))
  grouped_summary=dplyr::group_by(dplyr::ungroup(summarize_data),agglvl_code, mech, variable)
  within_rep_sd_dist=dplyr::summarise(grouped_summary,
                                       "min_within_rep_sd"=min(within_rep_sd),
                                       "mean_within_rep_sd"=mean(within_rep_sd),
                                       "median_within_rep_sd"=median(within_rep_sd),
                                       "max_within_rep_sd"=max(within_rep_sd),
                                       "min_within_rep_median"=min(within_rep_median),
                                       "mean_within_rep_median"=mean(within_rep_median),
                                       "median_within_rep_median"=median(within_rep_median),
                                       "max_within_rep_median"=max(within_rep_median))

  return(list(within_rep_sd_dist,summarize_data))
}

median_all=function(data){
  grouped_data=dplyr::group_by(data, agglvl_code,mech,variable,agglvl_codelab)
  summarize_data=dplyr::summarise(grouped_data,"median_rdif"=median(value))
  return(summaize_data)
}
