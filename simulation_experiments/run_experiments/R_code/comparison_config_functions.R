comparison_stems=function(folderin=paste0(basepath,"/compare_data"),
                          state.prefixes=NULL,
                          year=2016,
                          check.all.states=T){

  lf=unique(list.files(folderin))
  if(is.null(state.prefixes)==TRUE){
    state.prefixes=unique(gsub(paste0("_",year,"_1_[^!]*.csv"),"",lf))
    state.prefixes=paste0(state.prefixes,"_",year,"_1")
  }
  if(length(state.prefixes)>1){
    sc.fnames=lf[grepl(state.prefixes[1],lf)&grepl("agg_prep.csv|agg.csv",lf)]
    compare.suffixes=gsub("_agg_prep.csv|_agg.csv","",gsub(paste0(state.prefixes[1],"_"),"",sc.fnames))
    if((check.all.states==TRUE)&(length(state.prefixes)>1)){
      potentialnames=c(sapply(state.prefixes,function(sc)paste0(sc,"_",compare.suffixes,"_agg_prep.csv")),
                              sapply(state.prefixes,function(sc)paste0(sc,"_",compare.suffixes,"_agg.csv")))
      match.idx=(potentialnames%in% lf)
      if(sum(match.idx)!=length(potentialnames)){

        warning(paste("Some of the compare.suffixes are not present for all state prefixes. Following potential files not present: ",paste0(potentialnames[!match.idx],collapse=", ")))
      }
    }
  }else{
    sc.fnames=lf[grepl(state.prefixes,lf)&grepl("agg_prep.csv|agg.csv",lf)]
    compare.suffixes=gsub("_agg_prep.csv|_agg.csv","",gsub(paste0(state.prefixes[1],"_"),"",sc.fnames))
  }
  return(list("compare.suffixes"=compare.suffixes,"state.prefixes"=state.prefixes))
}

#extracts the config file names from column names
config_extractor=function(cnames,order="byacct"){
  meas.nm=cnames[grepl("wages",cnames)]
  nms=unique(gsub("_rel|_abs|_dif|_rdif","",gsub("[^!]*_qtrly_wages_","",meas.nm)))

  # reorder to blsvals_sqrt, blsvals_clip (i.e. the base values),
  # then be pairs of *_sqrt, *_clip where * is the stem of a config name,
  # followed by any config names without matching _sqrt or _clip version
  # i.e. if nms=c(blsvals_clip,blsvals_clip2,blsvals_sqrt),then ordered.nms=c(blsval_sqrt,blsval_clip,blsvals_clip2)
  stems=unique(gsub("_clip$|_sqrt$","",nms)) #stems that end with _clip or _sqrt
  if(order=="byacct"){
    ordered.nms=c("blsvals_sqrt",paste0(stems,"_sqrt"),"blsvals_clip",paste0(stems,"_clip"))
  }else{
  ordered.nms=c("blsvals_sqrt","blsvals_clip") #start ordered vector with base configs
  if(length(stems)>1){ #if more than 1 stem
    #indicator that there is *_sqrt and *_clip in nms
    stem.both=sapply(stems,function(x)paste0(x,c("_sqrt","_clip")),simplify=T)
    #add pairs of *_sqrt, *_clip to ordered.nms
    ordered.nms=c(ordered.nms,stem.both)
  }
  }
  #unique returns in the order that they first appear in the vector
  ordered.nms=unique(c(ordered.nms,nms))
  return(ordered.nms[ordered.nms%in%nms]) #return only ones that were in nms (fail safe in case something weird)
}
