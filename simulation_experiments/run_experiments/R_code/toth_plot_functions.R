## Code from Dr. Daniell Toth, Bureau of Labor Statistics
## Adapted by Kaitlyn Webb, Penn State University


##functions starting with alt have been altered to fit the data structure more similiar to Kaitlyn's similation results

#========================================================================================
#                                    Functions for doing plots
#========================================================================================

# Agraph is a function for scatter plot on y=x axis
# dat is compt1 or compt2
# confdat is confidential or original data
# sandat is sanitized or protected data
# aggcode is character string giving agg level code ie "57"
#
#  100*p is percent of difference for blue bands
# transform : do log transform of data or not
# logshift is additive term to force values to be positive before log-transform
# san lab is the label for the sanitized data and origlab is label for confidential/original data
#       (if transform==T, "log" will be added to the label)
# if suppressionflag==TRUE, then column 'dflg' in confdat is used to change color and size of points
#       corresponding to those which would be suppressed in the original data
altAgraph <- function(confdat,sandat,aggcode,confcolname="month1", p=.03, transform=TRUE, line=TRUE,
                   logshift=2, sanlab="sanitized",origlab="original",
                   suppressionflag=F,
                   plotcolors=list(upper=list(border="lightblue",fill="lightblue"),
                                   lower=list(border="lightblue",fill="lightblue"),
                                   suppressed="red")){

  confdat <- confdat[confdat$agglvl_code==aggcode,]
  sandat <- sandat[sandat$agglvl_code==aggcode,]

  if(transform==TRUE){
    x <- log(confdat[, grepl(confcolname,colnames(confdata))]+logshift) # original
    y <- log(sandat[, grepl(confcolname,colnames(sandata))]+logshift)  # protected
  }
  else{
    x <- confdat[, grepl(confcolname,colnames(confdata))] # original
    y <- sandat[, grepl(confcolname,colnames(sandata))]  # protected
  }

  if(transform==TRUE){
    # y=x line
    plot((min(x)-1):max(x), (min(x)-1):max(x), type="l",
         ylab=ifelse(grepl("log ",sanlab,ignore.case=T),sanlab,paste("log",sanlab)),
         xlab=ifelse(grepl("log ",origlab,ignore.case=T),origlab,paste("log",origlab)))
    }else{
      plot((min(x)-1):max(x), (min(x)-1):max(x), type="l", ylab=sanlab, xlab=origlab)
    }
  # upper bound
  polygon(x=c(0, x[order(x)], max(x)), y=c(0, x[order(x)]+x[order(x)]*p, max(x)),
          col=(plotcolors$upper)$fill, border=(plotcolors$upper)$border)
  #lower bound
  polygon(x=c(0, x[order(x)], max(x)), y=c(0, x[order(x)]-x[order(x)]*p, max(x)),
          col=(plotcolors$lower)$fill, border=(plotcolors$lower)$border)
  points(x, y, pch=20) # original vs protected points
  if(suppressionflag==T){
  points(confdat[which(confdat$dflg=="Y"), grepl(confcolname,colnames(confdat))],
         sandat[which(confdat$dflg=="Y"), grepl(confcolname,colnames(sandat))],
         pch=4, col=plotcolors$suppressed)
  }
  if(line==TRUE){abline(a=0, b=1)}

} #end of Agraph


###original code
Agraph <- function(dat, aggcode, p=.03, transform=TRUE, line=TRUE){

  dat <- dat[dat$ag_code==aggcode,]

  if(transform==TRUE){
    x <- log(dat[, "M3"]+2) # original
    y <- log(dat[, "M3_p"]+2)  # protected
  }
  else{
    x <- dat[, "M3"] # original
    y <- dat[, "M3_p"]  # protected
  }

  if(transform==TRUE){
    plot((min(x)-1):max(x), (min(x)-1):max(x), type="l", ylab="log published", xlab="log original")} # y=x line
  else{plot((min(x)-1):max(x), (min(x)-1):max(x), type="l", ylab="published", xlab="original")}
  # upper bound
  polygon(x=c(0, x[order(x)], max(x)), y=c(0, x[order(x)]+x[order(x)]*p, max(x)), col="lightblue", border="lightblue")
  #lower bound
  polygon(x=c(0, x[order(x)], max(x)), y=c(0, x[order(x)]-x[order(x)]*p, max(x)), col="lightblue",  border="lightblue")
  points(x, y, pch=20) # original vs protected points
  points(dat[which(dat$dflg=="Y"), 12], dat[which(dat$dflg=="Y"), 9], pch=4, col="red")
  if(line==TRUE){abline(a=0, b=1)}

} #end of Agraph


#code for histograms comparring outputs of methods
# compdf1 and compdf2 are datasets for comparison with either relative or absolute error metrics
# aggcode is character string giving agg level code ie "57"
# b1 and b2 are integers specifying number of breaks for histogram
# rel is relative difference or just difference
# ylim numeric vector specifying the lower and upper limits of y axis to plot
altHgraph <- function(compdf1,compdf2,aggcode,confcolname="month3",
                   b1=50, b2=50, rel=TRUE, ylim=NULL,
                   col1=rgb(1, .647, 0, .6),col2=rgb(.678, .847, .902, .6),
                   plotlabs=list(main="",xlab="Difference"){

  dat1 <- compdf1[compdf1$agglvl_code==aggcode,]
  dat2 <- compdf2[compdf2$agglvl_code==aggcode,]

  col.indic1=grepl(confcolname,colnames(dat1))
  col.indic2=grepl(confcolname,colnames(dat2))
  if(rel==TRUE){
    x1 <- dat1[, (grepl("_rel",colnames(dat1)))&(col.indic1)] # pnc
    x2 <- dat2[, (grepl("_rel",colnames(dat2)))&(col.indic2)]  # sqrt
    pltxlab=ifelse(grepl("Rel",plotlabs$xlab,ignore.case==T),plotlabs$xlab,paste("Relative",plotlabs$xtab))
  }
  else{
    x1 <- dat1[, (grepl("_abs",colnames(dat1)))&(col.indic1)] # pnc
    x2 <- dat2[, (grepl("_abs",colnames(dat2)))&(col.indic2)]  # sqrt
    pltxlab=plotlabs$xlab
  }

  hist(x1, breaks = b1,  col=col1, main=plotlabs$main, xlab=pltxlab,
       xlim=c(min(x1, x2)-.2*abs(min(x1, x2)), max(x1, x2)+.2*abs(max(x1, x2))), ylim=ylim)
  hist(x2, breaks = b2,  col=col2, add=TRUE)

} #end Hgraph


## Original code
#code for histograms comparring outputs of methods
# aggcode is character string giving agg level code ie "57"
# b1 and b2 are integers specifying number of breaks for histogram
# rel is relative difference or just difference
# ylim numeric vector specifying the lower and upper limits of y axis to plot
Hgraph <- function(aggcode, b1=50, b2=50, rel=TRUE, ylim=NULL){

  dat1 <- compt1[compt1$ag_code==aggcode,]
  dat2 <- compt2[compt2$ag_code==aggcode,]

  if(rel==TRUE){
    x1 <- dat1[, "rdM3"] # pnc
    x2 <- dat2[, "rdM3"]  # sqrt
  }
  else{
    x1 <- dat1[, "dM3"] # pnc
    x2 <- dat2[, "dM3"]  # sqrt
  }


  hist(x1, breaks = b1,  col=rgb(1, .647, 0, .6), main="", xlab="Difference",
       xlim=c(min(x1, x2)-.2*abs(min(x1, x2)), max(x1, x2)+.2*abs(max(x1, x2))), ylim=ylim)
  hist(x2, breaks = b2,  col=rgb(.678, .847, .902, .6), add=TRUE)

} #end Hgraph



# function for Boxplots
# aggcode is character string giving agg level code ie "57"
# rel = do relative difference or just difference
# pdat has column named "agglvl_code", column for method specified by methodcolname,
# absolute error (denoted by '_abs' suffix on column name) or relative error (denoted by '_rel')
altBgraph<-function(pdat,aggcode,confcolname="month3",methodcolname="method", rel=TRUE,
                    plotprops=list(lwd=2,col="magenta",xlab="Method",ylab="Absolute Error")){
   pdat<-pdat[pdat$agglvl_code==aggcode,]
   meassuffix=ifelse(rel==TRUE,"_rel","_abs")
   y=pdat[,(grepl(confcolname,colnames(pdat)))&(grepl(meassuffix,colnames(pdat)))]
   method=pdat[,grepl(methodcolname,colnames(pdat))]
   pltylab=ifelse((rel==TRUE)&(grepl("Rel",plotprops$ylab,ignore.case=T)==FALSE),
                  paste("Relative",plotprop$ylab),
                  plotprops$ylabs)
  boxplot(y~method, xlab=plotprops$xlab, ylab=pltylab)
  abline(h=0, col=plotprops$col, lwd=plotprops$lwd)
} #end of Bgraph

# function for Boxplots
# aggcode is character string giving agg level code ie "57"
# rel = do relative difference or just difference
Bgraph<-function(aggcode, rel=TRUE){

  subdat <- compt1[compt1$ag_code==aggcode,]
  subdat2 <- compt2[compt2$ag_code==aggcode,]
  pdat<-rbind(subdat, subdat2)

  if(rel==TRUE){
    boxplot(rdM3~method, data=pdat, xlab="Method", ylab="Relative Difference")
    abline(h=0, col="magenta", lwd=2)
  }
  else{
    boxplot(dM3~method, data=pdat, xlab="Method", ylab="Difference")
    abline(h=0, col="magenta", lwd=2)
  }

} #end of Bgraph
