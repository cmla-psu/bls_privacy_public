library(tidyverse)
inputpath = paste0(getwd(),"/PythonMicrodataSubsets")
outputpath = paste0(getwd(),"/input")

# Crosswalk file from 
## https://www.bls.gov/cew/classifications/industry/industry-supersectors.htm 
crosswalk_file = paste0(getwd(),"/DataIn/high_level_crosswalk.csv")


# ## OPTIONAL: Investigate first two microdata subset files
# estdf1= read.csv(paste0(inputpath,"/SynMicrodata1.csv"))
# estdf2= read.csv(paste0(inputpath,"/SynMicrodata2.csv"))
# head(estdf1)
# 
# # No NA values in data subset 1. Summary statistics seem reasonable
# sum(is.na(estdf1))
# summary(estdf1)
# summary(estdf2)



#BLS crosswalk file takes NAICS sectors (2-digit naics) and converts these to thier
# supersector (and domain values).
# This function processes the crosswalk file into a usable format
get_xwalk_naics = function(NAICScrosswalkfile=crosswalk_file){
  xwalk = read.csv(NAICScrosswalkfile) #read BLS cross walk
  xwalk = xwalk%>%
    #remove any nonnumeric characters from supersector & sector (w/ exception of '-' in sector)
    mutate("super_sector"=gsub("[^0-9]","",super_sector),
           "naics_sector"=gsub("[^0-9-]","",naics_sector))
  
  # 3 sector values have "-" in them, specifically 31-33, 44-45, and 48-49
  # we want each value in the range to have its own row
  expandrows = xwalk%>% 
    filter(grepl("-",naics_sector))%>%# filter to rows with "-" in sector
    slice(rep(1:3,times=c(3,2,2))) # split row1 3 times, split rows 2 and 3 each 2 times
  
  
  expandrows$naics_sector =as.character(c(31,32,33,44,45,48,49)) #manually set sector values

  xwalk = bind_rows(xwalk,expandrows) %>% #bind the expanded rows to original xwalk
    filter(!grepl("-",naics_sector)) #remove rows wit "-" in sector
  xwalk
}

get_xwalk_naics()
cut1=function(naicscode){ #function cuts 1 character off end of string
  str_sub(naicscode,start=1,end=-2)
}

# function takes filename of mircodata subset and fills in necessary columns
# need columns
## year, qtr, state, cnty, own, 
## naics, naics3,naics4, naics5, supersector,sector,
## m1emp, m2emp, m3emp, wage, 
## can_agg, rectype, primary_key
#### For all rows, own=5, can_agg="Y", rectype="C", year=2016,qtr=1
#### m1emp, m2emp, m3emp, wage, state, cnty are from Python Microdata Output
#### naics is naics6 code,naics3 to 5 can be found with naics6,
#### sector is 2-digits of naics, supersector comes from BLS crosswalk
postprocess_est_microdata_split = function(filename,yr=2016,qtr=1,xwalk=crosswalk){
  print(filename)
  estdf = read.csv(filename) #read in python subdata
  estdf = estdf%>% #get industry columns
    mutate("naics5"=cut1(naics6),"naics4"=cut1(naics5), #cut 1 character at a time
           "naics3"=cut1(naics4),"naics_sector"=cut1(naics3),
           "naics"=naics6)%>%
    left_join(xwalk%>%select(naics_sector,super_sector),by="naics_sector")%>% #get supersector
    mutate_all(as.numeric) # make all numeric columns
  estdf = estdf %>% #add constant valued columns
    mutate("year"=yr,"qtr"=qtr,"own"=5,"can_agg"='Y',"rectype"='C')%>%#,
          # "super_sector"=supersector,"naics_sector"=sector)%>%
    select(year,qtr,state,cnty,own,#select necessary columns
           naics,naics3,naics4,naics5,naics_sector,super_sector,
           m1emp,m2emp,m3emp,wage,can_agg,rectype)
  filenum = gsub(".csv","",unlist(str_split(filename,"SynMicrodata"))[2])
  estdf$filenumber = filenum
  estdf
}

#crosswalk = get_crosswalk_naics()
#head(postprocess_est_microdata_split(filename=paste0(inputpath,"/SynMicrodata1.csv")))


# reads in all files with 'SynMicrodata' in their name that are in inputpath folder
# postprocesses all of them and then combines into one data set
read_combine_micro = function(filebasename = "SynMicrodata",folder=inputpath){
  #get list of files in folder that have filebasename in their name  
  filenames = list.files(folder,pattern=filebasename,full.names=T)
    
    crosswalk = get_xwalk_naics() #get BLS crosswalk
    combineddf=bind_rows(
      parallel::mclapply( #for each filename (parallel processing)
        filenames,function(x) #post-process that file
          postprocess_est_microdata_split(x,yr=2016,qtr=1,xwalk=crosswalk)
        )
      ) #stack post-processed files
    combineddf$primary_key=seq(1,length(combineddf$naics)) #add primary key identifier
    combineddf
}

#Take the full data (inputed as microdata) and split it by state. 
# Each state is saved in a folder in outdir that has the naming convention of
# lowercase state abbreviation then FIPScode.The csv subset of data is saved as 
# [state abbrev][FIPS code]_qdb_2016_1.csv (for example ak02/ak02_qdb_2016_1.csv)
split_by_state = function(microdf,outdir=outputpath){
 #read in csv with fips codes and state names
  fipsstate = read.delim(paste0(getwd(),"/FIPSstatecodename.txt"),sep=",")
  fipsstate = fipsstate %>%
    mutate("FIPScodeFileFormat"= #make fips code a two-digit (leading zeros) string
             if_else(FIPScode>9,as.character(FIPScode),paste0("0",FIPScode)),
           #abbrevfips=lower case state abbreviation followed by FIPScodeFileFormat (ex. al01)
           "abbrevfips"= paste0(tolower(usdata::state2abbr(name)),FIPScodeFileFormat))
  for(i in seq(1,nrow(fipsstate))){ #for each fips code
    subname = fipsstate$abbrevfips[i] 
    fname=paste0(outdir,"/",subname,"/",subname,"_qdb_2016_1.csv") #output file name
    print(subname)
    subdata = microdf%>%filter(state==fipsstate$FIPScode[i]) #subset data into state
    if(nrow(subdata)>0){ #if folder for the subset of data does not exist, make it
      if(!file.exists(paste0(outdir,"/",subname))){
        dir.create(paste0(outdir,"/",subname))
      }
      write.csv(subdata,file=fname) #save data subset as specified file name
    }
  }
}

#split_by_state(microdf)

microdf = read_combine_micro()

## OPTIONAL: Explore combined dataset
#head(microdf)
#glimpse(microdf)
#sum(is.na(microdf))

if(!file.exists(outputpath)){
  dir.create(outputpath)
}
# Save combined dataset
save(microdf,file=paste0(outputpath,"/MicrodataFinal.RData"))
write.csv(microdf,file=paste0(outputpath,"/MicrodataFinal.csv"))

split_by_state(microdf)



