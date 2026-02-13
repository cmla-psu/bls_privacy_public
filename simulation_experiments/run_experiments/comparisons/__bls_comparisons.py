from readers import QCEWAggReader
from typing import List
import pandas as pd
import numpy as np
import csv
import os

EMP1 = QCEWAggReader.EMP1
EMP2 = QCEWAggReader.EMP2
EMP3 = QCEWAggReader.EMP3
WAGES = QCEWAggReader.WAGES


def compare_qcew_aggs(ground_truth: QCEWAggReader, others: List[QCEWAggReader], names: List[str]):
    assert len(others) == len(names), "a name must be given for each datasset for comparison"
    headcolumns = [QCEWAggReader.YEAR,
                   QCEWAggReader.QTR,
                   QCEWAggReader.AGGLVL_CODE,
                   QCEWAggReader.OWN_CODE,
                   QCEWAggReader.AREA_FIPS,
                   QCEWAggReader.INDUSTRY_CODE,
                   QCEWAggReader.SIZE_CODE,
                   QCEWAggReader.DISCLOSURE_CODE,
                   QCEWAggReader.QTRLY_ESTABS]
    mycompare = ground_truth.df[headcolumns].copy() #copy public attributes and codes
    #add ground truth employment and wage values
    mycompare[EMP1] = ground_truth.df[EMP1] 
    mycompare[EMP2] = ground_truth.df[EMP2]
    mycompare[EMP3] = ground_truth.df[EMP3]
    mycompare[WAGES] = ground_truth.df[WAGES]
    error_df = pd.DataFrame() #initialize erro dataframe to
    agg_spec = {} #dictionary for what function to aggregate errors by
    for myagg, myname in zip(others, names): #for each filename in others
        if not check_compatibility(ground_truth, myagg):
            raise Exception("DataFrames don't match on rows")
        else:
            #add employment and wages from each of the datasets from others
            mycompare[f"{EMP1}_{myname}"] = myagg.df[EMP1] 
            mycompare[f"{EMP2}_{myname}"] = myagg.df[EMP2]
            mycompare[f"{EMP3}_{myname}"] = myagg.df[EMP3]
            mycompare[f"{WAGES}_{myname}"] = myagg.df[WAGES] 
            #get difference compared to the ground truth
            error_df[f"{EMP1}_{myname}_dif"] = mycompare[EMP1] - mycompare[f"{EMP1}_{myname}"]
            error_df[f"{EMP2}_{myname}_dif"] = mycompare[EMP2] - mycompare[f"{EMP2}_{myname}"]
            error_df[f"{EMP3}_{myname}_dif"] = mycompare[EMP3] - mycompare[f"{EMP3}_{myname}"]
            error_df[f"{WAGES}_{myname}_dif"] = mycompare[WAGES] - mycompare[f"{WAGES}_{myname}"]
            #get absolute error compared to the ground truth
            error_df[f"{EMP1}_{myname}_abs"] = abs(mycompare[EMP1] - mycompare[f"{EMP1}_{myname}"])
            error_df[f"{EMP2}_{myname}_abs"] = abs(mycompare[EMP2] - mycompare[f"{EMP2}_{myname}"])
            error_df[f"{EMP3}_{myname}_abs"] = abs(mycompare[EMP3] - mycompare[f"{EMP3}_{myname}"])
            error_df[f"{WAGES}_{myname}_abs"] = abs(mycompare[WAGES] - mycompare[f"{WAGES}_{myname}"])
            #get relative error scaled by ground truth
            error_df[f"{EMP1}_{myname}_rel"] = error_df[f"{EMP1}_{myname}_abs"] / np.maximum(mycompare[EMP1], 1)
            error_df[f"{EMP2}_{myname}_rel"] = error_df[f"{EMP2}_{myname}_abs"] / np.maximum(mycompare[EMP2], 1)
            error_df[f"{EMP3}_{myname}_rel"] = error_df[f"{EMP3}_{myname}_abs"] / np.maximum(mycompare[EMP3], 1)
            error_df[f"{WAGES}_{myname}_rel"] = error_df[f"{WAGES}_{myname}_abs"] / np.maximum(mycompare[WAGES], 1)
             #get relative difference scaled by ground truth
            error_df[f"{EMP1}_{myname}_rdif"] = error_df[f"{EMP1}_{myname}_dif"] / np.maximum(mycompare[EMP1], 1)
            error_df[f"{EMP2}_{myname}_rdif"] = error_df[f"{EMP2}_{myname}_dif"] / np.maximum(mycompare[EMP2], 1)
            error_df[f"{EMP3}_{myname}_rdif"] = error_df[f"{EMP3}_{myname}_dif"] / np.maximum(mycompare[EMP3], 1)
            error_df[f"{WAGES}_{myname}_rdif"] = error_df[f"{WAGES}_{myname}_dif"] / np.maximum(mycompare[WAGES], 1)
            #absolute errors will be averages, relative errors will be summarized with median 
            agg_spec[f"{EMP1}_{myname}_abs"] = ["mean"]#,"std"]
            agg_spec[f"{EMP2}_{myname}_abs"] = ["mean"]#,"std"]
            agg_spec[f"{EMP3}_{myname}_abs"] = ["mean"]#,"std"]
            agg_spec[f"{WAGES}_{myname}_abs"] = ["mean"]#,"std"]
            agg_spec[f"{EMP1}_{myname}_rel"] = "median"
            agg_spec[f"{EMP2}_{myname}_rel"] = "median"
            agg_spec[f"{EMP3}_{myname}_rel"] = "median"
            agg_spec[f"{WAGES}_{myname}_rel"] = "median"
            agg_spec[f"{EMP1}_{myname}_dif"] = ["mean"]#,"std"]
            agg_spec[f"{EMP2}_{myname}_dif"] = ["mean"]#,"std"]
            agg_spec[f"{EMP3}_{myname}_dif"] = ["mean"]#,"std"]
            agg_spec[f"{WAGES}_{myname}_dif"] = ["mean"]#,"std"]
            agg_spec[f"{EMP1}_{myname}_rdif"] = "median"
            agg_spec[f"{EMP2}_{myname}_rdif"] = "median"
            agg_spec[f"{EMP3}_{myname}_rdif"] = "median"
            agg_spec[f"{WAGES}_{myname}_rdif"] = "median"
    sorted_cols = sorted(error_df.columns) 
    mycompare[sorted_cols] = error_df[sorted_cols]
    #group the errors by aggregate level code, summarize the errors based on agg_spec specifications
    aggresult = mycompare.groupby([QCEWAggReader.AGGLVL_CODE],as_index=False).agg(agg_spec)
    
    return mycompare, aggresult


def compare_files(ground_truth: str, others: List[str], names: List[str], outfile: str, aggfile: str):
    gt = QCEWAggReader()
    gt.read(ground_truth) #read ground truth dataset
    other_readers = [QCEWAggReader() for _ in others] #read the other datasets
    [x.read(fname) for x, fname in zip(other_readers, others)] 
    (result, aggresult) = compare_qcew_aggs(gt, other_readers, names)
    result.to_csv(outfile, index=False, quoting=csv.QUOTE_NONNUMERIC)
    aggresult.to_csv(aggfile, index=False, quoting=csv.QUOTE_NONNUMERIC)


def check_compatibility(agg1: QCEWAggReader, agg2: QCEWAggReader):
    fields = [QCEWAggReader.YEAR,
              QCEWAggReader.QTR,
              QCEWAggReader.OWN_CODE,
              QCEWAggReader.AGGLVL_CODE,
              QCEWAggReader.AREA_FIPS,
              QCEWAggReader.INDUSTRY_CODE,
              ]
    match = True
    for f in fields:
        if not all(agg1.df[f] == agg2.df[f]):
            match = False
            break
    return match

def prepare_agg_file(filename,outname="count_of_negative_protected_aggregates.txt",forcePos=False,empbelow=-200,wagebelow=-100000,includebelow=True):
    data= pd.read_csv(filename)
    
    # Force monthly employment and wage values to be intergers
    data['month1_emplvl']=data['month1_emplvl'].astype(int)
    data['month2_emplvl']=data['month2_emplvl'].astype(int)
    data['month3_emplvl']=data['month3_emplvl'].astype(int)
    data["total_qtrly_wages"]=data["total_qtrly_wages"].astype(int)
    
    # subset to only the variables we added protectiong to
    sensitive_data=data[['month1_emplvl','month2_emplvl',
                         'month3_emplvl','total_qtrly_wages']]
    data_bool=sensitive_data<0 #boolean if the values are negative
    neg_conf_count=data_bool.sum(axis=0).to_list() #count how many values are
    neg_count_percent=[x/len(data.index) for x in neg_conf_count] #get list of negative percentages
    neg_count_line=", ".join(map(str,neg_count_percent)) #comma-separated string of values
    if includebelow==True:
        below_emp_bool=sensitive_data[['month1_emplvl','month2_emplvl',
                                        'month3_emplvl']]<empbelow
        below_wage_bool=sensitive_data[['total_qtrly_wages']]<wagebelow
        below_bool=pd.concat([below_emp_bool,below_wage_bool],axis=1)
        below_count=below_bool.sum(axis=0).to_list() #count how many values are
        below_count_percent=[x/len(data.index) for x in below_count] #get list of percentages below specified values
        below_count_line=", ".join(map(str,below_count_percent)) #comma-separated string of values
        rowline_count_prop=neg_count_line+", "+below_count_line
        empbstr="propb"+str(abs(empbelow))+"_emp"
        wbstr="propb"+str(abs(wagebelow))+"_wage"
        header="filename, nrows, propneg_emp1, propneg_emp2, propneg_emp3, propneg_wage, "+empbstr+"1, "+empbstr+"2, "+empbstr+"3, "+wbstr+"\n"
    else:
        rowline_count_prop=neg_count_line
        header="filename, nrows, propneg_emp1, propneg_emp2, propneg_emp3, propneg_wage\n"
    
    
    #get string to represent the row with values:
    ## filename, number of rows, prop neg emp1, prop neg emp2, 
    ##  prop neg emp3, prop neg wages,
    ## (if includebelow==TRUE) 
    ## prop below emp1, prop below emp2, prop below emp3, prop below wages
    
    rowline_datainfo=filename.split("/")[-1]+", "+str(len(data.index))
    rowline=rowline_datainfo+", "+rowline_count_prop+"\n"
    
    if not os.path.isfile(outname): #if file doesn't exist make one
        neg_count_file=open(outname,"w")
        neg_count_file.writelines(header)
        
    else:
        neg_count_file=open(outname, "a") #otherwise open existing file
    neg_count_file.writelines(rowline)
    neg_count_file.close()
    
    if(forcePos==True):
        #force negative values to be 0
        data.loc[data['total_qtrly_wages']<0,'total_qtrly_wages']=0
        data.loc[data['month1_emplvl']<0,'month1_emplvl']=0
        data.loc[data['month2_emplvl']<0,'month1_emplvl']=0
        data.loc[data['month3_emplvl']<0,'month1_emplvl']=0
    
    #save as new file
    filename_split=filename.split(".")
    newfilename=filename_split[0]+"_prep."+filename_split[1]
    data.to_csv(newfilename,sep=',',index=False)
    #return newfilename

def get_compare_file_others_input(config_list,filebasename,groundtruth=None,countnegfile=["count_of_negative_protected_aggregates.txt",False,-200,-100000,True]):
    if groundtruth is None:
        groundtruth=filebasename
    for c in config_list: #for each string in config_list
        #if there is already a prepared file, then it would be named by the below convention
        prepared_protect_agg_fname=filebasename+"__"+c+"__protected_agg_prep.csv"
        if not os.path.isfile(prepared_protect_agg_fname): #if file does not exist
            protected_agg_fname=filebasename+"__"+c+"__protected_agg.csv"
            prepare_agg_file(protected_agg_fname,countnegfile[0],countnegfile[1],countnegfile[3],countnegfile[4]) #prepare the data
        if not os.path.isfile(groundtruth+"_agg_prep.csv"): #if ground truth file not prepped
            prepare_agg_file(groundtruth+"_agg.csv",countnegfile[0],countnegfile[1],countnegfile[3],countnegfile[4]) #prepare it
    prepared_aggfile_names=[filebasename+"__"+x+"__protected_agg_prep.csv" for x in config_list] #return list of prepared file names for 'others' input in compare_files
    return prepared_aggfile_names
    

def compare_configs(compare_configs,filebase,outsuffix,countnegfile=["count_of_negative_protected_aggregates.txt",False,-200,-100000,True]):
    for fb in filebase:
        #make the comparisons for file base name and config list
        comp_st=get_compare_file_others_input(compare_configs,fb,countnegfile)
        filestem="compare_data/"+fb+"_"+outsuffix
        compare_files("aggregated_data/"+fb+"_agg_prep.csv",comp_st,
                  compare_configs,
                  filestem+".csv",filestem+"_agg.csv")
    return compare_configs

def compare_configs_accttype(outsuffix,countnegfile=["count_of_negative_protected_aggregates.txt",False,-200,-100000,True]):
    compconfigs=["blsvals_clip","blsvals_sqrt"]
    ri=get_compare_file_others_input(compconfigs,
                                     "aggregated_data/ri44_qbp_2016_1")
    nj=get_compare_file_others_input(compconfigs,
                                     "aggregated_data/nj34_qbp_2016_1")
        #make the comparisons for file base name and config list
    filestemnj="compare_data/nj34_qpb_2016_1_"+outsuffix
    compare_files("aggregated_data/nj34_qbp_2016_1_agg_prep.csv",nj,
                  compconfigs,
                  filestemnj+"_prep.csv",filestemnj+"_agg_prep.csv")
    filestemri="compare_data/ri44_qbp_2016_1_"+outsuffix
    compare_files("aggregated_data/ri44_qbp_2016_1_agg_prep.csv",ri,
                  compconfigs,
                  filestemri+".csv",filestemri+"_agg.csv")
    return compare_configs

def compare_configs_ri_nj(compconfigs,outsuffix,countnegfile=["count_of_negative_protected_aggregates.txt",False,-200,-100000,True]):
    ri=get_compare_file_others_input(compconfigs,
                                     "aggregated_data/ri44_qbp_2016_1")
    nj=get_compare_file_others_input(compconfigs,
                                     "aggregated_data/nj34_qbp_2016_1")
        #make the comparisons for file base name and config list
    filestemnj="compare_data/nj34_qbp_2016_1_"+outsuffix
    compare_files("aggregated_data/nj34_qbp_2016_1_agg_prep.csv",nj,
                  compconfigs,
                  filestemnj+"_prep.csv",filestemnj+"_agg_prep.csv")
    filestemri="compare_data/ri44_qbp_2016_1_"+outsuffix
    compare_files("aggregated_data/ri44_qbp_2016_1_agg_prep.csv",ri,
                  compconfigs,
                  filestemri+".csv",filestemri+"_agg.csv")
    return compare_configs

def compare_configs_ri_nj_dict(compconfigs,outsuffix,countnegfile=["count_of_negative_protected_aggregates.txt",False,-200,-100000,True]):
    base=compconfigs['base']
    bfnames=base['file_stems']
    baseconfigs=[]
    for x in bfnames:
        fname=str(x).replace("yaml","").replace(".","")
        baseconfigs.append(fname+"_sqrt")
        baseconfigs.append(fname+"_clip")
    print(baseconfigs)
    ribase=get_compare_file_others_input(baseconfigs,
                                     "aggregated_data/ri44_qbp_2016_1")
    njbase=get_compare_file_others_input(baseconfigs,
                                     "aggregated_data/nj34_qbp_2016_1")
    config_names=baseconfigs
    other=compconfigs['new']
    ofnames=other['file_stems']
    odir=other['folder']
    oconfigs=[]
    for x in ofnames:
        fname=str(x).replace("yaml","").replace(".","")
        config_names.append(fname+"_clip")
        #fnamepath=odir+"/"+fname
        oconfigs.append(fname+"_clip")
        if odir!="accttype_clip_prob":
            oconfigs.append(fname+"_sqrt")
            config_names.append(fname+"_sqrt")
    print(oconfigs)
    print(odir)
    riother=get_compare_file_others_input(oconfigs,
                                     filebasename="aggregated_data/"+str(odir)+"/ri44_qbp_2016_1",
                                         groundtruth="aggregated_data/ri44_qbp_2016_1")
    njother=get_compare_file_others_input(oconfigs,
                                     filebasename="aggregated_data/"+str(odir)+"/nj34_qbp_2016_1",
                                         groundtruth="aggregated_data/nj34_qbp_2016_1")
    
        #make the comparisons for file base name and config list
    filestemnj="compare_data/nj34_qbp_2016_1_"+outsuffix
    compare_files("aggregated_data/nj34_qbp_2016_1_agg_prep.csv",njbase+njother,
                  config_names,
                  filestemnj+"_prep.csv",filestemnj+"_agg_prep.csv")
    filestemri="compare_data/ri44_qbp_2016_1_"+outsuffix
    compare_files("aggregated_data/ri44_qbp_2016_1_agg_prep.csv",ribase+riother,
                  config_names,
                  filestemri+".csv",filestemri+"_agg.csv")
    return compare_configs


# def compare_mech_mu_gamma():
#     # comparing mechanism, privacy budget, and square-root neighbor parameters    
#     compare_configs=["clip_1L_M_iL","clip_1H_M_iL","sqrt_1L_M_iL","sqrt_1H_M_iL",
#                      "clip_halfL_M_iL","clip_halfH_M_iL","clip_3L_M_iL","clip_3H_M_iL"]
#     #make the comparisons for nj and ri
#     ri=get_compare_file_others_input(compare_configs,
#                                      "aggregated_data_old/ri44_qbp_2016_1")
#     nj=get_compare_file_others_input(compare_configs,
#                                      "aggregated_data_old/nj34_qbp_2016_1")
#     compare_files("aggregated_data_old/nj34_qbp_2016_1_agg_prep.csv",nj,
#                   compare_configs,
#                   "compare_data_old/nj34_qbp_2016_1_accountant_budget_neighbors.csv",
#                   "compare_data_old/nj34_qbp_2016_1_accountant_budget_neighbors_agg.csv")
#     compare_files("aggregated_data/ri44_qbp_2016_1_agg_prep.csv",ri,
#                   compare_configs,
#                   "compare_data/ri44_qbp_2016_1_accountant_budget_neighbors.csv",
#                   "compare_data/ri44_qbp_2016_1_accountant_budget_neighbors_agg.csv")
#     return compare_configs


# def compare_privacy_allocation():
#     # comparing mechanism, privacy budget, and square-root neighbor parameters    
#     compare_configs=["clip_1L_M_iL","clip_halfL_M_iL","clip_3L_M_iL",
#                      "clip_1L_M_iH","clip_halfL_M_iH","clip_3L_M_iH",
#                      "clip_1L_M_issL","clip_halfL_M_issL","clip_3L_M_issL"]  
#     #make the comparisons for nj and ri
#     ri=get_compare_file_others_input(compare_configs,
#                                      "aggregated_data/ri44_qbp_2016_1")
#     nj=get_compare_file_others_input(compare_configs,
#                                      "aggregated_data/nj34_qbp_2016_1")
#     compare_files("aggregated_data/nj34_qbp_2016_1_agg_prep.csv",nj,
#                   compare_configs,
#                   "compare_data/nj34_qbp_2016_1_privacy_allocation.csv",
#                   "compare_data/nj34_qbp_2016_1_privacy_allocation_agg.csv")
#     compare_files("aggregated_data/ri44_qbp_2016_1_agg_prep.csv",ri,
#                   compare_configs,
#                   "compare_data/ri44_qbp_2016_1_privacy_allocation.csv",
#                   "compare_data/ri44_qbp_2016_1_privacy_allocation_agg.csv")
#     return compare_configs

# def compare_key_query_selection():
#     # comparing mechanism, privacy budget, and square-root neighbor parameters    
#     compare_configs=["clip_1L_M_iL","clip_1L_3I_iL","clip_1L_SS_iL",
#                      "clip_1L_SC_iL"]  
#     #make the comparisons for nj and ri
#     ri=get_compare_file_others_input(compare_configs,
#                                      "aggregated_data/ri44_qbp_2016_1")
#     nj=get_compare_file_others_input(compare_configs,
#                                      "aggregated_data/nj34_qbp_2016_1")
#     compare_files("aggregated_data/nj34_qbp_2016_1_agg_prep.csv",nj,
#                   compare_configs,
#                   "compare_data/nj34_qbp_2016_1_query_selection.csv",
#                   "compare_data/nj34_qbp_2016_1_query_selection_agg.csv")
#     compare_files("aggregated_data/ri44_qbp_2016_1_agg_prep.csv",ri,
#                   compare_configs,
#                   "compare_data/ri44_qbp_2016_1_query_selection.csv",
#                   "compare_data/ri44_qbp_2016_1_query_selection_agg.csv")
#     return compare_configs

# def compare_query_mu_gamma_weights():
#     # comparing mechanism, privacy budget, and square-root neighbor parameters    
#     compare_configs=["clip_1L_M_iL","clip_1L_4I_iL","clip_1L_BM_iL","clip_1L_SC_iL",
#                     "clip_1H_M_iL","clip_1H_4I_iL","clip_1H_BM_iL","clip_1H_SC_iL",
#                     "clip_1L_M_iH","clip_1L_4I_iH","clip_1L_BM_iH","clip_1L_SC_iH",
#                     "clip_1H_M_iH","clip_1H_4I_iH","clip_1H_BM_iH","clip_1H_SC_iH",
#                     "clip_1L_M_iscL","clip_1L_4I_iscL","clip_1L_BM_iscL",
#                     "clip_1H_M_iscL","clip_1H_4I_iscL","clip_1H_BM_iscL",
#                     "clip_3L_M_iL","clip_3L_4I_iL","clip_3L_BM_iL","clip_3L_SC_iL",
#                     "clip_3H_M_iL","clip_3H_4I_iL","clip_3H_BM_iL","clip_3H_SC_iL",
#                     "clip_3L_M_iH","clip_3L_4I_iH","clip_3L_BM_iH","clip_3L_SC_iH",
#                     "clip_3H_M_iH","clip_3H_4I_iH","clip_3H_BM_iH","clip_3H_SC_iH",
#                     "clip_3L_M_iscL","clip_3L_4I_iscL","clip_3L_BM_iscL",
#                     "clip_3H_M_iscL","clip_3H_4I_iscL","clip_3H_BM_iscL"]  
#     #make the comparisons for nj and ri
#     ri=get_compare_file_others_input(compare_configs,
#                                      "aggregated_data/ri44_qbp_2016_1")
#     nj=get_compare_file_others_input(compare_configs,
#                                      "aggregated_data/nj34_qbp_2016_1")
#     compare_files("aggregated_data/nj34_qbp_2016_1_agg_prep.csv",nj,
#                   compare_configs,
#                   "compare_data/nj34_qbp_2016_1_query_mu_gamma_weights.csv",
#                   "compare_data/nj34_qbp_2016_1_query_mu_gamma_weights_agg.csv")
#     compare_files("aggregated_data/ri44_qbp_2016_1_agg_prep.csv",ri,
#                   compare_configs,
#                   "compare_data/ri44_qbp_2016_1_query_mu_gamma_weights.csv",
#                   "compare_data/ri44_qbp_2016_1_query_mu_gamma_weights_agg.csv")
#     return compare_configs


#compare_mech_mu_gamma()
#compare_privacy_allocation()
#compare_key_query_selection()
#compare_query_mu_gamma_weights()
