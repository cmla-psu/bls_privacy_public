

import os
import pandas as pd
import numpy as np
import yaml
import re
import string
#os.chdir('/storage/work/krd5520/RunMechanisms') 
#os.getcwd()


def default_budget_prop(aggstats_path="R_tables_plots",
                        aggstats_fname_stem="agg_estnum_summary",
                        state_prefix="nj34_qbp_2016_1",
                        ngroup_breaks=[9, 49, 499, 1499],
                        gbquery_share=[0.03396, 0.28804, 0.28805, 0.28805, 0.38994],
                        wage_share=[0.25, 0.25, 0.25, 0.25, 0.25]):
    
    if aggstats_path is None:
        # Get the path similar to `rprojroot::find_rstudio_root_file()` in R
        # In Python, you can get the current script's directory (which simulates this)
        aggstats_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "R_tables_plots")
    
    assert len(gbquery_share) - len(ngroup_breaks) == 1, "Length mismatch between gbquery_share and ngroup_breaks"
    assert len(gbquery_share) == len(wage_share), "Length mismatch between gbquery_share and wage_share"
    
    aggstats_file = os.path.join(aggstats_path, f"{aggstats_fname_stem}_{state_prefix}.csv")
    
    if os.path.exists(aggstats_file):
        # Read the file if it exists
        aggstats = pd.read_csv(aggstats_file)
    else:
        # Fallback if the file doesn't exist (assuming `get_aggcode_info` is some function you can implement or use elsewhere)
        aggstats = get_aggcode_info(state_prefix=state_prefix, write_tex=aggstats_file.replace(".csv", ".tex"), overwrite=True)
        aggstats = pd.read_csv(aggstats_file)
    
    # Remove rows where 'agglvl' is NA
    aggstats = aggstats[aggstats['agglvl'].notna()]
    
    if 'share_cat' not in aggstats.columns:
        aggstats['share_cat'] = pd.cut(pd.to_numeric(aggstats['num_groups'].astype(str)), bins=[0] + ngroup_breaks + [10**10],labels=False)

    # Assign 'gbquery_share' and 'wage_share' based on 'share_cat'
    aggstats['gbquery_share'] = [gbquery_share[i] for i in aggstats['share_cat'].values]
    aggstats['wage_share'] = [wage_share[i] for i in aggstats['share_cat'].values]
    
    # Modify 'qname' column
    aggstats['qname'] = aggstats['agglvl'].str.replace("-digit|by |State ", "")
    
    hasnaics = aggstats['agglvl'].str.contains("NAICS")
    hassector = aggstats['agglvl'].str.contains("sector", case=False)
    hassupsector = aggstats['agglvl'].str.contains("Supersector")
    
    aggstats.loc[hasnaics & ~hassector, 'qname'] = aggstats.loc[hasnaics & ~hassector, 'qname'].str.replace("NAICS ", "NAICS")
    aggstats.loc[hassector & ~hassupsector, 'qname'] = aggstats.loc[hassector & ~hassupsector, 'qname'].str.replace("NAICS Sector", "Sector")
    
    iscounty = aggstats['agglvl'].str.contains("County")
    aggstats.loc[iscounty, 'qname'] = aggstats.loc[iscounty, 'qname'].str.replace("County\\s*", "County/")
    
    # Trim whitespace and modify 'qname'
    aggstats['qname'] = aggstats['qname'].str.strip()
    aggstats['qname'] = "By " + aggstats['qname']
    
    # Special cases for "State Total" and "County Total"
    aggstats.loc[aggstats['agglvl'] == "State Total", 'qname'] = "Sum Query"
    aggstats.loc[aggstats['agglvl'] == "County Total", 'qname'] = "By County"
    
    return aggstats



def get_agggroups(qname):
    if "Sum" in qname:
        return None
    outgroups=[]
    if "County" in qname:
        outgroups.append("cnty")
    
    if "sector" in qname.lower():
        if "super" in qname.lower():
            outgroups.append("super_sector")
        else:
            outgroups.append("sector")
    elif "NAICS" in qname:
        naicsdigit = re.sub(r"[^0-9]", "", qname)
        if naicsdigit=="6":
            outgroups.append("naics")
        else:
            outgroups.append("naics"+naicsdigit)
    return(outgroups)
          


def change_groupby_queries(groupby_queries, gb_musq,
                           override_big=None, override_small=None,
                           wage_override_big=None, wage_override_small=None,
                           override_default_budget_props=None,
                           return_aggstats=False):

    # If no override_default_budget_props is provided, call default_budget_prop
    if override_default_budget_props is None:
        aggstats = default_budget_prop()
    else:
        aggstats = default_budget_prop(**override_default_budget_props)

    # Handle override for biggest group
    if override_big is not None or wage_override_big is not None:
        biggest_idx = aggstats['num_groups'][aggstats['qname'].isin(qnames)].idxmax()
        if override_big is not None:
            aggstats.loc[biggest_idx, 'gbquery_share'] = override_big
        if wage_override_big is not None:
            aggstats.loc[biggest_idx, 'wage_share'] = wage_override_big

    # Handle override for smallest group
    if override_small is not None or wage_override_small is not None:
        smallest_idx = aggstats['num_groups'][aggstats['qname'].isin(qnames)].idxmin()
        if override_small is not None:
            aggstats.loc[smallest_idx, 'gbquery_share'] = override_small
        if wage_override_small is not None:
            aggstats.loc[smallest_idx, 'wage_share'] = wage_override_small

    # Compute the musq values
    aggstats['gb_musq'] = gb_musq * aggstats['gbquery_share']
    aggstats['wage_musq'] = aggstats['gb_musq'] * aggstats['wage_share']
    aggstats['emp_musq'] = aggstats['gb_musq'] * ((1 - aggstats['wage_share']) / 3)

    
    # Handle groupby_queries as either a list or numeric
    if isinstance(groupby_queries[0], (int, float)):  # If numeric
        groupby_queries = sorted(groupby_queries)
        qnames = aggstats['qname'][aggstats['agglvl_code'].isin(groupby_queries)].tolist()
    else:
        qnames = groupby_queries
    

    # Construct the gbqueries list
    gbqueries = [
        {
            'name': f'"{qname}"',
            'group': get_agggroups(qname),
            'mu': {
                'm1emp': float(round(np.sqrt(aggstats.loc[aggstats['qname'] == qname, 'emp_musq'].values[0]), 5)),
                'm2emp': float(round(np.sqrt(aggstats.loc[aggstats['qname'] == qname, 'emp_musq'].values[0]), 5)),
                'm3emp': float(round(np.sqrt(aggstats.loc[aggstats['qname'] == qname, 'emp_musq'].values[0]), 5)),
                'wage': float(round(np.sqrt(aggstats.loc[aggstats['qname'] == qname, 'wage_musq'].values[0]), 5))
            }
        }
        for qname in qnames
    ]

    # Return either the gbqueries or both gbqueries and aggstats
    if return_aggstats:
        return gbqueries, aggstats
    else:
        return gbqueries
    



def budget_matrix_to_yaml(bbudgets, gb_groups,check_budget=False,budget_goal=None,quietly=False):
            
    if check_budget:
        if budget_goal is not None:
            checkmusq=0
            for col in bbudgets.columns:
                for el in bbudgets[col].to_numpy():
                    checkmusq+=float(el)**2
            checkmu=np.sqrt(checkmusq)
            if np.abs(budget_goal-checkmu)>0.005:
                print(f"Something is wrong. Goal budget is: {round(budget_goal,3)} but the calculated budget is: {round(checkmu,3)}.")
                print(bbudgets)
            elif ~quietly:
                print("Budgets have been checked.")
        elif ~quietly:
            print("Can't check budget without a goal budget provided.")
    # Convert columns of bbudgets to lists
    empwage_list = [bbudgets.loc[:, i].to_dict() for i in bbudgets.columns]
    
    # Create the 'identity' part of the query
    iquery = {"identity": empwage_list[0]}
    
    groupby_budgets=bbudgets.drop('identity',axis=1)
    
    # Create the 'groupby' part of the query
    internal_gbquery=[]
    for i, col in enumerate(groupby_budgets.columns, start=1):
        internal_gbquery.append({"name":col, "group": gb_groups[i-1], "mu": bbudgets.loc[:, col].to_dict()})
    
    
    gbquery = {
        "groupby":internal_gbquery
    }
    
    # Combine the two parts into one dictionary and return
    return {**iquery, **gbquery}




def alter_config(out_yaml_stem="test",
                 acct=["clip", "sqrt"],
                 baseyaml_fname="blsvals_clip.yaml",
                 config_folder="configs",
                 out_subfolder=None,
                 total_budget=np.sqrt(5.32),
                 clip_prob=0.01,
                 wage_share_mult=1,
                 identity_share_mult=1,
                 max_share=0.99,
                 groupby_queries=None,
                 use_same_groupby_budgets=True,
                 aggstats=None,
                 config_seed=1,
                 quietly=True,
                 stopIfShareWarning=False,
                noaccountantchange=False):
    
    #if base_path is None:
    #    base_path = os.path.dirname(os.path.abspath(__file__))  # Adjust for your file structure
    
    # Load the base YAML file
    with open(os.path.join(config_folder, baseyaml_fname), 'r') as f:
        baseyaml = yaml.safe_load(f)
        
    bclip_prob=baseyaml['clipping_prob']
    
    # Get queries and budget information
    bquery = baseyaml['queries']
    bgroupby = bquery['groupby']
    
    bbudgets=pd.DataFrame(x['mu'] for x in bgroupby).transpose()
    bbudgets.columns=[x['name'] for x in bgroupby]
    bbudgets['identity']=bquery['identity'].values()
    bbudgets.reindex(columns=['identity']+[x['name'] for x in bgroupby])
    bbudgets['rows']=['m1emp','m2emp','m3emp','wage']
    bbudgets.set_index('rows',drop=True,inplace=True)
   

    # Group and square sums
    bgbgroups = [x['group'] for x in bgroupby]

    bmusq = np.sum(np.sum(bbudgets**2))

    # Share of budget-squared for each query
    bquery_musq=np.sum(bbudgets**2, axis=0)
    bquery_share = bquery_musq/ bmusq
    
    #bwage_share_win_query = bquery_share
    bwage_budgetsq=bbudgets.loc['wage',:]**2
    bwage_share_win_query=bwage_budgetsq/np.sum(bbudgets**2, axis=0)
    
    base_share = bbudgets**2 / bmusq
    check_shares=np.sum(np.sum(base_share))
    if np.abs(1-check_shares)>(10**(-6)):
        print("Shares don't sum to 1. What is wrong???")
        print(base_share)
        print(check_shares)
    
    
    
    newyaml = baseyaml  # Initialize new YAML information
    
    # Handle groupby queries logic
    if groupby_queries is None:
        if clip_prob != bclip_prob:
            if not quietly:
                print(f"Changing clipping probability from base: {round(bclip_prob, 4)} to: {round(clip_prob, 4)}")
            newyaml['clipping_prob']=clip_prob
        if identity_share_mult != 1:  # If changing identity share with multiplier
            identity_share = bquery_share['identity'] * identity_share_mult
            assert identity_share > 0
            if identity_share > max_share:
                print(f"Identity multiplier creates identity share of budget squared above maximum: {max_share}. Overriding to max value.")
                if stopIfShareWarning:
                    raise ValueError("Share warning raised")
            
            assert 0 < identity_share < 1
            if not quietly:
                print(f"Changing Identity query share from base: {round(bquery_share[0], 4)} to: {round(identity_share, 4)}")
            
            identity_musq=identity_share*bmusq
            not_identity = bmusq - identity_musq
            query_share_of_not_identity = bquery_musq[1:] / np.sum(bquery_musq[1:])
            rescale_not_ident = not_identity * query_share_of_not_identity
            rescale_wage = bwage_share_win_query * np.concatenate(([identity_musq], rescale_not_ident))
            rescale_emp = np.sqrt((np.concatenate(([identity_musq], rescale_not_ident))-rescale_wage)/3) 
            newbudgets = pd.DataFrame(np.vstack([rescale_emp, rescale_emp, rescale_emp, np.sqrt(rescale_wage)]),
                                      index=bbudgets.index, columns=bbudgets.columns)
            
            
            if np.abs(np.sqrt(bmusq) - total_budget) > 1e-6:  # If changing identity share & total budget
                new_share= (newbudgets**2)/(total_budget**2)
                check_new_share=np.sum(np.sum(new_share))
                if np.abs(1-check_new_share)>(10**(-6)):
                    print("Something wrong with new shares. They don't sum to 1.")
                    print(check_new_share)
                    print(new_share)
                new_share = pd.DataFrame(new_share, index=base_share.index, columns=base_share.columns)
                if not quietly:
                    print(f"Changing Budgets from base: {round(np.sqrt(bmusq), 4)} to: {round(total_budget, 4)}")
                bmusq = total_budget**2
            
                newbudgets = np.round(new_share * bmusq, 5)
            if wage_share_mult != 1:
                print("Cannot change wage share and identity share at the same time. Ignoring wage_share_mult.")
        
        elif wage_share_mult != 1:  # If changing wage share with multiplier
            new_wage_share = bwage_share_win_query * wage_share_mult 
            #print(new_wage_share)
            #print(bwage_share_win_query)
            assert np.all(new_wage_share > 0)
            too_large = new_wage_share > max_share
            if np.any(too_large):
                new_wage_share[too_large] = max_share
                print(f"Wage multiplier creates wage share of budget squared above maximum: {max_share}. Overriding to max value.")
                if stopIfShareWarning:
                    raise ValueError("Share warning raised")
            
            if not quietly:
                print(f"Changing Wage Share within query from base: {', '.join(map(str, np.round(bwage_share_win_query, 4)))} "
                      f"to: {', '.join(map(str, np.round(new_wage_share, 4)))} with multiplier: {str(wage_share_mult)}.")
            
            rescale_wage = new_wage_share * bquery_musq
            rescale_emp = np.sqrt((bquery_musq-rescale_wage)/3)
            newbudgets = pd.DataFrame(np.vstack([rescale_emp, rescale_emp, rescale_emp, np.sqrt(rescale_wage)]),
                                      columns=bbudgets.columns,index=bbudgets.index)
            
            if np.abs(np.sqrt(bmusq) - total_budget) > 1e-6:  # If changing wage share & total budget
                new_share=(newbudgets**2)/(total_budget**2)
                check_new_share=np.sum(np.sum(new_share))
                if np.abs(1-check_new_share)>(10**(-6)):
                    print("Something wrong with new shares. They don't sum to 1.")
                    print(check_new_share)
                    print(new_share)
                if not quietly:
                    print(f"Changing Budgets from base: {round(np.sqrt(bmusq), 4)} to: {round(total_budget, 4)}")
                new_share = pd.DataFrame(new_share, index=base_share.index, columns=base_share.columns)
                bmusq = total_budget**2
                newbudgets = np.round(new_share * bmusq, 5)
        
        elif np.abs(np.sqrt(bmusq) - total_budget) > 1e-6:  # If only changing overall budget
            if not quietly:
                print(f"Changing Budgets from base: {round(np.sqrt(bmusq), 4)} to: {round(total_budget, 4)}")
            newbudgets = np.round(np.sqrt(base_share * (total_budget**2)), 5)
        else:
            if not quietly:
                print("No budget or budget allocation changes.")
            newbudgets=bbudgets
            

        # Handle groupby queries update
        aggroups = [get_agggroups(col) for col in newbudgets.columns]
        newyaml['queries'] = budget_matrix_to_yaml(newbudgets, aggroups,check_budget=True,budget_goal=total_budget)
        aggstats = aggstats

    else:  # If changing queries (no other changes allowed simultaneously)
        if not quietly:
            print(f"Changing queries from base: {', '.join(bbudgets.columns)} to:")
            print(groupby_queries)
        
        if len(groupby_queries) == len(bgbgroups) and use_same_groupby_budgets:
            newbudgets = bbudgets
            if isinstance(groupby_queries[0], (int, float)):
                if aggstats is None:
                    aggstats = default_budget_prop()
                groupby_queries = aggstats['qname'][aggstats['agglvl_code'].isin(groupby_queries)].tolist()
            
            newbudgets.columns = ["identity"]+groupby_queries
            groupby_groups = [get_agggroups(q) for q in groupby_queries]
            newyaml['queries'] = budget_matrix_to_yaml(newbudgets, groupby_groups,check_budget=True,budget_goal=total_budget)
        else:
            groupby_musq = bmusq * (1 - bquery_share[0])
            tempout = change_groupby_queries(groupby_queries=groupby_queries, gb_musq=groupby_musq, return_aggstats=True)
            newyaml['queries']['groupby'] = tempout[0]
            aggstats = tempout[1]

    # Handling subfolder creation and YAML output
    if out_subfolder is not None:
        byamlout = baseyaml['output']
        bfolder_nm = [
            byamlout['aggregate']['folder'],
            byamlout['postprocessed']['folder'],
            byamlout['measure_folder'],
            config_folder
        ]
        for folder in bfolder_nm:
            os.makedirs(folder, exist_ok=True)
        
        subfolder_nm = [os.path.join(folder, out_subfolder) for folder in bfolder_nm]
        for folder in subfolder_nm:
            os.makedirs(folder, exist_ok=True)
        
        newyaml['output']['aggregate']['folder'] = subfolder_nm[0]
        newyaml['output']['postprocessed']['folder'] = subfolder_nm[1]
        newyaml['output']['measure_folder'] = subfolder_nm[2]
        out_config_stem = os.path.join(subfolder_nm[3], out_yaml_stem)
    else:
        out_config_stem = os.path.join(config_folder, out_yaml_stem)

    newyaml['seed'] = config_seed

    # Save the YAML to file
    if noaccountantchange:
        if not quietly:
            print(f"Saving sqrt config file as: {out_config_stem}_sqrt.yaml")
        #newyaml['accountant'] = "STANDGAUSAccountant"
        with open(f"{out_config_stem}.yaml", 'w') as f:
            yaml.safe_dump(newyaml, f)
    else:        
        if "sqrt" in acct:
            if not quietly:
                print(f"Saving sqrt config file as: {out_config_stem}_sqrt.yaml")
            newyaml['accountant'] = "SQRTAccountant"
            with open(f"{out_config_stem}_sqrt.yaml", 'w') as f:
                yaml.safe_dump(newyaml, f)

        if "clip" in acct or "pnc" in acct:
            if not quietly:
                print(f"Saving clipping config file as: {out_config_stem}_clip.yaml")
            newyaml['accountant'] = "ClippingAccountant"
            with open(f"{out_config_stem}_clip.yaml", 'w') as f:
                yaml.safe_dump(newyaml, f)

    return newyaml, aggstats


def experiment_configs(out_yaml_stem="test",vary_input="total_budget",
                       vary_values=[1,2,3,4,5],acct=["both"],
                       baseyaml_fname="blsvals_clip.yaml",
                       config_folder="configs",experiment_folder="test",aggstats=None,
                      compare_yamlfnames=["blsvals_sqrt.yaml"],
                      max_share=0.99,use_same_groupby_budgets=False,config_seed=1,noaccountantchange=False,quietly=True):
    basefnames=compare_yamlfnames.append(baseyaml_fname)
    #basefnames=[x.replace(".yaml","") for x in basefnames]
    baseconfig_fnames={"base":{"folder":config_folder, "file_stems":basefnames}}
    config_fnames=[]
    if vary_input=="clip_prob":
        for val in vary_values:
            str_val=str(round(val,3))
            str_val=str_val.replace(".","")
            config_fnames.append(out_yaml_stem+str_val)
            config, aggstats=alter_config(out_yaml_stem=out_yaml_stem+str_val, 
                                          acct=["clip"],baseyaml_fname=baseyaml_fname,
                                         config_folder=config_folder,out_subfolder=experiment_folder,
                                          clip_prob=val,aggstats=aggstats,config_seed=config_seed,quietly=quietly)
    elif vary_input!="groupby_queries":
        if len(acct)<len(vary_values):
            acct=acct*(1+(len(vary_values)//len(acct)))
            acct=acct[0:(len(vary_values))]
        for i in range(0,len(vary_values)):
            val=vary_values[i]
            str_val=str(round(val,3))
            str_val=str_val.replace(".","p")
            config_fnames.append(out_yaml_stem+str_val)
            if acct[i]=="both":
                acct_val=["sqrt","clip"]
            else:
                acct_val=[acct[i]]
            if vary_input=="total_budget":
                config, aggstats=alter_config(out_yaml_stem=out_yaml_stem+str_val, 
                                              acct=acct_val,baseyaml_fname=baseyaml_fname,
                                              config_folder=config_folder,out_subfolder=experiment_folder,
                                              total_budget=val,aggstats=aggstats,config_seed=config_seed,quietly=quietly)
            elif vary_input=="identity_share_mult":
                config, aggstats=alter_config(out_yaml_stem=out_yaml_stem+str_val, 
                                              acct=acct_val,baseyaml_fname=baseyaml_fname,
                                              config_folder=config_folder,out_subfolder=experiment_folder,
                                              identity_share_mult=val,aggstats=aggstats,max_share=max_share,config_seed=config_seed,quietly=quietly)
            elif vary_input=="wage_share_mult":
                config, aggstats=alter_config(out_yaml_stem=out_yaml_stem+str_val, 
                                              acct=acct_val,baseyaml_fname=baseyaml_fname,
                                              config_folder=config_folder,out_subfolder=experiment_folder,
                                              wage_share_mult=val,aggstats=aggstats,max_share=max_share,config_seed=config_seed,quietly=quietly)
            elif vary_input=="identity_share_mult":
                config, aggstats=alter_config(out_yaml_stem=out_yaml_stem+str_val, 
                                              acct=acct_val,baseyaml_fname=baseyaml_fname,
                                              config_folder=config_folder,out_subfolder=experiment_folder,
                                              identity_share_mult=val,aggstats=aggstats,config_seed=config_seed,quietly=quietly)
            elif vary_input=="config_seed":
                config, aggstats=alter_config(out_yaml_stem=out_yaml_stem+str_val, 
                                              acct=acct_val,baseyaml_fname=baseyaml_fname,
                                              config_folder=config_folder,out_subfolder=experiment_folder,
                                              aggstats=aggstats,config_seed=val,quietly=quietly,noaccountantchange=noaccountantchange)
            else:
                print("vary_input not recognized. It should be a string with one of the following values: 'clip_prob', 'total_budget','identity_share_mult', 'wage_share_mult','config_seed' or 'groupby_queries'.")
    else:
        if len(acct)<len(vary_values):
            acct=acct*(1+(len(vary_values)//len(acct)))
            acct=acct[0:(len(vary_values))]
        for i in range(0,len(vary_values)):
            if acct[i]=="both":
                acct_val=["sqrt","clip"]
            else:
                acct_val=[acct[i]]
            val=vary_values[i]
            if len(val)==1:
                str_val=str(val[0]).replace(" ","")
                str_val=str_val.replace("/","x")
                str_val=str_val.replace("-","")
            else:
                str_val=[]
                for q in val[0:]:
                    substr_val=str(q).replace(" ","")
                    substr_val=substr_val.replace("/","x")
                    substr_val=substr_val.replace("-","")
                    str_val.append(substr_val)
                str_val="_".join(str_val)
            config_fnames.append(out_yaml_stem+str_val)
            config, aggstats=alter_config(out_yaml_stem=out_yaml_stem+str_val, 
                                               acct=acct_val,baseyaml_fname=baseyaml_fname,
                                               config_folder=config_folder,out_subfolder=experiment_folder,
                                               aggstats=aggstats,config_seed=config_seed,groupby_queries=val,
                                               use_same_groupby_budgets=use_same_groupby_budgets,quietly=quietly,noaccountantchange=noaccountantchange)
    
    baseconfig_fnames.update({"new":{"folder":experiment_folder, "file_stems":config_fnames}})

                
    return(baseconfig_fnames)








