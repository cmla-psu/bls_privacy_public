import scipy.stats as ss
import numpy as np
from numpy import log
import pandas as pd
from matplotlib import pyplot as plt


def get_dp_var(fn, tn, numq):
    """ If you have numq epsilon-dp queries and you want their
    overall semantics to say that at the false negative rate fn
    and true negative rate tn > fn, the variance each query needs
    under additive noise is what gets returned.

    If S is the output set for which we declare a negative, 
    if D1 is a dataset for which a secret is false 
    and D2 is a neighboring dataset for which the secret is
    true the dp condition is:
    P(M(D1) in S) <= e^epsilon P(M(D2)  in S) 
    which is
    tn <= e^epsilon fn
    since we also have
    (1-fn) <= e^epsilon (1-tn) then this implies
    tn <= 1-e^{-epsilon}(1-fn) and so we have
    tn <= min(e^epsilon fn,   1-e^{-epsilon}(1-fn)
    """
    eps1 = np.log(tn/fn)
    eps2 = np.log((1-fn) / (1-tn))
    true_eps = np.maximum(eps1, eps2) # overall epsilon parameter
    per_query_eps = true_eps/numq # split across the queries
    variance = 2/per_query_eps**2 # laplace mechanism variance
    return variance
#print(get_dp_var(tn=np.array([0.99,0.95,0.9,0.8]),fn=np.array([0.1,0.2,0.25,0.4]),numq=3))

def get_therho(fn,tn,dyadic,sub_upper,upper,iter=0, maxdiagnostics=True,maxiter=5):
    alphas = np.hstack([np.linspace(100, int(sub_upper*100), int(2 ** dyadic + 1))/100,np.array(range(int((sub_upper+0.5)*2), 2 * upper + 1)) / 2])
    alphas = alphas[1:]  # remove alpha=1 since it gives rho=NA

    rhos = [(log((1 - tn) ** a * (1 - fn) ** (1 - a) + tn ** a * fn ** (1 - a))) / (a * (a - 1)) for a in alphas]

    therho = max(rhos)
    maxidx = np.argmax(rhos)
    changerhos = np.array(rhos[1:]) - np.array(rhos[:-1])
    isdecrease = np.array([diff < 0 for diff in changerhos])
    propdecrease=sum(isdecrease) / len(isdecrease)

    # if first value is the maximum, assume strictly decreasing.
    # use limit as alpha->1 (i.e. (1-tn)log(tn/fn)+tnlog((1-tn)/(1-fn))
    if maxidx == 0 and iter < maxiter and propdecrease==1.0:
        iter=iter+1
        if dyadic<5:
            dyadic=2*dyadic
        elif dyadic<15:
            dyadic=dyadic+7
        else:
            dyadic=dyadic+3

        print(f'iter={iter}: dyadic={dyadic}')
        therho = get_therho(fn=fn, tn=tn, dyadic=dyadic, upper=3,sub_upper=1+(1/(iter*10)), iter=iter, maxdiagnostics=maxdiagnostics,maxiter=maxiter)

        # print(f'prop decreasing is {sum(isdecrease) / len(isdecrease)}')
        #print(
        #    f'dyadic={dyadic}: prop decreasing is {sum(isdecrease) / len(isdecrease)}, max at index 0, alpha={alphas[maxidx]}. The values around max are {therho}, {rhos[maxidx + 1]}')
    elif maxidx==0 and propdecrease==1.0 and iter>=maxiter:
        therho = get_therho(fn=fn, tn=tn, dyadic=dyadic, sub_upper=1+(1/(iter*10)),upper=3, iter=iter, maxdiagnostics=maxdiagnostics,
                            maxiter=maxiter)
        print("Reached maxiter")
        if maxdiagnostics:
            print(f'dyadic={dyadic}: prop decreasing is 1, max at index 0, alpha={alphas[maxidx]},rho={therho}. The values around max are {therho}, {rhos[maxidx + 1]}')
    elif maxidx==0 and propdecrease<1:
        increase_idx=set(list(range(len(isdecrease))))-set([i for i,x in enumerate(isdecrease) if not x])
        if alphas[list(increase_idx)][0]<sub_upper:
            if dyadic < 5:
                dyadic = 2 * dyadic
            else:
                dyadic = dyadic + 5
            iter=iter+1
            print(f'iter={iter},dyadic={dyadic}, sub_upper={1+(1/(iter*10))}: increasing rhos in dyadic region')
            therho = get_therho(fn=fn, tn=tn, dyadic=dyadic, sub_upper=1+(1/(iter*10)), upper=upper, iter=iter,
                                maxdiagnostics=maxdiagnostics,
                                maxiter=maxiter+1)
        else:
            iter = iter + 1
            print(f'iter={iter}, dyadic={dyadic},sub_upper={sub_upper*2}: increasing rhos outside dyadic region')
            therho = get_therho(fn=fn, tn=tn, dyadic=dyadic, sub_upper=sub_upper*2, upper=((sub_upper+1)*2), iter=iter,
                                maxdiagnostics=maxdiagnostics,
                                maxiter=maxiter+1)
            #print(f'increasing alphas outside dyadic region')

        #print(f"increase alphas={alphas[list(increase_idx)]}")

    #    ratio1 = np.log(tn / fn)
    #    ratio2 = np.log((1 - tn) / (1 - fn))
    #    therho = (ratio2 * (1 - tn)) + (ratio1 * tn)
    # used for investigating properties of the maximum.
    # If the max is not the first value,
    # print % decreasing; index and alpha of max; max rho; rho values surround max
    if maxidx != 0 and maxdiagnostics:
        # print(f'prop decreasing is {sum(isdecrease) / len(isdecrease)}')
        print(
            f'dyadic={dyadic}: prop decreasing is {propdecrease}, max at index {maxidx}, alpha={alphas[maxidx]}, rho={therho}. The values around max are {rhos[maxidx - 1]},{therho}, {rhos[maxidx + 1]}, first rho={rhos[0]}')
    return therho


def get_zcdp_var_iterative(fn, tn, numq, dyadic, upper,sub_upper=2,maxdiagnostics=False):
    """ If you have numq rho-zcdp queries and you want their
    overall semantics to say that at the false negative rate fn
    and true negative rate tn > fn, the variance each query needs
    under additive noise is what gets returned. 

    Under rho-zcdp, an algorithm M that outputs "positive" or "negative"
    must satisfy (if D1 is a dataset for which a secret is false 
    and D2 is a neighboring dataset for which the secret is
    true) for all alpha > 1:

    P(M(D1)=positive)^alpha P(M(D2)=positive)^{1-alpha} + P(M(D1)=negative)^{alpha} + P(M(D2)=negative)^{1-alpha} <= exp(rho alpha (alpha-1))
  
    which is the same as:

    (1-tn)^alpha (1-fn)^{1-alpha} + tn^alpha fn^{1-alpha} <= exp(rho(alpha)(alpha-1))

    Thus, rho = max_{alpha>1} [ log( (1-tn)^alpha (1-fn)^{1-alpha} + tn^alpha fn^{1-alpha} ) ]/(alpha)(alpha-1)
   
    and variance is 1/(2rho) and the per query rho is rho/numq hence the per
    query variance is numq/(2rho)


    Thus, we have to try a bunch of alpha values, compute the corresponding rho and take the maximum rho. Hence we have 2 tuning parameters "dyadic" and "upper" that determine the alpha values we try. Specifically:
    1+1(2^{-dyadic}), 1+ 2(2^{-dyadic}), ..., 2   and then 3, 4, 5, ..., upper
    """
    therho = get_therho(fn=fn,tn=tn,dyadic=dyadic,upper=upper,sub_upper=sub_upper,maxdiagnostics=maxdiagnostics)
    per_query_rho = therho / numq
    return 1/(2*per_query_rho)


def get_zcdp_var(fn, tn, numq, dyadic, upper,  maxdiagnostics=False):
    """ If you have numq rho-zcdp queries and you want their
    overall semantics to say that at the false negative rate fn
    and true negative rate tn > fn, the variance each query needs
    under additive noise is what gets returned.

    Under rho-zcdp, an algorithm M that outputs "positive" or "negative"
    must satisfy (if D1 is a dataset for which a secret is false
    and D2 is a neighboring dataset for which the secret is
    true) for all alpha > 1:

    P(M(D1)=positive)^alpha P(M(D2)=positive)^{1-alpha} + P(M(D1)=negative)^{alpha} + P(M(D2)=negative)^{1-alpha} <= exp(rho alpha (alpha-1))

    which is the same as:

    (1-tn)^alpha (1-fn)^{1-alpha} + tn^alpha fn^{1-alpha} <= exp(rho(alpha)(alpha-1))

    Thus, rho = max_{alpha>1} [ log( (1-tn)^alpha (1-fn)^{1-alpha} + tn^alpha fn^{1-alpha} ) ]/(alpha)(alpha-1)

    and variance is 1/(2rho) and the per query rho is rho/numq hence the per
    query variance is numq/(2rho)


    Thus, we have to try a bunch of alpha values, compute the corresponding rho and take the maximum rho. Hence we have 2 tuning parameters "dyadic" and "upper" that determine the alpha values we try. Specifically:
    1+1(2^{-dyadic}), 1+ 2(2^{-dyadic}), ..., 2   and then 3, 4, 5, ..., upper
    """
    alphas = np.hstack(
        [np.linspace(1, 2, 2 ** dyadic + 1), np.array(range(5, 2 * upper + 1)) / 2])
    alphas = alphas[1:]  # remove alpha=1 since it gives rho=NA
    rhos = [(log((1 - tn) ** a * (1 - fn) ** (1 - a) + tn ** a * fn ** (1 - a))) / (a * (a - 1)) for a in alphas]

    therho = max(rhos)
    maxidx = np.argmax(rhos)
    changerhos = np.array(rhos[1:]) - np.array(rhos[:-1])
    isdecrease = np.array([diff < 0 for diff in changerhos])
    propdecrease = sum(isdecrease) / len(isdecrease)

    if maxidx==0:
        ratio1=log((1-tn)/(1-fn))
        ratio2=log(tn/fn)
        therho=(ratio1*(1-tn))+(ratio2*tn)
        if maxdiagnostics:
            print(
                f'dyadic={dyadic}: prop decreasing is {propdecrease}, max at index 0, alpha={alphas[maxidx]},rho={therho}. The values around max are {therho},{rhos[0]},{rhos[1]}')

    if maxidx != 0 and maxdiagnostics:
        # print(f'prop decreasing is {sum(isdecrease) / len(isdecrease)}')
        print(
            f'dyadic={dyadic}: prop decreasing is {propdecrease}, max at index {maxidx}, alpha={alphas[maxidx]}, rho={therho}. The values around max are {rhos[maxidx - 1]},{therho}, {rhos[maxidx + 1]}, first rho={rhos[0]}')

    per_query_rho = therho / numq
    return 1 / (2 * per_query_rho)

def test_tuning_zcpd(fn_vec,tn_vec,dyadic_vec=[1,2,3,5],upper=3,sub_upper=2,numq=5):
    # Used to investigate properties of zCDP max.
    combos=[(s,t) for s in fn_vec for t in tn_vec] #combinations of fn and tn
    for s,t in combos: #s=fn, t=tn
        if t>s:
            #B is limit as alpha->1 (l'hospital)
            ratio1 = np.log(t / s)
            ratio2 = np.log((1 - t) / (1 - s))
            B = ratio2 * (1 - t) + ratio1 * t
            print(f'True Neg:{t} False Neg: {s}, Calculated limiting bound B={B}')
            for dyadic in dyadic_vec: #check variety of dyadics
                rho=get_zcdp_var_iterative(s,t,numq,dyadic,upper,sub_upper=sub_upper,maxdiagnostics=True)
                if rho>B:
                    print("rho> limit as x->1")
            print("\n")
        else:
            pass




def get_gdp_var(fn, tn, numq):
    """ If you have numq mu-gaussian dp queries and you want their
    overall semantics to say that at the false negative rate fn
    and true negative rate tn > fn, the variance each query needs
    under additive noise is what gets returned 

    GDP has a parameter mu whose intuitive interpretation is
    that inference is as difficult as distinguishing between
    N(0, 1) and N(mu, 1), which is the same as distinguishing
    between N(0, sigma=1/mu) and N(1, sigma=1/mu)
   
    If S is the output set for which we declare a negative, 
    if D1 is a dataset for which a secret is false 
    and D2 is a neighboring dataset for which the secret is
    true, and Phi is the cdf of the standard normal,  the 
    guassian dp condition is:
    Phi^{-1}(P(M(D1) in S)) <= mu + Phi^{-1}( P(M(D2)  in S) ) 
    the worst case is when S = (-infty, x) 
    so, in terms of tn and fn
    Phi^{-1}(tn) <= mu + Phi^{-1}(fn)
    """
    mu = ss.norm.ppf(tn) - ss.norm.ppf(fn)
    per_query_mu = mu/np.sqrt(numq)
    zeros=per_query_mu[per_query_mu==0]
    if len(zeros)>0:
        print(f'GDP problem tns: {tn[per_query_mu==0]}, fns:{fn[per_query_mu==0]}, numq:{numq[per_query_mu==0]}')
    variance=np.full(len(per_query_mu),np.nan)
    variance[per_query_mu!=0] = 1/per_query_mu[per_query_mu!=0]**2
    return variance

#print(get_gdp_var(tn=np.array([0.99,0.95,0.9,0.8]),fn=np.array([0.1,0.2,0.25,0.4]),numq=np.array([3,4,3,4])))


def variances_across_fn(tn_vec=[0.99,0.95,0.9,0.8],numq_vec=[1,3,5,10],fn_dyadic=2,fn_min=10,fn_max=50,zcdp_max=15,zcdp_dyadic=15):
    fn_vec=np.linspace(fn_min,fn_max,fn_dyadic)/100 #create fn vector
    combos = [(s, t, numq) for s in fn_vec for t in tn_vec for numq in numq_vec if t>s]
    combo_fn=np.array([s for s,t,numq in combos])
    combo_tn = np.array([t for s, t, numq in combos])
    combo_numq=np.array([q for s,t,q in combos])
    var_gdp=get_gdp_var(combo_fn,combo_tn,combo_numq)
    var_pure=get_dp_var(combo_fn,combo_tn,combo_numq)
    var_zcdp=[]
    for s,t,numq in combos:
        var_zcdp.append(get_zcdp_var(s,t,numq,dyadic=zcdp_dyadic,upper=zcdp_max))
    df=pd.DataFrame({"tn": combo_tn, "fn": combo_fn, "numq": combo_numq, "var_gdp": var_gdp, "var_pure": var_pure,
                  "var_zcdp": var_zcdp})
    #df=df.loc[df['tn']>df['fn'],:]
    df['power']=1-df['fn']
    df['significance']=1-df['tn']
    df=df.loc[df['power']>df['significance'],:].copy()
    return df

def plot_compare_fns(df,colors=None,dpframe_labels=None,savefile=None):
    # make grid of line plots. Where x=false negative rate (fn).
    # The columns are true negative rates (tn) and the rows are number of queries (numq)
    # each subplot has a line for GDP, pure DP, and zCDP
    # df should have columns: numq, tn, fn, var_gdp, var_zcdp, var_pure
    # savefile is pdf file name for save location of figure
    # colors and dpframe_labels have hard-coded defaults
    # if provides they should be dictionaries with keys: var_gdp, var_zcdp, var_pure

    #unique tn and numq values
    tn_vals = sorted(df['significance'].unique())
    numq_vals = sorted(df['numq'].unique())

    #fill defaults
    if colors is None:
        colors = {'var_gdp': '#1f77b4', 'var_zcdp': '#ff7f0e', 'var_pure': '#2ca02c'}
    if dpframe_labels is None:
        dpframe_labels= {'var_gdp': 'GDP', 'var_zcdp': 'zCDP', 'var_pure': 'epsilon-DP'}

    # Create figure with subplots
    fig, axes = plt.subplots(len(numq_vals), len(tn_vals),
                             figsize=(4 * len(tn_vals), 3 * len(numq_vals)),
                             squeeze=False)#,sharex=True,sharey="row")

    for j, tn in enumerate(tn_vals): #for each tn rates
        for i,numq in enumerate(numq_vals): #for each number of queries
            plotdf=df[(df['significance']==tn)&(df['numq']==numq)] #subset data

            ax=axes[i,j]
            #plot each dp framework's variance
            for dpframe in ['gdp','zcdp','pure']:
                ax.plot(plotdf['power'], plotdf['var_'+dpframe],
                        color=colors['var_'+dpframe], label=dpframe_labels['var_'+dpframe], linewidth=2)
            #set labels and titles
            ax.set_xlabel('Power')
            ax.set_ylabel('Variance')
            ax.set_title(f'Alpha={tn:.2f}, {numq} Queries')
            ax.grid(True, alpha=0.3) #add grid to plot
    #overall title
    fig.suptitle('Variance of GDP, zCDP, and pure DP for True and False Negative Rates and Number of Queries',
                 fontsize=16, y=0.995)

    # Create a single legend for the entire figure
    handles, labels = axes[0, 0].get_legend_handles_labels()
    fig.legend(handles, labels, loc='center right',
               bbox_to_anchor=(1, 0.5), fontsize=12)

    # Adjust layout
    plt.tight_layout()
    if savefile is not None: #save file as pdf if given savefile
        fig.savefig(savefile, format="pdf")
    return fig



#investigate dyadic values for zCDP
#print(test_tuning_zcpd(tn_vec=[0.8,0.7,0.6],fn_vec=[.6,.7,.8],dyadic_vec=[5],sub_upper=4,upper=50,numq=5))

#get data
#tnfndf=variances_across_fn(tn_vec=np.linspace(50,99,50)/100,numq_vec=[5,2],fn_dyadic=50,fn_min=60,fn_max=85,zcdp_max=30,zcdp_dyadic=17)
#tnfndf.to_csv("scaling_tn_fn_data_low_power_low_numq.csv",index=False)

tnfndf=variances_across_fn(tn_vec=np.linspace(50,99,50)/100,numq_vec=[1,3,5],fn_dyadic=50,fn_min=50,fn_max=99,zcdp_max=30,zcdp_dyadic=17)
tnfndf.to_csv("scaling_tn_fn_data_low_power_low_numq.csv",index=False)
print("low_power done")

tnfndf=variances_across_fn(tn_vec=np.linspace(50,99,50)/100,numq_vec=[1,3,5],fn_dyadic=50,fn_min=1,fn_max=50,zcdp_max=30,zcdp_dyadic=17)
tnfndf.to_csv("scaling_tn_fn_data.csv",index=False)
print("high power done")
