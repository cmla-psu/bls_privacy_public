import pandas as pd
import scipy.stats as ss
import numpy as np

SQRT_SUFFIX = "_sqrt"
STD_SUFFIX = "_std"


def make_prng(seed):
    return np.random.default_rng(seed)


def std_col(colname):
    """ Given the name of a column that has privacy preserving noise
    added by a mechanism here, get the name of the column that holds an (estimate) of the standard deviation"""
    return f"{colname}{STD_SUFFIX}"


def sqrt_col(colname):
    """ Given the name of a column that has privacy preserving noise
    added by a sqrt mechanism here, get the name of the column that holds the noisy square root"""
    return f"{colname}{SQRT_SUFFIX}"


def gaussian_noise(prng, std, amount):
    """ Generates a column vector of Gaussian noise
    Inputs: 
      prng: a numpy psuedorandom number generator
      std: the standard deviation of the noise (sigma, not sigma^2)
      amount: the number of pseudorandom numbers to create
             if amount is None, then a scalar is returned, otherwise a numpy array
    Output:
      a numpy array of size `amount`
    """
    return ss.norm.rvs(size=amount, loc=0, scale=std, random_state=prng)


def sqrt_max_mech(prng, df, group_cols, agg_cols, std_list, identity=False):
    """ Applies the square root mechanism to a group-by **max** query on df, returns a new data frame
    Input:
        prng: a numpy pseudorandom number generator
        df: the dataframe (does not get modified)
        group_cols: the list of  group_by columns. Set to None or empty for overall aggregation
        agg_cols: the columns to aggregate
        std_list: a list of standard deviations of Gaussian noise to add for each aggregation column
        identity: whether this is the identity query on the table project on group cols, default false
    Output:
        A dataframe with these columns:
           a) grouping columns (if identity query, then there is only the index that matches the original df, if
               all rows form one group then there is only an index)
           b) agg_cols are retained and their values are the noisy answers
           c) for each col in agg_col, there is a column f"{col}{SQRT_SUFFIX}" which is the noisy square root
           d) for each col in agg_col, there is a column f"{col}{STD_SUFFIX}" which is an estimate of the std
                                                    of the noisy answer in col based only on the `std_list` parameter
                                                    for that column
                                                    and the noisy value in col, so this variance is public
    """
    return __sqrt_mechanism(prng, df, group_cols, agg_cols, std_list, "max", identity)


def sqrt_sum_mech(prng, df, group_cols, agg_cols, std_list, identity=False):
    """ Applies the square root mechanism to a group-by **sum** query on df, returns a new data frame
    Input:
        prng: a numpy pseudorandom number generator
        df: the dataframe (does not get modified)
        group_cols: the list of group_by columns. Set to None or empty for overall aggregation
        agg_cols: the columns to aggregate
        std_list: a list of standard deviations of Gaussian noise to add for each aggregation column
        identity: whether this is the identity query on the table project on group cols, default False
    Output:
        A dataframe with these columns:
           a) grouping columns (if identity query, then there is only the index that matches the original df, if
               all rows form one group there is only an index)
           b) agg_cols are retained and their values are the noisy answers
           c) for each col in agg_col, there is a column f"{col}{SQRT_SUFFIX}" which is the noisy square root
           d) for each col in agg_col, there is a column f"{col}{STD_SUFFIX}" which is an estimate of the std
                                                    of the noisy answer in col based only on the `std_list` parameter
                                                    for that column
                                                    and the noisy value in col, so this variance is public
    """
    return __sqrt_mechanism(prng, df, group_cols, agg_cols, std_list, "sum", identity)


def __sqrt_mechanism(prng, df, group_cols, agg_cols, std_list, aggfunc, identity=False):
    """ Applies the square root mechanism to df, returns a new data frame
    Do not call this function directly
    Input:
        prng: a numpy pseudorandom number generator
        df: the dataframe (does not get modified)
        group_cols: the list of group_by columns. Set to None or empty for overall aggregation
        agg_cols: the columns to aggregate
        std_list: a list of standard deviations of Gaussian noise to add for each aggregation column
        aggfunc: the function to apply to a group.
        identity: whether this is the identity query on the table project on group cols, default false
    Output:
        A dataframe with these columns:
           a) grouping columns (if identity query, then there is only the index that matches the original df, if
               all rows form one group then there is only an index)
           b) agg_cols are retained and their values are the noisy answers
           c) for each col in agg_col, there is a column f"{col}{SQRT_SUFFIX}" which is the noisy square root
           d) for each col in agg_col, there is a column f"{col}{STD_SUFFIX}" which is an estimate of the std
                                                    of the noisy answer in col based only on the `std_list` parameter
                                                    for that column
                                                    and the noisy value in col, so this variance is public
    """
    # first aggregate up the aggregation columns
    # so that the dataframe contains the grouping columns (if any) and the aggregation columns (which are temporarily 
    # noise free)
    assert len(agg_cols) != 0, "Aggregation columns must be specified"
    assert len(agg_cols) == len(std_list), "The number of aggregation columns must match the size of the std list"
    if identity:  # identity query
        query_df = df[(list(group_cols) if group_cols else []) + list(agg_cols)].copy()
        #  query_df = df[list(agg_cols)].copy()
    elif group_cols is None or len(group_cols) == 0:  # empty list, aggregate over all rows
        query_series = df[agg_cols].agg(aggfunc)
        colvals = [query_series[colname] for colname in agg_cols]
        query_df = pd.DataFrame([colvals])
        query_df.columns = agg_cols
    else:
        namedagg = {colname: pd.NamedAgg(column=colname, aggfunc=aggfunc) for colname in agg_cols}
        query_df = df.groupby(group_cols, as_index=False).agg(**namedagg)
        # now create the noisy square root column and make the aggregation column noisy and also the public variance
    nrows = query_df.shape[0]
    for colname, scale in zip(agg_cols, std_list):
        sqname = sqrt_col(colname)
        stdname = std_col(colname)
        query_df[sqname] = np.sqrt(query_df[colname]) + gaussian_noise(prng, scale, nrows)  # sqrt col, sanitized
        query_df[colname] = np.power(query_df[sqname], 2) - scale * scale  # desired aggregate, sanitized
        query_df[stdname] = np.sqrt(2 * np.power(scale, 4) + np.maximum(0, query_df[colname] * (
                4 * scale * scale)))  # std of aggregate, sanitized
    return query_df


def topcode_gmech(prng, df, group_cols, agg_cols, topcode_func_list, base_stds, identity=False):
    """ Applies the topcode gaussian mechanism to df to answer a group-by sum query, returns a new data frame
    Input:
        prng: a numpy pseudorandom number generator
        df: the dataframe (does not get modified)
        group_cols: the list of group_by columns. Set to None or empty for overall sum query
        agg_cols: the columns to aggregate
        topcode_func_list: a list of functions that provide a topcode, one for each column in agg_cols.
                 The input to each function is the grouping column values
        base_stds: a list of fucntions that provide std information for each aggregation column.
                  the std added to a group for an aggregation column is a function of the topcode value:
                   stdfunc(topcode threshold) for the group and column
        identity: whether this is the identity query on the table project on group cols, default False
    Output:
        A dataframe with these columns:
           a) grouping columns (if identity query, then there is only the index that matches the original df, if
               all rows form a group then there is only an index)
           b) agg_cols are retained and their values are the noisy answers
           d) for each col in agg_col, there is a column f"{col}{STD_SUFFIX}" which is the std
                                                    of the noisy answer in col based only on the `std_list` parameter
                                                    for that column, so this variance is public
    """
    assert len(agg_cols) != 0, "Aggregation columns must be specified"
    assert len(agg_cols) == len(
        topcode_func_list), "The number of aggregation columns must equal the number of topcode functions"
    assert len(agg_cols) == len(base_stds), "The number of aggregation columns must match the size of the std list"
    # create a df with each agg column topcoded
    if not group_cols:  # if there are no grouping columns this is easy
        clipped_df = df[list(agg_cols)].copy()  # make a copy
        for (aggc, topcode) in zip(agg_cols, topcode_func_list):
            the_topcode_number = topcode()  # the topcode is constant
            clipped_df[aggc] = np.minimum(clipped_df[aggc], the_topcode_number)  # topcode the aggregation field
    else:  # otherwise we need to go outside of pandas to apply custom topcode to each group (pd.apply is too slow)
        clipped_df = df[list(group_cols) + list(agg_cols)].copy()
        out_of_pandas = tuple(list(clipped_df[gcol] for gcol in group_cols))  # avoid pandas.apply, slow beyond belief
        for (aggc, topcode) in zip(agg_cols, topcode_func_list):
            out_of_agg = list(clipped_df[aggc])
            clipped_df[aggc] = [min(args[0], topcode(*args[1:])) for args in zip(out_of_agg, *out_of_pandas)]
    # now perform the groupby
    if identity:  # identity query
        query_df = clipped_df[list(group_cols) + list(agg_cols)]
        #  query_df = clipped_df[list(agg_cols)]
    elif group_cols is None or len(group_cols) == 0:  # empty list, sum query
        query_series = clipped_df[agg_cols].agg("sum")
        colvals = [query_series[colname] for colname in agg_cols]
        query_df = pd.DataFrame([colvals])
        query_df.columns = agg_cols
    else:
        namedagg = {colname: pd.NamedAgg(column=colname, aggfunc="sum") for colname in agg_cols}
        query_df = clipped_df.groupby(group_cols, as_index=False).agg(**namedagg)
    # now create the noisy answer column and std column
    if not group_cols:
        for colname, topcode, stdfun in zip(agg_cols, topcode_func_list, base_stds):
            stdname = std_col(colname)
            the_topcode_number = topcode()
            the_std_col_value = stdfun(the_topcode_number)
            query_df[stdname] = the_std_col_value
            query_df[colname] = query_df[colname] + gaussian_noise(prng, the_std_col_value, amount=query_df.shape[0])
    else:
        # again we do most calculations out of pandas to avoid the slowwww df.apply
        out_of_pandas = tuple(list(query_df[gcol] for gcol in group_cols))
        for colname, topcode, stdfun in zip(agg_cols, topcode_func_list, base_stds):
            stdname = std_col(colname)
            out_of_agg = list(query_df[colname])
            out_of_std = [stdfun(topcode(*args)) for args in zip(*out_of_pandas)]
            query_df[stdname] = out_of_std
            query_df[colname] = [args[0] + gaussian_noise(prng, args[1], None)
                                 for args in zip(out_of_agg, out_of_std, *out_of_pandas)]
    return query_df
