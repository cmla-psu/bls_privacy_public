import pandas as pd
import numpy as np
from packaging import version

""" This file contains helper functions for tests,
including checking that two dataframes are the same 
and generating testing fake qcew data """


def matrix_to_string_array(a, digits=10):
    """ converts every row of a matrix to a string, converting every number to `digits` characters
     (which can include the . ) """
    dense_a = a.toarray() if "toarray" in dir(a) else a
    # convert each row into a string
    spec = "{:." + str(digits) + "f}"
    strings_a = [" ".join([spec.format(v)[0:digits] for v in row]) for row in dense_a]
    return strings_a


def compare_row_permuted_matrices(a, b, digits=10):
    """ Compares two matrices to see if they are the same up to permutation of rows
    Input:
       a: first matrix
       b: second matrix
       digits: represent floats using "digits" characters to enable comparison
    Output:
       1 if they are the same up to the chosen floating precision
       0 otherwise
    """
    strings_a = sorted(matrix_to_string_array(a, digits))
    strings_b = sorted(matrix_to_string_array(b, digits))
    return strings_a == strings_b


def discretize(v, digits=10):
    """ if v is a float, it discretizes to a string representation that has `digits`
    characters (including . or - ) and return it as a string
    if v is not a float, it is returned as is` """
    if isinstance(v, float) or isinstance(v, np.float64) or isinstance(v, np.float32):
        negative_tol = 5 * np.power(10.0, -max(1, digits-1))
        if np.abs(v) < negative_tol:
            v2 = 0
        else:
            v2 = v
        theval = ("{:." + str(digits) + "f}").format(v2)[0:digits]
    else:
        theval = v
    return theval


def are_dfs_equal(df1, df2, digits=10, ordering=True, messages=True):
    """ Compares whether dataframe df1 equals df2

    Inputs:
        df1: pandas dataframe
        df2: pandas dataframe
        digits: number of characters in the string representation of floats to discretize to, default 10
        ordering: True if column orders should be the same
        messages: whether to print to stdout explanation of the differences

        Neither dataframe can have a __COUNT column
    """
    count_col = "__COUNT"
    # test 1: do both dataframes have the same columns
    if ordering:
        test1 = list(df1.columns) == list(df2.columns)
    else:
        test1 = set(df1.columns) == set(df2.columns)
    if messages and not test1:
        print("DataFrames have different column names")
    # test 2: do both have same number of rows
    test2 = (df1.shape[0] == df2.shape[0])
    if messages and not test2:
        print("DataFrames have different numbers of rows")
    # test 3: check that all column types are the same
    test3 = dict(df1.dtypes) == dict(df2.dtypes)
    if messages and not test3:
        print("DataFrames have different column types")
    test4 = False  # we will change its value later if necessary
    if test1 and test2 and test3:  # if all is good so far
        # now make copies and discretize floats
        def myanon(x):
            return discretize(x, digits)

        if version.parse(pd.__version__) < version.parse('2.1.0'):
            newdf1 = df1.applymap(myanon)
            newdf2 = df2.applymap(myanon)
        else:
            newdf1 = df1.map(myanon)
            newdf2 = df2.map(myanon)
        # full outer join them on all columns. If size is the same
        # then the dataframes are equal
        oldcols = list(newdf1.columns)
        assert count_col not in newdf1, f"DataFrames cannot have a {count_col} column"
        assert count_col not in newdf2, f"DataFrames cannot have a {count_col} column"
        newdf1[count_col] = 1
        newdf2[count_col] = 1
        grouped_df1 = newdf1.groupby(by=oldcols, as_index=False).agg({count_col: "count"})
        grouped_df2 = newdf2.groupby(by=oldcols, as_index=False).agg({count_col: "count"})
        joined = grouped_df1.merge(grouped_df2, on=list(grouped_df1.columns), how="inner")
        # check whether the two dataframes have the same rows and multiplicities
        test4 = (grouped_df1.shape[0] == grouped_df2.shape[0]) and \
                (grouped_df1.shape[0] == joined.shape[0])
        if messages and not test4:
            print("Rows of dataframes don't match")
    return test1 and test2 and test3 and test4
