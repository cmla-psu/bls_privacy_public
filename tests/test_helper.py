from tests.helper import are_dfs_equal
import pytest
import pandas as pd
from packaging import version


@pytest.fixture
def tmp_df():
    """ create a dataframe where rwos 1 and 4 are equal"""
    mydf = pd.DataFrame({"A": [1, 2, 3, 1, 2], "B": ["e", "e", "b", "c", "e"], "C": [1 / 3, 1 / 7, 1 / 5, 1 / 2, 1 / 7],
                         "D": [1 / 128, 3 / 128, 5 / 128, 7 / 128, 3 / 128]})
    return mydf


def test_equality(tmp_df):
    tmp_df2 = tmp_df.copy()
    assert are_dfs_equal(tmp_df, tmp_df2)


def test_col_ordering(tmp_df):
    tmp_df2 = tmp_df[list(reversed(list(tmp_df.columns)))]
    assert not are_dfs_equal(tmp_df, tmp_df2)
    assert are_dfs_equal(tmp_df, tmp_df2, ordering=False)


def test_row_ordering(tmp_df):
    tmp_df2 = tmp_df.loc[[4, 3, 2, 1, 0]]
    assert are_dfs_equal(tmp_df, tmp_df2)


def test_multiplicity(tmp_df):
    tmp_df2 = tmp_df.loc[[0, 1, 2, 3]]
    assert not are_dfs_equal(tmp_df, tmp_df2)


def test_typecheck(tmp_df):
    if version.parse(pd.__version__) < version.parse('2.1.0'):
        tmp_df2 = tmp_df.applymap(str)
    else:
        tmp_df2 = tmp_df.map(str)
    assert not are_dfs_equal(tmp_df, tmp_df2)


def test_approx_float1(tmp_df):
    tmp_df2 = tmp_df.copy()
    tmp_df2["C"] = (100 * (tmp_df2["C"] + 1 / 3) - 100 / 3) / 100
    assert tmp_df.iloc[1]["C"] != tmp_df2.iloc[1]["C"]
    assert are_dfs_equal(tmp_df, tmp_df2, digits=10)


def test_approx_float2(tmp_df):
    tmp_df2 = tmp_df.copy()
    tmp_df2["D"] = [0.007812, 0.023437, 0.039062, 0.054687, 0.023437]
    print(tmp_df)
    print(tmp_df2)
    assert are_dfs_equal(tmp_df, tmp_df2, digits=8)
    assert not are_dfs_equal(tmp_df, tmp_df2, digits=9)
