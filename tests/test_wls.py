import pytest
import pandas as pd
from tests.helper import compare_row_permuted_matrices, are_dfs_equal
import mechanisms
import numpy as np
import postprocess

AGG1 = "Agg1"
AGG2 = "Agg2"


@pytest.fixture
def prng():
    seed = 3
    return np.random.default_rng(seed)


@pytest.fixture
def wls_df():
    df = pd.DataFrame({
        "A": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        "B": [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
        "C": [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1],
        "D": [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1],
        AGG1: range(16),
        AGG2: reversed([x + 0.5 for x in range(16)])
    })
    return df


def test_matrix(wls_df):
    EQS = 23
    A1 = np.zeros((EQS, 16))  # agg col 1
    b1 = np.zeros(EQS)
    A2 = A1.copy()  # agg col 2
    b2 = b1.copy()
    b_group = pd.DataFrame({
        "B": [1, 0],
        AGG1: [2, 3],
        AGG2: [4, 5],
        mechanisms.std_col(AGG1): [1, 1],
        mechanisms.std_col(AGG2): [2.0, 0.5]
    })
    A1[0, 8:16] = 1
    b1[0] = 2
    A1[1, 0:8] = 1
    b1[1] = 3
    A2[0, 8:16] = 1 / 2.0
    b2[0] = 4 / 2.0
    A2[1, 0:8] = 1 / 0.5
    b2[1] = 5 / 0.5
    cb_group = pd.DataFrame({
        "C": [1, 0, 1, 0],
        "B": [0, 1, 1, 0],
        AGG1: [4, 3, 2, 1],
        AGG2: [1, 2, 3, 4.0],
        mechanisms.std_col(AGG1): [1, 2, 3, 4]
    })
    A1[2, 4:8] = 1 / 1
    b1[2] = 4 / 1
    A1[3, 8:12] = 1 / 2
    b1[3] = 3 / 2
    A1[4, 12:16] = 1 / 3
    b1[4] = 2 / 3
    A1[5, 0:4] = 1 / 4
    b1[5] = 1 / 4
    A2[2, 4:8] = 1
    b2[2] = 1
    A2[3, 8:12] = 1
    b2[3] = 2
    A2[4, 12:16] = 1
    b2[4] = 3
    A2[5, 0:4] = 1
    b2[5] = 4
    overall = pd.DataFrame({
        AGG2: [20],
        AGG1: [10],
        mechanisms.std_col(AGG2): [30]
    })
    A1[6, :] = 1
    b1[6] = 10
    A2[6, :] = 1 / 30
    b2[6] = 20 / 30
    identity = pd.DataFrame({
        AGG2: reversed(range(16)),
        AGG1: range(16),
        mechanisms.std_col(AGG1): 0.5 * np.ones(16)
    })
    for i in range(16):
        A1[7 + i, i] = 1 / 0.5
        b1[7 + i] = i / 0.5
        A2[7 + i, i] = 1
        b2[7 + i] = 15 - i

    wlsfromdf1 = postprocess.WLSFromDF(wls_df[["A", "B", "C", "D"]])
    wlsfromdf1.add_query(b_group, ["B"])
    wlsfromdf1.add_query(cb_group, ["B", "C"])
    wlsfromdf1.add_query(overall, None)
    wlsfromdf1.add_query(identity, None)
    wlsfromdf1.add_target(AGG1, mechanisms.std_col(AGG1))
    wlsfromdf1.add_target(AGG2, mechanisms.std_col(AGG2))

    # the difference is we reverse the order of group col names when giving cb_group
    wlsfromdf2 = postprocess.WLSFromDF(wls_df[["A", "B", "C", "D"]])
    wlsfromdf2.add_query(b_group, ["B"])
    wlsfromdf2.add_query(cb_group, ["C", "B"])
    wlsfromdf2.add_query(overall, None)
    wlsfromdf2.add_query(identity, None)
    wlsfromdf2.add_target(AGG1, mechanisms.std_col(AGG1))
    wlsfromdf2.add_target(AGG2, mechanisms.std_col(AGG2))

    A1test1, b1test1 = wlsfromdf1.getAB(AGG1, mechanisms.std_col(AGG1))
    A2test1, b2test1 = wlsfromdf1.getAB(AGG2, mechanisms.std_col(AGG2))
    A1test2, b1test2 = wlsfromdf2.getAB(AGG1, mechanisms.std_col(AGG1))
    A2test2, b2test2 = wlsfromdf2.getAB(AGG2, mechanisms.std_col(AGG2))

    A11 = np.hstack((A1test1.toarray(), b1test1.reshape(b1test1.size, 1)))
    A21 = np.hstack((A2test1.toarray(), b2test1.reshape(b2test1.size, 1)))
    A12 = np.hstack((A1test2.toarray(), b1test2.reshape(b1test2.size, 1)))
    A22 = np.hstack((A2test2.toarray(), b2test2.reshape(b2test2.size, 1)))

    assert compare_row_permuted_matrices(A11, A12)
    assert compare_row_permuted_matrices(A21, A22)

    assert compare_row_permuted_matrices(A11, np.hstack((A1, b1.reshape(b1.size, 1))))
    assert compare_row_permuted_matrices(A12, np.hstack((A1, b1.reshape(b1.size, 1))))
    assert compare_row_permuted_matrices(A21, np.hstack((A2, b2.reshape(b2.size, 1))))
    assert compare_row_permuted_matrices(A22, np.hstack((A2, b2.reshape(b2.size, 1))))


def test_reconstruction1(wls_df):
    wls_df2 = wls_df.copy()
    wls_df2[AGG1] = 1
    wls_df2[AGG2] = 2
    stdcol1 = mechanisms.std_col(AGG1)
    stdcol2 = mechanisms.std_col(AGG2)
    wls_df[stdcol1] = 1 + np.sqrt(wls_df[AGG1])
    wls_df[stdcol2] = 1 + np.sqrt(wls_df[AGG2])
    wls_df2[stdcol1] = 3
    universe = wls_df[["A", "B", "C", "D"]].copy()
    wlsfromdf = postprocess.WLSFromDF(universe)
    wlsfromdf.add_query(wls_df, None)
    wlsfromdf.add_query(wls_df2, None)
    wlsfromdf.add_target(AGG1, stdcol1)
    wlsfromdf.add_target(AGG2, stdcol2)
    expected_result = universe.copy()
    expected_result[AGG1] = (wls_df[AGG1] / (wls_df[stdcol1] ** 2) + wls_df2[AGG1] / (wls_df2[stdcol1] ** 2)) \
                             / (1 / (wls_df[stdcol1] ** 2) + 1 / (wls_df2[stdcol1] ** 2))
    expected_result[AGG2] = (wls_df[AGG2] / (wls_df[stdcol2] ** 2) + wls_df2[AGG2]) \
                             / (1 / (wls_df[stdcol2] ** 2) + 1)
    wlsfromdf.solve()
    got_result = wlsfromdf.universe
    assert are_dfs_equal(got_result, expected_result, digits=4)


def test_reconstruction2(wls_df, prng):
    universe = wls_df[["A", "B", "C", "D"]].copy()
    wlsfromdf = postprocess.WLSFromDF(universe)
    stdcol1 = mechanisms.std_col(AGG1)
    stdcol2 = mechanisms.std_col(AGG2)
    wlsfromdf.add_target(AGG1, stdcol1)
    wlsfromdf.add_target(AGG2, stdcol2)
    tmpdf = mechanisms.sqrt_sum_mech(prng, wls_df, None, [AGG1, AGG2], [0.0, 0.0],True)
    tmpdf[stdcol1] = 1+prng.random()
    tmpdf[stdcol2] = 1+prng.random()
    wlsfromdf.add_query(tmpdf, None)
    tmpdf = mechanisms.sqrt_sum_mech(prng, wls_df, ["B"], [AGG1, AGG2], [0.0, 0.0])
    tmpdf[stdcol1] = 1 + prng.random()
    tmpdf[stdcol2] = 1 + prng.random()
    wlsfromdf.add_query(tmpdf, ["B"])
    tmpdf = mechanisms.sqrt_sum_mech(prng, wls_df, ["C", "D"], [AGG1, AGG2], [0.0, 0.0])
    tmpdf[stdcol1] = 1 + prng.random()
    tmpdf[stdcol2] = 1 + prng.random()
    wlsfromdf.add_query(tmpdf, ["C", "D"])
    tmpdf = mechanisms.sqrt_sum_mech(prng, wls_df, ["C", "A"], [AGG1, AGG2], [0.0, 0.0])
    tmpdf[stdcol1] = 1 + prng.random()
    tmpdf[stdcol2] = 1 + prng.random()
    wlsfromdf.add_query(tmpdf, ["A", "C"])
    tmpdf = mechanisms.sqrt_sum_mech(prng, wls_df, None, [AGG1, AGG2], [0.0, 0.0])
    tmpdf[stdcol1] = 1 + prng.random()
    tmpdf[stdcol2] = 1 + prng.random()
    wlsfromdf.add_query(tmpdf, None)
    tmpdf = mechanisms.sqrt_sum_mech(prng, wls_df, ["C", "A", "D"], [AGG1, AGG2], [0.0, 0.0])
    tmpdf[stdcol1] = 1 + prng.random()
    tmpdf[stdcol2] = 1 + prng.random()
    wlsfromdf.add_query(tmpdf, ["A", "C", "D"])
    wlsfromdf.solve()
    wls_df[AGG1] += 0.0
    assert are_dfs_equal(wls_df, wlsfromdf.universe, digits=4)


def test_reconstruction3(wls_df):
    universe = wls_df[["A", "B", "C", "D"]].copy()
    wlsfromdf = postprocess.WLSFromDF(universe)
    stdcol1 = mechanisms.std_col(AGG1)
    stdcol2 = mechanisms.std_col(AGG2)
    identity = universe.copy()
    identity[AGG1] = list(range(1, 17))
    identity[AGG2] = list(reversed(range(1, 17)))
    identity[stdcol1] = 1
    identity[stdcol2] = np.sqrt(identity[AGG2])
    g1 = pd.DataFrame({"B": [0, 1], AGG1: [10, 20]})
    g2 = pd.DataFrame({AGG2: [15], stdcol2: [2]})
    wlsfromdf.add_target(AGG1, stdcol1)
    wlsfromdf.add_target(AGG2, stdcol2)
    wlsfromdf.add_query(identity, None)
    wlsfromdf.add_query(g1, ["B"])
    wlsfromdf.add_query(g2, None)
    wlsfromdf.solve()
    a1 = solv_id_sum(10, 1, list(identity[AGG1])[0:8], list(identity[stdcol1])[0:8])
    a2 = solv_id_sum(20, 1, list(identity[AGG1])[8:16], list(identity[stdcol1])[8:16])
    b = solv_id_sum(15, 2, list(identity[AGG2]), list(identity[stdcol2]))
    expected = universe.copy()
    expected[AGG1] = np.concatenate((a1, a2))
    expected[AGG2] = b
    assert are_dfs_equal(expected, wlsfromdf.universe, digits=4)



def solv_id_sum(sumq, sumstd, idq, idstd):
    """ solves the weighted least squares problem of a sum query sumq with std sumstd
    and identity query idq (list) with std list (ndarray)
    """
    sumv1 = sumstd**2
    sumv2 = sum(x**2 for x in idstd)
    newsum = (sumq/sumv1 + sum(idq)/sumv2) / (1/sumv1 + 1/sumv2)
    ndiff = (sumq - newsum)/sumv1
    newx = np.array([ndiff * std**2 + ans for (ans, std) in zip(idq, idstd)])
    return newx
