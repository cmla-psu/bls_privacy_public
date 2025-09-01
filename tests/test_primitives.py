import mechanisms.__primitives as prim
import numpy as np
import pandas as pd
import pytest
from tests.helper import are_dfs_equal

DEVELOPING_TEST = False  # set this to false once done creating test

# DFSIZE = 36
DFSIZE = 4000000
SEED = 3
GCOL1 = "Gcol1"
GCOL2 = "Gcol2"
AGGCOL1 = "Aggcol1"
AGGCOL2 = "Aggcol2"
CLIP1A = "Clip1A"
CLIP1B = "Clip1B"
CLIP1ALL = "Clip1All"
CLIP2A = "Clip2A"
CLIP2B = "Clip2B"
CLIP2ALL = "Clip2All"
INDIVIDUAL = "Individual"
COUNTCOL = "Countcol"
HALF = "Half"
LOWHALF = 1
HIGHHALF = 10


def std_formula(column, sqrt_std):
    """ This is the std of the postprocessed answer to the square root
    mechanism as a function of its noisy value and std of gaussian noise
    added to the square root

    Inputs:
        column: a view of the df column,
        sqrt_std: the std of noise added to the square root, can be a df column
    """
    return np.sqrt(2 * np.power(sqrt_std, 4) + np.maximum(0, column * (4 * sqrt_std ** 2)))


@pytest.fixture
def prng():
    return np.random.default_rng(SEED)


@pytest.fixture
def dup_prng():
    """ Returns two prngs, seeded at the same seed """
    return np.random.default_rng(SEED), np.random.default_rng(SEED)


@pytest.fixture
def tmp_df():
    """ Creates a dataframe for testing with columns held in the variables:
    GCOL1: a 0/1 integer column that can be used for grouping
    GCOL2: an "a"/"b" string column that can be used for grouping
    INDIVIDUAL: a sequential numbering of the rows
    COUNTCOL: a column full of 1s, allows a group by sum query and count query to give
              same answer
    AGGCOL1: an integer column for aggregation
    AGGCOL2: a float column for aggregation
    HALF:  rows for which INDIVIDUAL is even equal LOWHALF and otherwise HIGHHALF
    CLIP1A: topcode thresholds to be used for AGGCOL1 based on GCOL1 and GCOL2
    CLIP2A: topcode thresholds to be used for AGGCOL2 based on GCOL1 and GCOL2
    CLIP1B: topcode thresholds to be used for AGGCOL1 based on  GCOL1
    CLIP2B: topcode thresholds to be used for AGGCOL2 based on  GCOL1
    CLIP1ALL: global topcode thresholds to be used for AGGCOL1
    CLIP2ALL: global topcode thresholds to be used for AGGCOL2

    """
    dfdata = dict()
    dfdata[INDIVIDUAL] = range(DFSIZE)
    dfdata[GCOL1] = [1 if (x % 3) <= 1 else 0 for x in range(DFSIZE)]
    dfdata[GCOL2] = ["a" if (x % 4) <= 2 else "b" for x in range(DFSIZE)]
    dfdata[COUNTCOL] = [1 for _ in range(DFSIZE)]
    mydf = pd.DataFrame(dfdata)
    mydf[AGGCOL1] = 0
    mydf[AGGCOL2] = 0.0
    mydf[CLIP1A] = 0.0
    mydf[CLIP2A] = 0.0
    mydf[CLIP1B] = 0.0
    mydf[CLIP2B] = 0.0
    cycle = 3
    base = 10
    clip_all1 = 17
    clip_all2 = 10
    for g1 in [0, 1]:
        for g2 in ["a", "b"]:
            nrows = mydf.loc[(mydf[GCOL1] == g1) & (mydf[GCOL2] == g2)].shape[0]
            mydf.loc[(mydf[GCOL1] == g1) & (mydf[GCOL2] == g2), AGGCOL1] = [base + (x % cycle) for x in range(nrows)]
            mydf.loc[(mydf[GCOL1] == g1) & (mydf[GCOL2] == g2), AGGCOL2] = [base / 2 + 0.5 + (x % cycle) for x in
                                                                            range(nrows)]
            theclip1a = base + cycle / 2
            theclip2a = base / 2 + 0.5 + cycle / 2
            mydf.loc[(mydf[GCOL1] == g1) & (mydf[GCOL2] == g2), CLIP1A] = theclip1a
            mydf.loc[(mydf[GCOL1] == g1) & (mydf[GCOL2] == g2), CLIP2A] = theclip2a
            base *= 1.5
        mydf.loc[(mydf[GCOL1] == g1), CLIP1B] = base
        mydf.loc[(mydf[GCOL1] == g1), CLIP2B] = base / 2
    mydf[HALF] = (mydf[INDIVIDUAL] % 2) * (HIGHHALF - LOWHALF) + LOWHALF
    mydf[CLIP1ALL] = clip_all1
    mydf[CLIP2ALL] = clip_all2
    return mydf


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
def test_gaussian_noise(prng):
    std = 3.5
    g1 = prim.gaussian_noise(prng, std, None)
    assert isinstance(g1, float) or isinstance(g1, np.float64)
    g2 = prim.gaussian_noise(prng, std, 1000000)
    assert np.abs(np.std(g2) - std) <= 0.01


####################################
# Test the Square root mechanisms ##
####################################


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
@pytest.mark.parametrize("theagg", ["max", "sum"])
def test_sqrt_summax_mechanism_nonoise1(prng, tmp_df, theagg):
    """ Test that square root max/sum mechanism works correctly when no noise"""
    mechanism = prim.sqrt_max_mech if theagg == "max" else prim.sqrt_sum_mech
    std = 0.0
    agg = tmp_df.groupby(by=[GCOL2], as_index=False).agg({AGGCOL2: theagg, AGGCOL1: theagg})
    agg[AGGCOL1] += 0.0  # to make it a float
    agg[prim.sqrt_col(AGGCOL1)] = np.sqrt(agg[AGGCOL1])
    agg[prim.sqrt_col(AGGCOL2)] = np.sqrt(agg[AGGCOL2])
    agg[prim.std_col(AGGCOL1)] = std
    agg[prim.std_col(AGGCOL2)] = std
    cmp_agg = mechanism(prng, tmp_df, [GCOL2], [AGGCOL1, AGGCOL2], [std, std])
    cmp_agg2 = prim.__sqrt_mechanism(prng, tmp_df, [GCOL2], [AGGCOL1, AGGCOL2], [std, std], theagg)
    assert are_dfs_equal(agg, cmp_agg, digits=8, ordering=False)
    assert are_dfs_equal(agg, cmp_agg2, digits=8, ordering=False)


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
@pytest.mark.parametrize("theagg", ["max", "sum"])
def test_sqrt_summax_mechanism_nonoise2(prng, tmp_df, theagg):
    """ Test that square root sum/max mechanism works correctly when no noise"""
    mechanism = prim.sqrt_max_mech if theagg == "max" else prim.sqrt_sum_mech
    std = 0.0
    agg = tmp_df.groupby(by=[GCOL2, GCOL1], as_index=False).agg({AGGCOL2: theagg})
    agg[prim.sqrt_col(AGGCOL2)] = np.sqrt(agg[AGGCOL2])
    agg[prim.std_col(AGGCOL2)] = std
    cmp_agg = mechanism(prng, tmp_df, [GCOL2, GCOL1], [AGGCOL2], [std])
    cmp_agg2 = prim.__sqrt_mechanism(prng, tmp_df, [GCOL2, GCOL1], [AGGCOL2], [std], theagg)
    assert are_dfs_equal(agg, cmp_agg, ordering=False)
    assert are_dfs_equal(agg, cmp_agg2, ordering=False)


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
@pytest.mark.parametrize("theagg", ["max", "sum"])
def test_sqrt_summax_mechanism_yesnoise1(dup_prng, tmp_df, theagg):
    """ Test that the agg col can be computed from its square root col and std"""
    mechanism = prim.sqrt_max_mech if theagg == "max" else prim.sqrt_sum_mech
    std1 = 3.5
    std2 = 4.6
    tolerance = 1e-10
    prng1, prng2 = dup_prng
    cmp_agg = mechanism(prng1, tmp_df, [GCOL2, GCOL1], [AGGCOL2, AGGCOL1], [std2, std1])
    cmp_agg2 = prim.__sqrt_mechanism(prng2, tmp_df, [GCOL2, GCOL1], [AGGCOL2, AGGCOL1], [std2, std1], theagg)
    est_std2 = std_formula(cmp_agg[AGGCOL2], std2)
    est_std1 = std_formula(cmp_agg[AGGCOL1], std1)
    assert np.sum(np.abs(cmp_agg[prim.std_col(AGGCOL2)] - est_std2)) <= tolerance
    assert np.sum(np.abs(cmp_agg2[prim.std_col(AGGCOL2)] - est_std2)) <= tolerance
    assert np.sum(np.abs(cmp_agg[prim.std_col(AGGCOL1)] - est_std1)) <= tolerance
    assert np.sum(np.abs(cmp_agg2[prim.std_col(AGGCOL1)] - est_std1)) <= tolerance
    expected_sqrt1 = (cmp_agg[prim.sqrt_col(AGGCOL1)] ** 2) - std1 ** 2
    assert np.sum(np.abs(cmp_agg[AGGCOL1] - expected_sqrt1)) <= tolerance
    assert np.sum(np.abs(cmp_agg2[AGGCOL1] - expected_sqrt1)) <= tolerance
    expected_sqrt2 = (cmp_agg[prim.sqrt_col(AGGCOL2)] ** 2) - std2 ** 2
    assert np.sum(np.abs(cmp_agg[AGGCOL2] - expected_sqrt2)) <= tolerance
    assert np.sum(np.abs(cmp_agg2[AGGCOL2] - expected_sqrt2)) <= tolerance


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
@pytest.mark.parametrize("theagg", ["max", "sum"])
def test_sqrt_summax_mechanism_yesnoise2(dup_prng, tmp_df, theagg):
    """ Test that the estimated std is close to the actual std"""
    mechanism = prim.sqrt_max_mech if theagg == "max" else prim.sqrt_sum_mech
    std = 0.5
    prng1, prng2 = dup_prng
    tolerance = 1e-2
    cmp_agg = mechanism(prng1, tmp_df, [INDIVIDUAL], [HALF], [std])
    cmp_agg2 = prim.__sqrt_mechanism(prng2, tmp_df, [INDIVIDUAL], [HALF], [std], theagg)
    act_std_odd = std_formula(list(tmp_df.loc[tmp_df[INDIVIDUAL] == 1, HALF])[0], std)
    act_std_even = std_formula(list(tmp_df.loc[tmp_df[INDIVIDUAL] == 2, HALF])[0], std)
    assert np.abs(np.std(cmp_agg.loc[cmp_agg[INDIVIDUAL] % 2 == 1, HALF]) - act_std_odd) <= tolerance
    assert np.abs(np.std(cmp_agg.loc[cmp_agg[INDIVIDUAL] % 2 == 0, HALF]) - act_std_even) <= tolerance
    assert np.abs(np.std(cmp_agg2.loc[cmp_agg2[INDIVIDUAL] % 2 == 1, HALF]) - act_std_odd) <= tolerance
    assert np.abs(np.std(cmp_agg2.loc[cmp_agg2[INDIVIDUAL] % 2 == 0, HALF]) - act_std_even) <= tolerance


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
@pytest.mark.parametrize("theagg", ["max", "sum"])
def test_sqrt_summax_mechanism_identity_nonoise1(prng, tmp_df, theagg):
    """ Test that square root sum/max mechanism works correctly when no noise"""
    mechanism = prim.sqrt_max_mech if theagg == "max" else prim.sqrt_sum_mech
    std = 0.0
    tmp_df_small = tmp_df.iloc[0:1000]  # because checking large dfs for equality is slow
    agg = tmp_df_small[[GCOL2, GCOL1] + [AGGCOL2, AGGCOL1]].copy()
    agg[AGGCOL1] += 0.0  # to make it a float
    agg[prim.sqrt_col(AGGCOL1)] = np.sqrt(agg[AGGCOL1])
    agg[prim.sqrt_col(AGGCOL2)] = np.sqrt(agg[AGGCOL2])
    agg[prim.std_col(AGGCOL1)] = std
    agg[prim.std_col(AGGCOL2)] = std
    cmp_agg = mechanism(prng, tmp_df_small, [GCOL2, GCOL1],
                        [AGGCOL1, AGGCOL2], [std, std], True)
    cmp_agg2 = prim.__sqrt_mechanism(prng, tmp_df_small, [GCOL2, GCOL1],
                                     [AGGCOL1, AGGCOL2], [std, std], theagg, True)
    assert are_dfs_equal(agg, cmp_agg, ordering=False)
    assert are_dfs_equal(agg, cmp_agg2, ordering=False)


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
@pytest.mark.parametrize("theagg", ["max", "sum"])
def test_sqrt_summax_mechanism_identity_yesnoise1(dup_prng, tmp_df, theagg):
    """ Test that the estimated std is close to the actual std in identity"""
    mechanism = prim.sqrt_max_mech if theagg == "max" else prim.sqrt_sum_mech
    std = 0.5
    prng1, prng2 = dup_prng
    tolerance = 1e-2
    cmp_agg = mechanism(prng1, tmp_df, [GCOL1], [COUNTCOL], [std], True)
    cmp_agg2 = prim.__sqrt_mechanism(prng2, tmp_df, [GCOL1], [COUNTCOL], [std], theagg, True)
    act_std = std_formula(tmp_df.iloc[0][COUNTCOL], std)
    assert np.abs(np.std(cmp_agg[COUNTCOL]) - act_std) <= tolerance
    assert np.abs(np.std(cmp_agg2[COUNTCOL]) - act_std) <= tolerance


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
@pytest.mark.parametrize("theagg,group_cols", [("max", None), ("max", []), ("sum", None), ("sum", [])])
def test_sqrt_summax_mechanism_overall_nonoise(dup_prng, tmp_df, theagg, group_cols):
    """ Test that the overall aggregation is correct, trying both [] and None for group_cols"""
    mechanism = prim.sqrt_max_mech if theagg == "max" else prim.sqrt_sum_mech
    std = 0.0
    prng1, prng2 = dup_prng
    # agg on normal dataframe returns series, we need to convert to df
    tmpagg = dict(tmp_df[[AGGCOL1, AGGCOL2]].agg(theagg))
    for (k, v) in tmpagg.items():
        tmpagg[k] = [v + 0.0]  # make each singleton into a list, so we can convert to df
    agg = pd.DataFrame(tmpagg)
    agg[prim.sqrt_col(AGGCOL1)] = np.sqrt(agg[AGGCOL1])
    agg[prim.std_col(AGGCOL1)] = std
    agg[prim.sqrt_col(AGGCOL2)] = np.sqrt(agg[AGGCOL2])
    agg[prim.std_col(AGGCOL2)] = std
    cmp_agg1 = mechanism(prng1, tmp_df, group_cols, [AGGCOL1, AGGCOL2], [std, std])
    cmp_agg2 = prim.__sqrt_mechanism(prng2, tmp_df, group_cols, [AGGCOL1, AGGCOL2], [std, std], theagg)
    assert are_dfs_equal(agg, cmp_agg1, ordering=False, digits=9)
    assert are_dfs_equal(agg, cmp_agg2, ordering=False, digits=9)


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
@pytest.mark.parametrize("theagg,group_cols", [("max", None), ("max", []), ("sum", None), ("sum", [])])
def test_sqrt_summax_mechanism_overall_yesnoise(dup_prng, tmp_df, theagg, group_cols):
    """ Test that noise is added and square root columns are correct"""
    mechanism = prim.sqrt_max_mech if theagg == "max" else prim.sqrt_sum_mech
    std1 = 10  # lots of noise, first two digits should be different
    std2 = 20
    tolerance = 1e-6
    prng1, prng2 = dup_prng
    # agg on normal dataframe returns series, we need to convert to df
    tmpagg = dict(tmp_df[[AGGCOL1, AGGCOL2]].agg(theagg))
    for (k, v) in tmpagg.items():
        tmpagg[k] = [v + 0.0]  # make each singleton into a list, so we can convert to df
    agg = pd.DataFrame(tmpagg)
    cmp_agg1 = mechanism(prng1, tmp_df, group_cols, [AGGCOL1, AGGCOL2], [std1, std2])
    cmp_agg2 = prim.__sqrt_mechanism(prng2, tmp_df, group_cols, [AGGCOL1, AGGCOL2], [std1, std2], theagg)
    assert not are_dfs_equal(agg, cmp_agg1, ordering=False, digits=2)
    assert not are_dfs_equal(agg, cmp_agg2, ordering=False, digits=2)
    # check the square root stuff
    est_std1 = std_formula(cmp_agg1.iloc[0][AGGCOL1], std1)
    est_std2 = std_formula(cmp_agg1.iloc[0][AGGCOL2], std2)
    assert np.abs(cmp_agg1.iloc[0][prim.std_col(AGGCOL1)] - est_std1) <= tolerance
    assert np.abs(cmp_agg1.iloc[0][prim.std_col(AGGCOL2)] - est_std2) <= tolerance
    assert np.abs(cmp_agg2.iloc[0][prim.std_col(AGGCOL1)] - est_std1) <= tolerance
    assert np.abs(cmp_agg2.iloc[0][prim.std_col(AGGCOL2)] - est_std2) <= tolerance
    assert np.abs(cmp_agg1.iloc[0][AGGCOL1] - (cmp_agg1.iloc[0][prim.sqrt_col(AGGCOL1)] ** 2 - std1 ** 2)) <= tolerance
    assert np.abs(cmp_agg1.iloc[0][AGGCOL2] - (cmp_agg1.iloc[0][prim.sqrt_col(AGGCOL2)] ** 2 - std2 ** 2)) <= tolerance
    assert np.abs(cmp_agg2.iloc[0][AGGCOL1] - (cmp_agg2.iloc[0][prim.sqrt_col(AGGCOL1)] ** 2 - std1 ** 2)) <= tolerance
    assert np.abs(cmp_agg2.iloc[0][AGGCOL2] - (cmp_agg2.iloc[0][prim.sqrt_col(AGGCOL2)] ** 2 - std2 ** 2)) <= tolerance


####################################
# Test the Square root mechanisms ##
####################################

@pytest.fixture(scope="module", params=["A", "B", "ALL", "ONE"])  # "ONE" is a topcode of 1
def clip_type(request):
    """ This fixture allows us to go through the different clipping types """
    return request.param


@pytest.fixture
def doclip(tmp_df, clip_type):
    """ Preclips a copy of the df, and returns the clipping dictionaries for aggcol1 and aggcol2 """
    clipdict1 = {(0, "a"): None, (0, "b"): None, (1, "a"): None, (1, "b"): None}
    clipdict2 = {(0, "a"): None, (0, "b"): None, (1, "a"): None, (1, "b"): None}
    mydf = tmp_df.copy()
    if clip_type == "A":
        mydf[AGGCOL1] = np.minimum(mydf[AGGCOL1], mydf[CLIP1A])
        mydf[AGGCOL2] = np.minimum(mydf[AGGCOL2], mydf[CLIP2A])
        hint = mydf.groupby(by=[GCOL1, GCOL2], as_index=False).agg({CLIP1A: "max", CLIP2A: "max"})
        for i in range(4):
            row = hint.iloc[i]
            clipdict1[(row[GCOL1], row[GCOL2])] = row[CLIP1A]
            clipdict2[(row[GCOL1], row[GCOL2])] = row[CLIP2A]
    elif clip_type == "B":
        mydf[AGGCOL1] = np.minimum(mydf[AGGCOL1], mydf[CLIP1B])
        mydf[AGGCOL2] = np.minimum(mydf[AGGCOL2], mydf[CLIP2B])
        hint = mydf.groupby(by=[GCOL1, GCOL2], as_index=False).agg({CLIP1B: "max", CLIP2B: "max"})
        for i in range(4):
            row = hint.iloc[i]
            clipdict1[(row[GCOL1], row[GCOL2])] = row[CLIP1B]
            clipdict2[(row[GCOL1], row[GCOL2])] = row[CLIP2B]
    elif clip_type == "ALL":
        mydf[AGGCOL1] = np.minimum(mydf[AGGCOL1], mydf[CLIP1ALL])
        mydf[AGGCOL2] = np.minimum(mydf[AGGCOL2], mydf[CLIP2ALL])
        maxseries = mydf.iloc[0][[CLIP1ALL, CLIP2ALL]]
        for k in clipdict1:
            clipdict1[k] = maxseries[CLIP1ALL]
            clipdict2[k] = maxseries[CLIP2ALL]
    elif clip_type == "ONE":
        mydf[AGGCOL1] = np.minimum(mydf[AGGCOL1], 1)
        mydf[AGGCOL2] = np.minimum(mydf[AGGCOL2], 1)
        for k in clipdict1:
            clipdict1[k] = 1
            clipdict2[k] = 1
    else:
        raise (Exception(f"unknown clip type {clip_type} in pytest fixture doclip"))
    return mydf, lambda x, y: clipdict1[(x, y)], lambda x, y: clipdict2[(x, y)]


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
def test_topcode_mechanism_nonoise1(prng, tmp_df, doclip):
    """ Test that topcode mechanism works correctly when no noise"""
    clip_df, clipfun1, clipfun2 = doclip
    std = 0.0
    agg = clip_df.groupby(by=[GCOL1, GCOL2], as_index=False).agg({AGGCOL2: sum, AGGCOL1: sum})
    agg[AGGCOL1] += 0.0  # to make it a float
    agg[prim.std_col(AGGCOL1)] = std
    agg[prim.std_col(AGGCOL2)] = std
    cmp_agg = prim.topcode_gmech(prng, tmp_df, [GCOL1, GCOL2], [AGGCOL1, AGGCOL2],
                                 [clipfun1, clipfun2], [lambda x: std, lambda x: std])
    assert are_dfs_equal(agg, cmp_agg, digits=8, ordering=False)


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
def test_topcode_mechanism_nonoise2(prng, tmp_df, doclip):
    """ Test that topcode mechanism works correctly when no noise"""
    clip_df, clipfun1, clipfun2 = doclip
    std = 0.0
    agg = clip_df.groupby(by=[GCOL2, GCOL1], as_index=False).agg({AGGCOL2: sum})
    agg[prim.std_col(AGGCOL2)] = std
    cmp_agg = prim.topcode_gmech(prng, tmp_df, [GCOL2, GCOL1], [AGGCOL2],
                                 [lambda x, y: clipfun2(y, x)], [lambda x: std])
    assert are_dfs_equal(agg, cmp_agg, ordering=False)


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
def test_topcode_mechanism_yesnoise1(dup_prng, tmp_df, doclip):
    """ Test that the std col is correct """
    clip_df, clipfun1, clipfun2 = doclip
    std1 = 3.5
    std2 = 4.6
    prng1, prng2 = dup_prng
    cmp_agg = prim.topcode_gmech(prng1, tmp_df, [GCOL1, GCOL2], [AGGCOL2, AGGCOL1],
                                 [clipfun2, clipfun1], [lambda x: x*std2, lambda x: x*std1+3])
    for i in range(cmp_agg.shape[0]):
        row = cmp_agg.iloc[i]
        g1 = row[GCOL1]
        g2 = row[GCOL2]
        assert row[prim.std_col(AGGCOL2)] == pytest.approx(std2 * clipfun2(g1, g2))
        assert row[prim.std_col(AGGCOL1)] == pytest.approx(std1 * clipfun1(g1, g2) + 3)


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
@pytest.mark.parametrize("topcode", [0.9, 5])
def test_topcode_mechanism_yesnoise2(prng, tmp_df, topcode):
    """ Test that the estimated std is close to the actual std"""
    std = 5
    tolerance = 1e-2
    cmp_agg = prim.topcode_gmech(prng, tmp_df, [INDIVIDUAL], [HALF],
                                 [lambda x: topcode], [lambda x: np.sqrt(x) * std])
    # Compute the std for the high values of HALF (at odd INDIVIDUAL) and the low values of HALF (at even indexes)
    exp_std = std * np.sqrt(topcode)
    assert np.abs(np.std(cmp_agg.loc[cmp_agg[INDIVIDUAL] % 2 == 1, HALF]) - exp_std) / exp_std <= tolerance
    assert np.abs(np.std(cmp_agg.loc[cmp_agg[INDIVIDUAL] % 2 == 0, HALF]) - exp_std) / exp_std <= tolerance
    assert np.sum(np.abs(cmp_agg[prim.std_col(HALF)] - exp_std)) == pytest.approx(0.0)


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
def test_topcode_mechanism_identity_nonoise1(prng, tmp_df, doclip):
    """ Test that identity topcode mechanism works correctly when no noise"""
    std = 0.0
    smallrows = 1000
    tmp_df_small = tmp_df.iloc[0:smallrows]  # because checking large dfs for equality is slow
    clip_df, clipfun1, clipfun2 = doclip
    clip_df_small = clip_df.iloc[0:smallrows]
    agg = clip_df_small[[GCOL2, GCOL1] + [AGGCOL2, AGGCOL1]].copy()
    agg[AGGCOL1] += 0.0  # to make it a float
    agg[prim.std_col(AGGCOL1)] = std  # is 0
    agg[prim.std_col(AGGCOL2)] = std  # is 0
    cmp_agg = prim.topcode_gmech(prng, tmp_df_small, [GCOL2, GCOL1], [AGGCOL1, AGGCOL2],
                                 [lambda x, y: clipfun1(y, x), lambda x, y: clipfun2(y, x)],
                                 [lambda x: std, lambda x: std], identity=True)
    cmp_agg2 = prim.topcode_gmech(prng, tmp_df_small, [GCOL2, GCOL1], [AGGCOL1, AGGCOL2],
                                  [lambda x, y: clipfun2(y, x), lambda x, y: clipfun1(y, x)],
                                  [lambda x: std, lambda x: std])
    assert are_dfs_equal(agg, cmp_agg, ordering=False)
    assert not are_dfs_equal(agg, cmp_agg2, ordering=False)


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
@pytest.mark.parametrize("topcode", [0.9, 5])
def test_topcode_mechanism_identity_yesnoise1(prng, tmp_df, topcode):
    """ Test that the estimated std is close to the actual std in identity"""
    std = 3
    tolerance = 1e-2
    cmp_agg = prim.topcode_gmech(prng, tmp_df, [GCOL1], [COUNTCOL],
                                 [lambda x: topcode], [lambda x: np.sqrt(x)*std], True)
    act_std = np.sqrt(topcode) * std
    assert np.abs(np.std(cmp_agg[COUNTCOL]) - act_std) <= tolerance
    assert np.sum(np.abs(cmp_agg[prim.std_col(COUNTCOL)] - act_std)) == pytest.approx(0.0)


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
@pytest.mark.parametrize("group_cols", [None, []])
def test_topcode_mechanism_overall_nonoise(prng, tmp_df, group_cols):
    """ Test that the overall aggregation is correct, trying both [] and None for group_cols"""
    std = 0.0
    clip_df = tmp_df.copy()
    topcode1 = np.median(list(clip_df[AGGCOL1]))
    topcode2 = np.median(list(clip_df[AGGCOL2]))
    clip_df[AGGCOL1] = np.minimum(clip_df[AGGCOL1], topcode1)
    clip_df[AGGCOL2] = np.minimum(clip_df[AGGCOL2], topcode2)
    # agg on normal dataframe returns series, we need to convert to df
    tmpagg = dict(clip_df[[AGGCOL1, AGGCOL2]].agg("sum"))
    for (k, v) in tmpagg.items():
        tmpagg[k] = [v + 0.0]  # make each singleton into a list, so we can convert to df
    agg = pd.DataFrame(tmpagg)
    agg[prim.std_col(AGGCOL1)] = std  # is 0
    agg[prim.std_col(AGGCOL2)] = std  # is 0
    cmp_agg1 = prim.topcode_gmech(prng, tmp_df, group_cols, [AGGCOL1, AGGCOL2],
                                  [lambda: topcode1, lambda: topcode2],
                                  [lambda x: std, lambda x:std])
    assert are_dfs_equal(agg, cmp_agg1, ordering=False, digits=9)


@pytest.mark.skipif(DEVELOPING_TEST, reason="to speed up test development")
@pytest.mark.parametrize("group_cols", [None, []])
def test_topcode_mechanism_overall_yesnoise(prng, tmp_df, group_cols):
    """ Test that the std in overall aggregation is correct, trying both [] and None for group_cols"""
    std1 = 5
    std2 = 8
    clip_df = tmp_df.copy()
    topcode1 = np.median(list(clip_df[AGGCOL1]))
    topcode2 = np.median(list(clip_df[AGGCOL2]))
    cmp_agg = prim.topcode_gmech(prng, tmp_df, group_cols, [AGGCOL1, AGGCOL2],
                                 [lambda: topcode1, lambda: topcode2],
                                 [lambda x: np.sqrt(x) * std1, lambda x: x*std2])
    assert np.sum(np.abs(cmp_agg[prim.std_col(AGGCOL1)] - std1 * np.sqrt(topcode1))) == pytest.approx(0.0)
    assert np.sum(np.abs(cmp_agg[prim.std_col(AGGCOL2)] - std2 * topcode2)) == pytest.approx(0.0)
