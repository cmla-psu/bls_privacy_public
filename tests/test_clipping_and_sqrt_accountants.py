from scipy.stats import norm as gauss_info
import numpy as np
import pytest
import pandas as pd
import functools
import csv
from readers.__basereader import BaseReader
from accountant import ClippingAccountant, SQRTAccountant
from accountant.__configreader import NAME, GROUP, MU
import mechanisms
from .test_primitives import std_formula

A = "A"
B = "B"
C = "C"
D = "D"
X = "X"
Y = "Y"
Z = "Z"
X_DISTANCE = 1
Y_DISTANCE = 2
Z_DISTANCE = 3
CLIPPING_PROB = 0.01
A_DIM = 2
B_DIM = 3
C_DIM = 5
D_DIM = 7
X_MU = 5
Y_MU = 7
Z_MU = 9


class MyReader(BaseReader):

    @property
    def reading_schema(self):
        return {A: str, B: str, C: str, D: str, X: int, Y: int, Z: int}

    @property
    def project_cols(self):
        return [A, B, C, D, X, Y, Z]

    @property
    def public_cols(self):
        return [A, B, C, D]

    @property
    def private_cols(self):
        return [X, Y, Z]


@pytest.fixture
def datareader(tmp_path):
    dims = (A_DIM, B_DIM, C_DIM, D_DIM)
    public_names = (A, B, C, D)
    private_names = (X, Y, Z)
    private_base = (1, 10, 100)
    df_size = functools.reduce(lambda x, y: x * y, dims)
    df_dict = {}
    for dimsize, thename in zip(dims, public_names):
        df_dict[thename] = [f"{thename}{x % dimsize}" for x in range(df_size)]
    for base, pname in zip(private_base, private_names):
        df_dict[pname] = [x * base for x in range(df_size)]
    mydf = pd.DataFrame(df_dict)
    filebase = "test.csv"
    outputfilename = tmp_path.joinpath(filebase)
    mydf.to_csv(outputfilename, index=False, quoting=csv.QUOTE_NONNUMERIC)
    myreader = MyReader()
    myreader.read(outputfilename)
    return myreader


@pytest.fixture
def my_clip_accountant_info(datareader):
    seed = 2
    prng = mechanisms.make_prng(seed)
    distances = {X: X_DISTANCE, Y: Y_DISTANCE, Z: Z_DISTANCE}
    clipping_prob = CLIPPING_PROB
    the_accountant = ClippingAccountant(prng, datareader, distances, clipping_prob)
    return the_accountant, datareader


@pytest.fixture
def my_sqrt_accountant_info(datareader):
    seed = 2
    prng = mechanisms.make_prng(seed)
    distances = {X: X_DISTANCE, Y: Y_DISTANCE, Z: Z_DISTANCE}
    the_accountant = SQRTAccountant(prng, datareader, distances)
    return the_accountant, datareader



def test_gauss_max():
    numitems_list = [1, 100, 1000, 1_000_000, 1_000_000]
    clipping_prob_list = [0.1, 0.01, 0.01, 0.01, 0.0001]
    for numitems, prob in zip(numitems_list, clipping_prob_list):
        c = ClippingAccountant.get_gauss_max(numitems, prob)
        assert gauss_info.logcdf(c) * numitems >= np.log(1 - prob)
        assert gauss_info.logcdf(c) * numitems <= np.log(1 - prob * 0.99)


def test_clipping_accountant_error_checks1(my_clip_accountant_info):
    """ Group query (sum query) uses sensitive attribute not used by identity query, so its clipping
    values cannot be determined """
    my_clip_accountant, the_reader = my_clip_accountant_info  # get the accountant
    identity_mu_dict = {X: 3, Y: 5}  # no Z
    group_queries = [
        {NAME: "test1",
         GROUP: None,
         MU: {Z: 1}}
    ]
    my_clip_accountant.measure_identity(identity_mu_dict)
    with pytest.raises(AssertionError) as excinfo:
        my_clip_accountant.measure_groups(group_queries)
    assert "more private attributes than identity" in str(excinfo)


def test_clipping_accountant_error_checks2(my_clip_accountant_info):
    """ Group query (not sum query) uses sensitive attribute not used by identity query, so its clipping
        values cannot be determined """
    my_clip_accountant, the_reader = my_clip_accountant_info  # get the accountant
    identity_mu_dict = {X: 3, Y: 5}  # no Z
    group_queries = [
        {NAME: "test1",
         GROUP: [Z],
         MU: {X: 1}}
    ]
    my_clip_accountant.measure_identity(identity_mu_dict)
    with pytest.raises(Exception):
        my_clip_accountant.measure_groups(group_queries)


def test_clipping_accountant_error_checks3(my_clip_accountant_info):
    """ No identity query called before trying to measure group query, so clipping thresholds
    cannot be set """
    my_clip_accountant, the_reader = my_clip_accountant_info  # get the accountant
    group_queries = [
        {NAME: "test1",
         GROUP: [A],
         MU: {X: 1}}
    ]
    with pytest.raises(Exception):
        my_clip_accountant.measure_groups(group_queries)


def test_clipping_accountant_error_checks4(my_clip_accountant_info):
    """ Check that no exception is raised for proper queries """
    my_clip_accountant, the_reader = my_clip_accountant_info  # get the accountant
    identity_mu_dict = {X: 3, Y: 5}  # no Z
    group_queries = [
        {NAME: "test1",
         GROUP: [A],
         MU: {X: 1}},
        {NAME: "test2",
         GROUP: None,
         MU: {Y: 5}},
        {NAME: "test3",
         GROUP: [A, B],
         MU: {Y: 5}}
    ]
    my_clip_accountant.measure_identity(identity_mu_dict)
    try:
        my_clip_accountant.measure_groups(group_queries)
    except Exception:
        assert False, "Exception should not be raised in this test"


def test_clipping_accountant_error_checks5(my_clip_accountant_info):
    """ After group by queries are measured, no more queries get answered,
    because the clipping threshold has already been set based on existing group by
    queries """
    my_clip_accountant, the_reader = my_clip_accountant_info  # get the accountant
    identity_mu_dict1 = {X: 3, Y: 5}  # no Z
    group_queries1 = [
        {NAME: "test1",
         GROUP: [A],
         MU: {X: 1}},
        {NAME: "test2",
         GROUP: None,
         MU: {Y: 5}},
        {NAME: "test3",
         GROUP: [A, B],
         MU: {Y: 5}}
    ]
    my_clip_accountant.measure_identity(identity_mu_dict1)
    my_clip_accountant.measure_groups(group_queries1)
    aq = my_clip_accountant.answered_queries.copy()
    identity_mu_dict2 = {X: 3, Y: 5}
    group_queries2 = [
        {NAME: "test1",
         GROUP: [B],
         MU: {X: 1}},
        {NAME: "test2",
         GROUP: None,
         MU: {Y: 5}},
        {NAME: "test3",
         GROUP: [A, B],
         MU: {Y: 5}}
    ]
    my_clip_accountant.measure_identity(identity_mu_dict2)
    my_clip_accountant.measure_groups(group_queries2)
    assert aq == my_clip_accountant.answered_queries


def test_clipping_accountant_error_checks6(my_clip_accountant_info):
    """ Check that valid queries raise no exceptions"""
    my_clip_accountant, the_reader = my_clip_accountant_info  # get the accountant
    identity_mu_dict = {X: 3, Y: 5}  # no Z
    group_queries = [
        {NAME: "test1",
         GROUP: [A],
         MU: {X: 1}},
        {NAME: "test2",
         GROUP: None,
         MU: {Y: 5}},
        {NAME: "test3",
         GROUP: [A, B],
         MU: {Y: 5}}
    ]
    try:
        my_clip_accountant.make_noisy_measurements(identity_mu_dict, group_queries)
    except Exception:
        assert False, "Exception should not be raised in this test"


def test_clipping_accountant_id_checks7(my_clip_accountant_info):
    """ Test that the identity query  properly uses mu and distances """
    my_clip_accountant, the_reader = my_clip_accountant_info  # get the accountant
    tolerance = 1e-10
    identity_mu_dict = {X: X_MU, Y: Y_MU, Z: Z_MU}
    my_clip_accountant.measure_identity(identity_mu_dict)
    noisy_id = my_clip_accountant.answered_queries[0].noisy_answers
    est_std_X = std_formula(noisy_id[X], X_DISTANCE / X_MU)
    est_std_Y = std_formula(noisy_id[Y], Y_DISTANCE / Y_MU)
    est_std_Z = std_formula(noisy_id[Z], Z_DISTANCE / Z_MU)
    assert np.sum(np.abs(noisy_id[mechanisms.__primitives.std_col(X)] - est_std_X)) <= tolerance
    assert np.sum(np.abs(noisy_id[mechanisms.__primitives.std_col(Y)] - est_std_Y)) <= tolerance
    assert np.sum(np.abs(noisy_id[mechanisms.__primitives.std_col(Z)] - est_std_Z)) <= tolerance


def clip_to_std(sqrtmaxcol, c, mu, distance):
    """ Given a dataframe column containing group maxes, the c value (amount to add to the
    max), and the mu and distance parameters for the new aggregation, compute the intended std"""
    threshold = (sqrtmaxcol + c)**2
    sensitivity = threshold - (np.sqrt(threshold) - distance)**2
    std = sensitivity / mu
    return std


def test_clipping_accountant_proper_noise(my_clip_accountant_info):
    tolerance = 1e-10
    my_clip_accountant, the_reader = my_clip_accountant_info  # get the accountant
    numfinecells = the_reader.df.groupby(["A", "B", "C"]).agg({"Y": max}).shape[0]
    identity_mu_dict = {X: X_MU, Y: Y_MU}
    dinfo = {X: X_DISTANCE, Y: Y_DISTANCE, Z: Z_DISTANCE}
    group_queries = [
        {NAME: "test1",
         GROUP: [A],
         MU: {X: 1}},
        {NAME: "test2",
         GROUP: None,
         MU: {Y: 5}},
        {NAME: "test3",
         GROUP: [B],
         MU: {Y: 5}},
        {NAME: "test4",
         GROUP: [B,C],
         MU: {X: 2, Y: 3}}
    ]
    my_clip_accountant.measure_identity(identity_mu_dict)
    my_clip_accountant.measure_groups(group_queries)
    # compute the clipping threshold that should be used
    num_items = numfinecells * 2 # number of group by cells * # sensitive attributes used
    expected_threshold_dict = my_clip_accountant.get_topcode_adder(num_items)
    base_c = my_clip_accountant.get_gauss_max(num_items, CLIPPING_PROB)
    assert len(expected_threshold_dict) == 2
    assert np.abs(expected_threshold_dict[X] - base_c * X_DISTANCE/X_MU) <= tolerance
    assert np.abs(expected_threshold_dict[Y] - base_c * Y_DISTANCE/Y_MU) <= tolerance
    idq = my_clip_accountant.answered_queries[0].noisy_answers
    for index, query in enumerate(group_queries):
        groupbyclause = query[GROUP] if query[GROUP] is not None else lambda *x: 0
        group_answers = my_clip_accountant.answered_queries[index+1].noisy_answers
        sensitive_atts = sorted(query[MU].keys())
        metadata = idq.groupby(groupbyclause, as_index=True).agg({mechanisms.__primitives.sqrt_col(x): max for x in sensitive_atts}).copy()
        for att in sensitive_atts:
            std_name = mechanisms.__primitives.std_col(att)
            cmp_std_name = std_name + "2"
            sqrt_name = mechanisms.__primitives.sqrt_col(att)
            att_c = base_c * dinfo[att]/identity_mu_dict[att]
            metadata[cmp_std_name] = clip_to_std(metadata[sqrt_name], att_c, query[MU][att], dinfo[att])
            if query[GROUP] is None:
                assert np.abs(group_answers.iloc[0][std_name] - metadata.iloc[0][cmp_std_name]) <= tolerance
            else:
                joined = group_answers.join(metadata, on=query[GROUP], rsuffix="RIGHT")
                assert np.sum(np.abs(joined[std_name]-joined[cmp_std_name])) <= tolerance



def test_sqrt_accountant_no_exception(my_sqrt_accountant_info):
    """ Check that no exception is raised for proper queries """
    my_accountant, the_reader = my_sqrt_accountant_info  # get the accountant
    identity_mu_dict = {X: 3, Y: 5}  # no Z
    group_queries = [
        {NAME: "test1",
         GROUP: [A],
         MU: {X: 1}},
        {NAME: "test2",
         GROUP: None,
         MU: {Y: 5}},
        {NAME: "test3",
         GROUP: [A, B],
         MU: {Y: 5}}
    ]
    try:
        my_accountant.make_noisy_measurements(identity_mu_dict, group_queries)
    except Exception:
        assert False, "Exception should not be raised in this test"

def test_sqrt_accountant_noise_checks(my_sqrt_accountant_info):
    """ Test that the identity query  properly uses mu and distances """
    my_sqrt_accountant, the_reader = my_sqrt_accountant_info  # get the accountant
    tolerance = 1e-10
    identity_mu_dict = {X: X_MU, Y: Y_MU}
    dinfo = {X: X_DISTANCE, Y: Y_DISTANCE, Z: Z_DISTANCE}
    group_queries = [
        {NAME: "test1",
         GROUP: [A],
         MU: {X: 1}},
        {NAME: "test2",
         GROUP: None,
         MU: {Y: 5}},
        {NAME: "test3",
         GROUP: [B],
         MU: {Y: 5}},
        {NAME: "test4",
         GROUP: [B, C],
         MU: {X: 2, Y: 3}}
    ]
    my_sqrt_accountant.make_noisy_measurements(identity_mu_dict, group_queries)
    for queryinfo in my_sqrt_accountant.answered_queries:
        noisy_answers = queryinfo.noisy_answers
        sensitive_atts = sorted(queryinfo.mu_dict.keys())
        for att in sensitive_atts:
            std_name = mechanisms.__primitives.std_col(att)
            est_std = std_formula(noisy_answers[att], dinfo[att] / queryinfo.mu_dict[att])
            assert np.sum(np.abs(noisy_answers[std_name] - est_std)) <= tolerance
