from readers import QCEWAggReader as MyAggReader
from reports.__tabulateqcew import aggregate_qcew_from_file
from tests.helper import are_dfs_equal

DIR = "tests/testdata"
INPUT = f"{DIR}/yy99_qdb_2015_1.csv"
OUTPUT_WOUT_999_RECODE = f"{DIR}/yy99_aggregated_false999.csv"
OUTPUT_W_999_RECODE = f"{DIR}/yy99_aggregated_true999.csv"
TMP_OUT = "tmpout.csv"


def test_regression_with999(tmp_path):
    """ Aggregate a sample input file and compare it to a saved output file
    while recoding "99x" counties to "999". This tests reading in microdata,
    and aggregating it.
    """
    outputfilename = str(tmp_path.joinpath(TMP_OUT))
    aggregate_qcew_from_file(INPUT, outputfilename, county99=True)
    myagg_testing = MyAggReader()
    myagg_regression = MyAggReader()
    testing_agg_df = myagg_testing.read(outputfilename)
    regression_agg_df = myagg_regression.read(OUTPUT_W_999_RECODE)
    assert are_dfs_equal(testing_agg_df, regression_agg_df)


def test_regression_without999(tmp_path):
    """ Aggregate a sample input file and compare it to a saved output file
    without recoding "99x" counties to "999". This tests reading in microdata,
    and aggregating it.
    """
    outputfilename = str(tmp_path.joinpath(TMP_OUT))
    aggregate_qcew_from_file(INPUT, outputfilename, county99=False)
    myagg_testing = MyAggReader()
    myagg_regression = MyAggReader()
    testing_agg_df = myagg_testing.read(outputfilename)
    regression_agg_df = myagg_regression.read(OUTPUT_WOUT_999_RECODE)
    assert are_dfs_equal(testing_agg_df, regression_agg_df)
