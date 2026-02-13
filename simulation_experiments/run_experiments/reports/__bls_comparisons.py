from readers import QCEWAggReader
from typing import List
import pandas as pd
import numpy as np
import csv

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
    mycompare = ground_truth.df[headcolumns].copy()
    mycompare[EMP1] = ground_truth.df[EMP1]
    mycompare[EMP2] = ground_truth.df[EMP2]
    mycompare[EMP3] = ground_truth.df[EMP3]
    error_df = pd.DataFrame()
    agg_spec = {}
    for myagg, myname in zip(others, names):
        if not check_compatibility(ground_truth, myagg):
            raise Exception("DataFrames don't match on rows")
        else:
            mycompare[f"{EMP1}_{myname}"] = ground_truth.df[EMP1]
            mycompare[f"{EMP2}_{myname}"] = ground_truth.df[EMP2]
            mycompare[f"{EMP3}_{myname}"] = ground_truth.df[EMP3]
            error_df[f"{EMP1}_{myname}_abs"] = abs(mycompare[EMP1] - mycompare[f"{EMP1}_{myname}"])
            error_df[f"{EMP2}_{myname}_abs"] = abs(mycompare[EMP2] - mycompare[f"{EMP2}_{myname}"])
            error_df[f"{EMP3}_{myname}_abs"] = abs(mycompare[EMP3] - mycompare[f"{EMP3}_{myname}"])
            error_df[f"{WAGES}_{myname}_abs"] = abs(mycompare[WAGES] - mycompare[f"{WAGES}_{myname}"])
            error_df[f"{EMP1}_{myname}_rel"] = error_df[f"{EMP1}_{myname}_abs"] / np.maximum(mycompare[EMP1], 1)
            error_df[f"{EMP2}_{myname}_rel"] = error_df[f"{EMP2}_{myname}_abs"] / np.maximum(mycompare[EMP2], 1)
            error_df[f"{EMP3}_{myname}_rel"] = error_df[f"{EMP3}_{myname}_abs"] / np.maximum(mycompare[EMP3], 1)
            error_df[f"{WAGES}_{myname}_rel"] = error_df[f"{WAGES}_{myname}_abs"] / np.maximum(mycompare[WAGES], 1)
            agg_spec[f"{EMP1}_{myname}_abs"] = "mean"
            agg_spec[f"{EMP2}_{myname}_abs"] = "mean"
            agg_spec[f"{EMP3}_{myname}_abs"] = "mean"
            agg_spec[f"{WAGES}_{myname}_abs"] = "mean"
            agg_spec[f"{EMP1}_{myname}_rel"] = "median"
            agg_spec[f"{EMP2}_{myname}_rel"] = "median"
            agg_spec[f"{EMP3}_{myname}_rel"] = "median"
            agg_spec[f"{WAGES}_{myname}_rel"] = "median"
    sorted_cols = sorted(error_df.columns)
    mycompare[sorted_cols] = error_df[sorted_cols]
    aggresult = mycompare.groupby([QCEWAggReader.AGGLVL_CODE]).groupby(agg_spec)
    return mycompare, aggresult


def compare_files(ground_truth: str, others: List[str], names: List[str], outfile: str, aggfile: str):
    gt = QCEWAggReader()
    gt.read(ground_truth)
    other_readers = [QCEWAggReader() for _ in others]
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
