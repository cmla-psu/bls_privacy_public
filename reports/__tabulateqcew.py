from readers import QCEWMicroDataReader as Qcew
from readers import QCEWAggReader as Aggreader
import pandas as pd
import csv


def aggregate_qcew_from_file(inputfilename, outputfilename, county99=False):
    """ Aggregates data having the same schema as the confidential QCEW Microdata
    and generates a csv file with aggregation levels 50-58 and 50-70-78.
    See documentation for quarterly single file CSVs at
    https://www.bls.gov/cew/downloadable-data-files.htm
    """
    qcew = Qcew(county99)
    qcew.read(inputfilename)
    aggregate_df = aggregate_qcew(qcew.df)
    aggregate_df.to_csv(outputfilename, index=False, quoting=csv.QUOTE_NONNUMERIC)


def area_fips(state: str, county: str = None):
    """ Appends state fips to county fips. If county is missing
    this indicates a statewide aggregation area and so the value appended
    to the state fips is 000 """
    if county is None:
        recode = "000"
    else:
        recode = county
    return state + recode


def aggregate_qcew(qcew_df):
    """ This aggregates the QCEW quarterly wage and monthly employment data
    as well as establishment count according to aggregation levels 50-58
     (statewide) and 70-78 (countywide)
    It takes advantage of the fact that agglevel code xy can be obtained by
    aggregating agglevel code of the form xz when z=y+1 and also that 5x can
    be obtained by aggregating 7x.
    """
    groupby_cols = [Qcew.YEAR, Qcew.QTR, Qcew.STATE, Qcew.COUNTY, Qcew.OWN, Qcew.DOMAIN, Qcew.SUPERSECTOR,
                    Qcew.SECTOR, Qcew.NAICS3, Qcew.NAICS4, Qcew.NAICS5, Qcew.NAICS]
    # aggregate county from 78 down to 70 using the fact that the next agg level
    # can be obtained by aggregating the previous one
    df78 = aggify(qcew_df, agglvl="78", cols=groupby_cols, naicscol=Qcew.NAICS)  # County, NAICS 6-digit, By Ownership
    df77 = aggify(df78, agglvl="77", cols=groupby_cols[0:-1],
                  naicscol=Qcew.NAICS5)  # County, NAICS 5-digit, By Ownership
    df76 = aggify(df77, agglvl="76", cols=groupby_cols[0:-2],
                  naicscol=Qcew.NAICS4)  # County, NAICS 4-digit, By Ownership
    df75 = aggify(df76, agglvl="75", cols=groupby_cols[0:-3],
                  naicscol=Qcew.NAICS3)  # County, NAICS sub-sector (3-digit), By Ownership
    df74 = aggify(df75, agglvl="74", cols=groupby_cols[0:-4],
                  naicscol=Qcew.SECTOR)  # County, NAICS Sector, By Ownership
    df73 = aggify(df74, agglvl="73", cols=groupby_cols[0:-5], naicscol=Qcew.SUPERSECTOR)  # County,
    df72 = aggify(df73, agglvl="72", cols=groupby_cols[0:-6], naicscol=Qcew.DOMAIN)  # County, Domain, By Ownership
    df71 = aggify(df72, agglvl="71", cols=groupby_cols[0:-7], naicsval="10")  # County, Total, By Ownership
    df70 = aggify(df71, agglvl="70", cols=groupby_cols[0:-8], naicsval="10", own="0")  # County, Total, All Ownerships
    # aggregate each county level agg code into the corresponding state level aggregation
    state_cols = [Qcew.YEAR, Qcew.QTR, Qcew.STATE, Qcew.OWN, Aggreader.INDUSTRY_CODE]
    df58 = aggify(df78, agglvl="58", cols=state_cols,
                  naicscol=Aggreader.INDUSTRY_CODE)  # StateWide, NAICS 6-digit, By Ownership
    df57 = aggify(df77, agglvl="57", cols=state_cols,
                  naicscol=Aggreader.INDUSTRY_CODE)  # StateWide, NAICS 5-digit, By Ownership
    df56 = aggify(df76, agglvl="56", cols=state_cols,
                  naicscol=Aggreader.INDUSTRY_CODE)  # StateWide, NAICS 4-digit, By Ownership
    df55 = aggify(df75, agglvl="55", cols=state_cols,
                  naicscol=Aggreader.INDUSTRY_CODE)  # StateWide, NAICS sub-sector (3-digit), By Ownership
    df54 = aggify(df74, agglvl="54", cols=state_cols,
                  naicscol=Aggreader.INDUSTRY_CODE)  # StateWide, NAICS Sector, By Ownership
    df53 = aggify(df73, agglvl="53", cols=state_cols,
                  naicscol=Aggreader.INDUSTRY_CODE)  # StateWide, SuperSector, Ownership
    df52 = aggify(df72, agglvl="52", cols=state_cols, naicscol=Aggreader.INDUSTRY_CODE)  # StateWide, Domain, Ownership
    df51 = aggify(df71, agglvl="51", cols=state_cols, naicscol=Aggreader.INDUSTRY_CODE)  # StateWide, By Ownership
    df50 = aggify(df70, agglvl="50", cols=state_cols, naicscol=Aggreader.INDUSTRY_CODE)  # State, Total, All Ownerships
    my_sub_df = [df50, df51, df52, df53, df54, df55, df56, df57, df58,
                 df70, df71, df72, df73, df74, df75, df76, df77, df78]
    project_cols = [Aggreader.AREA_FIPS,
                    Aggreader.OWN_CODE,
                    Aggreader.INDUSTRY_CODE,
                    Aggreader.AGGLVL_CODE,
                    Aggreader.SIZE_CODE,
                    Aggreader.YEAR,
                    Aggreader.QTR,
                    Aggreader.DISCLOSURE_CODE,
                    Aggreader.QTRLY_ESTABS,
                    Aggreader.EMP1,
                    Aggreader.EMP2,
                    Aggreader.EMP3,
                    Aggreader.WAGES,
                    ]
    # Rename columns appropriately from the column names used by the input to
    #  those used by the output
    [finish_columns_in_place(x, project_cols) for x in my_sub_df]
    # combine
    return pd.concat(my_sub_df, axis=0, ignore_index=True)[project_cols]


def finish_columns_in_place(df, project_cols):
    """ Renames all columns that kept their original dataframe name
    through the aggregation (so that they match the desired column
    name in the output per BLS) and set the size code and disclosure
    code fields to appropriate default values (size code = 0) for this
     aggregation and disclosure code indicates that the data may not
     be ready for public release.

     Then only keeps the columns in project_cols """
    df.rename(columns={Qcew.OWN: Aggreader.OWN_CODE,
                       Qcew.YEAR: Aggreader.YEAR,
                       Qcew.QTR: Aggreader.QTR,
                       Qcew.M1EMP: Aggreader.EMP1,
                       Qcew.M2EMP: Aggreader.EMP2,
                       Qcew.M3EMP: Aggreader.EMP3,
                       Qcew.WAGE: Aggreader.WAGES,
                       }, inplace=True)
    df[Aggreader.SIZE_CODE] = "0"
    df[Aggreader.DISCLOSURE_CODE] = "INTERNAL-BLS-USE-ONLY"

    df.drop([x for x in df.columns if x not in project_cols], axis=1, inplace=True)
    df.sort_values(project_cols, inplace=True)
    return df[project_cols]


def aggify(df, *, agglvl, cols, naicscol=None, naicsval=None, own=None):
    """ Perform an aggregation 
    agglvl = the aggregation level code to put into AGG.AGGLVL_CODE colulmns
    cols = the group by columns
    naicscol = the column of the dataframe to use for industry code (if naicsval is None)
    naicsval = the value to use for the industry code if (naicscol is None)
    own = the ownership code to use if not the QCEW.OWN column

    In the output dataframe, there will be a new column whose name comes from
     QCEWAggReader.QTRLY_ESTABS. Meanwhile, the wages and employment columns will
     be aggregated but will retain their original names. It also creats the
     QCEWAggReader.INDUSTRY_CODE field based on the input parameters (this is the
     information about NAICS that is specified by the agglevel code) and
     QCEWAggReader.AREA_FIPS which is the concatenation of state and county fips
     or state and '000' for statewide aggregation.
    """

    # Prepare the aggregations we want for quarterly wage and monthly employment
    # These will use and keep the original column names from the input df
    # establishment size will be added later after checking whether this column
    # exists (because df has already been aggregated) or not (because the df
    # has not already been aggregated, as with agglevel 78, which is the start
    # of the aggregation)
    namedagg = {Qcew.M1EMP: pd.NamedAgg(column=Qcew.M1EMP, aggfunc="sum"),
                Qcew.M2EMP: pd.NamedAgg(column=Qcew.M2EMP, aggfunc="sum"),
                Qcew.M3EMP: pd.NamedAgg(column=Qcew.M3EMP, aggfunc="sum"),
                Qcew.WAGE: pd.NamedAgg(column=Qcew.WAGE, aggfunc="sum"),
                }
    if Aggreader.QTRLY_ESTABS not in df:  # do we need to start a quarterly establishment count?
        namedagg[Aggreader.QTRLY_ESTABS] = pd.NamedAgg(column=Qcew.WAGE, aggfunc="count")
    else:  # quarterly establishment coumn exists, just sum it up during aggregation
        namedagg[Aggreader.QTRLY_ESTABS] = pd.NamedAgg(column=Aggreader.QTRLY_ESTABS, aggfunc="sum")
    agg_df = df.groupby(cols, as_index=False).agg(**namedagg)
    # set the industry code as appropriate, based on input, which depends on
    # agglevel code
    if naicscol is not None:
        agg_df[Aggreader.INDUSTRY_CODE] = agg_df[naicscol]
    else:
        agg_df[Aggreader.INDUSTRY_CODE] = naicsval
    agg_df[Aggreader.AREA_FIPS] = agg_df.apply(
        lambda x: area_fips(x[Qcew.STATE], x[Qcew.COUNTY] if Qcew.COUNTY in cols else None), axis=1)
    agg_df[Aggreader.AGGLVL_CODE] = agglvl
    # this should only be True when the aggregation level does not use ownership code
    if own is not None:
        agg_df[Qcew.OWN] = own
    return agg_df
