from abc import ABC, abstractmethod
import pandas as pd
import numpy as np

""" This package contains code for reading csv files into pandas
dataframes. Subclass this file for a new csv data file.  It includes
confidentiality related code like:
- requiring public columns to be specified
- requiring private columns to be specified
- requiring types for each field in the input csv (just good practice)
- ensuring that the ordering of lines does not give away information
- allowing a subset of the columns to be projected (to get rid of unwanted fields)
- allowing derived columns to be added
Subclass of this class will automatically have checks that that the
types are specified, public and private columns are disjoint and exhaustive
and inherited behavior will ensure that row ordering does not leak private
information.

Requirement: input dataset and derived fields cannot use the filed name
  specified in _RANDCOL (this is checked) because this field is added 
  (then removed) during the process of ensuring row order leaks no information.
"""


class BaseReader(ABC):
    """ This is the base class for reading csv files. CSV files must have headers

    Subclasses can override the following:
    reading_schema: (required @property) a python dictionary of field name
         and data type for reading a csv. For good practice, requires types
         for all csv fields
    delimiter: (optional @property) what the csv is delimited by, default comma
    header_index: (optional @property) the 0-based index of the header line, default 0
    skip_lines: (optional @property) list of lines to skip (default empty)
    reading_schema: (required @property) python dictionary of csv field name and type
            the base class will enforce that **all** csv fields must have a type
    recode_columns: (optional function) add derived columns to read dataframe
    filterquery: (optional @property) the string to be used in padnas dataframe.query
            it is used after recode_columns
    project_cols: (required  @property) the columns to keep after reading and recode
    public_cols: (required @property) subset of the project_cols that are public
    private_cols: (required @property) subset of the project_cols taht are private
            the union of the private and public fields must equal the project_cols.
            This is automatically enforced.

    The class will create the property @df from which the resulting dataframe can
        be accessed
    """
    # random column to be used for ordering rows with the same public fields
    _RANDCOL = "__RANDOM"

    def __init__(self):
        self.__df = None  # variable to be retrieved by self.df

    def read(self, filename):
        """ reads a csv or zipped csv file into a dataframe """
        assert set(self.project_cols) == set(self.public_cols).union(self.private_cols)
        assert len(set(self.public_cols).intersection(self.private_cols)) == 0
        assert isinstance(self.reading_schema, dict), "reading_schema must be dict"
        tmpdf = pd.read_csv(filename, delimiter=self.delimiter, header=self.header_index, skiprows=self.skip_lines,
                            dtype=self.reading_schema)
        tmpdf = tmpdf[list(self.reading_schema.keys())].copy() # subset to columns that have types provided
        # assert set(tmpdf.columns) == set(self.reading_schema.keys()), "Types must be provided for all reading fields"
        if self.filterquery:
            tmpdf.query(self.filterquery, inplace=True)
        self.recode_columns(tmpdf)
        nrows = tmpdf.shape[0]
        # to ensure record order does not give away any information
        #  (e.g., what if record sorting involves private attributes?)
        # we are going to sort the data by public attributes and use a column of randomness to break ties.
        assert self._RANDCOL not in tmpdf, f"dataframe cannot have f{self._RANDCOL} as a column name"
        tmpdf[self._RANDCOL] = np.random.random(nrows)
        tmpdf.sort_values(list(self.public_cols) + [self._RANDCOL], inplace=True)
        # keep only projection columns to make sure that no cols are neither public nor private
        assert set(self.project_cols).issubset(tmpdf.columns), f"Project columns lists columns {self.project_cols} not in the dataframe {list(tmpdf.columns)}"
        tmpdf = tmpdf[self.project_cols]
        # reset index so it does not give away any information
        tmpdf.reset_index(drop=True, inplace=True)
        self.__df = tmpdf
        return self.df

    @property
    def df(self) -> pd.DataFrame:
        """ get the dataframe (careful, it is not a copy) """
        return self.__df

    @property
    def delimiter(self):
        """ Specify the delimiter in csv files, default comma"""
        return ","

    @property
    def header_index(self):
        """ 0-based line index of where the header is,
        after lines specified in skip_lines have been removed
        assumes header is only one line, default 0 """
        return 0

    @property
    def skip_lines(self):
        """ list of 0-based line indexes that are neither header nor data """
        return []

    @property
    @abstractmethod
    def reading_schema(self):
        """ Return a dictionary of column name and type to be used in reading 
        the dataframe; it will be used as the pandas dtype argument"""
        pass

    @property
    def filterquery(self):
        """ The filter condition, as a string, 
        that will be passed to pandas df.query, 
        used before project_columns"""
        return ""

    def recode_columns(self, tmpdf):
        """ once the dataframe is read, this function will be called to
        add additional columns or to recode existing columns, modify tmpdf in place """
        pass

    @property
    @abstractmethod
    def project_cols(self):
        """ List of columns from the dataframe to keep, used after
        filterquery and recode_columns """
        pass

    @property
    @abstractmethod
    def public_cols(self):
        """ Return a list of the public columns. It should be disjoint from
        the private columns. Public and private columns together should be
        the entire set of columns"""
        pass

    @property
    @abstractmethod
    def private_cols(self):
        """ Return a list of the private columns. It should be disjoint from
        the public columns. Public and private columns together should be
        the entire set of columns"""
        pass

    @classmethod
    def from_config(cls, config):
        return cls()
