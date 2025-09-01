import pytest
import readers.__basereader as br
import pandas as pd
import csv
from .helper import are_dfs_equal
import numpy as np


class NonAbstract(br.BaseReader):
    """ minimal non-abstract subclass of BaseReader """

    def __init__(self):
        super().__init__()

    @property
    def private_cols(self):
        return []

    @property
    def project_cols(self):
        return []

    @property
    def public_cols(self):
        return []

    @property
    def reading_schema(self):
        return []


# noinspection PyAbstractClass
class BadNoReadingSchema(br.BaseReader):
    """ minimal non-abstract subclass of BaseReader """

    def __init__(self):
        super().__init__()

    @property
    def private_cols(self):
        return []

    @property
    def project_cols(self):
        return []

    @property
    def public_cols(self):
        return []


# noinspection PyAbstractClass
class BadNoPublicsColOverride(br.BaseReader):
    """ minimal non-abstract subclass of BaseReader """

    def __init__(self):
        super().__init__()

    @property
    def private_cols(self):
        return []

    @property
    def project_cols(self):
        return []

    @property
    def reading_schema(self):
        return {}


# noinspection PyAbstractClass
class BadNoProjectColsOverride(br.BaseReader):
    """ minimal non-abstract subclass of BaseReader """

    def __init__(self):
        super().__init__()

    @property
    def private_cols(self):
        return []

    @property
    def public_cols(self):
        return []

    @property
    def reading_schema(self):
        return {}


# noinspection PyAbstractClass
class BadNoPrivateColsOverride(br.BaseReader):
    """ minimal non-abstract subclass of BaseReader """

    def __init__(self):
        super().__init__()

    @property
    def project_cols(self):
        return []

    @property
    def public_cols(self):
        return []

    @property
    def reading_schema(self):
        return {}


class ChangeableReader(br.BaseReader):
    """ Single class used to test different invariant requirements """

    def __init__(self):
        super().__init__()
        self._private_cols = ["D", "B", "Added"]
        self._project_cols = ["One", "B", "D", "A", "C", "Added"]
        self._public_cols = ["A", "C", "One"]
        self._reading_schema = {"A": str, "B": int, "C": float, "D": str}
        self._more_cols = []  # add columns with these names

    def recode_columns(self, tmpdf):
        nrows = tmpdf.shape[0]
        tmpdf["Added"] = range(nrows)
        tmpdf["One"] = 1
        for col in self._more_cols:
            tmpdf[col] = 2

    @property
    def header_index(self):
        return 1

    @property
    def skip_lines(self):
        return [0, 3]

    @property
    def delimiter(self):
        return "@"

    @property
    def filterquery(self):
        return "A != 'b'"

    @property
    def private_cols(self):
        return self._private_cols

    @property
    def project_cols(self):
        return self._project_cols

    @property
    def public_cols(self):
        return self._public_cols

    @property
    def reading_schema(self):
        return self._reading_schema


@pytest.fixture
def small_df(tmp_path):
    """ Creates a DataFrame and corresponding file so
    that when read by ChangeableReader, the result is equal
    the starting dataframe"""
    nrows = 4003
    myfilename = tmp_path.joinpath("smalldf.csv")
    mydf_full = pd.DataFrame({
        "A": ["skip", "b"] + ["a" for _ in range(nrows - 3)] + ["b"],
        "B": range(nrows),
        "C": [nrows - x + 0.5 for x in range(nrows)],
        "D": [str(x) for x in range(nrows)]
    })
    myview = mydf_full.query("A == 'a'").copy()
    myview["Added"] = range(myview.shape[0])
    myview["One"] = 1
    with open(myfilename, "w") as thefile:
        thefile.write("garbage 1\n garbage 2\n")
    mydf_full.to_csv(myfilename, header=True, sep="@", index=False, mode="a",
                     quoting=csv.QUOTE_NONNUMERIC)
    return myview, myfilename


def test_nonabstract():
    """ Check that overriding the reading_schema, public_cols, private_cols,
    and project_cols is enough to make a concrete class"""
    # noinspection PyUnusedLocal
    reader1 = NonAbstract()


def test_must_have_reading_schema():
    """ Test that subclassing fails if reading schema not overridden """
    with pytest.raises(TypeError):
        # noinspection PyUnusedLocal
        badreader1 = BadNoReadingSchema()


def test_must_have_public_cols_override():
    """ Test that subclassing fails if public cols not overridden """
    with pytest.raises(TypeError):
        # noinspection PyUnusedLocal
        badreader2 = BadNoPublicsColOverride()


def test_must_have_private_cols_override():
    """ Test that subclassing fails if private cols not overridden """
    with pytest.raises(TypeError):
        # noinspection PyUnusedLocal
        badreader3 = BadNoPrivateColsOverride()


def test_must_have_project_cols_override():
    """ Test that subclassing fails if project cols not overridden """
    with pytest.raises(TypeError):
        # noinspection PyUnusedLocal
        badreader4 = BadNoProjectColsOverride()


def test_basic_reading(small_df):
    """ test that a dataframe  is equal to the result when we write then read it """
    df, filename = small_df
    cr = ChangeableReader()
    read_df = cr.read(filename)
    assert not are_dfs_equal(df, read_df)
    assert are_dfs_equal(df[cr.project_cols], read_df)


def test_project_cols_equal_public_private(small_df):
    """ Test that the project columns are the union of private and public ones"""
    df, filename = small_df
    cr1 = ChangeableReader()
    cr2 = ChangeableReader()
    cr3 = ChangeableReader()
    cr1._project_cols = cr1._project_cols[1:]
    cr2._private_cols = cr2._private_cols[1:]
    cr3._public_cols = cr3._public_cols[1:]
    with pytest.raises(AssertionError):
        # noinspection PyUnusedLocal
        tmp2 = cr1.read(filename)
    with pytest.raises(AssertionError):
        # noinspection PyUnusedLocal
        tmp2 = cr2.read(filename)
    with pytest.raises(AssertionError):
        # noinspection PyUnusedLocal
        tmp3 = cr3.read(filename)


def test_public_private_disjoint(small_df):
    """ Test that the private and public columns are enforced to be disjoint """
    df, filename = small_df
    cr1 = ChangeableReader()
    cr2 = ChangeableReader()
    cr1._public_cols.append(cr1._private_cols[1])
    cr2._private_cols.append(cr2._public_cols[1])
    with pytest.raises(AssertionError):
        # noinspection PyUnusedLocal
        tmp1 = cr1.read(filename)
    with pytest.raises(AssertionError):
        # noinspection PyUnusedLocal
        tmp2 = cr2.read(filename)


def test_all_fields_must_have_types(small_df):
    """ test that all reading fields have types """
    df, filename = small_df
    cr = ChangeableReader()
    del cr._reading_schema["D"]
    with pytest.raises(AssertionError):
        # noinspection PyUnusedLocal
        tmp1 = cr.read(filename)


def test_random_ordering(small_df):
    """ test that items with same public fields are randomly permuted
    so that line ordering does not leak private information """
    df, filename = small_df
    cr = ChangeableReader()
    cr._public_cols = ["One"]
    cr._private_cols = ["Added"]
    cr._project_cols = ["One", "Added"]
    read_df = cr.read(filename)
    added = list(read_df["Added"])
    numpairs = len(added) // 2
    inverted = 0
    # the Added field is sequentially  numered so we are
    # counting the number of times one element is larger than
    # the element that follows. The expected value should be number of
    # pairs / 2 and with high probability it will be less than 6 std away
    # from the expected value.
    for i in range(numpairs):
        if added[2 * i] > added[2 * i + 1]:
            inverted = inverted + 1
    assert np.abs(inverted - numpairs / 2) <= 1.5 * np.sqrt(read_df.shape[0])


def test_randcol_not_allowed(small_df):
    """ test that df is not allowed to have a column name
    equal to __basereader._RANDCOL """
    df, filename = small_df
    cr = ChangeableReader()
    cr._more_cols.append(cr._RANDCOL)
    with pytest.raises(AssertionError):
        # noinspection PyUnusedLocal
        tmp1 = cr.read(filename)
