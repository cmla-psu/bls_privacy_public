from .__basereader import BaseReader
import numpy as np


class QCEWMicroDataReader(BaseReader):
    """ This is the class for reading data having the same
    schema as the confidential QCEW microdata. It is comma separated
     with the header appearing in the first row. It can optionally
     recode all counties of the form '99x' to '999' by passing True
     to the constructor """
    YEAR = "year"
    QTR = "qtr"
    STATE = "state"
    COUNTY = "cnty"
    OWN = "own"
    DOMAIN = "domain"
    NAICS = "naics"
    NAICS3 = "naics3"
    NAICS4 = "naics4"
    NAICS5 = "naics5"
    #NAICSPREFIX = "naicsprefix"
    SUPERSECTOR = "super_sector"
    SECTOR = "naics_sector"
    M1EMP = "m1emp"
    M2EMP = "m2emp"
    M3EMP = "m3emp"
    WAGE = "wage"
    PRIMARY_KEY = "primary_key"

    _CAN_AGG = "can_agg"
    _RECTYPE = "rectype"

    #_CONTRIB = "contrib"
    #_TXWAGE = "txwage"
    #_UI = "ui"
    #_RUNUM = "ru_num"
    #_EIN = "ein"

    # self.county99: whether county codes of the regex form "99[0-9]" should be changed to 999

    def __init__(self, county99=False):
        """ Constructor.
        Optional argument: county99, default is False.
        If True, it would recode counties of the form "99x" to "999"
        """
        super().__init__()
        self.county99 = county99

    @property
    def reading_schema(self):
        return {self.YEAR: str,
                self.QTR: str,
                self.STATE: str,
                self.COUNTY: str,
                self.OWN: str,
                self.NAICS: str,
                # self.NAICSPREFIX: str, this should be a derived field, if used at all
                self.SUPERSECTOR: str,
                self.SECTOR: str,
                self.M1EMP: float, # because privacy protected version would be float
                self.M2EMP: float,
                self.M3EMP: float,
                self.WAGE: float,
                self._CAN_AGG: str,
                self._RECTYPE: str,
                self.PRIMARY_KEY: str
                #self._CONTRIB: str,
                #self._TXWAGE: str,
                #self._UI: str,
                #self._RUNUM: str,
                #self._EIN: str
                }

    @property
    def filterquery(self):
        return f"{self._CAN_AGG}=='Y' and {self._RECTYPE}=='C' and {self.OWN}=='5'"

    def recode_columns(self, tmpdf):
        # tmpdf[self.NAICSPREFIX] = tmpdf[self.NAICS].str[0:2], not used
        tmpdf[self.NAICS3] = tmpdf[self.NAICS].str[0:3]
        tmpdf[self.NAICS4] = tmpdf[self.NAICS].str[0:4]
        tmpdf[self.NAICS5] = tmpdf[self.NAICS].str[0:5]
        tmpdf[self.DOMAIN] = tmpdf[self.SUPERSECTOR].str[0:3]
        if self.county99:
            tmpdf[self.COUNTY] = tmpdf.apply(lambda x: "999" if x[self.COUNTY][0:2] == "99" else x[self.COUNTY], axis=1)
        # rewrite primary key so it has no private information
        nrows = tmpdf.shape[0]
        tmpdf[self.PRIMARY_KEY] = [str(x) for x in np.random.permutation(range(nrows))]

    @property
    def project_cols(self):
        return [self.PRIMARY_KEY,
                self.YEAR,
                self.QTR,
                self.STATE,
                self.COUNTY,
                self.OWN,
                self.DOMAIN,
                self.SUPERSECTOR,
                self.SECTOR,
                self.NAICS3,
                self.NAICS4,
                self.NAICS5,
                self.NAICS,
                self.M1EMP,
                self.M2EMP,
                self.M3EMP,
                self.WAGE,
                self._RECTYPE,
                self._CAN_AGG
                ]

    @property
    def public_cols(self):
        return [self.PRIMARY_KEY,
                self.YEAR,
                self.QTR,
                self.STATE,
                self.COUNTY,
                self.OWN,
                self.DOMAIN,
                self.SUPERSECTOR,
                self.SECTOR,
                self.NAICS3,
                self.NAICS4,
                self.NAICS5,
                self.NAICS,
                self._RECTYPE,
                self._CAN_AGG
                ]

    @property
    def private_cols(self):
        return [self.M1EMP,
                self.M2EMP,
                self.M3EMP,
                self.WAGE
                ]
