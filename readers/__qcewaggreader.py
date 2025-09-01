from .__basereader import BaseReader


class QCEWAggReader(BaseReader):
    """ This is the class for reading aggregated QCEW data
    produced by the reports module. The difference with the
    public aggregated single csv files is that there is no tax data
    """
    AREA_FIPS = "area_fips"
    OWN_CODE = "own_code"
    INDUSTRY_CODE = "industry_code"
    AGGLVL_CODE = "agglvl_code"
    SIZE_CODE = "size_code"
    YEAR = "year"
    QTR = "qtr"
    DISCLOSURE_CODE = "disclosure_code"
    QTRLY_ESTABS = "qtrly_estabs"
    EMP1 = "month1_emplvl"
    EMP2 = "month2_emplvl"
    EMP3 = "month3_emplvl"
    WAGES = "total_qtrly_wages"

    @property
    def reading_schema(self):
        return {self.AREA_FIPS: str,
                self.OWN_CODE: str,
                self.INDUSTRY_CODE: str,
                self.AGGLVL_CODE: str,
                self.SIZE_CODE: str,
                self.YEAR: str,
                self.QTR: str,
                self.DISCLOSURE_CODE: str,
                self.QTRLY_ESTABS: int,
                self.EMP1: int,
                self.EMP2: int,
                self.EMP3: int,
                self.WAGES: float,
                }

    def recode_columns(self, tmpdf):
        tmpdf.fillna({self.DISCLOSURE_CODE: ""}, inplace=True)

    @property
    def project_cols(self):
        return [self.AREA_FIPS,
                self.OWN_CODE,
                self.INDUSTRY_CODE,
                self.AGGLVL_CODE,
                self.SIZE_CODE,
                self.YEAR,
                self.QTR,
                self.DISCLOSURE_CODE,
                self.QTRLY_ESTABS,
                self.EMP1,
                self.EMP2,
                self.EMP3,
                self.WAGES,
                ]

    @property
    def public_cols(self):
        return [self.AREA_FIPS,
                self.OWN_CODE,
                self.INDUSTRY_CODE,
                self.AGGLVL_CODE,
                self.SIZE_CODE,
                self.YEAR,
                self.QTR,
                self.DISCLOSURE_CODE,
                self.QTRLY_ESTABS,
                self.EMP1,
                self.EMP2,
                self.EMP3,
                self.WAGES,
                ]

    @property
    def private_cols(self):
        return []

    def read(self, filename):
        super().read(filename)
        self.sort()
        return self.df

    def sort(self):
        self.df.sort_values([
            self.YEAR,
            self.QTR,
            self.AGGLVL_CODE,
            self.OWN_CODE,
            self.AREA_FIPS,
            self.INDUSTRY_CODE,
            self.SIZE_CODE,
            self.DISCLOSURE_CODE,
            self.QTRLY_ESTABS,
            self.EMP1,
            self.EMP2,
            self.EMP3,
            self.WAGES,
        ], inplace=True)
