""" This module contains code that ingests the configuration files for
formal privacy for establishment data

NOTE: if you change configreader, you may also need to change
      configs/example.yaml
      test/testconfigs/example.yaml
"""
import yaml
from inspect import isclass
import os
import re
import mechanisms

#########################################################
# Strings that appear in valid YAML configuration file #
#########################################################
SEED = "seed"
CLIPPING_PROB = "clipping_prob"
INPUT_FILES = "input_files"
OUTPUT = "output"
AGGREGATE = "aggregate"
FOLDER = "folder"
SUFFIX = "suffix"
POSTPROCESS = "postprocessed"
ACCOUNTANT = "accountant"
READER = "reader"
DISTANCES = "privacy_distances"
QUERIES = "queries"
IDENTITY = "identity"
GROUPBY = "groupby"
NAME = "name"
GROUP = "group"
MU = "mu"
MEASUREFOLDER = "measure_folder"


#########################################
# Discovery Code ########################
#########################################

def discover_readers():
    """ discovers registered data reader classes that are imported in readers/__init__.py"""
    reader_module = __import__("readers")
    base_reader = reader_module.__basereader.BaseReader
    readers = [v for v in vars(reader_module).values() if isclass(v) and issubclass(v, base_reader)]
    return {x.__name__: x for x in readers}


def discover_accountants():
    """ discovers registered privacy accountant that are imported in accountant/__init__.py"""
    accountant_module = __import__("accountant")
    base_accountant = accountant_module.__baseaccountant.BaseAccountant
    accountants = [v for v in vars(accountant_module).values() if isclass(v) and issubclass(v, base_accountant)]
    return {x.__name__: x for x in accountants}


def ensure_dir_exists(dirname):
    if os.path.exists(dirname):
        if os.path.isdir(dirname):
            pass  # good
        else:
            raise Exception(f"Error: file {dirname} should be a directory.")
    else:
        os.makedirs(dirname)


def keep_good_chars(name: str):
    good = [c for c in name if c.isalnum() or c in ['-', '_']]
    return "".join(good)

##############################################
# Configuration Object
##############################################

class FPConfig:
    """ This class represents the ingested version of the configuration yaml file """
    __accountant_class = None  # class name of accountant to use
    __reader_class = None  # name of the class that can parse the input data
    __seed = None  # random seed to be used for reproducibility, or None
    __prng = None  # psuedorandom number generator
    __input_files = None  # list of input files to process
    __aggregate_suffix = None  # when an input file has been tabulated, this suffix is added before the .csv
    __postprocess_suffix = None  # file suffix added when an input file has been turned into privacy protected microdata
    __aggregate_folder = None  # folder to place tabulated data into
    __postprocess_folder = None  # folder to place privacy-protected microdata into
    __measure_folder = None  # folder to store noisy measurements in
    __distances = None  # distance parameters for privacy model
    __identity_query = None  # parameters for the identity query (budget allocations for private fields)
    __group_by_queries = None  # list of groupby query parameters (group by cols, name, and budget allocations)
    __clipping_prob = None  # clipping probability for clipping accountant
    __marker = None  # base name of the config file attached to all output files.

    def __init__(self, conf_name):
        """ reads in a yaml configuration file given its name and validates its structure """
        with open(conf_name, "r") as conf_name_fp:
            config = yaml.safe_load(conf_name_fp)
        self.__ingest_and_validate(config)
        self.__marker = os.path.basename(conf_name).split(".", 1)[0]

    def __ingest_and_validate(self, config):
        """ check for errors and missing fields in a loaded yaml configuration
        it does not check whether the queries are valid (use field names that appear in the data);
        this is intentionally left for the accountant to do for security purposes (just in case
        accountant is run without loading configuration using this class) """

        # check for seed
        assert SEED in config, f"Config file is missing integer '{SEED}' value."
        self.__seed = None if config[SEED] is None else int(config[SEED])
        self.__prng = mechanisms.make_prng(self.__seed)

        # check for optional clipping probability
        if CLIPPING_PROB in config and config[CLIPPING_PROB] is not None:
            assert isinstance(config[CLIPPING_PROB], float), "Clipping probability must be either None or a float"
            self.__clipping_prob = config[CLIPPING_PROB]

        # check for accountant class info
        assert ACCOUNTANT in config, f"Config file is missing '{ACCOUNTANT}'."
        accountants = discover_accountants()
        assert config[ACCOUNTANT] in accountants, f"'{config[ACCOUNTANT]}' is not a registered accountant name."
        self.__accountant_class = accountants[config[ACCOUNTANT]]

        # check for data reader class info
        assert READER in config, f"Config file is missing '{READER}'"
        readers = discover_readers()
        assert config[READER] in readers, f"'{config[READER]}' is not a registered data file reader."
        self.__reader_class = readers[config[READER]]

        # check that input files are specified and they exist
        assert INPUT_FILES in config, f"Config file is missing '{INPUT_FILES}'."
        self.__input_files = config[INPUT_FILES]
        for f in self.__input_files:
            assert os.path.isfile(f), f"The file '{f}' does not exist."

        # check the structure of the output data (should contain information for aggregated/tabulated data
        # and for privacy protected (postprocessed) microdata
        assert OUTPUT in config and isinstance(config[OUTPUT], dict), f"Config file is missing '{OUTPUT}'."
        assert AGGREGATE in config[OUTPUT], f"Config file is missing '{AGGREGATE}' inside of '{OUTPUT}'."
        assert isinstance(config[OUTPUT][AGGREGATE], dict), f"'{AGGREGATE}' in '{OUTPUT}' must be dictionary."
        assert POSTPROCESS in config[OUTPUT], f"Config file is missing '{POSTPROCESS}' in '{OUTPUT}'"
        assert isinstance(config[OUTPUT][POSTPROCESS], dict), f"'{POSTPROCESS}' in '{OUTPUT}' must be dictionary."
        # check for measurement folder and set its value
        assert MEASUREFOLDER in config[OUTPUT], f"Config file is missing '{MEASUREFOLDER}' in '{OUTPUT}'"
        assert isinstance(config[OUTPUT][MEASUREFOLDER], str), f"'{MEASUREFOLDER}' must be a string"
        self.__measure_folder = config[OUTPUT][MEASUREFOLDER]
        assert os.path.isdir(self.__measure_folder), f"Folder '{self.__measure_folder}' does not exist."

        # check that output file suffixes are given and are appropriate (use word characters)
        assert SUFFIX in config[OUTPUT][AGGREGATE], f"Config file is missing '{SUFFIX}' in '{OUTPUT}: {AGGREGATE}'"
        assert SUFFIX in config[OUTPUT][POSTPROCESS], f"Config file is missing '{SUFFIX}' in '{OUTPUT}: {POSTPROCESS}'"
        self.__aggregate_suffix = config[OUTPUT][AGGREGATE][SUFFIX]
        self.__postprocess_suffix = config[OUTPUT][POSTPROCESS][SUFFIX]
        assert re.match("^\\w+$", self.__aggregate_suffix) is not None, "Suffixes must only contain word characters"
        assert re.match("^\\w+$", self.__postprocess_suffix) is not None, "Suffixes must only contain word characters"
        assert self.__aggregate_suffix != self.__postprocess_suffix, "Aggregation/postprocess suffixes cannot be same."

        # check that the output folders exist
        assert FOLDER in config[OUTPUT][AGGREGATE], f"'{FOLDER}' missing in '{OUTPUT}: {AGGREGATE}'"
        assert FOLDER in config[OUTPUT][POSTPROCESS], f"'{FOLDER}' missing in '{OUTPUT}: {POSTPROCESS}'"
        self.__aggregate_folder = config[OUTPUT][AGGREGATE][FOLDER]
        self.__postprocess_folder = config[OUTPUT][POSTPROCESS][FOLDER]
        assert os.path.isdir(self.__aggregate_folder), f"Folder '{self.__aggregate_folder}' does not exist."
        assert os.path.isdir(self.__postprocess_folder), f"Folder '{self.__postprocess_folder}' does not exist."

        # check that distances are a dictionary of string, float but leave column validation to accountant
        assert DISTANCES in config and isinstance(config[DISTANCES], dict), f"'{DISTANCES}' list is missing."
        self.__distances = config[DISTANCES]
        for colname in config[DISTANCES]:
            msg = f"Dictionary '{DISTANCES}' must have type string, float"
            assert isinstance(colname, str) and isinstance(config[DISTANCES][colname], float), msg
            assert config[DISTANCES][colname] > 0, "distances must be positive"

        # check for queries being specified and information about the identity query
        # if you do not want to use the identity query, set it to an empty dictionary
        assert QUERIES in config and isinstance(config[QUERIES], dict), f"'{QUERIES}' information is missing."
        msg = f"Information about identity query in field '{QUERIES}: {IDENTITY}' is missing."
        assert IDENTITY in config[QUERIES] and isinstance(config[QUERIES][IDENTITY], dict), msg
        self.__identity_query = config[QUERIES][IDENTITY]
        for colname in config[QUERIES][IDENTITY]:
            msg = f"'{QUERIES}: {IDENTITY}' dictionary must have type string, float"
            assert isinstance(colname, str) and isinstance(config[QUERIES][IDENTITY][colname], float), msg

        # check that the groupby query information is a list, has column names (null, or list of strings)
        # and that mu is dictionary of str, float
        msg = f"Information about groupby query list in field '{QUERIES}: {GROUPBY}' is missing."
        assert GROUPBY in config[QUERIES] and isinstance(config[QUERIES][GROUPBY], list), msg
        msg = (f"Every query in the '{QUERIES}: {GROUPBY}' list must have '{NAME}' (string), "
               f"'{GROUP}' (list of strings, or null), '{MU}' (dict of str, float)")
        for gq in config[QUERIES][GROUPBY]:
            assert isinstance(gq, dict) and NAME in gq and GROUP in gq and MU in gq, msg
            assert isinstance(gq[NAME], str) and isinstance(gq[MU], dict), msg
            assert gq[GROUP] is None or isinstance(gq[GROUP], list), msg
            if isinstance(gq[GROUP], list):
                for mycol in gq[GROUP]:
                    assert isinstance(mycol, str), msg
            for mycol in gq[MU]:
                assert isinstance(mycol, str) and isinstance(gq[MU][mycol], float), msg
        self.__group_by_queries = config[QUERIES][GROUPBY]

    def to_nested_structure(self):
        """ Converts the information in this class into a nested data structure of ints and lists
        that can be serialized back into a yaml file """
        data = {
            SEED: self.__seed,
            ACCOUNTANT: self.__accountant_class.__name__,
            READER: self.__reader_class.__name__,
            INPUT_FILES: self.__input_files,
            OUTPUT: {
                AGGREGATE: {
                    FOLDER: self.__aggregate_folder,
                    SUFFIX: self.__aggregate_suffix
                },
                POSTPROCESS: {
                    FOLDER: self.__postprocess_folder,
                    SUFFIX: self.__postprocess_suffix
                },
                MEASUREFOLDER: self.__measure_folder
            },
            DISTANCES: self.__distances,
            QUERIES: {
                IDENTITY: self.__identity_query,
                GROUPBY: self.__group_by_queries
            }
        }
        if self.clipping_prob is not None:
            data[CLIPPING_PROB] = self.clipping_prob
        return data

    def save(self, filename):
        """ Saves this configuration object as a yaml file """
        with open(filename, "w") as filefp:
            yaml.safe_dump(self.to_nested_structure(), filefp, indent=4)

    @property
    def input_files(self):
        return self.__input_files.copy()

    @property
    def clipping_prob(self):
        return self.__clipping_prob

    @property
    def identity_info(self):
        return self.__identity_query

    @property
    def groupby_info(self):
        return self.__group_by_queries

    @property
    def seed(self):
        return self.__seed

    @property
    def prng(self):
        """ Returns a pseudorandom number generator, based on config information, associated with
         this config instance"""
        return self.__prng

    @property
    def reader_class(self):
        return self.__reader_class

    @property
    def accountant_class(self):
        return self.__accountant_class

    @property
    def distances(self):
        return self.__distances

    def fname(self, data_filename, *, aggregated: bool, postprocessed: bool):
        """ Name of output file that is an aggregated, postprocessed, or both version of data_filename"""
        assert aggregated or postprocessed, "fname() requires either aggregated or postprocessed to be true"
        basename = os.path.basename(data_filename)
        nameparts = basename.rsplit(".", 1)
        stem = nameparts[0]
        extension = f".{nameparts[1]}" if len(nameparts) == 2 else ".csv"
        suffix1 = ""
        suffix2 = ""
        mymarker = f"__{self.__marker}_"
        if aggregated:
            folder = self.__aggregate_folder
        elif postprocessed:
            folder = self.__postprocess_folder
        else:
            raise Exception("fname() requires aggregated or postprocessed to be true")
        if aggregated and not postprocessed:
            mymarker = ""  # ground truth aggregated, so no marker needed
        if aggregated:
            suffix2 = f"_{self.__aggregate_suffix}"
        if postprocessed:
            suffix1 = f"_{self.__postprocess_suffix}"
        filename = f"{folder}/{stem}{mymarker}{suffix1}{suffix2}{extension}"
        return filename

    def noisy_measurement_subfolder(self, data_file_name):
        postprocessed_location = self.fname(data_file_name, aggregated=False, postprocessed=True)
        basename = os.path.basename(postprocessed_location).split(".", 1)[0]
        subfolder_name = f"{self.__measure_folder}/{basename}"
        ensure_dir_exists(subfolder_name)
        return subfolder_name

    def noisy_measurement_filename(self, data_file_name, query_index, query_name):
        subfolder = self.noisy_measurement_subfolder(data_file_name)
        query_name_processed = keep_good_chars(query_name)
        measurename = f"{subfolder}/nmf_{query_index}_{query_name_processed}.csv"
        return measurename
