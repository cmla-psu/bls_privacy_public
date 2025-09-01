import numpy as np
from abc import ABC, abstractmethod

import pandas as pd

import mechanisms
from postprocess.wls import WLSFromDF
from typing import List

class QueryInfo:
    IDENTITY = "IDENTITY"
    GROUPBY = "GROUPBY"

    def __init__(self, group_cols, mu_dict, noisy_answers, qtype, name=None):
        self.group_cols = group_cols  # columns that the query was grouped on
        self.mu_dict = mu_dict
        self.noisy_answers = noisy_answers  # data frame of noisy answers
        assert qtype in [self.IDENTITY, self.GROUPBY]
        self.qtype = qtype
        self.name = name if name is not None else ""


class BaseAccountant(ABC):
    # self.__prng: pseudo random number generator
    _no_more_noisy_measurements = False
    _distances = None
    answered_queries: List[QueryInfo] = list()

    def __init__(self, prng, reader, distances):
        """ Creates an accounting with a pseeudorandom number generator
        and a data reader instance that has already loaded data.
        distances is a dictionary of private column to its distance parameter """
        self.__prng = prng
        self.__reader = reader
        self.__plb_spent_squared = 0.0  # keeps track of the square of plb, as this is additive
        self._nonprivate_universe = self.__reader.df[self.__reader.public_cols].copy()
        self.answered_queries = []
        self._distances = distances
        self._no_more_noisy_measurements = False

    @abstractmethod
    def make_noisy_measurements(self, identity_mu_dict, groupby_info):
        """ Take noisy measurements given the mu dict fin identity_mu_dict and
        the list of groupby queries, which a dictionary containing 'name' (__configreader.NAME) of the query
        'group' (group by columns, or null for overall sum query) (__configreader.GROUP)
        and 'mu' (the mu dictionary) (configreader.MU)
        """
        pass

    def postprocess_into_microdata(self):
        # set the public fields
        mysolver = WLSFromDF(self._nonprivate_universe)
        # set the targets for inference
        for privcol in self.__reader.private_cols:
            stdcol = mechanisms.std_col(privcol)
            mysolver.add_target(privcol, stdcol)
        for q in self.answered_queries:
            mysolver.add_query(q.noisy_answers, q.group_cols)
        protected_microdata = mysolver.solve()
        return protected_microdata

    @classmethod
    def from_config(cls, config, reader):
        """ given a config object and a data reader, create the accountant"""
        return cls(config.prng, reader, config.distances)

    def validate_query(self, rawgroupcols, mu_dict):
        """ Checks whether a query is valid, grouping only on
        public attributes and aggregating on only private attributes with valid mu values
        input:
           rawgroupcols: list of strings representing columns to group by
           mu_dict: dictionary of private attribute to nonnegative mu value
        output:
           empty string if everything is ok, otherwise a string with an error message.
        """
        groupcols = rawgroupcols if rawgroupcols is not None else []
        public_cols = set(self.__reader.public_cols)
        private_cols = set(self.__reader.private_cols)
        messages = []
        if not public_cols.issuperset(groupcols):
            messages.append(f"Group cols '{', '.join(groupcols)}' are not subset of public columns")
        agg_cols = list(mu_dict.keys())
        if not private_cols.issuperset(agg_cols):
            messages.append(f"Aggregation cols '{', '.join(agg_cols)}' are not a subset of the private columns")
        for m in mu_dict.values():
            if not (isinstance(m, float) or isinstance(m, int) or isinstance(m, np.float64)):
                messages.append(f"mu value '{m}' has type {type(m)} instead of int or float.")
                break
            if m < 0:
                messages.append(f"mu value '{m}' must be nonnegative.")
                break
        return "\n".join(messages)

    @property
    def plb_spent(self):
        """ PLB spent so far in terms of mu-gdp """
        return np.sqrt(self.__plb_spent_squared)

    def answer_identity_query_using_sqrt(self, mu_dict, name="Identity"):
        """ Answers identity query using the square root mechanism. The group cols are the public fields
        and mu_dict is a dictionary that specifies a private aggregation field and its mu privacy loss
        budget. The budget spent gets updated. The answered query is added to the self.answered_queries
        list as a QueryInfo object as long as the mu is nonzero """
        if self._no_more_noisy_measurements:
            print("No more noisy measurements are allowed")
            return
        group_cols = self.__reader.public_cols
        message = self.validate_query(group_cols, mu_dict)
        if len(message) > 0:
            raise Exception(message)
        self.__plb_spent_squared += sum([x ** 2 for x in mu_dict.values()])
        agg_cols = [x for x in mu_dict.keys() if mu_dict[x] > 0.0]
        std_list = [self._distances[x] / mu_dict[x] for x in agg_cols]
        if len(agg_cols) == 0:
            return
        answer = mechanisms.sqrt_sum_mech(self.__prng, self.__reader.df, group_cols,
                                          agg_cols, std_list, identity=True)
        self.answered_queries.append(QueryInfo(group_cols, mu_dict, answer, QueryInfo.IDENTITY, name=name))

    def answer_groupby_query_using_sqrt(self, group_cols, mu_dict, name=None):
        """ Answers groupby query using the square root mechanism and the speicified group_cols.
        mu_dict is a dictionary that specifies a private aggregation field and its mu privacy loss
        budget. The budget spent gets updated. Returns a QueryInfo object or None if no mu is spent on
        any columns """
        if self._no_more_noisy_measurements:
            print("No more noisy measurements are allowed")
            return
        message = self.validate_query(group_cols, mu_dict)
        if len(message) > 0:
            raise Exception(message)
        self.__plb_spent_squared += sum([x ** 2 for x in mu_dict.values()])
        agg_cols = [x for x in mu_dict.keys() if mu_dict[x] > 0.0]
        std_list = [self._distances[x] / mu_dict[x] for x in agg_cols]
        if len(agg_cols) == 0:
            return
        answer = mechanisms.sqrt_sum_mech(self.__prng, self.__reader.df, group_cols,
                                          agg_cols, std_list, identity=False)
        self.answered_queries.append(QueryInfo(group_cols, mu_dict, answer, QueryInfo.GROUPBY, name=name))

    def answer_groupby_query_using_clip(self, group_cols, mu_dict, topcode_dict, name=None):
        """ Answer groupby query on group_cols using the topcode mechanism. Aggregation column
        mu values are provided by mu_dict, and topcode_dict is the dictionary of topcode functions
        for each aggregation column. The topcode function takes as input the group_col values """
        if self._no_more_noisy_measurements:
            print("No more noisy measurements are allowed")
            return
        message = self.validate_query(group_cols, mu_dict)
        if len(message) > 0:
            raise Exception(message)
        assert set(mu_dict.keys()) == set(topcode_dict.keys()), "keys for mu dictionary do not match topcode dictionary"
        self.__plb_spent_squared += sum([x ** 2 for x in mu_dict.values()])
        agg_cols = [x for x in mu_dict.keys() if mu_dict[x] > 0.0]
        # input to an std fun is a topcode for a group and the output is the std
        std_funs = [(lambda y, m=mu_dict[x], c=self._distances[x]: np.maximum(c ** 2, 2 * c * np.sqrt(y) - c ** 2) / m)
                    for x in agg_cols]
        tflist = [topcode_dict[x] for x in agg_cols]
        if len(agg_cols) == 0:
            return
        answer = mechanisms.topcode_gmech(self.__prng, self.__reader.df, group_cols, agg_cols, topcode_func_list=tflist,
                                          base_stds=std_funs, identity=False)
        self.answered_queries.append(QueryInfo(group_cols, mu_dict, answer, QueryInfo.GROUPBY, name=name))

    def get_identity_query(self):
        result = None
        for q in self.answered_queries:
            if q.qtype == QueryInfo.IDENTITY:
                result = q
        return result