from .__baseaccountant import BaseAccountant
from scipy.stats import norm as gauss_stat
import numpy as np
from .__configreader import NAME, GROUP, MU
import pandas as pd
import mechanisms


class ClippingAccountant(BaseAccountant):
    __clipping_prob = None

    def __init__(self, prng, reader, distances, clipping_prob):
        super().__init__(prng, reader, distances)
        self.__clipping_prob = clipping_prob
        assert clipping_prob is not None, "Clipping probability must be specified"

    @classmethod
    def from_config(cls, config, reader):
        """ given a config object and a data reader, create the accountant"""
        return cls(config.prng, reader, config.distances, config.clipping_prob)

    def measure_identity(self, identity_mu_dict):
        """ Measures the identity query  """
        if self._no_more_noisy_measurements:
            print("The set of noisy measurements can only be taken once")
            return
        # handle identity query
        identity_mu_spent = 0
        private_attributes_used_by_identity = set()
        for myatt in identity_mu_dict:
            m = identity_mu_dict[myatt]
            if m > 0:
                identity_mu_spent += m
                private_attributes_used_by_identity.add(myatt)
        if identity_mu_spent == 0:
            raise Exception("Identity Query has no mu assigned")
        self.answer_identity_query_using_sqrt(identity_mu_dict)

    def make_noisy_measurements(self, identity_mu_dict, groupby_info):
        if self._no_more_noisy_measurements:
            print("The set of noisy measurements can only be taken once")
            return
        self.measure_identity(identity_mu_dict)
        self.measure_groups(groupby_info)
        self._no_more_noisy_measurements = True

    def measure_groups(self, groupby_info):
        idq = self.get_identity_query()  # to compute clipping thresholds
        #  we get the attributes measured in the identity query
        #  in order to make sure the groupbys measure a subset of them
        private_attributes_used_by_identity = set()
        for myatt in idq.mu_dict:
            m = idq.mu_dict[myatt]
            if m > 0:
                private_attributes_used_by_identity.add(myatt)
        # now for the group by queries
        # first we need to know the entire set of groupby columns
        # and aggregation columns
        private_attributes_used_by_groups = set()
        all_grouping_columns = set()
        uses_overall_agg = False # is there an overall aggregation (i.e., overall sum query)
        for query in groupby_info:
            if query[GROUP] is not None:
                all_grouping_columns.update(query[GROUP])
            else:
                uses_overall_agg = True
            for myatt in query[MU]:
                if query[MU][myatt] > 0:
                    private_attributes_used_by_groups.add(myatt)
        message = "Group by queries use more private attributes than identity"
        assert private_attributes_used_by_groups.issubset(private_attributes_used_by_identity), message
        # get number of groups in the finest possible partition
        if len(list(all_grouping_columns)) == 0:
            num_fine_cells = 1
        else:
            num_fine_cells = self._nonprivate_universe.groupby(list(all_grouping_columns)).ngroups
        # number of private attributes
        num_private_atts = len(private_attributes_used_by_groups)
        numitems = num_fine_cells * num_private_atts
        # get dictionary of what to add to square roots to figure out topcode
        topcode_adder_dict = self.get_topcode_adder(numitems)
        print("#### Clipping information #####")
        print(f"Number of cells: {numitems}")
        for tad in topcode_adder_dict:
            print(f"Attribute {tad}: {topcode_adder_dict[tad]}")
        # now process group by queries
        clip_suffix = "_CLIP"
        for query in groupby_info:
            group_cols = query[GROUP]
            namedagg = {}
            for myatt in query[MU]:  # prepare the aggreagtion dictionary
                if query[MU][myatt] > 0:
                    sqcol = mechanisms.sqrt_col(myatt)
                    namedagg[myatt + clip_suffix] = pd.NamedAgg(column=sqcol, aggfunc="max")
            #  aggregate the identity query to get the largest noisy square roots
            if query[GROUP] is not None:
                id_agg_result = idq.noisy_answers.groupby(group_cols, as_index=True).agg(**namedagg)
            else:
                id_agg_result = idq.noisy_answers.groupby(lambda *x: 0, as_index=True).agg(**namedagg)
            topcode_func_dict = {}  # this is the topcode_dict for answer_groupby_query_using_clip()
            for myatt in query[MU]:
                if query[MU][myatt] > 0:
                    adder = topcode_adder_dict[myatt]
                    if query[GROUP] is None:
                        fun = lambda code_df=id_agg_result, mycol=myatt + clip_suffix, c=adder: (code_df.loc[0][mycol] + c) ** 2
                    elif len(query[GROUP]) == 1:
                        fun = lambda x, code_df=id_agg_result, mycol=myatt + clip_suffix, c=adder: (code_df.loc[x][mycol] + c) ** 2
                    else:
                        fun = lambda *x, code_df=id_agg_result, mycol=myatt + clip_suffix, c=adder: (code_df.loc[x][mycol] + c) ** 2
                    topcode_func_dict[myatt] = fun
            self.answer_groupby_query_using_clip(group_cols, query[MU], topcode_func_dict, name=query[NAME])
        # no more noisy measurements because otherwise
        # the clipping threshold prob would be violated
        self._no_more_noisy_measurements = True

    def get_topcode_adder(self, numitems, tolerance=0.0000001):
        base_t = self.get_gauss_max(numitems, self.__clipping_prob, tolerance)
        # find the identity query
        idq = self.get_identity_query()
        assert idq is not None, "Identity query has not been posed yet"
        topcodes = {}
        # iterate through the idenity query mu dictionary
        for private_att in idq.mu_dict:
            topcodes[private_att] = base_t * self._distances[private_att] / idq.mu_dict[private_att]
        return topcodes

    @classmethod
    def get_gauss_max(cls, numitems, clipping_prob, tolerance=0.0000001):
        """ find the treshold t such that P(max of numitems N(0,1) gaussians > t) <= clipping_prob
         which is P(max of numitems N(0,1) gaussians <= t) >= 1-clipping_prob
         which is P(N(0,1) <= t)^numitems >= 1-clipping_prob
          which is log cdf(t) >= log(1-clipping_prob)/numitems
        """
        rhs = np.log1p(-clipping_prob) / numitems
        upper_t = np.sqrt(-2 * np.log(clipping_prob / numitems))  # upper tail bound
        lower_t = 0  # lower bound on t
        assert gauss_stat.logcdf(upper_t) >= rhs, "Cannot find upper tail bound"
        assert gauss_stat.logcdf(lower_t) <= rhs, "Cannot find lower tail bound"
        while np.abs(upper_t - lower_t) > tolerance:
            mid = (upper_t + lower_t) / 2
            if gauss_stat.logcdf(mid) >= rhs:
                upper_t = mid
            else:
                lower_t = mid
        return upper_t
