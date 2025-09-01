from .__baseaccountant import BaseAccountant
from .__configreader import NAME, GROUP, MU


class SQRTAccountant(BaseAccountant):
    def make_noisy_measurements(self, identity_mu_dict, groupby_info):
        # handle identity query
        identity_mu_spent = 0
        for m in identity_mu_dict.values():
            if m > 0:
                identity_mu_spent += m
        if identity_mu_spent == 0:
            raise Exception("Identity Query has no mu assigned")
        # groupby queries
        self.answer_identity_query_using_sqrt(identity_mu_dict)
        for query in groupby_info:
            self.answer_groupby_query_using_sqrt(query[GROUP], query[MU], name=query[NAME])


