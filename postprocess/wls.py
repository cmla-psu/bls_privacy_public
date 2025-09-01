from scipy.sparse.linalg import lsqr
from scipy import sparse
import numpy as np


class WLSFromDF:
    """ This class takes a set of  (noisy) group-by sum query answers on a set of aggregation attributes
    and solves a weighted least squares optimization problem on them.

    We first start with a "Universe" dataframe with 1 row per universe element. The columns are the group
    by attributes. Then one specifies the noisy group by query answers, which is a dataframe and a tuple of the
    group by attributes for the query. Each one of these dataframes has some aggregation columns. Each
    aggregation column optionally contains a corresponding std column (it is used to create an inverse-variance
    weighted least squares solution). The std column should contain std values, not variance values.

    Then one specifies the aggregation columns of interest (along with their optional std columns) and it
    synthesizes, for each row in the universe matrix, an estimate of its value of the aggregation columns.
    The result is a new dataframe.
    """

    def __init__(self, universe_df):
        """ Initialize class with the universe pandas dataframe, which has 1 row for each universe element
        and only the group-by columns listed. Internally, a **copy** of the universe df is stored and updated.
        """
        self.__universe = universe_df.copy()  # make sure to create a copy, since we will be modifying this df
        # Next is a list of (query answer df, group-by columns),  if groupby columns is [] or None then
        # this query is interpreted as an identity query if the number of rows is the same as in the universe
        # and is interpreted as the sum query if the number of rows is 1, otherwise it is an error
        self.__queries = []
        # Next is the list of aggregation columns. The list elements have the form (agg column, std column)
        # where agg column is the name of the pandas column for this aggregation column (all group-by queries
        # in which it appears have to have the same name for the column). std column is either the name of the
        # pandas df columns that contain the std for this agg column, or is None, in which case std=1 is used.
        self.__targets = []

    @property
    def universe(self):
        """ After solving, this has the solved dataframe"""
        return self.__universe

    def add_query(self, df, groupcols):
        """ add a query to be used in the weighted least squares solve
        Inputs:
            df : a pandas dataframe that resulted from a group-by sum query.
            groupcols: a tuple or list of the columns that were grouped on.
                       use None for the overall sum query or identity query.
        """
        # If the group by columns are None, make sure the query is either the identity (same number of rows
        #   as the universe dataframe) or overall sum (1 row)
        if groupcols is None:
            nrows = df.shape[0]
            universe_rows = self.universe.shape[0]
            if nrows != 1 and nrows != universe_rows:
                raise Exception("Groupcols is None, but this is neither the identity or overall sum query")
        else:  # Otherwise check that the group by columns actually appear in the dataframe
            for g in groupcols:
                assert g in df, "group by column g is not in the dataframe"
        self.__queries.append((df, groupcols))

    def add_target(self, aggcol, stdcol):
        """ Add a target column to be reconstructed via weighted least squares
        inputs:
           aggcol: the name of the aggregation column as it appears in all the relevant query dataframes
           stdcol: the name of the column in those dataframes that provides the std to be used in
                   inverse variance weighting (note this is giving the std, not the variance). This
                   field can be None, in which case the default weighting of 1 is used. If the aggregation
                   column is missing in a dataframe, it is treated as 1.
        """
        self.__targets.append((aggcol, stdcol))

    def solve(self):
        """ Solves the weighted least squares problem and returns a dataframe containing its solution """
        for (aggcol, stdcol) in self.__targets:
            self.solve_for(aggcol, stdcol)
        return self.universe

    def solve_for(self, aggcol, stdcol):
        """ Solves the weighted least squares problem for a specific column and add it to the universe dataframe
        inputs:
            aggcol: the aggregation column
            stdcol: the column that has the std to be used in inverse variance weighting. 1 is used if
                    stdcol is None. If any groupby dataframes do not have the stdcol, 1 is also used
        """
        A, b = self.getAB(aggcol, stdcol)
        x, _, itn, r1norm = sparse.linalg.lsqr(A, b)[0:4]
        print(f"Solution found in {itn} iterations with residual norm: {r1norm}")
        self.universe[aggcol] = x

    def getAB(self, aggcol, stdcol):
        """ Generates the A and b matrix for the weighted least squares problem (||Ax-b||^2) for a specific column
        inputs:
            aggcol: the aggregation column
            stdcol: the column that has the std to be used in inverse variance weighting. 1 is used if
                    stdcol is None. If any groupby dataframes do not have the stdcol, 1 is also used
        """
        # first create the sparse matrix A for the Ax=b solve and the dense matrix b
        # A will be in COO format because it is fast to create, then it will be converted to csr before
        # being passed to a solver.
        ncolsA = self.__universe.shape[0]  # number of columns of A is the number of universe items
        nrowsA = 0  # we need to iteratively compute the number of rows (scalar queries)
        nrelevant_queries = 0  # number of queries that aggregate over aggcol
        for df, gcol in self.__queries:
            if aggcol in df:
                nrowsA += df.shape[0]
                nrelevant_queries += 1
        # Compute the number of nonzero entries in A. Since every universe item appears in exactly one group
        # in each group by query, the number of nonzero entries is number of universe items times number of queries
        sparse_length = self.__universe.shape[0] * nrelevant_queries
        # initialize COO data
        i_list = np.zeros(sparse_length, dtype=np.int32)
        j_list = np.zeros(sparse_length, dtype=np.int32)
        data_list = np.zeros(sparse_length, dtype=np.float64)
        # initialize b vector:
        b = np.zeros(nrowsA)
        # create a mapping from specific group to scalar query (this indexes both the rows of A and b).
        indexer = []
        row_index = 0  # row of A and b we are processing
        coo_index = 0  # index into the current data item in coo representation
        for df, gcol in self.__queries:
            if aggcol not in df:  # is this query relevant?
                continue
            # get aggregation column and std
            aggcol_values = df[aggcol].to_numpy()
            if stdcol in df:
                std_values = df[stdcol].to_numpy()
            else:
                std_values = np.ones(aggcol_values.size)
            if (not gcol) and (len(aggcol_values) == 1):  # sum query
                the_agg = aggcol_values[0]
                the_std = std_values[0]
                b[row_index] = the_agg / the_std
                for column_index in range(ncolsA):  # go through each item of the universe
                    i_list[coo_index] = row_index
                    j_list[coo_index] = column_index
                    data_list[coo_index] = 1 / the_std
                    coo_index += 1
                row_index += 1
            elif len(aggcol_values) == ncolsA:  # identity query
                for column_index, (the_agg, the_std) in enumerate(zip(aggcol_values, std_values)):
                    i_list[coo_index] = row_index
                    j_list[coo_index] = column_index
                    data_list[coo_index] = 1 / the_std
                    b[row_index] = the_agg / the_std
                    coo_index += 1
                    row_index += 1
            elif not gcol:
                raise Exception("Groupby with empty gcol given, but it is not identity or sum query")
            else:  # normal groupby query. We will store the query index information and fill coo values later
                mydict = {}
                gcol_tuples = zip(*tuple(list(df[g]) for g in gcol))
                for gtuple, the_agg, the_std in zip(gcol_tuples, aggcol_values, std_values):
                    mydict[gtuple] = (row_index, the_std)
                    b[row_index] = the_agg / the_std
                    row_index += 1
                indexer.append((mydict, gcol))
        assert row_index == nrowsA  # if everything went correctly, these should be the same, as b is done
        # now go through each item in the universe and find the group by query groups it is in
        out_of_pandas = zip(*tuple(list(self.universe[g]) for g in list(self.universe.columns)))
        lookup = {g: i for i, g in enumerate(list(self.universe.columns))}
        for (j, universe_record) in enumerate(out_of_pandas):
            for row_index_dict, gcol in indexer:
                features = tuple(universe_record[lookup[g]] for g in gcol)
                row_index, the_std = row_index_dict[features]
                i_list[coo_index] = row_index
                j_list[coo_index] = j
                data_list[coo_index] = 1 / the_std
                coo_index += 1
        assert coo_index == sparse_length  # these should be the same
        a_csr = sparse.coo_array((data_list, (i_list, j_list)), shape=(nrowsA, ncolsA)).tocsr()
        return a_csr, b
