import sys
import os
import accountant
import csv
import reports
import time

def start(config: accountant.FPConfig):
    for infile in config.input_files:
        # get names of files needed to be produced
        aggregated_fname = config.fname(infile, postprocessed=False, aggregated=True)
        postprocessed_fname = config.fname(infile, postprocessed=True, aggregated=False)
        post_agg_fname = config.fname(infile, postprocessed=True, aggregated=True)
        print(f"##### Processing Data ######")
        print(time.asctime())
        print(f"Microdata file name: {infile}")
        print(f"Aggregated microdata file name: {aggregated_fname}")
        print(f"Privacy protected microdata name: {postprocessed_fname}")
        print(f"Aggregated protected file name: {post_agg_fname}")
        print()

        # Create protected microdata
        if os.path.isfile(postprocessed_fname):
            print(f"Postprocessed microdata file {postprocessed_fname} exists, skipping ...")
        else:
            print("Reading data ... (" + time.asctime() + ")")
            my_reader = config.reader_class.from_config(config)
            my_reader.read(infile)
            my_accountant = config.accountant_class.from_config(config, my_reader)
            print(f"Protecting ... ({time.asctime()})")
            my_accountant.make_noisy_measurements(config.identity_info, config.groupby_info)
            protected_data = my_accountant.postprocess_into_microdata()
            print("Saving protected microdata ... (" + time.asctime() + ")")
            protected_data.to_csv(postprocessed_fname, index=False, quoting=csv.QUOTE_NONNUMERIC)
            for query_index, queryinfo in enumerate(my_accountant.answered_queries):
                query_name = queryinfo.name if queryinfo.name is not None else ""
                nmfname = config.noisy_measurement_filename(infile, query_index, query_name)
                if not os.path.exists(nmfname):
                    print(f"Saving noisy measurement {nmfname} (" + time.asctime() + ")")
                    queryinfo.noisy_answers.to_csv(nmfname, index=False, quoting=csv.QUOTE_NONNUMERIC)
        # tabulate
        print("Tabulating ... (" + time.asctime() + ")")
        if os.path.isfile(aggregated_fname):
            print(f"Aggregated ground truth file {aggregated_fname} exists, skipping ...")
        else:
            reports.aggregate_qcew_from_file(infile, aggregated_fname)
        if os.path.isfile(post_agg_fname):
            print(f"Aggregated protected data {post_agg_fname} exists, skipping ....")
        else:
            reports.aggregate_qcew_from_file(postprocessed_fname, post_agg_fname)
        print("Done with this round. (" + time.asctime() + ")")
        print()

def main():
    """ Validate command line arguments, get config and then call start()"""
    if len(sys.argv) != 2:
        print("Config file not specified")
        print(f"Usage: python {sys.argv[0]} config_file_name")
        return
    conf_file = sys.argv[1]
    if not os.path.isfile(conf_file):
        print(f"Cannot locate file named {conf_file} ")
    config = accountant.FPConfig(conf_file)
    start(config)


if __name__ == "__main__":
    main()
