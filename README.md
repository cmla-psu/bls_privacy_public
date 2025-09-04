# BLS PROJECT REPOSITORY

This repository contains the experimental materials and completed privacy model 1 code for the BLS project. 

## Folders
accountant, postprocess, readers, reports, mechanisms: all contain python functions to implement the sqrt mechanism and pnc mechanism, wls postprocessing, and creating reports and tabulations of the sanitized data. These functions are called in main.py

simulation_experiments: There are two sub-folders, synthetic_generation and run_experiments. In synthetic_generation is the python and R code with instructions on running the code to generate synthetic QCEW data from CBP and QWI sources. We summarize the process described in detail in generate_synthetic_instructions.txt below. In run_experiments there is python, R, and .sh files to sanitize the synthetic data and then analyze it across two main experiments. The first "equalwages" looks at replicates for when wages and employment have equal allocation within a query. The second "budgets" looks at replicates across several total privacy loss budgets. We summarize the process described in detail in run_experiments_instructions.txt below. 

### In experiments directory:

#### Summary of generation_synthetic/generate_synthetic_instructions.txt

First, retrieve the CBP, QWI, and imputed CBP datasets and place them in thier respective folders within generation_synthetic/DataIn. Then run preprocess_combine.py followed by getNAICS6data.R. Next use the Jupyter notebook microdataFromPython to create establishment level wage and employment data. Finally, run MicrodataPostprocessing.R to make the final datasets by state with additional variables.

#### Summary run_experiments/run_experiments_instructions.txt
First, copy main.py and the following folders accountant, comparisons, mechanisms, readers, and reports into run_experiments folder. Create the additional needed directories: measurements, compare_data, aggregated_data, and protected_data. To get the sanitized results run simulated_experiments.ipynb and follow the additional commented instructions about when to run the .sh files. Finally, the analysis of the experimental results is in main_tables.R, main_equalwages_plots.R, and main_budgets_plots.R in the experiments/run_experiments/R_code folder.




