# Getting under the Skin: The Impact of Terrorist Attacks on Native and Immigrant Sentiment 

[![DOI](https://zenodo.org/badge/424333565.svg)](https://zenodo.org/badge/latestdoi/424333565)


This is a repository to accompany '_Getting under the Skin: The Impact of Terrorist Attacks on Native and Immigrant Sentiment_'. All code on which this analysis is based was written in the [**R**](https://www.r-project.org/) statistical programming language.


## Abstract
There is growing academic interest in examining how terrorist attacks shape the majority’s attitudes towards minority groups. Yet, little is known of how these minority groups react to the backlash such events provoke. This paper leverages the exogenous occurrence of a series of terrorist attacks during the fieldwork period of two surveys to estimate how such events affect the sentiment of both citizens and asylum seekers in Germany. Results of the natural experiment reveal that the 2016 terror attacks in Nice, Würzburg, and Ansbach substantially increased anti-refugee sentiment among German respondents. In line with this increase in hostility, refugees experienced more discrimination, felt less welcome in Germany, and suffered clinically relevant declines in mental health in the aftermath of the attacks. These results provide a more holistic understanding of how terrorism corrodes intergroup relations and how it affects those that are blamed for the events and thus suffer the brunt of the backlash following their occurrence.


## Survey access
To replicate this paper, one has to first apply for and obtain access to the [ALLBUS](https://www.gesis.org/allbus/allbus) and [SOEP](https://www.diw.de/en/diw_01.c.601584.en/data_access.html) survey data, and place the raw survey data files in the `data\raw\soep` and `data\raw\allbus` subfolders. You can generate the structure of the repository by running the `generate_folder_structure.R` script. 

## Structure
The structure of the repository is as follows:
* `\src`: this folder contains all R scripts
* `\data`: this folder contains the data on which the analysis is based. 
* `\output`: this folder includes all output generated by the `R` code. This folder in turn includes the following subfolders: `\models`, `\tables`, and `\plots`, which are each further subdivided to differentiate between descriptive (`\desc`), main (`\main`), and supplementary (`\supp`) output. 

## Working directory
To execute the code, specify the main repository folder as the working directory. Alternatively, execute all `R` code from within `nice-paper_public.Rproj`. 

## Code 
* `00_run_scripts.R`: runs all scripts, and generates all tables and plots that feature in the paper and the appendix. 
* `gen_folder_structure.R`: this script creates the folder structure of the R Project (you only need to run this once, at the start of working on the project). 
* `01_cleaning_allbus.R`: Cleans the ALLBUS survey data. 
* `01_cleaning_soep.R`: Cleans the SOEP survey data.  
* `02_imputation.R`: imputes missing values.
* `03_standardize_vars.R`: standardises continuous variables
* `04_desc.R`: generates the descriptive statistics. 
* `05_main.R`: generates the main results reported in the paper.  
* `05_r_*`: generates the robustness and placebo estimates: each script re-estimates the main results but using a different specification. 
* `06_plot.R`: generates all plots used in the main analysis and across all robustness checks. 

## Authors
__Arun Frey__ is a Postdoctoral Researcher with the Leverhulme Centre for Demographic Science, and a member of the Department of Sociology at the University of Oxford. His research interests include immigration and group conflict, inequality, and causal inference and computational social science. His work has appeared in the _Proceedings of the National Academy of Sciences_, _Social Forces_, _PLOS One_, and the _European Sociological Review_. 


## License 
The replication material is licenced under the [MIT License](https://choosealicense.com/licenses/mit/). 
