The goal of the scripts within this folder is to analyze the dominance data that I obtained from captive and wild populations of house sparrows.

List of scripts and actions:

001_Dominance_databases_cleaning_and_summary: it takes the dominance database created during the analysis of the videos and cleans it. This involves searching for typos, missidentifications, wrong sex assimments, un-existent colour combinations, removing of colour combinations assigned to more than 1 bird during the study period, etc, etc, etc

002_Elo-rating_estimation: it estimates the Standardized (0-1) Elo-rating (StElo) for each individual. By default, one StElo will be estimated per event per individual, but there is commented code that will do it for the whole database or even more detailed.

003_Databases_construction_Lundy: it generates the Lundy databases needed for the following statistical analyses.

003_Databases_construction_Seewiesen: it generates the final database to be used for the captive analyses. It does the whole process, so it would be equivalent to what the scripts: 002_Elo-rating_estimation and 003_Databases_construction_Lundy do for the Lundy data.