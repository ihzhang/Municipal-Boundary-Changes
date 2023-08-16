Municipal Boundary Changes Code 

# THE REPOSITORY IS ACTIVELY UNDER CONSTRUCTION AND IS INCOMPLETE.

This is the publicly available repository for the Municipal Boundary Changes project. This repo contains a lot of code. Collecting underlying data needed for this project involved: 

* DATA_1. Downloading block- and place-level shapefiles for 13 years for 40 states 
* DATA_2. Downloading RAC and WAC (LEHD-LODES data) for 13 years for 40 states 
* DATA_3. Downloading block (2000, 2010, and 2020 Census), place-level demographic data (13 sets of 5-Year ACS data), and 2020-to-2010 and 2000-to-2010 block-to-block crosswalk files from the NGHIS. 
* DATA_4. Downloading Census Boundary and Annexation Survey Data (2000-2010 in one file), then annually from 2011-2021, the most recent year available. 

Most files are too large to store on Github. 

DATA REPOSITORY: LINK TK. 

You can find all replication code in the folder "code".
The analysis process needed for this project involved: 
* CODE_1: Scraping code to bulk download DATA_1
* CODE_2: Scraping code to bulk download DATA_2
* CODE_3: Cleaning DATA_3 after manual download from Social Explorer and NHGIS; some linear interpolation of block-level data was performed on an HPC located at Stanford called Sherlock. 
* CODE_4: Cleaning DATA_4 
* CODE_5: Code that generates the block-to-place crosswalk for intercensal years at the baseline and end year (e.g., 2007 blocks on 2007 places; 2007 blocks on 2008 places; 2007 blocks on 2013 places) --> mostly performed on Sherlock
* CODE_6: Code that generates buffers defining annexable blocks to places --> mostly performed on Sherlock
* CODE_7: Code that identifies annexations (6-year periods) via analysis of outputs from CODE_5 and CODE_6
* CODE_8: Code that transforms blocks-level data (output from CODE_7) into place-level data with relevant demographic information (6-year periods) (output from CODE_3) and preps data for panel form; also uses output from CODE_4
* CODE_9: Code for statistical analyses presented in the paper via output from CODE_8
* CODE_10: Code for descriptive statistics presented in the paper using output from CODE_8
* CODE_11: Code for maps presented in the paper using output from CODE_7
* CODE_12: Code that identifies annexations (1-year periods)
* CODE_13: Code that transforms blocks-level data into place-level data with relevant demographic information (1-year) periods
* CODE_14: Code for statistical analyses/robustness checks based on 1- and 2-year periods

TK 

You can find all results -- in the raw form and in the formatted form -- in the folder called "results".
