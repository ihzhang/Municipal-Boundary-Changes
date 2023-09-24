Municipal Boundary Changes Replication Package. 

This is the publicly available repository for my forthcoming paper in the _Du Bois Review_, "THE LIMITS OF PRECLEARANCE: _Municipal Annexations Before and After Shelby County v. Holder._" by Iris H. Zhang. This repo contains all the data & code for complete replication of the paper. **Please cite the paper if using any code/data from this repository.** My contact: ihzhang@stanford.edu. 

# DATA 
Collecting underlying data needed for this project involved: 
* DATA_1. Downloading block- and place-level shapefiles for 13 years for 40 states 
* DATA_2. Downloading RAC and WAC (LEHD-LODES data) for 13 years for 40 states 
* DATA_3. Downloading block (2000, 2010, and 2020 Census), place-level demographic data (13 sets of 5-Year ACS data), and 2020-to-2010 and 2000-to-2010 block-to-block crosswalk files from the NGHIS. 
* DATA_4. Downloading Census Boundary and Annexation Survey Data (2000-2010 in one file), then annually from 2011-2021, the most recent year available. 

## data-repo Google Drive 

Most files are too large to store on Github and therefore are on a Google Drive link instead. Everything is at this link: https://drive.google.com/drive/folders/1Rk4K8ukEiER4gSTwzacWsp61jxNeLYP3?usp=sharing.

In this folder you will find: 
* annexations_blocklevel: contains the output from CODE_7
* BAS: DATA_4
* block_ids: contains the .csv version of block IDS from block-level shapefiles, downloaded using CODE_1, part of DATA_1 
* block_to_block_cw: crosswalks downloaded from NHGIS to convert 2000 and 2020 blocks to 2010 block boundaries; part of DATA_3
* block_to_place_cw_final: contains the output from CODE_5
* buffers_final: contains the output from CODE_6 
* dem_data_clean: blocks contains the output from SHERLOCK_interpolate_blocks for using DATA_3; places contains the output from CODE_3 on DATA_3
* dem_data_raw: raw version of DATA_3
* lodes: raw and cleaned versions of DATA_2; actually available at this link: https://drive.google.com/drive/folders/1qErzxikdsnOGJv6OlAgeWQVdovwfKG9y?usp=sharing. 
* shapefiles: place-level shapefiles, downloaded from CODE_1, part of DATA_1. Block-level shapefiles are at this link: https://drive.google.com/drive/folders/1lSv4SmHQ73NkqEBPNIr5UUa-_UDxc85Z?usp=sharing. Please be advised that the block-level shapefiles are very large. It may in fact be more efficient to download them from the source using the code outlined in CODE_1. 

# CODE 
You can find all replication code in the folder "code". Please note that the directory names will have changed from the code that correspond to the folder structure in this data repository. If you run into issues or questions on how to navigate that, please reach out. 

## Code filenames index
The analysis process needed for this project involved the following code, numbered in sequence that correspond to the steps of the project: 
* CODE_1: Scraping code to bulk download DATA_1
* CODE_2: Scraping code to bulk download DATA_2 and clean it
* CODE_3: Cleaning DATA_3 after manual download from Social Explorer and NHGIS; some linear interpolation of block-level data was performed on an HPC located at Stanford called Sherlock. 
* SHERLOCK_interpolate_blocks: Generate interpolated block-level data for intercensal years --> conducted on Sherlock 
* CODE_4: Cleaning DATA_4 
* CODE_5: Code that generates the block-to-place crosswalk for intercensal years at the baseline and end year (e.g., 2007 blocks on 2007 places; 2007 blocks on 2008 places; 2007 blocks on 2013 places) --> mostly performed on Sherlock
* CODE_6: Code that generates buffers defining annexable blocks to places --> mostly performed on Sherlock
* CODE_7: Code that identifies annexations (6-year periods & 1-year periods) via analysis of outputs from CODE_5 and CODE_6
* CODE_8: Code that transforms blocks-level data (output from CODE_7) into place-level data with relevant demographic information (6-year periods) (output from CODE_3) and preps data for panel form; also uses output from CODE_4
* CODE_9: Code for statistical analyses presented in the paper via output from CODE_8
* CODE_10: Code for descriptive statistics presented in the paper using output from CODE_8
* CODE_11: Code for maps presented in the paper using output from CODE_7 and CODE_8
* CODE_12: Code that transforms blocks-level data into place-level data with relevant demographic information (1-year) periods
* CODE_13: Code for statistical analyses/robustness checks based on 1- and 2-year periods

* state laws database: contains code and compiled state laws governing annexations -- not used in paper. 

### Please do not attempt to sift through code/data/notes in folders marked "archive" without reaching out to me. These files are not useful for replication but document many iterations of code and data generation. 

# RESULTS

You can find all results -- in the raw form and in the formatted form -- in the folder called "results". 

## Folder structure/index 
There are sub-folders labeled according to the time interval: 
* t0020: this the 6-year period used in the core analysis
* one_year: this is the 1-year period used in supplementary analysis 
* two_year: this is the 2-year period used in supplementary analysis 
* supplementary: these contains outputs from robustness/sensitivity analyses mentioned in the paper 
* maps: these contain the outputs from CODE_11
* analysis.RData is an RData file saving files from CODE_9, which I often used as a way to store outputs when putting down/picking up various aspects of the analysis. 
