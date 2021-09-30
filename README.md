Municipal Boundary Changes Code and data 

DISCLAIMER: this is very much still a work in progress. Please do NOT attempt replication based on this repository as it is. Instead, please contact me at ihzhang@stanford.edu if there are questions about getting the data and analyzing it. Please feel free to refer to the draft but plaese do not distribute or cite wthout permission. 

CONTENTS

Code: follow the 1-2-3 steps implied by the names of the .R files

1. ONE. (type = .R file) contains code used to clean 1990, 2000 and 2010 Census data
place-level data, which producees lagged 1990-2000 and 2000-2010 place-level variables, and 2000 block-level variables. Run this file to get demographic data
2. TWO. buffer (type = .R file) containts code used to identify all annexable blocks to unique places 
using a 400-meter buffer
3. THREE. Spatial Merges (type = .R file) contains code used to identify annexing blocks and merges demographic data from ONE and TWO. This is done to have all demographic data for places that annexed betweeen 2000-2010; which blocks they annexed and their demographic data; which blocks they could have annexed and their demographic data 
4. FOUR. Analysis (type = .R file) contains code used to generate model estimates, plots, and descriptives
5. Census cleaning contains code to generate plots describing demographic changes used in the paper 

Please note that you will have to change your working directory in each of these files depending on 
where you download your files to from the data repository. 

Data repository is at this Google Drive link: https://drive.google.com/drive/folders/1SxBi9-UOBPHSu04K7fAF5efH1LVgJyCB?usp=sharing. 