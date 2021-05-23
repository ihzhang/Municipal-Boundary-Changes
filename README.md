Municipal Boundary Changes Code and data 

DISCLAIMER: this is very much still a work in progress. Please do NOT attempt replication based on this repository as it is. Instead, please contact me at ihzhang@stanford.edu if there are questions about getting the data and analyzing it. Please feel free to refer to the draft but plaese do not distribute or cite wthout permission. 

CONTENTS
Code: follow the 1-2-3 steps implied by the names of the .R filese

1. ONE. (type = .R file) contains code used to clean 1990, 2000 and 2010 Census data
place-level data, which producees lagged 1990-2000 and 2000-2010 place-level variables, and 2000 block-level variables. Run this file to get demographic data
2. Shapefiles_contig (type = folder) contains all contiguous blocks to places by-state in .csv format, used in step 3. 
3. TWO. (type = .R file) contains code used to identify annexing blocks and merges demographic data from ONE. This is done to have all demographic data for places that annexed betweeen 2000-2010; which blocks they annexed and their demographic data; which blocks they could have annexed and their demographic data 
4. final analytical sample produced after running ONE and TWO and used for THREE
5. THREE. contains code for statistical modeling of the data 
6. place-level data for 1990, 2000, and 2010 Census data
7. Latest draft of paper 


NOT INCLUDED

8. block-level shapefiles (will be made public after figuring out storage, as each file is well over 100mb). 
9. analysis code to identify contiguous blocks used in 2. They were made in ArcGIS using shapefiles mentioned in 8.