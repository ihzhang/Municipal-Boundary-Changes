This folder LODES_Data gathers the LODES Data from https://lehd.ces.census.gov/data/
- for 49 states (other than Hawaii)
- the Residential Area Characteristic and the Working Area Characteristic (RAC) and (WAC) for each state
- for years 2010 and 2014
Total of 49*4=196 files.


- NOTE: the bash script is currently buggy, for 100% chance of this working, just run the Python script!
Unfortunately this means uncompressing and moving around the files manuallly. 




files:
readme.txt: this file
lodes_data_download_script.py: The Python script contains a web scraper using Selenium (overkill! made initially when I didn't realize that all the download links are very systematically written)
download.sh: the bash script to fill in the files folder

folders:
files: all LODES data files

To Do: Run in Terminal "bash download.sh". All files will populate in the appropriate folder within a few minutes.

Note: If running the Python script, you will have to install Selenium and then configure an appropriate Chrome Web Driver.

Note: Currently, the files don't seem to allow unzipping. Will need to investigate this further.


Note: If downloading this folder, just downloading this file is enough! Run it and the rest will populate.
