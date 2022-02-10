#! /bin/bash
ORIGINALDIR="/Users/phalpha/Downloads"
MKDIR files/
baseURL="https://lehd.ces.census.gov/data/lodes/LODES7"
FIPS=("al" "ak" "az" "ar" "ca" "co" "ct" "de" "fl" "ga" "id" "il" "in" "ia" "ks" "ky" "la" "me" "md" "ma" "mi" "mn" "ms" "mo" "mt" "ne" "nv" "nh" "nj" "nm" "ny" "nc" "nd" "oh" "ok" "or" "pa" "ri" "sc" "sd" "tn" "tx" "ut" "vt" "va" "wa" "wv" "wi" "wy")
for i in ${!FIPS[@]}
do
  URL1=${baseURL}/${FIPS[$i]}/wac/${FIPS[$i]}_wac_S000_JT00_2010.csv.gz
  URL2=${baseURL}/${FIPS[$i]}/rac/${FIPS[$i]}_wac_S000_JT00_2014.csv.gz
  URL3=${baseURL}/${FIPS[$i]}/wac/${FIPS[$i]}_rac_S000_JT00_2010.csv.gz
  URL4=${baseURL}/${FIPS[$i]}/rac/${FIPS[$i]}_rac_S000_JT00_2014.csv.gz
  curl -O "$URL1"
  curl -O "$URL2"
  curl -O "$URL3"
  curl -O "$URL4"
  mv ${FIPS[$i]}_wac_S000_JT00_2010.csv.gz files
  mv ${FIPS[$i]}_wac_S000_JT00_2014.csv.gz files
  mv ${FIPS[$i]}_rac_S000_JT00_2010.csv.gz files
  mv ${FIPS[$i]}_rac_S000_JT00_2014.csv.gz files
done
