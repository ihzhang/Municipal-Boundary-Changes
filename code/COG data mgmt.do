*******************************************************************************
************************** ASLG/Census of Gov's *******************************
*******************************************************************************
*https://www.census.gov/programs-surveys/gov-finances/data/historical-data.html

* For previous version, see "/Users/beckucdenver/Google Drive/Research & Writing/Decline of Broken Windows/Data/Expenditure Data/ASLG/2020.10.01 ASLG Data Mgmt Do - Munis.do"

* Merge 'a', 'b', and 'c' Files Together
cd "/Users/beckucdenver/Google Drive/Research/City Dataset 2.0/Data/Expenditure data - in Decline folder v1, on ExternalHD v2/_IndFin_1967-2012/"
local filelist : dir . files "*.Txt"
foreach file of local filelist{
insheet using `file'
save "`file'.dta"
clear
}

use "IndFin90a.Txt.dta"
merge 1:1 id using "IndFin90b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin90c.Txt.dta"
drop _merge
destring censusregion fipscodestate, replace
save "1990.dta", replace

use "IndFin91a.Txt.dta"
merge 1:1 id using "IndFin91b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin91c.Txt.dta"
drop _merge
destring censusregion fipscodestate, replace
save "1991.dta", replace

use "IndFin92a.Txt.dta"
merge 1:1 id using "IndFin92b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin92c.Txt.dta"
drop _merge
destring censusregion fipscodestate, replace
save "1992.dta", replace

use "IndFin93a.Txt.dta"
merge 1:1 id using "IndFin93b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin93c.Txt.dta"
drop _merge
destring censusregion fipscodestate, replace
save "1993.dta", replace

use "IndFin94a.Txt.dta"
merge 1:1 id using "IndFin94b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin94c.Txt.dta"
drop _merge
destring censusregion fipscodestate, replace
save "1994.dta", replace

use "IndFin95a.Txt.dta"
merge 1:1 id using "IndFin95b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin95c.Txt.dta"
drop _merge
destring censusregion fipscodestate, replace
save "1995.dta", replace

use "IndFin96a.Txt.dta"
merge 1:1 id using "IndFin96b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin96c.Txt.dta"
drop _merge
save "1996.dta", replace

use "IndFin97a.Txt.dta"
merge 1:1 id using "IndFin97b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin97c.Txt.dta"
drop _merge
save "1997.dta", replace

use "IndFin98a.Txt.dta"
merge 1:1 id using "IndFin98b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin98c.Txt.dta"
drop _merge
save "1998.dta", replace

use "IndFin99a.Txt.dta"
merge 1:1 id using "IndFin99b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin99c.Txt.dta"
drop _merge
save "1999.dta", replace

use "IndFin00a.Txt.dta"
merge 1:1 id using "IndFin00b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin00c.Txt.dta"
drop _merge
tostring yearofdata, replace
tostring fyenddate, replace
save "2000.dta", replace

use "IndFin01a.Txt.dta"
merge 1:1 id using "IndFin01b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin01c.Txt.dta"
drop _merge
save "2001.dta", replace

use "IndFin02a.Txt.dta"
merge 1:1 id using "IndFin02b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin02c.Txt.dta"
drop _merge
save "2002.dta", replace

use "IndFin03a.Txt.dta"
merge 1:1 id using "IndFin03b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin03c.Txt.dta"
drop _merge
save "2003.dta", replace

use "IndFin04a.Txt.dta"
merge 1:1 id using "IndFin04b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin04c.Txt.dta"
drop _merge
tostring revisedate, replace
save "2004.dta", replace

use "IndFin05a.Txt.dta"
merge 1:1 id using "IndFin05b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin05c.Txt.dta"
drop _merge
save "2005.dta", replace

use "IndFin06a.Txt.dta"
merge 1:1 id using "IndFin06b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin06c.Txt.dta"
drop _merge
save "2006.dta", replace

use "IndFin07a.Txt.dta"
merge 1:1 id using "IndFin07b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin07c.Txt.dta"
drop _merge
save "2007.dta", replace

use "IndFin08a.Txt.dta"
merge 1:1 id using "IndFin08b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin08c.Txt.dta"
drop _merge
save "2008.dta", replace

use "IndFin09a.Txt.dta"
merge 1:1 id using "IndFin09b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin09c.Txt.dta"
drop _merge
save "2009.dta", replace

use "IndFin10a.Txt.dta"
merge 1:1 id using "IndFin10b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin10c.Txt.dta"
drop _merge
save "2010.dta", replace

use "IndFin11a.Txt.dta"
merge 1:1 id using "IndFin11b.Txt.dta"
drop _merge
merge 1:1 id using "IndFin11c.Txt.dta"
drop _merge
save "2011.dta", replace

* see 2012 historical/solo comparison in previous syntax

* Append 1990 to 2011
use "1990.dta"
save "abc_1990_2011.dta", replace
append using 1991 1992 1993 1994 1995 1996 1997 1998 1999
replace fyenddate ="" if fyenddate=="BBBB"
append using 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011
replace fyenddate ="" if fyenddate=="BBBB"
destring fyenddate, replace
save "abc_1990_2011.dta", replace

* Build City/Municipal Data
sort id year4
rename year4 year
rename population popaslg
la var popaslg "Population from IndFin data"
* drop counties, states, and feds
keep if type==2 | type==3
tostring fipscodestate, replace format("%02.0f")
rename fipscodestate STATEFP
tostring county, replace format("%03.0f")
rename county CTYFP
save "abc_1990_2011.dta", replace

/* Build GOVID to FIPS crosswalk
https://www.census.gov/programs-surveys/gov-finances/data/historical-data.html
	- _IndFin_1967-2012 [<1.0 MB]
	- "GOVS_ID_to_FIPS_Place_Codes_2012.xls"
	- click "unprotect sheet"
	- delete top 14 rows
	- delete column a and columns d-e "GOVS state code" - "GOVS county code"
	- delete bottom 4 rows
	- rename vars govid govid_14 name county STATEFP CTYFP PLACEFP in excel file
	- Delete the top two remaining rows
	- save as csv "govtofips.csv"
*/
clear
import delimited "govtofips.csv", case(preserve) stringcols(1 2 5 6 7)
rename ïgovid govid
save "GOVStoFIPS.dta", replace

use "abc_1990_2011.dta"
rename id idnum
tostring idnum, replace
gen str9 govid = string(real(idnum),"%09.0f")
merge m:1 govid using GOVStoFIPS
/*
    Result                      Number of obs
    -----------------------------------------
    Not matched                         1,263
        from master                     1,263  (_merge==1)
        from using                          0  (_merge==2)

    Matched                           230,602  (_merge==3)
    -----------------------------------------
*/
drop if _merge != 3
drop _merge
rename govid_14 govidlong
save "abc_1990_2011.dta", replace

sort govidlong year
format %60s name
* Build variables minus capital outlays (includes construction)
gen totexplc = totalexpenditure - totalcapitaloutlays
* Police expenditure
gen polexplc = policeprottotalexp - policeprotcapoutlay
* Corrections expenditure
gen correxplc = correcttotalexp - correctcapoutlay
* Judicial expenditure
gen judexplc = judicialtotalexpend - judicialcapoutlay
* Health
gen healexplc = healthtotalexpend - healthcapitaloutlay
* Hospitals
gen hospexplc = totalhospitaltotalexp - totalhospitalcapout
* Housing & Community Dev
gen houscommexplc = houscomtotalexp - houscomcapoutlay
* Parks & Rec
gen parksexplc = parksrectotalexp - parksreccapoutlay
* Public assistance
gen pubassexplc = publicwelftotalexp - publicwelfcapoutlay
* Libraries
gen libexplc = librariestotalexpend - librariescapoutlay

keep govid govidlong year name STATEFP PLACEFP fyenddate popaslg totalexpenditure totalrevenue totaltaxes propertytax  totalcapitaloutlays propertytax finesandforfeits totexplc polexplc correxplc judexplc healexplc hospexplc houscommexplc parksexplc pubassexplc libexplc
order govid year STATEFP PLACEFP name
save "abc_1990_2011.dta", replace

********************************************************************************
/* 	Solo files

	- https://www.census.gov/programs-surveys/gov-finances/data/datasets.html
	- For each year, download "Public Use Files"
	- Unzip each file and drag its contents into the same folder as the historical data.

	Included variables are from 2011 Classification Manual p. 20, 200, & 256.
	Some X and Y prefixed vars are revenue and some are expenditure. Solo year vars not
	listed in the Classification Manual were included in total expenditure or revenue with
	other same-prefixed vars.)

	- Total expenditure: delete all Ns Os Ps and Rs first bc not in muni data
	- Police expenditure: Non-muni data (Is, Ns, Os, Ps, Rs) and capital data (Gs, Fs)
		omitted. E is current exp, L & M are IG transfers.) */

****
cd "/Volumes/Tosh 4TB/ASLG 67-12/_IndFin_1967-2012, 2.0/"

* 2012
clear
infix str id 1-14 str item 15-17 amount 18-29 year 30-33 str flag 34 using "2012FinEstDAT_10162019modp_pu.txt"
save "2012long.dta", replace
drop year
reshape wide amount flag, i(id) j(item) string
* Shorten variable names
rename amount* a*
save "2012MunisRaw.dta", replace

* Add Geo identifiers
clear
infix str id 1-14 str name 15-78 str county 79-113 str fipsstate 114-115 str fipscounyt 116-118 str fipsplace 119-123 popaslg 124-132 popyear 133-134 enroll 135-141 enrollyear 142-143 str function 144-145 str school 146-147 str fyend 148-151 str surveyyear 152-153 using "Fin_GID_2012.txt"
save "2012Geo.dta", replace
use "2012MunisRaw.dta"
merge 1:1 id using "2012Geo.dta"
drop _merge
order _all, alpha
order id name popaslg county-fipsstate function-surveyyear

* Drop non-muni observations
gen typecode = substr(id, 3, 1)
keep if type=="2"|type=="3"
* Build expenditure variables
* Total taxes
egen totaltaxes = rowtotal(aT01-aT99)
* Total revenue
egen totalrevenue = rowtotal(aA01-aD94 aT01-aT99 aU01-aU99 aX01 aX02 aX05 aX08 aY01 aY02 aY04 aY11 aY12 aY51 aY52)
* Total expenditure
egen totalexpenditure = rowtotal(aE01-aS89 aX11 aX12 aY05 aY06 aY14 aY53)
* Total capital outlays
egen totalcapitaloutlays = rowtotal(aF01-aG94)
* Total property tax
gen propertytax = aT01
* Fines and fees
gen finesandforfeits = aU30
* Total expenditure minus capital outlays
gen totexplc = totalexpenditure - totalcapitaloutlays
* Police expenditure
egen polexplc = rowtotal(aE62 aL62 aM62)
* Corrections expenditure
egen correxplc = rowtotal(aE04 aE05 aL04 aL05 aM04 aM05)
* Judicial expenditure
egen judexplc = rowtotal(aE25 aL25 aM25)
* Health
egen healexplc = rowtotal(aE32 aL32 aM32)
* Hospitals
egen hospexplc = rowtotal(aE36 aL36 aM36)
* Housing & Community Dev
egen houscommexplc = rowtotal(aE50 aL50 aM50)
* Parks & Rec
egen parksexplc = rowtotal(aE61 aL61 aM61)
* Public assistance
egen pubassexplc = rowtotal(aE74 aE75 aE77 aE79 aJ67 aJ68 aL67 aL79 aM67 aM68 aS67)
* Libraries
egen libexplc = rowtotal(aE52 aL52 aM52)
keep id name popaslg fipscounyt fipsplace fipsstate fyend popaslg totalexpenditure totalrevenue totaltaxes propertytax totalcapitaloutlays propertytax finesandforfeits totexplc polexplc correxplc judexplc healexplc hospexplc houscommexplc parksexplc pubassexplc libexplc

* Make compatible with IndFin var names and types
rename id govidlong
format %60s name
rename fipsstate STATEFP
rename fipscounyt CTYFP
rename fipsplace PLACEFP
rename fyend fyenddate
gen year = 2012
order govidlong year
save "2012Munis.dta", replace

********************************************************************************
* 2013
clear
infix str id 1-14 str item 15-17 amount 18-29 year 30-33 str flag 34 using "2013FinEstDAT_10162019modp_pu.txt"
save "2013long.dta", replace
drop year
reshape wide amount flag, i(id) j(item) string
rename amount* a*
save "2013MunisRaw.dta", replace

* Geo Identifiers
clear
infix str id 1-14 str name 15-78 str county 79-113 str fipsstate 114-115 str fipscounyt 116-118 str fipsplace 119-123 popaslg 124-132 popyear 133-134 enroll 135-141 enrollyear 142-143 str function 144-145 str school 146-147 str fyend 148-151 str surveyyear 152-153 using "Fin_GID_2013.txt"
save "2013Geo.dta", replace

use "2013MunisRaw.dta"
merge 1:1 id using "2013Geo.dta"
drop _merge
order _all, alpha
order id name popaslg county-fipsstate function-surveyyear
save "2013Munis.dta", replace

* Drop non-muni observations
gen typecode = substr(id, 3, 1)
keep if type=="2"|type=="3"
* Build variables
egen totaltaxes = rowtotal(aT01-aT99)
egen totalrevenue = rowtotal(aA01-aD94 aT01-aT99 aU01-aU99 aX01 aX02 aX05 aX08 aY01 aY02 aY04 aY11 aY12 aY51 aY52)
egen totalexpenditure = rowtotal(aE01-aS89 aX11 aX12 aY05 aY06 aY14 aY53)
egen totalcapitaloutlays = rowtotal(aF01-aG94)
gen propertytax = aT01
gen finesandforfeits = aU30
gen totexplc = totalexpenditure - totalcapitaloutlays
egen polexplc = rowtotal(aE62 aL62 aM62)
egen correxplc = rowtotal(aE04 aE05 aL04 aL05 aM04 aM05)
egen judexplc = rowtotal(aE25 aL25 aM25)
egen healexplc = rowtotal(aE32 aL32 aM32)
egen hospexplc = rowtotal(aE36 aL36 aM36)
egen houscommexplc = rowtotal(aE50 aL50 aM50)
egen parksexplc = rowtotal(aE61 aL61 aM61)
egen pubassexplc = rowtotal(aE74 aE75 aE77 aE79 aJ67 aJ68 aL67 aL79 aM67 aM68 aS67)
egen libexplc = rowtotal(aE52 aL52 aM52)
keep id name popaslg fipscounyt fipsplace fipsstate fyend popaslg totalexpenditure totalrevenue totaltaxes propertytax totalcapitaloutlays propertytax finesandforfeits totexplc polexplc correxplc judexplc healexplc hospexplc houscommexplc parksexplc pubassexplc libexplc

* Make compatible with IndFin var names and types
rename id govidlong
format %60s name
rename fipsstate STATEFP
rename fipscounyt CTYFP
rename fipsplace PLACEFP
rename fyend fyenddate
gen year = 2013
order govidlong year
save "2013Munis.dta", replace
********************************************************************************
* 2014
clear
infix str id 1-14 str item 15-17 amount 18-29 year 30-33 str flag 34 using "2014FinEstDAT_10162019modp_pu.txt"
save "2014long.dta", replace
drop year
reshape wide amount flag, i(id) j(item) string
rename amount* a*
save "2014MunisRaw.dta", replace

* Geo Identifiers
clear
infix str id 1-14 str name 15-78 str county 79-113 str fipsstate 114-115 str fipscounyt 116-118 str fipsplace 119-123 popaslg 124-132 popyear 133-134 enroll 135-141 enrollyear 142-143 str function 144-145 str school 146-147 str fyend 148-151 str surveyyear 152-153 using "Fin_GID_2014.txt"
save "2014Geo.dta", replace

use "2014MunisRaw.dta"
merge 1:1 id using "2014Geo.dta"
drop _merge
order _all, alpha
order id name popaslg county-fipsstate function-surveyyear
save "2014Munis.dta", replace

* Drop non-muni observations
gen typecode = substr(id, 3, 1)
keep if type=="2"|type=="3"
* Build variables
egen totaltaxes = rowtotal(aT01-aT99)
egen totalrevenue = rowtotal(aA01-aD94 aT01-aT99 aU01-aU99 aX01 aX02 aX05 aX08 aY01 aY02 aY04 aY11 aY12 aY51 aY52)
egen totalexpenditure = rowtotal(aE01-aS89 aX11 aX12 aY05 aY06 aY14 aY53)
egen totalcapitaloutlays = rowtotal(aF01-aG94)
gen propertytax = aT01
gen finesandforfeits = aU30
gen totexplc = totalexpenditure - totalcapitaloutlays
egen polexplc = rowtotal(aE62 aL62 aM62)
egen correxplc = rowtotal(aE04 aE05 aL04 aL05 aM04 aM05)
egen judexplc = rowtotal(aE25 aL25 aM25)
egen healexplc = rowtotal(aE32 aL32 aM32)
egen hospexplc = rowtotal(aE36 aL36 aM36)
egen houscommexplc = rowtotal(aE50 aL50 aM50)
egen parksexplc = rowtotal(aE61 aL61 aM61)
egen pubassexplc = rowtotal(aE74 aE75 aE77 aE79 aJ67 aJ68 aL67 aL79 aM67 aM68 aS67)
egen libexplc = rowtotal(aE52 aL52 aM52)
keep id name popaslg fipscounyt fipsplace fipsstate fyend popaslg totalexpenditure totalrevenue totaltaxes propertytax totalcapitaloutlays propertytax finesandforfeits totexplc polexplc correxplc judexplc healexplc hospexplc houscommexplc parksexplc pubassexplc libexplc

* Make compatible with IndFin var names and types
rename id govidlong
format %60s name
rename fipsstate STATEFP
rename fipscounyt CTYFP
rename fipsplace PLACEFP
rename fyend fyenddate
gen year = 2014
order govidlong year
save "2014Munis.dta", replace
********************************************************************************
* 2015
clear
infix str id 1-14 str item 15-17 amount 18-29 year 30-33 str flag 34 using "2015FinEstDAT_10162019modp_pu.txt"
save "2015long.dta", replace
drop year
reshape wide amount flag, i(id) j(item) string
rename amount* a*
save "2015MunisRaw.dta", replace

* Geo Identifiers
clear
infix str id 1-14 str name 15-78 str county 79-113 str fipsstate 114-115 str fipscounyt 116-118 str fipsplace 119-123 popaslg 124-132 popyear 133-134 enroll 135-141 enrollyear 142-143 str function 144-145 str school 146-147 str fyend 148-151 str surveyyear 152-153 using "Fin_GID_2015.txt"
save "2015Geo.dta", replace

use "2015MunisRaw.dta"
merge 1:1 id using "2015Geo.dta"
drop _merge
order _all, alpha
order id name popaslg county-fipsstate function-surveyyear
save "2015Munis.dta", replace

* Drop non-muni observations
gen typecode = substr(id, 3, 1)
keep if type=="2"|type=="3"
* Build variables
egen totaltaxes = rowtotal(aT01-aT99)
egen totalrevenue = rowtotal(aA01-aD94 aT01-aT99 aU01-aU99 aX01 aX02 aX05 aX08 aY01 aY02 aY04 aY11 aY12 aY51 aY52)
egen totalexpenditure = rowtotal(aE01-aS89 aX11 aX12 aY05 aY06 aY14 aY53)
egen totalcapitaloutlays = rowtotal(aF01-aG94)
gen propertytax = aT01
gen finesandforfeits = aU30
gen totexplc = totalexpenditure - totalcapitaloutlays
egen polexplc = rowtotal(aE62 aL62 aM62)
egen correxplc = rowtotal(aE04 aE05 aL04 aL05 aM04 aM05)
egen judexplc = rowtotal(aE25 aL25 aM25)
egen healexplc = rowtotal(aE32 aL32 aM32)
egen hospexplc = rowtotal(aE36 aL36 aM36)
egen houscommexplc = rowtotal(aE50 aL50 aM50)
egen parksexplc = rowtotal(aE61 aL61 aM61)
egen pubassexplc = rowtotal(aE74 aE75 aE77 aE79 aJ67 aJ68 aL67 aL79 aM67 aM68 aS67)
egen libexplc = rowtotal(aE52 aL52 aM52)
keep id name popaslg fipscounyt fipsplace fipsstate fyend popaslg totalexpenditure totalrevenue totaltaxes propertytax totalcapitaloutlays propertytax finesandforfeits totexplc polexplc correxplc judexplc healexplc hospexplc houscommexplc parksexplc pubassexplc libexplc

* Make compatible with IndFin var names and types
rename id govidlong
format %60s name
rename fipsstate STATEFP
rename fipscounyt CTYFP
rename fipsplace PLACEFP
rename fyend fyenddate
gen year = 2015
order govidlong year
save "2015Munis.dta", replace
********************************************************************************
* 2016
clear
infix str id 1-14 str item 15-17 amount 18-29 year 30-33 str flag 34 using "2016FinEstDAT_10162019modp_pu.txt"
save "2016long.dta", replace
drop year
reshape wide amount flag, i(id) j(item) string
rename amount* a*
save "2016MunisRaw.dta", replace

* Geo Identifiers
clear
infix str id 1-14 str name 15-78 str county 79-113 str fipsstate 114-115 str fipscounyt 116-118 str fipsplace 119-123 popaslg 124-132 popyear 133-134 enroll 135-141 enrollyear 142-143 str function 144-145 str school 146-147 str fyend 148-151 str surveyyear 152-153 using "Fin_GID_2016.txt"
save "2016Geo.dta", replace

use "2016MunisRaw.dta"
merge 1:1 id using "2016Geo.dta"
drop _merge
order _all, alpha
order id name popaslg county-fipsstate function-surveyyear
save "2016Munis.dta", replace

* Drop non-muni observations
gen typecode = substr(id, 3, 1)
keep if type=="2"|type=="3"
* Build variables
egen totaltaxes = rowtotal(aT01-aT99)
egen totalrevenue = rowtotal(aA01-aD94 aT01-aT99 aU01-aU99 aX01 aX02 aX05 aX08 aY01 aY02 aY11 aY12 aY51 aY52)
egen totalexpenditure = rowtotal(aE01-aS89 aX11 aX12 aY05 aY06 aY14 aY53)
egen totalcapitaloutlays = rowtotal(aF01-aG94)
gen propertytax = aT01
gen finesandforfeits = aU30
gen totexplc = totalexpenditure - totalcapitaloutlays
egen polexplc = rowtotal(aE62 aL62 aM62)
egen correxplc = rowtotal(aE04 aE05 aL04 aL05 aM04 aM05)
egen judexplc = rowtotal(aE25 aL25 aM25)
egen healexplc = rowtotal(aE32 aL32 aM32)
egen hospexplc = rowtotal(aE36 aL36 aM36)
egen houscommexplc = rowtotal(aE50 aL50 aM50)
egen parksexplc = rowtotal(aE61 aL61 aM61)
egen pubassexplc = rowtotal(aE74 aE75 aE77 aE79 aJ67 aJ68 aL67 aL79 aM67 aM68 aS67)
egen libexplc = rowtotal(aE52 aL52 aM52)
keep id name popaslg fipscounyt fipsplace fipsstate fyend popaslg totalexpenditure totalrevenue totaltaxes propertytax totalcapitaloutlays propertytax finesandforfeits totexplc polexplc correxplc judexplc healexplc hospexplc houscommexplc parksexplc pubassexplc libexplc

* Make compatible with IndFin var names and types
rename id govidlong
format %60s name
rename fipsstate STATEFP
rename fipscounyt CTYFP
rename fipsplace PLACEFP
rename fyend fyenddate
gen year = 2016
order govidlong year
save "2016Munis.dta", replace

********************************************************************************
* 2017
clear
infix str id 1-12 str item 13-15 amount 16-27 year 28-31 str flag 32 using "2017FinEstDAT_06102021modp_pu.txt"
save "2017MunisRaw.dta", replace
drop year
reshape wide amount flag, i(id) j(item) string
rename amount* a*
save "2017MunisRaw.dta", replace

* Geo Identifiers
clear
infix str fipsstate 1-2 str fipscounyt 4-6 str id 1-12 str name 13-76 str county 77-111 str fipsplace 112-116 popaslg 117-125 popyear 126-127 enroll 128-136 enrollyear 137-138 str function 139-140 str school 141-142 str fyend 143-144 str surveyyear 145-146 using "Fin_PID_2017.txt"
save "2017Geo.dta", replace

use "2017MunisRaw.dta"
merge 1:1 id using "2017Geo.dta"
drop _merge
order _all, alpha
order id name popaslg county-fipsstate function-surveyyear
save "2017Munis.dta", replace

* Drop non-muni observations
gen typecode = substr(id, 3, 1)
keep if type=="2"|type=="3"
* Build variables
egen totaltaxes = rowtotal(aT01-aT99)
egen totalrevenue = rowtotal(aA01-aD94 aT01-aT99 aU01-aU99 aY01 aY02 aY11 aY12 aY51 aY52)
egen totalexpenditure = rowtotal(aE01-aS89 aY05 aY06 aY14 aY53)
egen totalcapitaloutlays = rowtotal(aF01-aG94)
gen propertytax = aT01
gen finesandforfeits = aU30
gen totexplc = totalexpenditure - totalcapitaloutlays
egen polexplc = rowtotal(aE62 aL62 aM62)
egen correxplc = rowtotal(aE04 aE05 aL04 aL05 aM04 aM05)
egen judexplc = rowtotal(aE25 aL25 aM25)
egen healexplc = rowtotal(aE32 aL32 aM32)
egen hospexplc = rowtotal(aE36 aL36 aM36)
egen houscommexplc = rowtotal(aE50 aL50 aM50)
egen parksexplc = rowtotal(aE61 aL61 aM61)
egen pubassexplc = rowtotal(aE74 aE75 aE77 aE79 aJ67 aJ68 aL67 aL79 aM67 aM68 aS67)
egen libexplc = rowtotal(aE52 aL52 aM52)
keep id name popaslg county fipsstate fipsplace fipscounyt fyend popaslg totalexpenditure totalrevenue totaltaxes propertytax totalcapitaloutlays propertytax finesandforfeits totexplc polexplc correxplc judexplc healexplc hospexplc houscommexplc parksexplc pubassexplc libexplc

* Make compatible with IndFin var names and types
rename id govidlong
format %60s name
rename fipsstate STATEFP
rename fipscounyt CTYFP
rename fipsplace PLACEFP
gen year = 2017
order govidlong year
save "2017Munis.dta", replace
********************************************************************************
* 2018
clear
infix str id 1-12 str item 13-15 amount 16-27 year 28-31 str flag 32 using "2018FinEstDAT_06102021modp_pu.txt"
save "2018MunisRaw.dta", replace
drop year
reshape wide amount flag, i(id) j(item) string
rename amount* a*
save "2018MunisRaw.dta", replace

* Geo Identifiers
clear
infix str fipsstate 1-2 str fipscounyt 4-6 str id 1-12 str name 13-76 str county 77-111 str fipsplace 112-116 popaslg 117-125 popyear 126-127 enroll 128-134 enrollyear 135-136 str function 137-138 str school 139-140 str fyend 141-144 str surveyyear 145-146 using "Fin_PID_2018.txt"
save "2018Geo.dta", replace

use "2018MunisRaw.dta"
merge 1:1 id using "2018Geo.dta"
drop _merge
order _all, alpha
order id name popaslg county-fipsstate function-surveyyear
save "2018Munis.dta", replace

* Drop non-muni observations
gen typecode = substr(id, 3, 1)
keep if type=="2"|type=="3"
* Build variables
egen totaltaxes = rowtotal(aT01-aT99)
egen totalrevenue = rowtotal(aA01-aD94 aT01-aT99 aU01-aU99 aY01 aY02 aY11 aY12 aY51 aY52)
egen totalexpenditure = rowtotal(aE01-aS89 aY05 aY06 aY14 aY53)
egen totalcapitaloutlays = rowtotal(aF01-aG94)
gen propertytax = aT01
gen finesandforfeits = aU30
gen totexplc = totalexpenditure - totalcapitaloutlays
egen polexplc = rowtotal(aE62 aL62 aM62)
egen correxplc = rowtotal(aE04 aE05 aL04 aL05 aM04 aM05)
egen judexplc = rowtotal(aE25 aL25 aM25)
egen healexplc = rowtotal(aE32 aL32 aM32)
egen hospexplc = rowtotal(aE36 aL36 aM36)
egen houscommexplc = rowtotal(aE50 aL50 aM50)
egen parksexplc = rowtotal(aE61 aL61 aM61)
egen pubassexplc = rowtotal(aE74 aE75 aE77 aE79 aJ67 aJ68 aL67 aL79 aM67 aM68 aS67)
egen libexplc = rowtotal(aE52 aL52 aM52)
keep id name popaslg county fipsstate fipsplace fipscounyt fyend popaslg totalexpenditure totalrevenue totaltaxes propertytax totalcapitaloutlays propertytax finesandforfeits totexplc polexplc correxplc judexplc healexplc hospexplc houscommexplc parksexplc pubassexplc libexplc

* Make compatible with IndFin var names and types
rename id govidlong
format %60s name
rename fipsstate STATEFP
rename fipscounyt CTYFP
rename fipsplace PLACEFP
rename fyend fyenddate
gen year = 2018
order govidlong year
save "2018Munis.dta", replace
********************************************************************************
* 2019
clear
infix str id 1-12 str item 13-15 amount 16-27 year 28-31 str flag 32 using "2019FinEstDAT_06102021modp_pu.txt"
save "2019MunisRaw.dta", replace
drop year
reshape wide amount flag, i(id) j(item) string
rename amount* a*
save "2019MunisRaw.dta", replace

* Geo Identifiers
clear
infix str fipsstate 1-2 str fipscounyt 4-6 str id 1-12 str name 13-76 str county 77-111 str fipsplace 112-116 popaslg 117-125 popyear 126-127 enroll 128-134 enrollyear 135-136 str function 137-138 str school 139-140 str fyend 141-144 str surveyyear 145-146 using "Fin_PID_2019.txt"
save "2019Geo.dta", replace

use "2019MunisRaw.dta"
merge 1:1 id using "2019Geo.dta"
drop _merge
order _all, alpha
order id name popaslg county-fipsstate function-surveyyear
save "2019Munis.dta", replace

* Drop non-muni observations
gen typecode = substr(id, 3, 1)
keep if type=="2"|type=="3"
* Build variables
egen totaltaxes = rowtotal(aT01-aT99)
egen totalrevenue = rowtotal(aA01-aD94 aT01-aT99 aU01-aU99 aY01 aY02 aY11 aY12 aY51 aY52)
egen totalexpenditure = rowtotal(aE01-aS89 aY05 aY06 aY14 aY53)
egen totalcapitaloutlays = rowtotal(aF01-aG94)
gen propertytax = aT01
gen finesandforfeits = aU30
gen totexplc = totalexpenditure - totalcapitaloutlays
egen polexplc = rowtotal(aE62 aL62 aM62)
egen correxplc = rowtotal(aE04 aE05 aL04 aL05 aM04 aM05)
egen judexplc = rowtotal(aE25 aL25 aM25)
egen healexplc = rowtotal(aE32 aL32 aM32)
egen hospexplc = rowtotal(aE36 aL36 aM36)
egen houscommexplc = rowtotal(aE50 aL50 aM50)
egen parksexplc = rowtotal(aE61 aL61 aM61)
egen pubassexplc = rowtotal(aE74 aE75 aE77 aE79 aJ67 aJ68 aL67 aL79 aM67 aM68 aS67)
egen libexplc = rowtotal(aE52 aL52 aM52)
keep id name popaslg county fipsstate fipsplace fipscounyt fyend popaslg totalexpenditure totalrevenue totaltaxes propertytax totalcapitaloutlays propertytax finesandforfeits totexplc polexplc correxplc judexplc healexplc hospexplc houscommexplc parksexplc pubassexplc libexplc

* Make compatible with IndFin var names and types
rename id govidlong
format %60s name
rename fipsstate STATEFP
rename fipscounyt CTYFP
rename fipsplace PLACEFP
rename fyend fyenddate
gen year = 2019
order govidlong year
save "2019Munis.dta", replace
********************************************************************************
* Append and Combine Years
cd "/Volumes/Tosh 4TB/ASLG 67-12/_IndFin_1967-2012, 2.0/"
use "abc_1990_2011.dta"
tostring fyenddate, replace format("%02.0f")
append using 2012Munis 2013Munis 2014Munis 2015Munis 2016Munis 2017Munis 2018Munis 2019Munis
rename govidlong govid14
sort govid14 year
rename govid govid9
order govid14 year name STATEFP CTYFP PLACEFP fyenddate popaslg
compress
format %25s name
save "Munis1990_2019.dta", replace

duplicates report STATEFP PLACEFP year
duplicates tag STATEFP PLACEFP year, gen(temp)
drop if temp>0
drop temp
save, replace

* Adjust Fiscal Years to Calendar Years
destring fyenddate, replace
bysort year: tab fyenddate, m
gen fyear = year
replace fyear = year-1 if fyenddate<=630

/* 	Adjust for Inflation CPI-All Urban Consumers (Current Series) https://data.bls.gov/timeseries/CUUR0000SA0?output_view=pct_1mth
Select "More formtatting options" and change "All time periods" to "Select one time period: Annual Data" and generate the output. Saved to: /Users/beckucdenver/Google Drive/Research/City Dataset 2.0/Data/CPI/SeriesReport-20220522121339_75e0d0.xlsx*/
gen cpiu19 = .
la var cpiu19 "CPIU ratio adjusting to 2019"
replace cpiu19 = 255.657/270.970 if year==2021
replace cpiu19 = 255.657/258.811 if year==2020
replace cpiu19 = 1 if year==2019
replace cpiu19 = 255.657/251.107 if year==2018
replace cpiu19 = 255.657/245.120 if year==2017
replace cpiu19 = 255.657/240.007 if year==2016
replace cpiu19 = 255.657/237.017 if year==2015
replace cpiu19 = 255.657/236.736 if year==2014
replace cpiu19 = 255.657/232.957 if year==2013
replace cpiu19 = 255.657/229.594 if year==2012
replace cpiu19 = 255.657/224.939 if year==2011
replace cpiu19 = 255.657/218.056 if year==2010
replace cpiu19 = 255.657/214.537 if year==2009
replace cpiu19 = 255.657/215.303 if year==2008
replace cpiu19 = 255.657/207.342 if year==2007
replace cpiu19 = 255.657/201.6 if year==2006
replace cpiu19 = 255.657/195.3 if year==2005
replace cpiu19 = 255.657/188.9 if year==2004
replace cpiu19 = 255.657/184.0 if year==2003
replace cpiu19 = 255.657/179.9 if year==2002
replace cpiu19 = 255.657/177.1 if year==2001
replace cpiu19 = 255.657/172.2 if year==2000
replace cpiu19 = 255.657/166.6 if year==1999
replace cpiu19 = 255.657/163.0 if year==1998
replace cpiu19 = 255.657/160.5 if year==1997
replace cpiu19 = 255.657/156.9 if year==1996
replace cpiu19 = 255.657/152.4 if year==1995
replace cpiu19 = 255.657/148.2 if year==1994
replace cpiu19 = 255.657/144.5 if year==1993
replace cpiu19 = 255.657/140.3 if year==1992
replace cpiu19 = 255.657/136.2 if year==1991
replace cpiu19 = 255.657/130.7 if year==1990
replace cpiu19 = 255.657/124.0 if year==1989
foreach x of varlist totalrevenue totaltaxes propertytax finesandforfeits totalexpenditure totalcapitaloutlays totexplc polexplc correxplc judexplc healexplc hospexplc houscommexplc parksexplc pubassexplc libexplc{
gen r`x' = `x'*cpiu19
}

* Criminal justice expenditure
gen rcjexplc = rpolexplc + rjudexplc + rcorrexplc
save "Munis1990_2019.dta", replace

* Variable checks
twoway mband rpolexplc year if popaslg>25000 & popaslg<., yscale(range(1.5 1.9))
graph bar (mean) rpolexplc if popaslg>25000 & popaslg<. & year<=2010, over(year)

save "/Volumes/Tosh 4TB/ASLG 67-12/_IndFin_1967-2012, 2.0/ASLG 1990-2019.dta", replace
************************* END ASLG Syntax **************************
