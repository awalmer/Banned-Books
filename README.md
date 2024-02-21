# Book Censorship in the U.S.

Documentation…. 👇

Data Wrangling Steps:

(1) DECISION VARIABLE: DISTILLATION
* Simplify the decision variable category for map purposes. (Map where bubble color = decision outcome.)
    * (1) Banned (2) Overturned (3) Unresolved (4) Unrelated Removal (5) Unknown
    * Distilled from 22 unique values in data — emailed TM about this

(2) TACKLING MISSING COUNTY INFORMATION
* There are 5,070 rows in the data for which county is NA — how to fill in missing info?
* TWO library types: school & public:
* SCHOOL LIBRARY / SCHOOL DISTRICT <—> COUNTY [nrows = 11,208]
    * IES NCES data table of public school districts: https://nces.ed.gov/ccd/elsi/tableGenerator.aspx?savedTableID=646279 
    * Census Data “School Districts and Associated Counties” https://www.census.gov/programs-surveys/saipe/guidance-geographies/districts-counties.html 
* PUBLIC LIBRARY <—> COUNTY [1,165]
    * Public Libraries Survey https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey
    * Using PLS_FY21_AE_pud21i file rather than Outlet file because just pubic libraries, not its “service outlets” (docs here pg. 8) 
    * Most recent = 2021, hopefully up to date.
* Merged in estimated county information by school district name and public library name (ID = state + district/lib name) - this is an estimation because some districts existed in more than one county. So the actual case may have been in a neighboring county, rather than the estimate. 

(3) ADDITIONAL DATA CLEANING
* Year variable typo: changed 2223 to 2023
* Adding county identifier variables, adding simplified challenge outcome variables
* 


Bringing Data into Flourish:

* DATA PREP:
    * Merge data with Flourish’s county information
    * Group data with sum of challenge outcomes per county
    * 
