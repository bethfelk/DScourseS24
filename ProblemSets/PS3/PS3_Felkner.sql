-- Create a table to hold the data
CREATE TABLE FL_insurance_table(
    "policyID" INTEGER,
    "statecode" TEXT,
    "county" TEXT,
    "eq_site_limit" REAL,
    "hu_site_limit" REAL,
    "fl_site_limit" REAL,
    "fr_site_limit" REAL,
    "tiv_2011" REAL,
    "tiv_2012" REAL,
    "eq_site_deductible" REAL,
    "hu_site_deductible" REAL,
    "fl_site_deductible" REAL,
    "fr_site_deductible" REAL,
    "point_latitude" REAL,
    "point_longitude" REAL,
    "line" TEXT,
    "construction" TEXT,
    "point_granularity" INTEGER
);

-- Tell SQL that we will be reading a .csv file
.mode csv

-- Import the csv file
.import FL_insurance_sample.csv FL_insurance_table

-- Print the first 10 rows (note that I am using LIMIT 11 for this because SQL is recognzing the column headers as a 
-- row and printing them too as one of the N rows. I tried AI's suggestion of  ".headers on"  but that didn't work
-- so this seemed like the simplest fix)
SELECT * FROM FL_insurance_table LIMIT 11;

-- List distinct counties in the sample
SELECT DISTINCT county FROM FL_insurance_table;

-- Calculate average property appreciation from 2011 to 2012
SELECT AVG(tiv_2012 - tiv_2011) AS avg_prop_appreciation FROM FL_insurance_table;

-- Create frequency table for construction variable
SELECT construction, COUNT(*) AS frequency FROM FL_insurance_table GROUP BY construction;
