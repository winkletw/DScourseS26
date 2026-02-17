-- (a)  read in fl insurance data 
.mode csv
.import FL_insurance_sample.csv fl_insurance

-- (b) print first 10 rows 
SELECT * FROM fl_insurance LIMIT 10;

-- (c) list unique counties 
SELECT DISTINCT county FROM fl_insurance;

-- (d) compute average property appreciation from 2011 to 2012 
SELECT AVG(tiv_2012 - tiv_2011) FROM fl_insurance; 

-- (e) fraction of buildings by material
SELECT construction, COUNT(*) FROM fl_insurance GROUP BY construction;
