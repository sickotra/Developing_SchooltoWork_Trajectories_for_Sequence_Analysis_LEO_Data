-- Higher Education Statistics Agency (HESA) - ACTIVITY STATES 


SELECT DISTINCT
-- [table_name or the defined alias].[field_name]
	SC_Pupil.[pupilmatchingrefanonymous_spr11],


	-- Select variables from the HESA table
	HESA.he_comdate, -- Commencement date
	HESA.he_enddate, -- End date
	HESA.he_xlev301  -- Level of study (Postgraduate = 1, Undergraduate = 2, FE = 3)



-- FROM [database].[schema].[table] own alias SC_Pupil
FROM [NPD].[2002237].[Spring_Census_2011] SC_Pupil

-- Inner join SC Pupil to HESA - HESA has different tables for each year - CHANGE THIS PER ACADEMIC YEAR REQUIRED
-- NOTE! The HESA table labels academic years differently to the norm. e.g. In HESA, 2013 refers to the 2012/13 academic year
INNER JOIN [HESA].[2002237].[HESA_2013] HESA
	ON SC_Pupil.[pupilmatchingrefanonymous_spr11] = HESA.[HE_PupilMatchingRefAnonymous]


WHERE 
	-- Filter the data for 2010/11 school-leaver cohort using month and year of birth
	((SC_Pupil.yearofbirth_spr11 = '1994' 
	AND
	SC_Pupil.monthofbirth_spr11 >=9)
	OR
	(SC_Pupil.yearofbirth_spr11 = '1995' 
	AND
	SC_Pupil.monthofbirth_spr11 <=8))


	-- Only retain those who are still on roll (=1) as SEN/FSM/LSOA is missing for those not on roll 
	AND ((SC_Pupil.onroll_spr11 = '1')

	-- Record status identifies duplicate PMRs. If NULL then this is a unique PMR
	-- Where duplicated, retain only the 'Main' record as shown in the LEO metadata. These are indicated with following values.
	AND (SC_Pupil.recordstatus_spr11 IS NULL OR
		 SC_Pupil.recordstatus_spr11 = '1' OR
		 SC_Pupil.recordstatus_spr11 = '2' OR
		 SC_Pupil.recordstatus_spr11 = '3' OR
		 SC_Pupil.recordstatus_spr11 = '4' OR
		 SC_Pupil.recordstatus_spr11 = '5' OR
		 SC_Pupil.recordstatus_spr11 = '6' OR
		 SC_Pupil.recordstatus_spr11 = '7' OR
		 SC_Pupil.recordstatus_spr11 = '8' OR
		 SC_Pupil.recordstatus_spr11 = '9' OR
		 SC_Pupil.recordstatus_spr11 = '10' OR
		 SC_Pupil.recordstatus_spr11 = '20'
		 ))
	

	-- Filter to keep only active HESA records (excludes those with inactive/suspended studies)
	AND ((HESA.he_notact != '1')

	-- Filter for ALL records with start date within the study period (2011/12 academic year to 2018/19 academic year)
	AND ((HESA.he_comdate >= '2011-09-01' AND HESA.he_comdate <= '2019-08-31')))


ORDER BY
		HESA.he_comdate ASC;