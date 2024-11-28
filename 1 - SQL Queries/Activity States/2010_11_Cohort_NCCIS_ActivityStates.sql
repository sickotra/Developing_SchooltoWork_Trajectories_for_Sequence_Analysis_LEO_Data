-- National Client Caseload Information System (NCCIS) - ACTIVITY STATES 


SELECT DISTINCT
	-- [table_name or the defined alias].[field_name]
	SC_Pupil.[pupilmatchingrefanonymous_spr11], 


	-- Select variables from the NCCIS table
	-- May be more than one activity recorded for an individual per academic year
	NCCIS.NCCIS_Current_Activity_Code,
	NCCIS.NCCIS_Current_Activity_Start_Date,
	NCCIS.NCCIS_Current_Activity_Verification_Date



-- FROM [database].[schema].[table] own alias SC_Pupil
FROM [NPD].[2002237].[Spring_Census_2011] SC_Pupil
-- Inner Join to retain all those in the selected cohort and in NCCIS
-- During R Analysis will use Individual Charactersitics PMR ID to retain records for only those who appear in the LEO matched 2010/11 school census cohort

INNER JOIN [NPD].[2002237].[NCCIS_2011_to_2019] NCCIS
-- Since the join is One to Many, multiple records for 1 person will be created, but select DISTINCT will only keep completely unique records
	ON SC_Pupil.[pupilmatchingrefanonymous_spr11] = NCCIS.[NCCIS_PupilMatchingRefAnonymous]


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
		 

	AND
	-- UPDATE THIS FOR EACH ACADEMIC YEAR TO PRODUCE MULTIPLE OUTPUTS
	-- NCCIS is observed from 2011/12 to 2018/19
	(NCCIS.NCCIS_ACADYR = '2012/2013')

ORDER BY
	SC_Pupil.pupilmatchingrefanonymous_spr11, NCCIS.NCCIS_Current_Activity_Start_Date ASC;

