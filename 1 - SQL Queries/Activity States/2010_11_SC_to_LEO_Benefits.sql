-- DWP Benefits - ACTIVITY STATES 


SELECT DISTINCT
-- [table_name or the defined alias].[field_name]
	SC_Pupil.[pupilmatchingrefanonymous_spr11],


	-- Select variables from the Benefits table 
	Benefit.startdate,
	Benefit.enddate 



-- FROM [database].[schema].[table] own alias SC_Pupil
FROM [NPD].[2002237].[Spring_Census_2011] SC_Pupil

--- USE LOOKUP PMR TO AEID, THEN AEID TO EDUKEY
INNER JOIN [LEO].[2002237].[LEARNER_AE_ID_TO_PMR_LOOKUP] PMR_AEID
	ON SC_Pupil.[pupilmatchingrefanonymous_spr11] = PMR_AEID.[PUPILMATCHINGREFANONYMOUS]

-- AE_ID to Edukey Linking, alias AEID_EDUKEY
INNER JOIN [LEO].[2002237].[LEARNER_AE_ID_TO_EDUKEY_LOOKUP] AEID_EDUKEY
	ON PMR_AEID.AE_ID = AEID_EDUKEY.AE_ID

-- Link to Benefit table, alias Benefit
INNER JOIN [LEO].[2002237].[LEO_Benefit] Benefit
	ON AEID_EDUKEY.edukey = Benefit.Edukey


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
	-- Filter only out of work benefits
	((Benefit.out_of_work_benefit = '1')

	AND
	-- Filter for years required - from the start of the 2011/12 tax year, 6th April 2011
	(Benefit.startdate >= '2011-04-06'))


ORDER BY
	SC_Pupil.pupilmatchingrefanonymous_spr11, Benefit.startdate ASC;