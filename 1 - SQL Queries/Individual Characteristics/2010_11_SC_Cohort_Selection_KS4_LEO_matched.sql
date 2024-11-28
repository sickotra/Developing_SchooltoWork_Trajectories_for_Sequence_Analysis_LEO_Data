-- 2010/11 SCHOOL LEAVER COHORT SELECTION - Spring 2011 Census (Jan/Feb) to select 2010/11 cohort

-- Linked School Census to KS4 and matched to LEO
-- Note: duplicate PMR IDs will be present because of KS4 Duplicates (cases where 2 records, one with GSCE attained, one not attained)



SELECT
	SC_Pupil.[pupilmatchingrefanonymous_spr11],
-- Select variables from the NPD School Census
-- Gender, Ethniticy, Special Educational Needs (SEN), Free School Meals (FSM), Lower Level Super Output Layer Code (LLSOA)
	SC_Pupil.gender_spr11,
	SC_Pupil.ethnicgroupmajor_spr11,
	SC_Pupil.senprovision_spr11,
	SC_Pupil.everfsm_all_spr11,
	SC_Pupil.llsoa_spr11,

-- Select variables from the NPD KS4 table
-- Achieved 5+ A*-C GCSEs or equiv.including English and Maths
	KS4.KS4_LEVEL2_EM



-- FROM [database].[schema].[table] - alias SC_Pupil
FROM [NPD].[2002237].[Spring_Census_2011] SC_Pupil

-- LINKED TO KS4 TABLE, alias KS4
INNER JOIN [NPD].[2002237].[KS4Pupil_2002_to_2014] KS4
	ON SC_Pupil.pupilmatchingrefanonymous_spr11 = KS4.KS4_PupilMatchingRefAnonymous

--- USE LOOKUP PMR TO AEID, THEN AEID TO EDUKEY to match NPD to LEO
INNER JOIN [LEO].[2002237].[LEARNER_AE_ID_TO_PMR_LOOKUP] PMR_AEID
	ON KS4.KS4_PupilMatchingRefAnonymous = PMR_AEID.[PUPILMATCHINGREFANONYMOUS]

-- AE_ID to Edukey Linking, alias AEID_EDUKEY
INNER JOIN [LEO].[2002237].[LEARNER_AE_ID_TO_EDUKEY_LOOKUP] AEID_EDUKEY
	ON PMR_AEID.AE_ID = AEID_EDUKEY.AE_ID



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
	-- Select values for GCSE results from the 2010/2011 academic year   
	(KS4.KS4_ACADYR = '2010/2011')
		 

