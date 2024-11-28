-- LEO Geography Table - GEOGRAPHY STATES 


-- Select the PMR ID from the Spring 2011 NPD School Census to extract the correct 2010/11 cohort
SELECT DISTINCT
	-- [table_name or the defined alias].[field_name]
	SC_Pupil.[pupilmatchingrefanonymous_spr11],
	
	-- Select variables from the Geog Table
	Geog.LAUA,     -- Local Authority District Code
	Geog.LAUANM,   -- Local Authortiy District Name
	Geog.GOR,      -- Government Office Region Code
	Geog.GORNM     -- Government Office Region Name


-- FROM [database].[schema].[table] own alias SC_Pupil
FROM [NPD].[2002237].[Spring_Census_2011] SC_Pupil

--- USE LOOKUP PMR TO AEID, THEN AEID TO EDUKEY
INNER JOIN [LEO].[2002237].[LEARNER_AE_ID_TO_PMR_LOOKUP] PMR_AEID
	ON SC_Pupil.[pupilmatchingrefanonymous_spr11] = PMR_AEID.[PUPILMATCHINGREFANONYMOUS]

-- AE_ID to Edukey Linking, alias AEID_EDUKEY
INNER JOIN [LEO].[2002237].[LEARNER_AE_ID_TO_EDUKEY_LOOKUP] AEID_EDUKEY
	ON PMR_AEID.AE_ID = AEID_EDUKEY.AE_ID

-- Link to Geography table, alias Geog
INNER JOIN [LEO].[2002237].[LEO_Geography] Geog
	ON AEID_EDUKEY.edukey = Geog.Edukey



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
	-- UPDATE THIS FOR EACH TAX YEAR TO PRODUCE MULTIPLE OUTPUTS
	(Geog.TaxYear = '201819');
