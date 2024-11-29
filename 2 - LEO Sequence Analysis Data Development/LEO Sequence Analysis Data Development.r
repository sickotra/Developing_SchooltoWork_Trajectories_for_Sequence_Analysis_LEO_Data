---
title: "Developing School-to-work Trajectories for Sequence Analysis Using the Longitudinal Education Outcomes (LEO) Data "
author: "Shivani Sickotra"
date: "July 2023"
output:
  html_document:
    toc: true
    number_sections: true
  github_document: # can leave html_document first if you set `html_preview: false` under github_document.
   html_preview: false
knit: (function(inputFile, encoding) {
  rmarkdown::render(inputFile, encoding = encoding,
  output_format = "all") })
  
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, eval = FALSE, warning = FALSE)
```

--------------------

</p> This code utilises the Department for Education **Longitudinal Educations Outcomes (LEO)** data to create a bespoke dataset of longitudinal school-to-work activity histories specifically designed for input into **Sequence Analysis** alogrothims. </p>


**Developed Data Summary:**

</p> The LEO data used from the Standard Extract Iteration 1 included:

* **UK Department for Education** - National Pupil Database (NPD), Higher Education Statistics Agency (HESA)

* **UK Department for Work and Pensions** - Employment, Self-Employment, Benefits and Geography data </p>

</p> This code used the Spring School Census from the NPD, the National Client Caseload Information System from the NPD, HESA, DWP Employment, Self-employment and Benefits data to create a longitudinal record of the yearly activity histories for **556,182 individuals from the 2010/11 school-leaver cohort in England**. The activity histories begin from the first non-compulsory observed state in 2011/12 until the 2018/19 academic year, corresponding to ages 16/17 to ages 23/24. Individual-level socio-demographic charactersitics are also prepared, as well as exploratory longitudinal geographic and longitudinal earnings data. </p>

</p> The code first imports data extracted from the LEO database using SQL in Section 1. Then these are preprocessed separately into the required format for sequence analysis in Section 2. Once all activities have been prepared, they are integrated into one dataset containing all activity histories in Section 3. The Combined Authority that an individual was residing in at school-leaving age is linked to their corresponding activity history. This can be used to subset individuals by Combined Authorities for sequence analysis. </p>

</p> The data was developed as part of my **Data Analytics and Society PhD research** [https://gtr.ukri.org/projects?ref=studentship-2433665](https://gtr.ukri.org/projects?ref=studentship-2433665). The code is shared to facilitate accessibility of the sequence analysis technique by detailing how to create suitable input data, demonstrate the capability of the LEO data and promote reproducibility. </p>


</p> **LEO Data Security and Access:**
Data access was obtained through the Office for National Statistics Secure Research Service (ONS SRS) and this code was ran within this environment. All code has been granted ONS SRS security clearance to allow sharing. The instuctions to apply to access the Department for Education LEO data can be found [here](https://www.gov.uk/guidance/apply-to-access-the-longitudinal-education-outcomes-leo-dataset).  </p>

</p> **Data Citation:**
Department for Education; HM Revenue and Customs; Department for Work and Pensions; Higher Education Statistics Agency, released 29 September 2022, ONS SRS Metadata Catalogue, dataset, Longitudinal Education Outcomes SRS Iteration 1 Standard Extract - England, [https://doi.org/10.57906/pzfv-d195](https://doi.org/10.57906/pzfv-d195).  </p>



```{r Load packages, error=FALSE, message=FALSE, warning=FALSE, results='hide'}
# R Version 4.0.2
# R Studio Version 2022.07.2 Build 576
# Load required packages
library("readr") # for reading in cvs data 
library("readxl") # for reading in excel file
library("tidyverse") # for tidy r operations
library("visdat") # for missingness visualisation
library("naniar") # for missingness visualisation
library("TraMineR") # Version 2.2-5
library("grid") # for arranging plots
```

# Import Data

## Cohort and Individual Charactersitics 
```{r Import data, error=FALSE, message=FALSE, warning=FALSE, results='hide'}
# Read in required data
# Note: MCA is used to describe both Mayoral Combined Authorities and non-mayoral Combined Authorities (North Eat non-mayoral at the time of analysis).

# Spring school Census 2010/11 school-leaver cohort and individual Characteristics matched to LEO
ind_char <- read_csv(
  "../SQL/Cohort_Indiviudal Characterisitcs/2010_11_SC_Cohort_Selection_KS4_LEO_matched.csv",
  col_types = cols(everfsm_all_spr11 = col_integer()))

# Read in geography lookup files
# LSOA 2001 to LSOA 2011 to LAD 2011
LSOA01_LSOA11_LAD11 <- read_csv("Preprocessing/Lookups/LSOA01_LSOA11_LAD11.csv")
# 2011 LAD to MCA created in excel
LAD_to_MCA <- read_excel("Preprocessing/Lookups/LAD_to_MCA.xlsx")
# 2011 LAD to GOR
LAD_to_GOR <- read_csv("Preprocessing/Lookups/LA_to_GOR.csv")

# Read in LSOA level variables - 2015 IDACI and 2011 Urban/Rural
IDACI_2015_deciles <- read_csv("Preprocessing/Geographic Charactersitics/IDACI_2015/IDACI_2015_Deciles.csv")
UrbanRural_2011 <- read_csv("Preprocessing/Geographic Charactersitics/UrbanRural/UrbanRural_2011.csv")

```


## NCCIS
 
```{r, warning=FALSE}
# Read in the NCCIS activity states 2011/12 academic year to 2017/18 academic year
# No data was observed in 2018/19 for the 2010/11 school leaver cohort

NCCIS_11_12 <- read_csv("../SQL/Activity_States/NCCIS/2011_12_NCCIS.csv",
                    col_types = cols(NCCIS_Current_Activity_Code = col_integer()))

NCCIS_12_13 <- read_csv("../SQL/Activity_States/NCCIS/2012_13_NCCIS.csv",
                    col_types = cols(NCCIS_Current_Activity_Code = col_integer()))

NCCIS_13_14 <- read_csv("../SQL/Activity_States/NCCIS/2013_14_NCCIS.csv",
                    col_types = cols(NCCIS_Current_Activity_Code = col_integer()))

NCCIS_14_15 <- read_csv("../SQL/Activity_States/NCCIS/2014_15_NCCIS.csv",
                    col_types = cols(NCCIS_Current_Activity_Code = col_integer()))

NCCIS_15_16 <- read_csv("../SQL/Activity_States/NCCIS/2015_16_NCCIS.csv",
                    col_types = cols(NCCIS_Current_Activity_Code = col_integer()))

NCCIS_16_17 <- read_csv("../SQL/Activity_States/NCCIS/2016_17_NCCIS.csv",
                    col_types = cols(NCCIS_Current_Activity_Code = col_integer()))

NCCIS_17_18 <- read_csv("../SQL/Activity_States/NCCIS/2017_18_NCCIS.csv",
                    col_types = cols(NCCIS_Current_Activity_Code = col_integer()))
``` 

## HESA
```{r, message=FALSE}
# Read in the files - HESA 2013/14 - 2018/19 HESA academic reporting year 
HESA_2011_12 <- read_csv("../SQL/Activity_States/HESA/2011_12_HESA.csv") 
HESA_2012_13 <- read_csv("../SQL/Activity_States/HESA/2012_13_HESA.csv") 
HESA_2013_14 <- read_csv("../SQL/Activity_States/HESA/2013_14_HESA.csv") 
HESA_2014_15 <- read_csv("../SQL/Activity_States/HESA/2014_15_HESA.csv") 
HESA_2015_16 <- read_csv("../SQL/Activity_States/HESA/2015_16_HESA.csv") 
HESA_2016_17 <- read_csv("../SQL/Activity_States/HESA/2016_17_HESA.csv") 
HESA_2017_18 <- read_csv("../SQL/Activity_States/HESA/2017_18_HESA.csv") 
HESA_2018_19 <- read_csv("../SQL/Activity_States/HESA/2018_19_HESA.csv") 
```


## DWP Out of Work Benefits, Employment and Self-employment

```{r, error=FALSE, message=FALSE, warning=FALSE, results='hide'}
# Read in the Out of Work Benefits, Employment and Self Employment data
benefits <- read_csv("../SQL/Activity_States/Benefits/2010_11_Cohort_OfW_Benefits_Spells.csv") 
emp <- read_csv("../SQL/Activity_States/Employment/2010_11_Cohort_Emp_Spells.csv") 

SEmp_201415 <- read_csv(
  "../SQL/Activity_States/Self_Employment/2010_11_Cohort__2014_15_TaxYear_SEmp.csv") 
SEmp_201516 <- read_csv(
  "../SQL/Activity_States/Self_Employment/2010_11_Cohort__2015_16_TaxYear_SEmp.csv") 
SEmp_201617 <- read_csv(
  "../SQL/Activity_States/Self_Employment/2010_11_Cohort__2016_17_TaxYear_SEmp.csv") 
SEmp_201718 <- read_csv(
  "../SQL/Activity_States/Self_Employment/2010_11_Cohort__2017_18_TaxYear_SEmp.csv") 
SEmp_201819 <- read_csv(
  "../SQL/Activity_States/Self_Employment/2010_11_Cohort__2018_19_TaxYear_SEmp.csv") 
```



------------------------------------------------------------------------------------
# Preprocessing 


## Preprocess Cohort and Individual Characteristics

```{r}
# Check for duplicate ID_PMR, expected to include duplicates due to KS4 linking - TRUE
#any(duplicated(ind_char$pupilmatchingrefanonymous_spr11))

# Count how many UNIQUE IDs 
#length(unique(ind_char$pupilmatchingrefanonymous_spr11))


# Investigate nature of the duplicates - these are expected to be 2 records where KS4 is attained and not attained
# Subset the PMRs with the duplicates 
ind_char_dup_IDs <- ind_char[duplicated(ind_char$pupilmatchingrefanonymous_spr11), ]

# Dataframe with both the 'original' and duplicated IDs to see the difference in GCSE column
ind_char_orig_and_dup <- ind_char_dup_IDs %>%
  select(pupilmatchingrefanonymous_spr11) %>%
  inner_join(ind_char)

# From those duplicated, We retain only the record for the people obtaining the GCSE criteria in the KS4 data
 ind_char <- ind_char %>%
  # Order by ID, then KS4 column in descending order ie. 1, 0
  arrange(pupilmatchingrefanonymous_spr11, desc(KS4_LEVEL2_EM)) %>%
  # Retain only 1st record of all duplicates
  distinct(pupilmatchingrefanonymous_spr11, .keep_all = TRUE)
 
# IDs are now unique
# any(duplicated(ind_char$pupilmatchingrefanonymous_spr11))
```

```{r}
# Rename the columns
ind_char <- ind_char %>% rename(
  "ID_PMR" = "pupilmatchingrefanonymous_spr11",
  "Gender" = "gender_spr11",
  "Ethnicity" = "ethnicgroupmajor_spr11",
  "SEN" = "senprovision_spr11",
  "FSM" = "everfsm_all_spr11",
  "LSOA_code_2001" = "llsoa_spr11",
  "GCSE_attainment" = "KS4_LEVEL2_EM")

# Recode gender from F to 1 and M to 0
ind_char <- ind_char %>% mutate(Gender = recode(Gender,
                                                'F' = 1, 'M' = 0))
# Recode ethnicity values 
ind_char <- ind_char %>% mutate(Ethnicity = recode(Ethnicity,
                                                   'ASIA' = 'Asian',
                                                   'WHIT' = 'White',
                                                   'MIXD' = 'Mixed',
                                                   'AOEG' = 'Other',
                                                   'UNCL' = 'Unknown',
                                                   'CHIN' = 'Chinese',
                                                   'BLAC' = 'Black'))
# Create new SEN indicator variable with binary values
# 0 = No Special Educational Needs
# 1 = School Action or early years action (up to 2014/15),
# school action plus or early years action plus (up to 2014/15),
# statement (up to 2017/18), SEN support (since 2014/15), education,
# health and care plan (since 2014/15) 
ind_char <- ind_char %>%
  mutate(SEN = case_when(SEN == 'N' ~ 0, 
                         SEN == 'A' ~ 1,
                         SEN == 'S' ~ 1,
                         SEN == 'P' ~ 1,
                         TRUE ~ 0)) # where SEN is null, assume no SEN

# Where FSM null, assume no FSM requirement
ind_char$FSM[is.na(ind_char$FSM)] <- 0
```

```{r Join lookups/data to Ind char}
# Join Lookup files

# There will be duplicate LSOA01 codes for those which split into multiple 
# LSOAs in 2011. Keep only the 1st occurrence to ensure 1-1 match with the school-leaver cohort
# Loose small level of detail 
#any(duplicated(LSOA01_LSOA11_LAD11$LSOA01CD))
LSOA01_LSOA11_LAD11 <- LSOA01_LSOA11_LAD11 %>%
  # Retain only 1st record of all duplicates
  distinct(LSOA01CD, .keep_all = TRUE)


# Left join 2001 LSOA to 2011 LSOA to 2011 LAD lookup using LSOA 2001 code
# This lookup is only for England and Wales 
ind_char <- merge(x=ind_char, y=LSOA01_LSOA11_LAD11,
                  by.x = "LSOA_code_2001", by.y = "LSOA01CD", all.x = TRUE)
#Delete 2001 LSOA column
ind_char <- subset(ind_char, select = -c(LSOA_code_2001))

# Left join LAD to MCA using LAD name
ind_char <- merge(x=ind_char, y=LAD_to_MCA,
                  by.x = "LAD11NM", by.y="LAD_school_leaving", all.x = TRUE)

# Left join LAD to GOR using LAD code
ind_char <- merge(x=ind_char, y=LAD_to_GOR,
                  by.x = "LAD11CD", by.y = "LA", all.x = TRUE)


# View which have not been correctly translated into GOR
# ie. which still have NA recorded for Region
#GOR_missing <- ind_char %>%
#  filter(is.na(Region)) %>%
#  distinct(LAD11NM)

# Replace any missing with their respective Government Office Region 
ind_char <- ind_char %>%
  mutate(Region = replace(Region,
                          LAD11NM == "St Albans", "East of England")) %>%
  mutate(Region = replace(Region,
                          LAD11NM == "Welwyn Hatfield", "East of England"))

# Replace GOR "East" with "East of England"
ind_char$Region[ind_char$Region == "East"] <- "East of England"


# Retain only those who had geographic location recorded in 2010/11 academic year at school-leaving age
# Discard those in Wales or missing LAD location data 
# Retain only individual characteristics data for England
ind_char <- ind_char[ind_char$Region != "Wales", ]
ind_char <- ind_char %>% drop_na(Region)
#any(duplicated(ind_char$ID_PMR)) # check all IDs still unique




# Left join LSOA level Variables using LSOA codes
# 2011 Urban Rural classification 
ind_char <- merge(x=ind_char, y=UrbanRural_2011,
                  by = "LSOA11CD", all.x = TRUE)
# 2015 IDACI Decile Variable
ind_char <- merge(x=ind_char, y=IDACI_2015_deciles,
                  by.x = "LSOA11CD", by.y = "LSOA_code_2011", all.x = TRUE)

# Group Urban Rural classifications into either Urban, Rural, Unknown
ind_char <- ind_char %>%
  mutate(RUC11 = replace(RUC11,
                         RUC11 == "Urban major conurbation", "Urban")) %>%
  mutate(RUC11 = replace(RUC11,
                         RUC11 == "Urban city and town", "Urban")) %>%
  mutate(RUC11 = replace(RUC11,
                         RUC11 == "Urban minor conurbation", "Urban")) %>%
  mutate(RUC11 = replace(RUC11,
                         RUC11 == "Urban city and town in a sparse setting","Urban")) %>%
  mutate(RUC11 = replace(RUC11,
                         RUC11 == "Rural village and dispersed", "Rural")) %>%
  mutate(RUC11 = replace(RUC11,
                         RUC11 == "Rural town and fringe", "Rural")) %>%
  mutate(RUC11 = replace(RUC11,
                         RUC11 == "Rural village and dispersed in a sparse setting", "Rural")) %>%
  mutate(RUC11 = replace(RUC11,
                         RUC11 == "Rural town and fringe in a sparse setting", "Rural")) %>%
  mutate(RUC11 = replace(RUC11,is.na(RUC11), "Unknown"))

# Create flag to indicate whether the IDACI is top 10% most deprived 
# ie. if decile is 1 then remain as 1, else mark as 0
ind_char$IDACI_Decile_Top10 = replace(x = ind_char$IDACI_Decile,
                                    list = !ind_char$IDACI_Decile %in% c(1),
                                    values = 0)
# Delete the now redundant IDACI var columns
ind_char <- subset(ind_char, select = -c(IDACI_Decile))

# Reorder columns 
ind_char <- ind_char[, c(4:9, 12:13, 1:3, 10:11)]
# Rename the added columns
ind_char <- ind_char %>% rename("Urban_Rural" = "RUC11",
                                "IDACI_Decile_MostDep" = "IDACI_Decile_Top10",
                                "LSOA_code" = "LSOA11CD",
                                "LAD_code" = "LAD11CD",
                                "LAD_Name" = "LAD11NM")
```

```{r Export Ind char}
# Visualise missingness in the individual characteristics
#vis_dat(ind_char, warn_large_data = FALSE)

# Export preprocessed ind_char to CSV for later use
#write.csv(ind_char,"Code Outputs/Compartmentalised/ind_char.csv",
#          row.names=FALSE)
```




## Preprocess NCCIS Activity States (2011/12 - 2017/18 academic year)

```{r, message = FALSE}
# Read in preprocessed ind_char 
ind_char <- read_csv("Code Outputs/Compartmentalised/ind_char.csv")

# Select the individual characteristics ID - to use for left joining the first NCCIS activity
cohort_ID <- ind_char %>%
  select(ID_PMR)
```

```{r Preprocess NCCIS Activity States}
# Preprocess the activity states 

# Change Activity State column headers
NCCIS_11_12 <- NCCIS_11_12 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "A_Activity" = "NCCIS_Current_Activity_Code")
NCCIS_12_13 <- NCCIS_12_13 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "B_Activity" = "NCCIS_Current_Activity_Code")
NCCIS_13_14 <- NCCIS_13_14 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "C_Activity" = "NCCIS_Current_Activity_Code")
NCCIS_14_15 <- NCCIS_14_15 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "D_Activity" = "NCCIS_Current_Activity_Code")
NCCIS_15_16 <- NCCIS_15_16 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "E_Activity" = "NCCIS_Current_Activity_Code")
NCCIS_16_17 <- NCCIS_16_17 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "F_Activity" = "NCCIS_Current_Activity_Code")
NCCIS_17_18 <- NCCIS_17_18 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "G_Activity" = "NCCIS_Current_Activity_Code")


# Left join first activity NCCIS_11_12 to this -
# ie. retain the first activity for only the people in LEO matched cohort
cohort_NCCIS_A <- merge(x=cohort_ID, y=NCCIS_11_12, by = "ID_PMR", all.x = TRUE)


# Calculate the 'mode' activity for each individual within the years
# There is more than 1 activity recorded per year,
# this will find the most frequent to have 1 activity in each year
# if there are multiple modes/no mode, will keep the 1st appearing mode value
# if there is no mode, the 1st value will be retained 

# Function to calculate categorical mode
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))] 
} 
A_NCCIS_mode <- cohort_NCCIS_A %>% 
  group_by(ID_PMR) %>%
  summarise(mode = calculate_mode(A_Activity))

B_NCCIS_mode <- NCCIS_12_13 %>% 
  group_by(ID_PMR) %>%
  summarise(mode = calculate_mode(B_Activity))

C_NCCIS_mode <- NCCIS_13_14 %>% 
  group_by(ID_PMR) %>%
  summarise(mode = calculate_mode(C_Activity))

D_NCCIS_mode <- NCCIS_14_15 %>% 
  group_by(ID_PMR) %>%
  summarise(mode = calculate_mode(D_Activity))

E_NCCIS_mode <- NCCIS_15_16 %>% 
  group_by(ID_PMR) %>%
  summarise(mode = calculate_mode(E_Activity))

F_NCCIS_mode <- NCCIS_16_17 %>% 
  group_by(ID_PMR) %>%
  summarise(mode = calculate_mode(F_Activity))

G_NCCIS_mode <- NCCIS_17_18 %>% 
  group_by(ID_PMR) %>%
  summarise(mode = calculate_mode(G_Activity))

# check ID_PMR is unique in all -  replace with name of respective data frame
#any(duplicated(G_NCCIS_mode$ID_PMR))


# Merge the activity states using a left join/wide format
# Keep all participants present in the individual characteristics 2010/11,
# even if there is missing data in subsequent activity years

# Create list of all activity states to merge
activitylist <- list(A_NCCIS_mode, B_NCCIS_mode,
                     C_NCCIS_mode, D_NCCIS_mode, E_NCCIS_mode,F_NCCIS_mode, G_NCCIS_mode)

# Left join these using ID_PMR
NCCIS_merged <- purrr::reduce(activitylist, left_join, by = 'ID_PMR', all.x=TRUE)


# Update column headers 
NCCIS_merged <- NCCIS_merged %>% rename("NCCIS_11_12" = "mode.x", "NCCIS_12_13" = "mode.y",
                            "NCCIS_13_14" = "mode.x.x",  "NCCIS_14_15" = "mode.y.y",
                            "NCCIS_15_16" = "mode.x.x.x", "NCCIS_16_17" = "mode.y.y.y", 
                            "NCCIS_17_18" = "mode")
# Rename the dataset with ID_PMR for later use
NCCIS_merged_ID <- NCCIS_merged

# Dataset subset without the ID_PMR 
NCCIS_merged <- subset(NCCIS_merged_ID, select = -c(ID_PMR))


# Save merged preprocessed NCCIS states prior to grouping the types of activities
#write.csv(NCCIS_merged_ID,"Code Outputs/Compartmentalised/NCCIS_merged_activities_Ungrouped.csv",
#          row.names=FALSE)
```


### Group NCCIS states for Smaller Sequence Analysis Alphabet

```{r}
# Read in merged preprocessed NCCIS States
NCCIS_merged_ID <- read.csv("Code Outputs/Compartmentalised/NCCIS_merged_activities_Ungrouped.csv")
# Remove ID
NCCIS_merged <- subset(NCCIS_merged_ID, select = -c(ID_PMR))

# Find frequency of the various NCCIS state values
NCCIS_activity_freq <- as.data.frame(table(unlist(NCCIS_merged)))

# Arrange in descending order
NCCIS_activity_freq <- NCCIS_activity_freq %>%
  arrange(desc(Freq))

```

```{r fig.align="center", echo=FALSE, fig.width=15, fig.height=6}
# Plot the frequency of the various states before grouping

#jpeg(filename = "Code Outputs/Figures/NCCIS_Ungrouped_Activity_Freq.jpeg", width = 3800, height = 1500,
 #    res = 300)
 
ggplot(NCCIS_activity_freq,
       aes(x= Var1, y=Freq)) +
  geom_col() +
  theme_minimal() +
  ylab("Frequency") +
  xlab("NCCIS Current Activity Code") +
  ggtitle("NCCIS Ungrouped Current Activity Frequencies in 2010/11 English School-leaver Cohort")
```


```{r}
# Recode the numeric activity values to the descriptive equivalent and group these
# NOTE: These are not counts. They are the original numeric codes provided by NCCIS available online and in 
# the LEO variable request form for the NCCIS Current Activity Variable
NCCIS_merged <- as.data.frame(lapply(NCCIS_merged, function(x) replace(x,x %in% 210,
                                             "School 6th Form")))
NCCIS_merged <- as.data.frame(lapply(NCCIS_merged, function(x) replace(x,x %in% 220,
                                             "6th Form College")))
NCCIS_merged <- as.data.frame(lapply(NCCIS_merged, function(x) replace(x,x %in% 230,
                                             "Further Education")))
NCCIS_merged <- as.data.frame(lapply(NCCIS_merged, function(x) replace(x,x %in% 240,
                                             "Higher Education")))
NCCIS_merged <- as.data.frame(lapply(NCCIS_merged, function(x) replace(x,x %in% c(310),
                                             "Apprenticeship")))
NCCIS_merged <- as.data.frame(lapply(NCCIS_merged, function(x) replace(x,x %in% c(320, 330, 340, 350, 360),
                                             "Employment")))
NCCIS_merged <- as.data.frame(lapply(NCCIS_merged, function(x) replace(x,x %in% c(380, 381),
                                             "Self Employment")))
NCCIS_merged <- as.data.frame(lapply(NCCIS_merged, function(x) replace(x,x %in% c(410, 430, 440, 450, 460),
                                             "Government Supported")))
NCCIS_merged <- as.data.frame(lapply(NCCIS_merged, function(x) replace(x,x %in% c(510, 520,
                                                        530, 610,615, 616, 619, 
                                                        620, 630, 640, 650, 660,
                                                        670, 680),
                                             "NEET")))
NCCIS_merged <- as.data.frame(lapply(NCCIS_merged,function(x) replace(x,x %in% c(110, 130, 140, 250,
                                                                                 260, 270, 280, 540,
                                                                                 550, 710, 720),
                                              "Other")))
NCCIS_merged <- as.data.frame(lapply(NCCIS_merged, function(x) replace(x,x %in% c(150, 810, 820, 830),
                                             "Unknown")))

# Replace "Unknown" string values with NAs 
NCCIS_merged <- replace(NCCIS_merged, NCCIS_merged == "Unknown", NA)


# Visualise missingness in English 2010/11 school-leaver cohort
# Note overplotting is present because of large data,
# there are also individuals with NCCIS states in the last 3 years but are not visible on plot
vis_dat(NCCIS_merged, warn_large_data = FALSE)
```

```{r}
# Add PMR ID back to the NCCIS grouped states 
# Order of individuals has remained the same,therefore can column bind

# Select the individual characteristics ID
cohort_ID <- NCCIS_merged_ID %>%
  select(ID_PMR)
# Column bind
NCCIS_merged_activities_grouped <- cbind(cohort_ID, NCCIS_merged)

# Save grouped preprocessed NCCIS states
#write.csv(NCCIS_merged_activities_grouped,"Code Outputs/Compartmentalised/NCCIS_merged_activities_Grouped.csv",
 #         row.names=FALSE)
```




## Preprocess HESA Activity States (2013/14 - 2018/19 [HESA reporting] academic year)


```{r}
# HESA academic reporting period runs from 1st August in year 1 to 31st  July in year 2.
# In the NCCIS data, academic years ran from 1st September in year 1 to 31st August in year 2
# However, in this preprocessing they are treated as the same 

# Check if there are duplicate IDs in all years - change HESA dataframe name each time
# There are no duplicates in any
#any(duplicated(HESA_2018_19$pupilmatchingrefanonymous_spr11))


# Rename columns - The 2011-12 academic year is labelled as 2011 
HESA_2011_12 <- HESA_2011_12 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "start_2011" = "he_comdate",
                                      "end_2011" = "he_enddate",
                                      "HESA_StudyLvl_2011" = "he_xlev301")
HESA_2012_13 <- HESA_2012_13 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "start_2012" = "he_comdate",
                                      "end_2012" = "he_enddate",
                                      "HESA_StudyLvl_2012" = "he_xlev301")
HESA_2013_14 <- HESA_2013_14 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "start_2013" = "he_comdate",
                                      "end_2013" = "he_enddate",
                                      "HESA_StudyLvl_2013" = "he_xlev301")
HESA_2014_15 <- HESA_2014_15 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "start_2014" = "he_comdate",
                                      "end_2014" = "he_enddate",
                                      "HESA_StudyLvl_2014" = "he_xlev301")
HESA_2015_16 <- HESA_2015_16 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "start_2015" = "he_comdate",
                                      "end_2015" = "he_enddate",
                                      "HESA_StudyLvl_2015" = "he_xlev301")
HESA_2016_17 <- HESA_2016_17 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "start_2016" = "he_comdate",
                                      "end_2016" = "he_enddate",
                                      "HESA_StudyLvl_2016" = "he_xlev301")
HESA_2017_18 <- HESA_2017_18 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "start_2017" = "he_comdate",
                                      "end_2017" = "he_enddate",
                                      "HESA_StudyLvl_2017" = "he_xlev301")
HESA_2018_19 <- HESA_2018_19 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "start_2018" = "he_comdate",
                                      "end_2018" = "he_enddate",
                                      "HESA_StudyLvl_2018" = "he_xlev301")


# Full join all dataframes to retain every individual no matter what year they appear
# Create list of all to merge
HESAlist <- list(HESA_2011_12, HESA_2012_13, HESA_2013_14, HESA_2014_15, HESA_2015_16, HESA_2016_17,
                 HESA_2017_18, HESA_2018_19)
HESA_all <- purrr::reduce(HESAlist, dplyr::full_join, by = 'ID_PMR')


# Check duplicates across entire rows and IDs - NO duplicates
#any(duplicated(HESA_all))
#any(duplicated(HESA_all$ID_PMR))


# Linking has created a dataframe with 1 row per person, with a start and end date recorded in any one of the years, with NULL values between these period (which indicate individual is still taking the course)
# We can see where individuals change from an Undergraduate course to a Postgraduate etc with study level

# The first two academic years (2011/12 and 2012/13) are outliers. They do not have an end date, but instead have another start date in 2013/14. These were extracted from SQL in case a person was undertaking FE level qualification, but data shows Level of Study was not FE. Therefore these years are removed. Hence, HESA data used in the analysis runs from 2013/14 - 2018/19 academic years
HESA_all <- subset(HESA_all, select = -c(start_2011, end_2011, HESA_StudyLvl_2011,
                                         start_2012, end_2012, HESA_StudyLvl_2012))

# Study Level tells us in the academic year what type of activity a person was undertaking
# This is completed for the length of the study period ie. all years between the start and end date
# Use this variable to create the yearly longitudinal record of HESA education activities 
HESA_activites <- subset(HESA_all, select = c(ID_PMR, HESA_StudyLvl_2013, HESA_StudyLvl_2014,
                                              HESA_StudyLvl_2015, HESA_StudyLvl_2016,
                                              HESA_StudyLvl_2017, HESA_StudyLvl_2018))

# Recode the level of study to arbitrary values
# Recode Further Education to 9
HESA_activites[HESA_activites == 3] <- 9
# Recode Undergraduate to 8
HESA_activites[HESA_activites == 2] <- 8
# Recode postgraduate to 7
HESA_activites[HESA_activites == 1] <- 7

# Remove cases where NA in all years
HESA_activites <- HESA_activites %>%
  filter(!if_all(HESA_StudyLvl_2013:HESA_StudyLvl_2018, is.na))



# Retain only the records for people in the LEO matched cohort

# Read in preprocessed ind_char 
ind_char <- read_csv("Code Outputs/Compartmentalised/ind_char.csv")

# Select the individual characteristics ID - to use for left joining the preprocessed activities
cohort_ID <- ind_char %>%
  select(ID_PMR)

# Left join HESA to ind_char
HESA_activites <- merge(x=cohort_ID, y=HESA_activites, by = "ID_PMR", all.x = TRUE)

#Save preprocessed HESA activities 
#write.csv(HESA_activites,"Code Outputs/Compartmentalised/HESA_merged_activities.csv",
 #         row.names=FALSE)
```


## Preprocess DWP Out of Work Benefits, Employment and Self-employment Activities


### Out of Work Benefits Data (2011/12 - 2018/19 tax year)
```{r Benefits}
# Check data type of column
#spec(benefits)
# Check if there are more than one spell per person - TRUE
#any(duplicated(benefits$pupilmatchingrefanonymous_spr11))

# Data contains some end dates "9999-12-31"
# Set these "2019-04-05" end of the 2018/19 tax year as these were entered to
# indicate ongoing benefit claims
benefits <- benefits %>%
  mutate(enddate = replace(enddate, enddate == "9999-12-31", "2019-04-05"))

# Add new column which labels the Benefits state as arbitrary value of 3
benefits$State <- 3
```


### Employment Data (2011/12 - 2018/19 tax year)
```{r Employment}
# Check data type of column
#spec(emp)
# Check if there are more than one spell per person - TRUE
#any(duplicated(emp$pupilmatchingrefanonymous_spr11))

# Data contains some end dates "9999-12-31"
# Set these "2019-04-05" end of the 2018/19 tax year to indicate ongoing employment 
emp <- emp %>%
  mutate(enddate = replace(enddate, enddate == "9999-12-31", "2019-04-05"))

# Add new column which labels the Employment state as arbitrary value of 2
emp$State <- 2
```

### Merge Benefits and Employment Spells
```{r Merge benefits and employment}
# Row bind the benefits and employment spells
benefits_emp_spells <- rbind(benefits, emp)

# Rename the pupilmatchingrefanonymous_spr11 to ID_PMR
benefits_emp_spells <- benefits_emp_spells %>%
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11")

# Change the dates into Tax Year calendar
# ie. if start date is 2016-06-01, then TaxYear StartDate is 2016
# Must lose granularity of the monthly data and maintain consistency with NCCIS yearly data

# If start date falls within the tax year period, mark as the beginning year of the tax period
benefits_emp_spells <- benefits_emp_spells %>% 
  mutate(TaxYr_startdate =
           case_when(startdate >= "2011-04-06" & startdate <= "2012-04-05" ~ "2011",
                     startdate >= "2012-04-06" & startdate <= "2013-04-05" ~ "2012",
                     startdate >= "2013-04-06" & startdate <= "2014-04-05" ~ "2013",
                     startdate >= "2014-04-06" & startdate <= "2015-04-05" ~ "2014",
                     startdate >= "2015-04-06" & startdate <= "2016-04-05" ~ "2015",
                     startdate >= "2016-04-06" & startdate <= "2017-04-05" ~ "2016",
                     startdate >= "2017-04-06" & startdate <= "2018-04-05" ~ "2017",
                     startdate >= "2018-04-06" & startdate <= "2019-04-05" ~ "2018",
                     # Any other as 0
                     TRUE ~ "0"))

# If end date falls within the tax year period, mark as the beginning year of the tax period
benefits_emp_spells <- benefits_emp_spells %>% 
  mutate(TaxYr_enddate = 
           case_when(enddate >= "2011-04-06" & enddate <= "2012-04-05" ~ "2011",
                     enddate >= "2012-04-06" & enddate <= "2013-04-05" ~ "2012",
                     enddate >= "2013-04-06" & enddate <= "2014-04-05" ~ "2013",
                     enddate >= "2014-04-06" & enddate <= "2015-04-05" ~ "2014",
                     enddate >= "2015-04-06" & enddate <= "2016-04-05" ~ "2015",
                     enddate >= "2016-04-06" & enddate <= "2017-04-05" ~ "2016",
                     enddate >= "2017-04-06" & enddate <= "2018-04-05" ~ "2017",
                     enddate >= "2018-04-06" & enddate <= "2019-04-05" ~ "2018",
                     # Any other as 0
                     TRUE ~ "0"))

# Convert column data type to integer 
benefits_emp_spells$TaxYr_startdate <- as.integer(benefits_emp_spells$TaxYr_startdate)
benefits_emp_spells$TaxYr_enddate <- as.integer(benefits_emp_spells$TaxYr_enddate)

# Where TaxYr start and end date is 0, replace with NA
benefits_emp_spells <- benefits_emp_spells %>%
  mutate_at(c('TaxYr_startdate', 'TaxYr_enddate'), ~na_if(., 0))

# If the tax year start and end dates are the same, create new variable with this year
benefits_emp_spells <- benefits_emp_spells %>%
  mutate(Aggregate_State = if_else(TaxYr_startdate == TaxYr_enddate,
                                   TaxYr_startdate,
                                   NULL))
```

```{r Reshape data, message=FALSE, results='hide'}
# Reshaping data into appropriate long format
# Extract the cases where the activity lasts more than one tax year
gr1yr <- subset(benefits_emp_spells, is.na(Aggregate_State))

# Create a copy of the dataset
gr1yr_enddate <- data.frame(gr1yr)

# In orginal, Fill Aggregate_State column with the TAX YEAR START DATE
gr1yr$Aggregate_State <- gr1yr$TaxYr_startdate

# In copy, Fill Aggregate_State column with the TAX YEAR END DATE
gr1yr_enddate$Aggregate_State <- gr1yr_enddate$TaxYr_enddate

# Filter only the ID_PMR and the Aggregate State column in both original and copy
gr1yr <- subset(gr1yr, select = c(ID_PMR, State, Aggregate_State))
gr1yr_enddate <- subset(gr1yr_enddate, select = c(ID_PMR, State, Aggregate_State))

# rbind the two dataframes to create a long format - multiple rows per individual,
# 1 with the start date of the state and other with the end date
DWP_TxYr_longformat <- rbind(gr1yr, gr1yr_enddate)

# Separate cases where state = 3 and state = 2
Benefit_long <- subset(DWP_TxYr_longformat, DWP_TxYr_longformat$State == 3)
Emp_long <- subset(DWP_TxYr_longformat, DWP_TxYr_longformat$State == 2)


# Long to wide format 
Benefit_wide <- Benefit_long %>%
  pivot_wider(id_cols = ID_PMR,
              names_from = Aggregate_State, values_from = State,
              names_sort = TRUE,
              values_fill = NA,
              values_fn = ~mean(.x, na.rm = TRUE))

Emp_wide <- Emp_long %>%
  pivot_wider(id_cols = ID_PMR,
              names_from = Aggregate_State, values_from = State,
              names_sort = TRUE,
              values_fill = NA,
              values_fn = ~mean(.x, na.rm = TRUE))
```

Fill *gaps* of any length with value 3 for Out of Work benefit or 2 for Employment. Since the start date and end date were used, it is known that the gaps between these dates will be the same (the gaps created were intentional in order to bring the data into the required format). There is a risk that individuals who had start & end date in the SAME tax year ie. 2016, then another spell with start & end date in another year ie. 2018, would be filled from 2016 to 2018 in the wide format, however during coalesce and integration with NCCIS and HESA data, these will be overlaid with the correct value where available.The left or right missing values in the wide-format Benefits and Employment dataframes were not imputed since Tax Year Start and End date min/max time period were dealt with earlier.

```{r Fill gaps with known data, message=FALSE}
# Impute the intentional gaps created
Benefit_wide_noID <- subset(Benefit_wide, select = -c(ID_PMR))
Benefit_wide_fillgap <- seqdef(Benefit_wide_noID,
                               # Fill with arbitary 3 for Out of Work Benefit
                               gaps=3)
Emp_wide_noID <- subset(Emp_wide, select = -c(ID_PMR))
Emp_wide_fillgap <- seqdef(Emp_wide_noID,
                           # Fill with arbitary 2 for Employment
                           gaps=2)

# Join ID back to imputted data
Benefit_wide <- cbind(Benefit_wide$ID_PMR, Benefit_wide_fillgap)
Emp_wide <- cbind(Emp_wide$ID_PMR, Emp_wide_fillgap)

# Replace any * and % with NA - the seqdef function automatically adds these for NAs
Benefit_wide[Benefit_wide == '*' | Benefit_wide == '%'] <- NA
Emp_wide[Emp_wide == '*' | Emp_wide == '%'] <- NA

# Rename the columns
Benefit_wide <- Benefit_wide %>% rename("ID_PMR" = "Benefit_wide$ID_PMR",
                                        "Benefits_2011_12_Tx" = "2011",
                                        "Benefits_2012_13_Tx" = "2012",
                                        "Benefits_2013_14_Tx" = "2013",
                                        "Benefits_2014_15_Tx" = "2014",
                                        "Benefits_2015_16_Tx" = "2015",
                                        "Benefits_2016_17_Tx" = "2016",
                                        "Benefits_2017_18_Tx" = "2017",
                                        "Benefits_2018_19_Tx" = "2018")

Emp_wide <- Emp_wide %>% rename("ID_PMR" = "Emp_wide$ID_PMR",
                                "Emp_2011_12_Tx" = "2011",
                                "Emp_2012_13_Tx" = "2012",
                                "Emp_2013_14_Tx" = "2013",
                                "Emp_2014_15_Tx" = "2014",
                                "Emp_2015_16_Tx" = "2015",
                                "Emp_2016_17_Tx" = "2016",
                                "Emp_2017_18_Tx" = "2017",
                                "Emp_2018_19_Tx" = "2018")

# Retain only the records for people in the LEO matched cohort
# Read in preprocessed ind_char 
ind_char <- read_csv("Code Outputs/Compartmentalised/ind_char.csv")

# Select the individual characteristics ID - to use for left joining the preprocessed activities
cohort_ID <- ind_char %>%
  select(ID_PMR)

# Left join Benefits to ind_char -
Benefit_wide <- merge(x=cohort_ID, y=Benefit_wide, by = "ID_PMR", all.x = TRUE)
# Left join Employment to ind_char -
Emp_wide <- merge(x=cohort_ID, y=Emp_wide, by = "ID_PMR", all.x = TRUE)

                      

# Export the prepared wide format Out of Work Benefits and Employemnt dataframes as .csvs
#write.csv(Benefit_wide,
 #         "Code Outputs/Compartmentalised/Benefit_merged_activities.csv", row.names=FALSE)
#write.csv(Emp_wide,
 #         "Code Outputs/Compartmentalised/Employment_merged_activities.csv", row.names=FALSE)

```


### Self Employment Data (2014/15 - 2018/19 tax year)
```{r Self employment}
# Rename columns
SEmp_201415 <- SEmp_201415 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "2014" = "Self_Employed")
SEmp_201516 <- SEmp_201516 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "2015" = "Self_Employed")
SEmp_201617 <- SEmp_201617 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "2016" = "Self_Employed")
SEmp_201718 <- SEmp_201718 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "2017" = "Self_Employed")
SEmp_201819 <- SEmp_201819 %>% rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
                                      "2018" = "Self_Employed")

# Full join all dataframes to retain every individual no matter what year they appear
# Create list of all to merge
SEmplist <- list(SEmp_201415, SEmp_201516, SEmp_201617, SEmp_201718, SEmp_201819)
SEmp_wide <- purrr::reduce(SEmplist, dplyr::full_join, by = 'ID_PMR')

# Rename the columns
SEmp_wide <- SEmp_wide %>% rename("SEmp_2014_15_Tx" = "2014",
                                  "SEmp_2015_16" = "2015",
                                  "SEmp_2016_17" = "2016",
                                  "SEmp_2017_18" = "2017",
                                  "SEmp_2018_19" = "2018")

# Self employment already is coded as an arbitrary value of 1

# Left join Self employment to ind_char -
SEmp_wide <- merge(x=cohort_ID, y=SEmp_wide, by = "ID_PMR", all.x = TRUE)

# Export the prepared wide format Self Employemnt dataframe as .csv
#write.csv(SEmp_wide,"Code Outputs/Compartmentalised/Self_Emp_merged_activities.csv",row.names=FALSE)

```

# Integrate NCCIS, HESA and DWP Benefits, Employment and Self-employment Activities

```{r, message=FALSE, warning=FALSE}
# Read in all preprocessed data
ind_char <- read_csv("Code Outputs/Compartmentalised/ind_char.csv")

NCCIS_merged_activities_Grouped <- read_csv(
  "Code Outputs/Compartmentalised/NCCIS_merged_activities_Grouped.csv")
HESA_merged_activities <- read_csv("Code Outputs/Compartmentalised/HESA_merged_activities.csv")
Benefit_merged_activities <- read_csv("Code Outputs/Compartmentalised/Benefit_merged_activities.csv")
Employment_merged_activities <- read_csv(
  "Code Outputs/Compartmentalised/Employment_merged_activities.csv")
Self_Emp_merged_activities <- read_csv(
  "Code Outputs/Compartmentalised/Self_Emp_merged_activities.csv")
```



```{r}
# Prepare for activities integration

# Ensure the order of all IDs is the same across all dataframes
NCCIS_merged_activities_Grouped <- NCCIS_merged_activities_Grouped[order(NCCIS_merged_activities_Grouped$ID_PMR), ]
HESA_merged_activities <- HESA_merged_activities[order(HESA_merged_activities$ID_PMR), ]
Benefit_merged_activities <- Benefit_merged_activities[order(Benefit_merged_activities$ID_PMR), ]
Employment_merged_activities <- Employment_merged_activities[order(Employment_merged_activities$ID_PMR), ]
Self_Emp_merged_activities <- Self_Emp_merged_activities[order(Self_Emp_merged_activities$ID_PMR), ]


# Add empty columns where data was not present for certain years
# ie. Make all dataframes have the same number of columns
# NCCIS ran from 2011/12 - 2017/18 acad year, add 2018/19 column
NCCIS_merged_activities_Grouped <- add_column(NCCIS_merged_activities_Grouped, NCCIS_18_19 = NA, .after = 8)

# HESA ran from 2013/14-2018/19 acad year, add 2011/12 and 2012/13 columns
HESA_merged_activities <- add_column(HESA_merged_activities, HESA_11_12 = NA, .after = 1)
HESA_merged_activities <- add_column(HESA_merged_activities, HESA_12_13 = NA, .after = 2)

# Self Employment ran from 2014/15 - 2018/19 tax year, add 2011/12 - 2013/14 columns
Self_Emp_merged_activities <- add_column(Self_Emp_merged_activities, SEmp_11_12 = NA, .after = 1)
Self_Emp_merged_activities <- add_column(Self_Emp_merged_activities, SEmp_12_13 = NA, .after = 2)
Self_Emp_merged_activities <- add_column(Self_Emp_merged_activities, SEmp_13_14 = NA, .after = 3)


# Re-code the values from numeric to descriptive values as integration requires same data types
HESA_merged_activities <- as.data.frame(lapply(HESA_merged_activities, as.character))
Benefit_merged_activities <- as.data.frame(lapply(Benefit_merged_activities, as.character))
Employment_merged_activities <- as.data.frame(lapply(Employment_merged_activities, as.character))
Self_Emp_merged_activities <- as.data.frame(lapply(Self_Emp_merged_activities, as.character))

HESA_merged_activities[HESA_merged_activities == 9] <- "Further Education"
# We label HESA 'Undergraduate' as Higher Education since it is not limited to first degrees, includes 
# other HE level qualifications. This will 'merge' with the NCCIS Higher Education state
HESA_merged_activities[HESA_merged_activities == 8] <- "Higher Education"
HESA_merged_activities[HESA_merged_activities == 7] <- "Postgraduate"

Benefit_merged_activities[Benefit_merged_activities == 3] <- "Out of Work Benefits"
Employment_merged_activities[Employment_merged_activities == 2] <- "Employment"
Self_Emp_merged_activities[Self_Emp_merged_activities == 1 ] <- "Self Employment"
```



```{r message=FALSE}
# Visualise missingness for each of the activity datasets - to make integration more intuitive 
# NOTE that large dataset size means that some cases may not be represented/over plotting
# Visualisations are to give a general idea of the observed activities data

#NCCIS
NCCIS_miss <- NCCIS_merged_activities_Grouped %>%
  subset(select = -c(ID_PMR)) %>%
  vis_miss(large_data_size = 4500000, show_perc_col = FALSE) + 
  scale_fill_grey() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_text(size=6),
        legend.position = "none",
        plot.title = element_text(hjust=0.5)) +
  ylab("NCCIS")  +
  ggtitle("Observed Data in 2010/11 English School-leaver\nCohort Pre-processed Activities")

# HESA
HESA_miss <- HESA_merged_activities %>%
  subset(select = -c(ID_PMR)) %>%
  vis_miss(large_data_size = 4500000, show_perc_col = FALSE) + 
  scale_fill_grey() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_text(size=6),
        legend.position = "none") +
  ylab("HESA")

# Benefits
Benefit_miss <- Benefit_merged_activities %>%
  subset(select = -c(ID_PMR)) %>%
  vis_miss(large_data_size = 4500000, show_perc_col = FALSE) + 
  scale_fill_grey() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_text(size=6),
        legend.position = "none") +
  ylab("DWP Out of Work Benefits")

# Employment
Emp_miss <- Employment_merged_activities %>%
  subset(select = -c(ID_PMR)) %>%
  vis_miss(large_data_size = 4500000, show_perc_col = FALSE) + 
  scale_fill_grey() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_text(size=6),
        legend.position = "none") +
  ylab("DWP Employment")

# Self Employment
SEmp_miss <- Self_Emp_merged_activities %>%
  subset(select = -c(ID_PMR)) %>%
  vis_miss(large_data_size = 4500000, show_perc_col = FALSE, show_perc = FALSE) + 
  scale_fill_grey(labels = c("Observed", "Unobserved")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_text(size=6),
        legend.position = "bottom", legend.title = element_blank()) +
  scale_x_discrete(position = "bottom", 
                   labels = c("2011/12", "2012/13", "2012/14", "2014/15", "2015/16",
                              "2016/17","2017/18", "2018/19")) + 
  ylab("DWP Self Employment") + 
  xlab("Academic/Tax Year")
```

```{r fig.align="center", echo=FALSE, fig.height=14}
# Combine plots into 1 figure 
#jpeg(filename = "Code Outputs/Figures/English_2010_11_cohort_activities_observed.jpeg", width = 2000, height = 2500,
 #    res = 300)

grid.newpage()

grid.draw(rbind(ggplotGrob(NCCIS_miss),
                ggplotGrob(HESA_miss),
                ggplotGrob(Benefit_miss),
                ggplotGrob(Emp_miss),
                ggplotGrob(SEmp_miss), size = "last"))
dev.off()
```

```{r}
# Impose hierarchy of NCCIS <- HESA <- Benefits <- Emp <- SEmp
# Effectively overlaying dataframes to create an integrated version of activities

# Get the column names from NCCIS dataframe
NCCIS_col <- colnames(NCCIS_merged_activities_Grouped)

# Fill NA values in NCCIS with values from HESA, based on column order
integrated_activities <- NCCIS_merged_activities_Grouped %>%
  mutate(across(everything(), ~ coalesce(., HESA_merged_activities[[which(NCCIS_col == cur_column())]])))

# Fill remaining NA values with values from Benefits, based on column order
integrated_activities_col <- colnames(integrated_activities)

integrated_activities <- integrated_activities %>%
  mutate(across(everything(), ~ coalesce(., Benefit_merged_activities[[which(integrated_activities_col == cur_column())]])))

# Fill remaining NA values with values from Employment, based on column order
integrated_activities <- integrated_activities %>%
  mutate(across(everything(), ~ coalesce(., Employment_merged_activities[[which(integrated_activities_col == cur_column())]])))

# Fill remaining NA values with values from Self-employment, based on column order
integrated_activities <- integrated_activities %>%
  mutate(across(everything(), ~ coalesce(., Self_Emp_merged_activities[[which(integrated_activities_col == cur_column())]])))


# Rename columns
integrated_activities <- integrated_activities %>% 
  rename("State_11_12" = "NCCIS_11_12",
         "State_12_13" = "NCCIS_12_13",
         "State_13_14" = "NCCIS_13_14",
         "State_14_15" = "NCCIS_14_15",
         "State_15_16" = "NCCIS_15_16",
         "State_16_17" = "NCCIS_16_17",
         "State_17_18" = "NCCIS_17_18",
         "State_18_19" = "NCCIS_18_19")


# Drop any with no activities for all years
integrated_activities <- integrated_activities %>%
 filter_at(vars(State_11_12, State_12_13, State_13_14, State_14_15,
                State_15_16, State_16_17,State_17_18, State_18_19),
                any_vars(complete.cases(.)))
```

```{r message=FALSE}
# Visualise missingness in the integrated activities - Full 2010/11 school-leaver cohort in England

#jpeg(filename = "English_2010_11_cohort_INTEGRATED_activities_observed.jpeg",
 #    width = 2000, height = 1500,
  #   res = 300)

integrated_activities %>%
  subset(select = -c(ID_PMR)) %>%
  vis_miss(large_data_size = 4500000, show_perc_col = FALSE) + 
  scale_fill_grey(labels = c("Observed (92.2%)", "Unobserved (7.8%)")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(hjust=0.5)) +
  scale_x_discrete(position = "bottom", 
                   labels = c("2011/12", "2012/13", "2012/14", "2014/15", "2015/16",
                              "2016/17","2017/18", "2018/19")) + 
  ylab(" ") + 
  xlab("Academic Year") +
  ggtitle("Observed Data in 2010/11 English School-leaver Cohort\nIntegrated Activities")

dev.off()
```

```{r}
# Add geography at school-leaving age to the merged activity data -
# in order to subset activities per Combined Authority
# Select the ID_PMR and MCA Names columns from ind_char
ind_char_MCA <- ind_char %>% 
  select(ID_PMR, MCA_Name)

# Left join the MCA var from ind_char to the activity states using ID_PMR
 integrated_activities_MCA <- merge(x=integrated_activities,
                                          y=ind_char_MCA,
                                          by = "ID_PMR",
                                          all.x = TRUE)
```

```{r}
# Export the integrated activities with MCA name as .csv
write.csv(integrated_activities_MCA,
          "Code Outputs/Compartmentalised/2010_11_English_Cohort_Integrated_activities.csv",row.names=FALSE)
```

```{r}
# Percentage of the various states possible in the whole English cohort

# Retain only the columns with states
integrated_activities_MCA <- integrated_activities_MCA %>%
  select(-c("ID_PMR", "MCA_Name"))

# Where NA, fill with missing - this is only to view the % of missing in this summary
integrated_activities_MCA[is.na(integrated_activities_MCA)] <- "Missing"

# Frequency of States
activity_freq <- activity_freq %>%
  arrange(desc(Freq))

# Add a percentage column, rounded to 2 dp
activity_freq <- activity_freq %>%
  mutate(Percentage = round(Freq/sum(Freq)*100,1)) %>%
  # Rename column
  rename("Activity State" = "Var1")
```



# Geographic States (2011/12 - 2018/19 tax year)

```{r message=FALSE}
# Read in the Geography states data 2011/12 - 2018/19 tax year
GEOG_2011_12 <- read_csv("../SQL/Geography_States/2011_12_GEOG.csv") 
GEOG_2012_13 <- read_csv("../SQL/Geography_States/2012_13_GEOG.csv") 
GEOG_2013_14 <- read_csv("../SQL/Geography_States/2013_14_GEOG.csv") 
GEOG_2014_15 <- read_csv("../SQL/Geography_States/2014_15_GEOG.csv") 
GEOG_2015_16 <- read_csv("../SQL/Geography_States/2015_16_GEOG.csv") 
GEOG_2016_17 <- read_csv("../SQL/Geography_States/2016_17_GEOG.csv") 
GEOG_2017_18 <- read_csv("../SQL/Geography_States/2017_18_GEOG.csv") 
GEOG_2018_19 <- read_csv("../SQL/Geography_States/2018_19_GEOG.csv") 

# 2011 LAD to MCA created in excel
LAD_to_MCA <- read_excel("Preprocessing/Lookups/LAD_to_MCA.xlsx")

# Read in preprocessed ind_char 
ind_char <- read_csv("Code Outputs/Compartmentalised/ind_char.csv")
```
```{r}
# Check ID is unique in all - All are unique
#any(duplicated(GEOG_2018_19$pupilmatchingrefanonymous_spr11))


# Left join Combined Authority lookup to all the dataframes
A <- merge(x=GEOG_2011_12, y=LAD_to_MCA,
           by.x = "LAUANM", by.y = "LAD_school_leaving", all.x = TRUE)
B <- merge(x=GEOG_2012_13, y=LAD_to_MCA,
           by.x = "LAUANM", by.y = "LAD_school_leaving", all.x = TRUE)
C <- merge(x=GEOG_2013_14, y=LAD_to_MCA,
           by.x = "LAUANM", by.y = "LAD_school_leaving", all.x = TRUE)
D <- merge(x=GEOG_2014_15, y=LAD_to_MCA,
           by.x = "LAUANM", by.y = "LAD_school_leaving", all.x = TRUE)
E <- merge(x=GEOG_2015_16, y=LAD_to_MCA,
           by.x = "LAUANM", by.y = "LAD_school_leaving", all.x = TRUE)
F <- merge(x=GEOG_2016_17, y=LAD_to_MCA,
           by.x = "LAUANM", by.y = "LAD_school_leaving", all.x = TRUE)
G <- merge(x=GEOG_2017_18, y=LAD_to_MCA,
           by.x = "LAUANM", by.y = "LAD_school_leaving", all.x = TRUE)
H <- merge(x=GEOG_2018_19, y=LAD_to_MCA,
           by.x = "LAUANM", by.y = "LAD_school_leaving", all.x = TRUE)
```

```{r Geography states}
# Create a new variable for all geog tax years with MCA name,
# otherwise use the Governemnt Office Region name

# GEOG_2011_12
# Create new variable for the Geog state that will be used
# If LAD is part of an MCA, populate with the MCA Name
# For non MCA LAD's, populate with the GOR name. 
GEOG_2011_12 <-
  A %>%
  mutate(A_Geog = MCA_Name) %>%
  mutate(A_Geog = coalesce(A_Geog, GORNM))

# Remove redundant columns
GEOG_2011_12 <- GEOG_2011_12 %>% 
  # Rename the ID column
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11") %>%
  # select only the ID and the final geog state
  select(ID_PMR, A_Geog)

#-----
# GEOG_2012_13
GEOG_2012_13 <-
  B %>%
  mutate(B_Geog = MCA_Name) %>%
  mutate(B_Geog = coalesce(B_Geog, GORNM))

# Remove redundant columns
GEOG_2012_13 <- GEOG_2012_13 %>% 
  # Rename the ID column
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11") %>%
  # select only the ID and the final geog state
  select(ID_PMR, B_Geog)

#-----
# GEOG_2013_14
GEOG_2013_14 <-
  C %>%
  mutate(C_Geog = MCA_Name) %>%
  mutate(C_Geog = coalesce(C_Geog, GORNM))

# Remove redundant columns
GEOG_2013_14 <- GEOG_2013_14 %>% 
  # Rename the ID column
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11") %>%
  # select only the ID and the final geog state
  select(ID_PMR, C_Geog)

#-----
# GEOG_2014_15
GEOG_2014_15 <-
  D %>%
  mutate(D_Geog = MCA_Name) %>%
  mutate(D_Geog = coalesce(D_Geog, GORNM))

# Remove redundant columns
GEOG_2014_15 <- GEOG_2014_15 %>% 
  # Rename the ID column
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11") %>%
  # select only the ID and the final geog state
  select(ID_PMR, D_Geog)

#-----
# GEOG_2015_16
GEOG_2015_16 <-
  E %>%
  mutate(E_Geog = MCA_Name) %>%
  mutate(E_Geog = coalesce(E_Geog, GORNM))

# Remove redundant columns
GEOG_2015_16 <- GEOG_2015_16 %>% 
  # Rename the ID column
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11") %>%
  # select only the ID and the final geog state
  select(ID_PMR, E_Geog)

#-----
# GEOG_2016_17
GEOG_2016_17 <-
  F %>%
  mutate(F_Geog = MCA_Name) %>%
  mutate(F_Geog = coalesce(F_Geog, GORNM))

# Remove redundant columns
GEOG_2016_17 <- GEOG_2016_17 %>% 
  # Rename the ID column
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11") %>%
  # select only the ID and the final geog state
  select(ID_PMR, F_Geog)

#-----
# GEOG_2017_18
GEOG_2017_18 <- 
  G %>%
  mutate(G_Geog = MCA_Name) %>%
  mutate(G_Geog = coalesce(G_Geog, GORNM))

# Remove redundant columns
GEOG_2017_18 <- GEOG_2017_18 %>% 
  # Rename the ID column
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11") %>%
  # select only the ID and the final geog state
  select(ID_PMR, G_Geog)

#-----
# GEOG_2018_19
GEOG_2018_19 <- 
  H %>%
  mutate(H_Geog = MCA_Name) %>%
  mutate(H_Geog = coalesce(H_Geog, GORNM))
  
# Remove redundant columns
GEOG_2018_19 <- GEOG_2018_19 %>% 
  # Rename the ID column
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11") %>%
  # select only the ID and the final geog state
  select(ID_PMR, H_Geog)

```

```{r}
# Full join all dataframes to retain every individual no matter what year they appear
# Create list of all to merge
Geog_list <- list(GEOG_2011_12, GEOG_2012_13, GEOG_2013_14, GEOG_2014_15,
                     GEOG_2015_16,GEOG_2016_17, GEOG_2017_18, GEOG_2018_19)
Geog_merged <- purrr::reduce(Geog_list, dplyr::full_join, by = 'ID_PMR')

#any(duplicated(Geog_merged$ID_PMR)) # IDs all unique

# Left join to preprocessed ind_char ID to retain only those in selected cohort
# Select ID from ind char used to left join earnings in 2011/12 tax year
cohort_ID <- ind_char %>%
  select(ID_PMR)
Geog_merged <- merge(x=cohort_ID, y=Geog_merged,
           by = "ID_PMR", all.x = TRUE)

```

```{r}
# Export the merged geography states as .csv
write.csv(Geog_merged,
          "Code Outputs/Compartmentalised/2010_11_English_Cohort_Geog_merged.csv",row.names=FALSE)
```


# Employment Earnings (2011/12 - 2018/19 tax year)

```{r message=FALSE}
# Read in the Employment earnings data 2011/12 - 2018/19 tax year
# This is only employment earnings - not self employment 
EmpEarn_2011_12 <- read_csv("../SQL/Employment_Earnings/2011_12_Emp_Earnings.csv") 
EmpEarn_2012_13 <- read_csv("../SQL/Employment_Earnings/2012_13_Emp_Earnings.csv") 
EmpEarn_2013_14 <- read_csv("../SQL/Employment_Earnings/2013_14_Emp_Earnings.csv") 
EmpEarn_2014_15 <- read_csv("../SQL/Employment_Earnings/2014_15_Emp_Earnings.csv") 
EmpEarn_2015_16 <- read_csv("../SQL/Employment_Earnings/2015_16_Emp_Earnings.csv") 
EmpEarn_2016_17 <- read_csv("../SQL/Employment_Earnings/2016_17_Emp_Earnings.csv") 
EmpEarn_2017_18 <- read_csv("../SQL/Employment_Earnings/2017_18_Emp_Earnings.csv") 
EmpEarn_2018_19 <- read_csv("../SQL/Employment_Earnings/2018_19_Emp_Earnings.csv") 

# Read in preprocessed ind_char 
ind_char <- read_csv("Code Outputs/Compartmentalised/ind_char.csv")
```
```{r}
 # Rename columns 
EmpEarn_2011_12 <- EmpEarn_2011_12 %>% 
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
         "EmpEarn_2011_12" = "earnings")
EmpEarn_2012_13 <- EmpEarn_2012_13 %>% 
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
         "EmpEarn_2012_13" = "earnings")
EmpEarn_2013_14 <- EmpEarn_2013_14 %>% 
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
         "EmpEarn_2013_14" = "earnings")
EmpEarn_2014_15 <- EmpEarn_2014_15 %>% 
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
         "EmpEarn_2014_15" = "earnings")
EmpEarn_2015_16 <- EmpEarn_2015_16 %>% 
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
         "EmpEarn_2015_16" = "earnings")
EmpEarn_2016_17 <- EmpEarn_2016_17 %>% 
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
         "EmpEarn_2016_17" = "earnings")
EmpEarn_2017_18 <- EmpEarn_2017_18 %>% 
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
         "EmpEarn_2017_18" = "earnings")
EmpEarn_2018_19 <- EmpEarn_2018_19 %>% 
  rename("ID_PMR" = "pupilmatchingrefanonymous_spr11",
         "EmpEarn_2018_19" = "earnings")


# Full join all dataframes to retain every individual no matter what year they appear
# Create list of all to merge
EmpEarn_list <- list(EmpEarn_2011_12, EmpEarn_2012_13, EmpEarn_2013_14, EmpEarn_2014_15,
                     EmpEarn_2015_16,EmpEarn_2016_17, EmpEarn_2017_18, EmpEarn_2018_19)
EmpEarn_merged <- purrr::reduce(EmpEarn_list, dplyr::full_join, by = 'ID_PMR')

#any(duplicated(EmpEarn_merged$ID_PMR)) - IDs all unique

# Left join to preprocessed ind_char ID to retain only those in selected cohort
# Select ID from ind char used to left join earnings in 2011/12 tax year
cohort_ID <- ind_char %>%
  select(ID_PMR)
EmpEarn_merged <- merge(x=cohort_ID, y=EmpEarn_merged,
           by = "ID_PMR", all.x = TRUE)

```


```{r}
# Export the merged employment earnings as .csv
write.csv(EmpEarn_merged,
          "Code Outputs/Compartmentalised/2010_11_English_Cohort_Employment_Earnings.csv",row.names=FALSE)
```


_MIT License Copyright (c) 2024 Shivani Sickotra_
