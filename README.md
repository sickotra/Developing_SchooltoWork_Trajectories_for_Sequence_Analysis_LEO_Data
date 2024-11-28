# Developing School-to-work Trajectories for Sequence Analysis Using the Longitudinal Education Outcomes (LEO) Data

This code utilises the Department for Education **Longitudinal Educations Outcomes (LEO)** data to create a bespoke dataset of longitudinal school-to-work activity histories specifically designed for input into **Sequence Analysis** alogrothims.

---------------------------------------------------------------------------------
## Developed Data Summary

The LEO data used from the Standard Extract Iteration 1 included:

* **UK Department for Education** - National Pupil Database (NPD), Higher Education Statistics Agency (HESA)
* **UK Department for Work and Pensions** - Employment, Self-Employment, Benefits and Geography data

This code used the Spring School Census from the NPD, the National Client Caseload Information System from the NPD, HESA, DWP Employment, Self-employment and Benefits data to create a longitudinal record of the yearly activity histories for **556,182 individuals from the 2010/11 school-leaver cohort in England**. The activity histories begin from the first non-compulsory observed state in 2011/12 until the 2018/19 academic year, corresponding to ages 16/17 to ages 23/24. Individual-level socio-demographic charactersitics are also prepared, as well as exploratory longitudinal geographic and longitudinal earnings data.

The code first imports data extracted from the LEO database using SQL in Section 1. Then these are preprocessed separately into the required format for sequence analysis in Section 2. Once all activities have been prepared, they are integrated into one dataset containing all activity histories in Section 3. The Combined Authority that an individual was residing in at school-leaving age is linked to their corresponding activity history. This can be used to subset individuals by Combined Authorities for sequence analysis.

The data was developed as part of my **Data Analytics and Society PhD research** [https://gtr.ukri.org/projects?ref=studentship-2433665](https://gtr.ukri.org/projects?ref=studentship-2433665). The code is shared to facilitate accessibility of the sequence analysis technique by detailing how to create suitable input data, demonstrate the capability of the LEO data and promote reproducibility. 


-------------------------------------------------------------------
## LEO Data Security and Access

Data access was obtained through the **Office for National Statistics Secure Research Service (ONS SRS)** and this code was ran within this environment. 

All code files within this repository have been granted ONS SRS security clearance to allow sharing. 

The R code was permitted clearance as an `.r` file, which has then been saved as an RMarkdown `.rmd` file outside of the SRS. The code will not run without getting access to the LEO data. Once access to LEO has been obtained, the user will need to ingest all code files and lookups into the SRS as part of their approved project. They can then convert the `.r` file to an `.rmd` RMarkdown file to run in RStudio within the SRS.   
The RMarkdown setup code chunk has `eval = false` and so will not be evaluated/run, this must be changed to `eval=true` if the user has access to the LEO data and is attempting to run the code within the SRS environment.

**LEO Data Access:**
The instuctions to apply to access the Department for Education LEO data can be found [here](https://www.gov.uk/guidance/apply-to-access-the-longitudinal-education-outcomes-leo-dataset).

**Data Citation:**
Department for Education; HM Revenue and Customs; Department for Work and Pensions; Higher Education Statistics Agency, released 29 September 2022, ONS SRS Metadata Catalogue, dataset, Longitudinal Education Outcomes SRS Iteration 1 Standard Extract - England, [https://doi.org/10.57906/pzfv-d195](https://doi.org/10.57906/pzfv-d195). 

*Note:* The data development work was conducted between Jan 2023 to Aug 2023 using the LEO Standard Extract Iteration 1. There is now an Iteration 2 of the data which was released in Nov 2023, hence new data access applications will be for Iteration 2. Some results produced by Iteration 2 data may not exactly match those from Iteration 1. Some code may need to be adapted to be compatible with updated versions of the LEO data. 

----------------------------------------------------------------------


## Structure of folders in this repository:

1) **SQL Queries** - This includes the inital SQL queries used to extract the relevant LEO data for the activity states, individual charactersitics, geography states and employment earnings. 
2) **LEO Sequence Analysis Data Development** - This includes the R code used to create the bespoke sequence analysis dataset
3) **Lookups and Other Data** - This includes the publically available geographic lookups and additional data used in the R code 



