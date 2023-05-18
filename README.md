# CASC_fish_growth
    This readme.txt file was generated on 17 May 2023 by Mike Verhoeven

Recommended citation for the data: 
    <insert citation>

-------------------
GENERAL INFORMATION
-------------------


1. Title of Dataset 
    Collated length-at-age data for the Midwestern US

2. Author Information


  Principal Investigator Contact Information
    Name: Mike Verhoeven
    Institution: UNiversity of Minnesota-Twin Cities
    Address: 135 Skok Hall; 2003 Upper Bufor Circle; St Paul, MN 55108
    Email: michael.verhoeven.mrv@gmail.com
	  ORCID: 0000-0002-6340-9490

  Associate or Co-investigator Contact Information
    Name: Holly Kundel
    Institution: 
    Address: 
    Email: 
	  ORCID: 

  Associate or Co-investigator Contact Information
    Name: Gretchen Hansen
    Institution: 
    Address: 
    Email: 
	  ORCID: 


3. Date published or finalized for release:
    NOT FINALIZED

4. Date of data collection (single date, range, approximate date) <suggested format YYYYMMDD>

5. Geographic location of data collection (where was data collected?):  

6. Information about funding sources that supported the collection of the data: 

7. Overview of the data (abstract): 


--------------------------
SHARING/ACCESS INFORMATION
-------------------------- 


1. Licenses/restrictions placed on the data:

2. Links to publications that cite or use the data:

3. Was data derived from another source?
           If yes, list source(s):

4. Terms of Use: Data Repository for the U of Minnesota (DRUM) By using these files, users agree to the Terms of Use. https://conservancy.umn.edu/pages/drum/policies/#terms-of-use




---------------------
DATA & FILE OVERVIEW
---------------------


1. File List
   A. Filename: length_age_merged.RData       
      Short description:  length and age observations for individual fish, including many covariates       

   B. Filename:        
      Short description:        
        
   C. Filename:        
      Short description:


2. Relationship between files:        



--------------------------
METHODOLOGICAL INFORMATION
--------------------------

1. Description of methods used for collection/generation of data: 
    observational data collated from state agencies across the region

2. Methods for processing the data: <describe how the submitted data were generated from the raw or collected data>
    see scripts in this repo

3. Instrument- or software-specific information needed to interpret the data:


4. Standards and calibration information, if appropriate:


5. Environmental/experimental conditions:


6. Describe any quality-assurance procedures performed on the data:


7. People involved with sample collection, processing, analysis and/or submission:



-----------------------------------------
DATA-SPECIFIC INFORMATION FOR: laa or length_age_merged.RData (each row in this dataset represents a fish for which an age and/or length observation were made)
-----------------------------------------

1. Number of variables:
    129


2. Number of cases/rows: 
    2,164,849



3. Missing data codes:
        Code/symbol         Definition
        NA                  Data were not available from datasource for this cell


4. Variable List
                  
    _. Name: <variable name>
       Description: <description of the variable>
                    Value labels if appropriate
    
    A. Name: state
       Description: State in which the data contributing agency is located (usually, but not always the same as the state in which the observation was made) 

    B. Name: county
       Description: county in which the waterbody the fish was collected at is located
    
    C. Name: lake_name
       Description: name of waterbody from which the fish was collected
       
    D. Name: lake_id
       Description: identification code for the waterbody provided by the contributing agency (most uses would be better served by nhdhr_id values, which are being added now[17May2023])
       
    E. Name: secondary_lake_id
       Description: an alternative identification code for the waterbody provided by the contributing agency (most uses would be better served by nhdhr_id values, which are being added now[17May2023])
       
    F. Name: nhdhr_id
       Description: COMING SOON national hydrography plud high definition identifier of waterbody from which the fish was collected (https://www.usgs.gov/national-hydrography/nhdplus-high-resolution)

    G. Name: site_id.1
       Description: identifying information for a site within a waterbody where a fish was collected (if provided) 

    H. Name: latitude
       Description: lattitude (decimal degrees, significant figures not consistent) of (these are somewhat ambiguous and not consistent across all data or even within a state's contribution): the waterbody where the fish was collected (sometimes a geospatial centroid), the sub-waterbody from which a fish was collected, or the actual point location where the fish was collected.
       
    I. Name: longitude
       Description: longitude (decimal degrees, significant figures not consistent) of (these are somewhat ambiguous and not consistent across all data or even within a state's contribution): the waterbody where the fish was collected (sometimes a geospatial centroid), the sub-waterbody from which a fish was collected, or the actual point location where the fish was collected.
       
    J. Name: year
       Description: year in which a fish was collected, as provided by the data contributor
       
    K. Name: date_clean
       Description: date of fish collection from waterbody. This date has been cleaned by the authors of the dataset and is generated from all date data provided by data contributors. More information on the decisions made for date assignments can be found in the "c_collate_by_state.R" script.
       
    L. Name: survey_id
       Description: an identifier sometimes provided by data contributors as a unique key for each survey they contributed data from (use of this variable will require careful assessment of whether this is indeed a unique survey key within each state)

    M. Name: species.1
       Description: common species name of the individual fish in this row. This name was assigned by reviewing common names and species codes from data contributors. More information on the decisions made for species assignments can be found in the "c_collate_by_state.R" script.
       
    N. Name: age
       Description: age in years assigned to this individual fish observation
       
    O. Name: length.1
       Description: numeric value of length of this individual fish observation (we presume these are total lengths, but cannot exclude the possibility of fork lengths or maximum standard lengths being reported). This length was assigned by reviewing all length data (sometimes length.2, length.3) and choosing a metric system measurement if provided. We then converted to millimeters. Where no units were reported for a dataset, we used distributions of lengths in that dataset to infer units. More information on the decisions made for length assignments can be found in the "c_collate_by_state.R" script.   

    P. Name: length_unit.1
       Description: units used for length.1 field. Because of our standardization of length.1, these should all be millimeters. 

    Q. Name: REMAINING COLUMN DESCRIPTIONS WILL BE UPDATED SOON 
       Description: All remaininn columns exist in the formats in which they were submitted. We have renamed the fields using the crosswalk/key here: https://docs.google.com/spreadsheets/d/1iRLN0IYwlGuhAR46Tbj_g1Ps8g2g_0c5/edit?usp=sharing&ouid=103884323050402170010&rtpof=true&sd=true

                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
-----------------------------------------
DATA-SPECIFIC INFORMATION FOR: [FILENAME]
-----------------------------------------
<create sections for each dataset included>


1. Number of variables:


2. Number of cases/rows: 




3. Missing data codes:
        Code/symbol        Definition
        Code/symbol        Definition


4. Variable List
                  

    A. Name: <variable name>
       Description: <description of the variable>
                    Value labels if appropriate


    B. Name: <variable name>
       Description: <description of the variable>
                    Value labels if appropriate
