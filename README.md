**Study Description**

Housing quality reflects housing adequacy and affordability constraints,
and access to quality housing is stratified by race, immigrant status,
and other social locations. There is a hidden housing risk at the
intersection of disadvantaged social locations.

This study examines quality housing at the intersection of social
locations. We look at:

1)  How do (1) immigrant status & visible minority status, (2) marital
    status & parental status, and (3) gender & marital status affect the
    probability of living in high quality dwelling.

Using the dataset from Canadian Census Health and Environment Cohort
provided to CAnD3 by Statistics Canada, our analysis finds non-white
natives, single people with children, and single men are the most likely
to be placed in a dwelling in need of repair.

You can find the related information on how to use the R and Stata codes
to reproduce our analyses below.

**Data Description**

**Data Source Availability Statement**

The Canadian Census Health and Environment Cohort is a synthetic dataset
replicating linked Census -- Mortality records in Canada, produced for
CAnD3 by Statistics Canada. We conducted our analysis in a 10% random
sample provided by CAnD3.

As part of McGill University, the CAND3 initiative has a license to use
the data for the purposes of training. Those outside of McGill
university should not use the data provided through CAND3\'s training
activities for purposes not related to their CAND3 training. Trainees
who belong to another DLI institution should re-download the data using
the ODESI site using the login provided by their institution if they
wish to make use of the data for other purposes.

**Instructions for Data Preparation and Analysis**

Stata do file and R files includes all syntax for data operations and
regression models. As we used the data format .dta and translated code
from R to Stata, we used the haven package to make R read a stata file
format.

**Computational Requirements**

The following packages are required to reproduce our analyses:

| Software | Packages |
|---|---|
| RStudio 2025.05.1+513 | "haven" "dplyr" "ggplot2" "margins" "prediction" "scales" |
