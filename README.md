# Patient Specific PROMIS Reference Values 🖥 

## Overview

- This repository contains all code for wrangling data and producing the [Shiny app](http://www.common-metrics.org/PROMIS_PF_and_PI_Reference_scores.php) companion for PROMIS PF, UE, PI reference data.
- personograph_package.R contains the adapted functions to produce the plots

This web application can be used to obtain patient-specific reference values for PROMIS Physical Functioning, Upper Extremity, and Pain Interference item banks in general populations from the USA, UK, and Germany. By entering information about your patient you can obtain 1) plots to discuss their Physical Functioning, Upper Extremities, and Pain interference test results and 2) obtain patient-specific reference values. These values can help identifying if the obtained test results are higher, similar, or lower compared to patient specific reference group i.e. with the same age, sex, and from the same country.

It is based on the publication [How Are Age, Gender, and Country Differences Associated With PROMIS Physical Function, Upper Extremity, and Pain Interference Scores?](https://journals.lww.com/clinorthop/fulltext/2024/02000/how_are_age,_gender,_and_country_differences.9.aspx ).


## How To Use This App
On the left/above, you can see 4 different sections, in which you can enter information on your patient.
To obtain patient-specific reference values, you need to enter:
1. Country, information on the country of your patient, currently only Germany, UK, and US are implemented.
2. Age, use the slider to select an age from 50 to 100 years.
3. Sex, indicate the sex of the patient.
4. PROMIS Measures, Optional/to obtain plots, you can input the T-Scores for PROMIS Physical Functioning, Upper Extremities, and Pain Interference.

## Plots
In the Tab Plots you can see so called people plots that indicates how many people would achieve a higher, a similar, or a lower score than your patient These plots are based on the T-Scores you entered for PROMIS Physical Functioning, Upper Extremities, or Pain Interference and the information you entered about your patient.

## Tables
In the Tab Tables you can see the reference tables for PROMIS Physical Functioning, Upper Extremities, or Pain Interference based on the information you entered about your patient.
