---
title: "Study plan"
output:
#  word_document: default
 # pdf_document: default
bibliography: citations.bib
csl: bmcemerg.csl
link-citations: yes

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Introduction

Defined as physical injury and the body´s consequential response, trauma, is one of the leading causes of mortality and morbidity in all age groups and the leading cause of mortality in people below the age of 44 [@David2021],[@champion1990]. Approximately 5.8 million people die each year due to trauma. Motor vehicle crashes alone stand for more than one million deaths and roughly between 20 and 50 million injuries annually [@Committee2013]. With an average of more than 7 days in the hospital each stay [@champion1990], it is one of the most prevalent reasons for admission. In 2020 road traffic accidents were third in disease burden worldwide, measured by Disability-Adjusted Life Years (DALY),  a term used to describe the impact of health problems and to measure the significance of improvement in medical care [@Murray1996],[@Haagsma2015].

In many cases the outcome is mostly dependent on the quality of care acquired [@Dogrul2020]. In a teaching hospital in Tehran, reviews of all trauma cases in 1 year displayed inappropriate care in 45% of all deaths and implied that approximately 26% of all trauma deaths were preventable [@Zafarghandi2003].Other studies have estimated preventable and potentially preventable trauma death rates up to 60% [@Konadu2020]. Preventable death panels aim to decrease the number of preventable deaths by investigating common factors between the cases [@Jung2019]. Airway management, inadequate chest compression, inadequate blood or fluid supply are some factors that previously were found in need of improvement in trauma care [@Zafarghandi2003],[@Maio1996]. Such advances in trauma care are the leading cause of the decreased number of preventable deaths according to the American College of Surgeons Committee [@Committee2013].

It has for a long period of time been of great importance to investigate factors that can be improved in trauma care and different methods are used to do so. The golden standard being mortality and morbidity (M&M) conferences. Some cases get selected for review to further investigate the reason behind the mortality or morbidity [@WHO2009]. The results are used to improve the trauma care and to decrease the number of preventable errors. Although these conferences are a vital part for advancement in trauma care, it remains a process that requires a great amount of resources and is still complicated to this day.

Despite the evidence supporting the use of predefined models for case selection, there is limited data on specific factors associated with opportunities for improvement in trauma care [@Slater2020]. Therefore, methods such as audit filters are adopted in trauma quality improvement programs. Audit filters are predefined factors used in the selection of cases for review and represent an unfavourable alternation, proclaimed leading to a disadvantageous outcome [@WHO2009],[@Evans2009]. Set audit filters are for instance systolic blood pressure under 90, Glasgow Coma Scale less than 9 and not intubated, time to acute intervention more than 60 minutes and a few more. A systematic review of audit filters in 2009 found no studies meeting set criteria determining the effectiveness of set audit filters [@Evans2009].

To this day trauma quality improvement programs rely on set filters for the selection of cases for M&M conferences. Some studies have found no major opportunities for improvement in currently used audit filters and believe further advancements are essential [@Cryer1996],[@Copes1995]. We hypothesize that the selection of cases for multidisciplinary mortality and morbidity reviews can be refined by the usage of other factors. We aim therefore to find factors associated with opportunities for improvement in trauma care for later development of models trained to identify cases with capacity for improvement. This can be investigated by the examination of many other factors registered in our database.

# Methods

### Study design

We conducted a retrospective cohort study using data from the Karolinska University Hospital trauma registry and the trauma care quality database. The trauma care quality database consists of the cases selected for review and the presence of opportunity for improvement is noted. The registries where linked and possible factors associated with opportunities for improvement were extracted. All statistical analysis were firstly done on randomized data to insure objectivity. Bivariate and multivariable logistic regression was used to determine significant correlation with the presence of opportunity for improvement. 

### Setting

In Sweden, pre-hospital care is managed by paramedics and specially trained physicians. The trauma patient is triaged by EMS personnel at the scene. These patients are divided into three different types of priorities depending on the injury and vital signs, for instance open head injuries are priority one. All trauma patients in Stockholm with priority one or two are transported to Karolinska University Hospital to receive care by dedicated trauma teams with sufficient competence. These teams consist of a trauma surgeon, an anaesthetist, an orthopaedic surgeon, a radiologist and specialized nurses. Karolinska University Hospital in Solna is a level one trauma center, with direct access to radiology, operation, intensive care and interventions [@Social2015],[@NKS2020].

Cases gets selected for M&M conferences by specialized nurses mainly based on previously mentioned audit filters and reviewed by experienced specialists from all of the fields involved in the trauma team. Preventable errors in the care of the patient are identified in each case and registered to the database as a categorical variable. The absence of such errors is also registered. A plan to solve the problems identified is also presented by the board.

### Participants

All trauma patients recorded in the Karolinska care quality registry between 2014 and 2021 are included. The trauma registry includes all patients admitted with trauma team activation, regardless of injury severity score, as well as patients admitted without trauma team activation but found to have an injury severity score of more than 9. Selection for the Karolinska care quality registry and the associated multidisciplinary mortality and morbidity review is done using following audit filters: systolic blood pressure less than 90, Glasgow coma scale less than 9 and not intubated, injury severity score more than 15 but not admitted to the intensive care unit, time to acute intervention more than 60 minutes, time to computed tomography more than 30 minutes, and death within 30 days after trauma. We did a complete case analysis, hence missing data in any of the covariates or outcome resulted in exclusion. 

### Variables

**Study outcome**

The study outcome is significantly associated with the outcome variable using any or all predictive variables. The outcome variable is the presence of opportunities for improvement, as labelled by the mortality and morbidity review board, and defined as a binary variable with the levels "Yes - At least one opportunity for improvement identified" and "No - No opportunities for improvement identified". Data on this outcome will be extracted from the trauma care quality database. 

**Predictors**

Since our aim is to identify factors associated with improvement in trauma care, the database used will therefore have some factors that can be further examined. The information registered in the database is divided into sections.[@Swetrau2020] Selected factors to be further examined are divided into categorical and continuous variables. The categorical variables consist of gender, survival after 30 days, highest hospital care level, Glasgow Coma Scale (GCS), respiratory rate, systolic blood pressure and intubation of the patient. The continuous variables consist of age, Injury Severity Score (ISS), time from arrival at the hospital until first CT and time from arrival at the hospital until first intervention.

### Data sources and measurements

All of the data is extracted from the Karolinska University Hospital trauma registry and the trauma care quality database. Age and gender are collected from the patient´s personal number. Vital signs are measured on arrival to the emergency department by staff and the other variables are registered from the patient´s charts.

### Bias

Simulated data will be used to develop the analysis model and after satisfactory results implement it on the data collected from the databases. A selection biased, caused by the inclusion criteria for the Karolinska care quality registry and the trauma registry, is possible.

### Study size

We will include all patients from the Karolinska University Hospital trauma care quality registry and the corresponding information from the Karolinska University Hospital trauma registry. Registration took place between 2014 and 2021 with approximately 21000

### Quantitative variables

Such variables include age, blood pressure, respiratory rate, ISS, GCS and time to CT and intervention as previously described. All registered to the databases.

### Statistical methods

We did a complete case analysis of all patients in the Karolinska trauma care quality registry. Variables were handled and converted in accordance with the SweTrau Manual. Potential variables were selected based on currently used audit filters, expert consensus and common demographics. Due to GCS and respiratory rate not being registered in patients intubated before arrival at the emergency department we determined that the most reasonable solution would be to use pre-hospital values registered by paramedics, to include these patients. Bivariate and multivariable logistic regression was used to determine significant correlation with the presence of opportunity for improvement. A p value of less than 0,05 was considered significant.

# References
