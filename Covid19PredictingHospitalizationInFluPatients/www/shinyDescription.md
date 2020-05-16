***Predicting Patients Requiring Hospitalization When Initially Presenting with Flu or Flu like symptoms (To be investigated for the use on Patients, with or suspected to have, Covid-19)***

This shiny application contains the results of the internal and external validations of the models developed to predict risk of hospitalisation within 30 days of intitial presentation to a healthcare professional with symptoms.

During manuscript development and the subsequent review period, these results are considered under embargo and should not be disclosed without explicit permission and consent from the authors.

---
### Development Status: Under Development

Below is the abstract of the manuscript that summarizes the findings:

**Importance** COVID-19 is causing high mortality worldwide. Developing models that can quantify the risk of negative outcomes in patients infected with COVID-19 using their medical history could help countries develop strategies to shield the most vulnerable from the virus.

**Objective** To develop and externally validate internationally simple personalized risk scores that quantify the risk of patients requiring hospitlization, requiring intensive care or dying due to COVID-19 infection.

**Design** Multinational, distributed network cohorts

**Setting** We used real world (electronic medical records and claims) data from 6 data sources and 4 countries (Netherlands, South Korea, Spain, and the US), mapped to a common data model and analysed in a federated manner. 

**Participants** Model development used a large patient population consisting of >2 million patients with an outpatient (OP), general practice (GP) or emergency room (ER) visit with suspected or diagnosed influenza or influenza-like symptoms any time prior to 2020. The model was externally validated on patients with an OP/GP/ER visit in 2020 with a confirmed or suspected COVID-19 diagnosis across two different databases from Korea and Spain.

**Exposures and outcomes** Age, gender, historical diagnoses and previous drug use prior to the index visit were studied as candidate covariates. Outcomes included i) hospital admission for pneumonia, ii) hospitalization for pneumonia requiring intensive care or iii) death in the 30 days after a valid GP/ER/OP visit with a confirmed COVID-19 diagnosis. 

**Results** Within the flu data we identified 7 predictors (history of cancer, COPD, diabetes, heart disease, hypertension, hyperlipidemia, and kidney disease) in additional to age and gender that were able to discriminate which patients with a visit would i) require hospitalization with pneumonia, ii) require intensive care with pneumonia,  or iii) die.  The AUCs ranged between 0.66-0.84 for predicting hospitalization, 0.65-0.84 for predicting intensive care and 0.63-0.9 for predicting death in flu patients across the world.  When transported to COVID-19 specific patients with an outpatient or ER visit, the AUCs ranged between 0.75-0.81 for predicting hospitalization, 0.89-0.91 for predicting intensive care and 0.86-0.89 for predicting death. Calibration in the COVID-19 patients was overall acceptable, with underestimated risk in the most elderly and highest risk strata.,,

**Conclusions and relevance**
We were able to develop parsimonious models containing nine predictors using large databases of flu patients that transported well to COVID-19 patients.  Our models could be used to quantify a patient’s risk of becoming severely or critically ill due to COVID-19 infection based on their age, gender and medical history.  This can be used to identify the patients that require being shielded from COVID-19 to reduce the virus impact on morbidity and mortality.


Below are links for study-related artifacts that have been made available as part of this study:

**Protocol:** [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/blob/master/HospitalizationInSentHomePatients/docs/PLP_protocol_Q1%2BQ2_20200329.docx)

### Packages ###

- OHDSI model development: [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/tree/master/HospitalizationInSymptomaticPatients)
- OHDSI full model validation: [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/tree/master/HospInOutpatientVal)
- OHDSI simple model validation: [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/tree/master/CovidSimpleModels)

The Observational Health Data Sciences and Informatics (OHDSI) international community is hosting a COVID-19 virtual study-a-thon this week (March 26-29) to inform healthcare decision-making in response to the current global pandemic. The preliminary research results on this web-based application are from a retrospective, real-world, observational study in support of this activity and will subsequently be submitted to a peer-reviewed, scientific journal. 