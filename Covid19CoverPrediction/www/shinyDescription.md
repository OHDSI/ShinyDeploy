### Seek COVER: Development and validation of a personalized risk calculator for COVID-19 outcomes in an international network

**Development Status: Under Development**

This shiny application contains the results of the internal and external validations of the models developed to predict risk of hospitalisation within 30 days of intitial presentation to a healthcare professional with symptoms.

During manuscript development and the subsequent review period, these results are considered under embargo and should not be disclosed without explicit permission and consent from the authors.


**Abstract**
Importance
COVID-19 is causing high mortality worldwide. Developing models to quantify the risk of poor outcomes in infected patients could help develop strategies to shield the most vulnerable during de-confinement.

**Objective**
To develop and externally validate COVID-19 Estimated Risk (COVER) scores that quantify a patient’s risk of hospital admission (COVER-H), requiring intensive services (COVER-I), or fatality (COVER-F) in the 30-days following COVID-19 diagnosis.

**Design**
Multinational, distributed network cohorts.

**Setting**
We analyzed a federated network of electronic medical records and administrative claims data from 14 data sources and 6 countries, mapped to a common data model. 

**Participants**
Model development used a patient population consisting of >2 million patients with a general practice (GP), emergency room (ER), or outpatient (OP) visit with diagnosed influenza or flu-like symptoms any time prior to 2020. The model was validated on patients with a GP, ER, or OP visit in 2020 with a confirmed or suspected COVID-19 diagnosis across five databases from South Korea, Spain and the United States.

**Outcomes**
Age, sex, historical conditions, and drug use prior to index date were considered as candidate predictors. Outcomes included i) hospitalization with pneumonia, ii) hospitalization with pneumonia requiring intensive services or death, and iii) death in the 30 days after index date.

**Results**
Overall, 44,507 COVID-19 patients were included for model validation, after initial model development and validation using 6,869,127 patients with influenza or flu-like symptoms. We identified 7 predictors (history of cancer, chronic obstructive pulmonary disease, diabetes, heart disease, hypertension, hyperlipidemia, and kidney disease) which combined with age and sex could discriminate which patients would experience any of our three outcomes. The models achieved high performance in influenza. When transported to COVID-19 cohorts, the AUC ranges were, COVER-H: 0.69-0.81, COVER-I: 0.73-0.91, and COVER-F: 0.72-0.90. Calibration was overall acceptable, with overestimated risk in the most elderly and highest risk strata.,,

**Conclusions and relevance**
A 9-predictor model performs well for COVID-19 patients for predicting hospitalization, intensive services and fatality. The models could aid in providing reassurance for low risk patients and shield high risk patients from COVID-19 during de-confinement to reduce the virus’ impact on morbidity and mortality.


### Useful Links
A preprint of this research is available here: 
Below are links for study-related artifacts that have been made available as part of this study: [link](https://www.medrxiv.org/content/10.1101/2020.05.26.20112649v1)

**Protocol:** [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/blob/master/HospitalizationInSentHomePatients/docs/PLP_protocol_Q1%2BQ2_20200329.docx)

### Packages

- OHDSI model development: [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/tree/master/HospitalizationInSymptomaticPatients)
- OHDSI full model validation: [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/tree/master/HospInOutpatientVal)
- OHDSI simple model validation: [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/tree/master/CovidSimpleModels)

The Observational Health Data Sciences and Informatics (OHDSI) international community is hosting a COVID-19 virtual study-a-thon this week (March 26-29) to inform healthcare decision-making in response to the current global pandemic. The preliminary research results on this web-based application are from a retrospective, real-world, observational study in support of this activity and will subsequently be submitted to a peer-reviewed, scientific journal. 