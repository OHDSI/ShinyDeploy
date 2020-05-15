### Predicting Severely Ill Patients In Those Admitted To Hospital For Pneumonia (To be investigated for the use on Patients, with or suspected to have, Covid-19)

**Development Status: Under Development**

### Information

This shiny application contains the results of the internal and external validations of the models developed to predict risk of use of intensive services or death in patients who are admitted to hospital with pneumonia

During manuscript development and the subsequent review period, these results are considered under embargo and should not be disclosed without explicit permission and consent from the authors.

Below are links for study-related artifacts that have been made available as part of this study:

**Protocol:** [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/blob/master/SevereInHospitalizedPatients/docs/PLP_protocol_Q3_20200329.docx)

### Abstract 

Below is the abstract of the manuscript that summarizes the findings:

**Background:** The novel virus COVID-19 is causing unprecedented demand for healthcare resources around the world.  There are global concerns that hospitals resources may be overwhelmed during the peak of the crisis.  This has resulted in demand for prediction models that could be used to aid resource prioritization frameworks.

**Methods:** A retrospective cohort design using large data from non-COVID-19 patients was used to develop machine learning models that predict the requirement of intensive care or mortality in patients hospitalized for pneumonia.  A network of international databases, including patient populations with COVID-19, were used to externally validate the models and determine whether the models transport to COVID-19 patients. 

**Results:**  We developed complex and simple models using features consisting of medical records on or prior to hospitalization.  The complex models performed moderately (AUC ~ 0.6) and reasonably (AUC ~0.7) in predicting which hospitalised COVID-19 patients will require intensive care or die, respectively. The simple models were unable to discriminate the requirement of intensive care but showed promise in being used to predict mortality.

**Discussion:** We investigated whether machine learning models trained using big data in non-COVID-19 populations could be transported to COVID-19 patients.  Our results show that we are only able to achieve moderate predictive performance when discriminating which patients hospitalised with pnuemonia will require intensive care using medical records prior to, or on the data of, hospitalization.  AS COVID-19 datasets grow, models developed on COVID-19 patients should be investigated. 


### Study Packages

- OHDSI model development: [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/tree/master/SevereInHospitalizedPatients)
- OHDSI model validation: [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/tree/master/SevereInHospVal)


The Observational Health Data Sciences and Informatics (OHDSI) international community is hosting a COVID-19 virtual study-a-thon this week (March 26-29) to inform healthcare decision-making in response to the current global pandemic. The preliminary research results on this web-based application are from a retrospective, real-world, observational study in support of this activity and will subsequently be submitted to a peer-reviewed, scientific journal.