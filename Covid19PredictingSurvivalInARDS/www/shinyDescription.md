### Development and validation of patient-level prediction models that predict short-term and long-term survival in patients requiring intensive care for pneumonia and ADRS

**Development Status: Under Development**

### Information

This shiny application contains the results of the internal and external validations of the models developed to predict risk of use of death in patients who are admitted to hospital and require intensive care with pneumonia and ARDS

During manuscript development and the subsequent review period, these results are considered under embargo and should not be disclosed without explicit permission and consent from the authors.

Below are links for study-related artifacts that have been made available as part of this study:

**Protocol:** [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/blob/master/CovidSimpleSurvival/docs/PLP_protocol_Q4_20200416.docx)

### Abstract 

Below is the abstract of the manuscript that summarizes the findings:

**Background:** The novel virus COVID-19 is causing unprecedented demand for intensive care resources. It is becoming apparent that prioritization frameworks will be required to identify which patients should be provided with treatment that may improve prognosis. These frameworks depend on estimates for short-term and long-term survival probabilities given appropriate treatment. In this paper we propose learning a data-driven model to predict short-term and long-term survival. This will enable prioritization framework to use evidence-based models rather than hypothetical models.  

**Methods:** A cohort design prediction study developing regression models using a large US claims dataset. Models were developed using retrospectively collected data to predict which of the 155,881 patients requiring intensive care for pneumonia and acute respiratory deficiency syndrome (ARDS) survive short-term (60-days) and long-term (365-days). Models were extensively validated across N claims and electronic healthcare record databases around the world. In addition, models were validated on COVID-19 patient data from South Korea.

**Findings:** We developed complex models that predict short-term and long-term survival for patients who received intensive care for pneumonia and ARDS. We then simplified these into score-based models that use ten predictors in addition to age and gender to determine a patients short-term and long-term survival. These simple models obtained moderate discriminative performance across US claims data and transported to data containing COVID-19 patients in Korea. These data-driven models could be used to replace non-evidence-based survival scores used in prioritization schemes.

**Interpretation:** Our objective and transparent simple models could be used within an intensive care prioritization scheme. This will help identify which patients should be prioritised to receive intensive care resources due to them having the highest probability of a favourable outcome. The simple models did not transport across all patient populations investigated and may not be suitable for every country. 

### Study Package

- OHDSI model validation: [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/tree/master/CovidSimpleSurvival)


The Observational Health Data Sciences and Informatics (OHDSI) international community is hosting a COVID-19 virtual study-a-thon this week (March 26-29) to inform healthcare decision-making in response to the current global pandemic. The preliminary research results on this web-based application are from a retrospective, real-world, observational study in support of this activity and will subsequently be submitted to a peer-reviewed, scientific journal.