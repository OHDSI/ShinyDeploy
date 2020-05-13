### External validation of the covid-19 vulnerability index patient-level prediction across an international network of observational healthcare data datasets

**Development Status: Under Development**

### Information

This shiny application contains the results of the external validations of a model developed to predict risk of hospitalization with pneumonia in patients with flu or covid-19.

During manuscript development and the subsequent review period, these results are considered under embargo and should not be disclosed without explicit permission and consent from the authors.

Below are links for study-related artifacts that have been made available as part of this study:

**Protocol:** [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/blob/master/CovidVulnerabilityIndex/docs/PLP_protocol_cvi_20200416.docx)

### Abstract 

Below is the abstract of the manuscript that summarizes the findings:

**Background:**  The COVID-19 illness is straining healthcare systems globally.  Evidence based medicine that can be used to discriminate between COVID-19 patients requiring hospitalization and those who do not are need to reduce the likelihood that hospitals reach capacity during the pandemic. The COVID-19 vulnerability index, a model that predict which patients with pneumonia require hospitalization, has been developed and proposed as a tool for decision making during the COVID-19 outbreak.  However, the model has not been extensively externally validated.

**Methods:** We translated the model so it could be implemented on any data in the Observational Medical Outcome Partnership (OMOP) common data model format.  We implemented the model on patients at the point they have COVID-19/Flu or related symptoms during an outpatient visit to predict their risk of hospitalization with pneumonia during the following 0 to 30 days. We then validated the model across a network of N databases spanning the US, Europe and Asia.  The validation included non-COVID-19 datasets in addition to COVID-19 datasets.

**Findings:** The internal validation performance of the COVID-19 vulnerability index on non-COVID-19 patients was 0.xx.  When externally validated on N non-COVID-19 patients across the OHDSI network the AUC ranged between 0.xx-0.xx.  Transported to COVID-19 data the model obtained AUCs of 0.xx, 0.xx and 0.xx on A,B and C datasets respectively.  The calibration …

**Interpretation:** The results show that the discriminative performance of the model was lower than the reported internal validation performance across non-COVID-19 data and [add COVID-19 summary].  The calibration results mean [add].  We therefore [do/do not] recommend using this model [causously] as a means to identify which COVID-19 patients should be hospitalized. 

### Study Packages

- Model validation: [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/tree/master/CovidVulnerabilityIndex)

The Observational Health Data Sciences and Informatics (OHDSI) international community is hosting a COVID-19 virtual study-a-thon this week (March 26-29) to inform healthcare decision-making in response to the current global pandemic. The preliminary research results on this web-based application are from a retrospective, real-world, observational study in support of this activity and will subsequently be submitted to a peer-reviewed, scientific journal.