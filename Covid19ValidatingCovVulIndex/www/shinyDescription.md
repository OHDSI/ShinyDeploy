### When can we trust the machine learning? Demonstrating the importance of external validation by investigating the COVID-19 Vulnerability index across an international network of observational healthcare datasets

**Development Status: Under Development**

### Information

This shiny application contains the results of the external validations of a model developed to predict risk of hospitalization with pneumonia in patients with flu or covid-19.

During manuscript development and the subsequent review period, these results are considered under embargo and should not be disclosed without explicit permission and consent from the authors.

Below are links for study-related artifacts that have been made available as part of this study:

**Protocol:** [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/blob/master/CovidVulnerabilityIndex/docs/PLP_protocol_cvi_20200416.docx)

### Abstract 

Below is the abstract of the manuscript that summarizes the findings:

**Background:**  COVID-19 is straining healthcare systems globally. Evidence based medicine used to discriminate between patients requiring hospitalization and those who do not are needed to reduce the burden on hospitals during the pandemic. The COVID-19 vulnerability (C-19) index, a model that predicts which patients will be admitted to hospital for treatment of pneumonia or pneumonia proxies, has been developed and proposed as a valuable tool for decision making during the COVID-19 outbreak. However, the model scores high on risk of bias according to the PROBAST score and has not been externally validated.

**Methods:** We follow the OHDSI framework for external validation to assess the reliability of the C-19 model.  We evaluated the model on two different target populations: i) patients that have COVID-19 at an outpatient or emergency room visit and ii) patients that have influenza or related symptoms during an outpatient or emergency room visit, to predict their risk of hospitalization with pneumonia during the following 0 to 30 days. In total we validated the model across a network of 14 databases spanning the US, Europe, Australia and Asia.  

**Findings:** The internal validation performance of the C-19 index was a c-statistic of 0.73 and calibration was not reported by the authors.  When we externally validated it by transporting it to COVID-19 data the model obtained c-statistics of 0.36, 0.53 and 0.56 on Spanish, US and South Korean datasets respectively. The calibration was poor with the model under-estimating risk. When validated on 12 datasets containing influenza patients across the OHDSI network the c-statistics ranged between 0.40-0.68. 

**Interpretation:** The results show that the discriminative performance of the C-19 model was lower than the reported internal validation across influenza cohorts. More importantly, we report very poor performance in the first validation of C-19 amongst COVID-19 patients in the US, Spain and Korea.  These results suggest that C-19 should not be used to aid decision making during the COVID-19 pandemic. Our findings highlight the importance of performing external validation to determine a prediction model’s reliability. In the field of prediction, extensive validation is required to create appropriate trust in a model.   

### Study Packages

- Model validation: [link](https://github.com/ohdsi-studies/Covid19PredictionStudies/tree/master/CovidVulnerabilityIndex)

The Observational Health Data Sciences and Informatics (OHDSI) international community is hosting a COVID-19 virtual study-a-thon this week (March 26-29) to inform healthcare decision-making in response to the current global pandemic. The preliminary research results on this web-based application are from a retrospective, real-world, observational study in support of this activity and will subsequently be submitted to a peer-reviewed, scientific journal.