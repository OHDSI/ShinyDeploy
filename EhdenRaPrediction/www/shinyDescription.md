### Development and external validation of prediction models for adverse health outcomes in rheumatoid arthritis: a multinational real-world cohort analysis

**Development Status: Under Development**
### Development Status: Under Development
Study lead: **Cynthia Yang, Ross Williams**

Study lead forums tag: **[cynthiayang](https://forums.ohdsi.org/u/cynthiayang), [RossW](https://forums.ohdsi.org/u/RossW)**



### Information



This shiny application contains the results of models to for the prediction of adverse health outcomes in rheumatoid arthritis (RA) developed on the database Optum Claims. 

During manuscript development and the subsequent review period, these results are considered under embargo and should not be disclosed without explicit permission and consent from the authors.



Below are links for study-related artifacts that have been made available as part of this study:



**Protocol:** [link](https://github.com/ohdsi-studies/EhdenRaPrediction/blob/master/documents/RA_PLP_protocol_09042021.docx)



### Abstract 


Below is the abstract of the manuscript that summarizes the findings:



**Background:**  
Identification of rheumatoid arthritis (RA) patients at high risk of adverse health outcomes remains a major challenge. We aimed to develop and validate prediction models for a variety of adverse health outcomes in RA patients initiating first-line methotrexate (MTX) monotherapy. 

**Methods:** 
RA patients initiating first-line MTX monotherapy were identified in 15 claims and electronic health record databases across 9 countries. Prediction models were developed and internally validated on Optum® De-identified Clinformatics® Data Mart Database using L1-regularized logistic regression to estimate the risk of various adverse health outcomes within 3 months (leukopenia, pancytopenia, infection), 2 years (myocardial infarction (MI) and stroke), and 5 years (cancers [colorectal, breast, uterine]) after treatment initiation. Models were externally validated on all other databases. Performance was assessed using the area under the receiver operator characteristic curve (AUC) and calibration plots.

**Findings:** 
The models were developed and internally validated on 21,547 RA patients and externally validated on 131,928 RA patients. Models for serious infection (AUC: internal 0.743, external ranging from 0.616 to 0.828), MI (AUC: internal 0.764, external ranging from 0.563 to 0.819), and stroke (AUC: internal 0.770, external ranging from 0.630 to 0.949), showed good discrimination and adequate calibration. Models for the other outcomes showed modest internal discrimination (AUC < 0.65) and were not externally validated.

**Interpretation:**
We developed and validated prediction models for a variety of adverse health outcomes in RA patients initiating first-line MTX monotherapy. The final models for serious infection, MI, and stroke demonstrated good performance across multiple databases and can be studied for clinical use.

### Study Packages

- Model validation: [link](https://github.com/ohdsi-studies/EhdenRaPrediction/tree/master/validationPackage/EhdenRaPredictionValidation)



