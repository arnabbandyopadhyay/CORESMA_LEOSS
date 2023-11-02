# CORESMA_LEOSS
This study included a total of >10,000 patients who were enrolled in the LEOSS (Lean European Open Survey on SARS-CoV-2) registry (https://leoss.net/). These patients were recruited from March 18, 2020, from 122 hospitals across Europe, with the majority of them located in Germany. All patients were diagnosed with SARS-CoV-2 through positive results from PCR testing. Data collection was conducted retrospectively and anonymously, focusing on the documentation of standard of care treatment. Only patients with information available on follow-up and at the end of the treatment (recovery or death) were included in the analysis.

Upon the initial positive test result for SARS-CoV-2 and at the first point of reference with the hospitals, the patients were divided into different groups based on their clinical conditions, namely uncomplicated (UC), complicated (CO), critical (CR) and recovered (RC). Several hematological (e.g lymphocyte count, neutrophil count), inflammatory (e.g C-reactive protein, procalcitonin etc.), immunological and biochemical (IL6, Ddimer, CK, troponin, AST) were measured. The study received ethical approval from the responsible ethics committee at each participating study site.

We want to determine the predictive power of the expression of biomarkers for disease severity by considering only those patients who were at different disease state using Machine learning methods. Analysis were performed in R version 4.1.2. Following libraries were used: caret, dlookr, MASS, MLeval, readxl, stringr, dplyr, grid, gridExtra, combinat, ggpubr, ggplot2.

A link to the data is not possible to provide for legal reasons. 

machine_learning_uc_co_cr.R analyses the biomarker expression among different disease states, UC, CO and CR.

machine_learning_co_cr.R analyses the biomarker expression of CO and CR patients.

ML_variant.R and ML_variant_of_concern.R analyses the biomarker expression in CR patients infected with either Wt, alpha and other variant of concern.
