Masters Thesis Repository

This Repository contains my final masters thesis paper and all relevant R code.
------------------------------------------------------------------------------------------------------------------------------------------------------
Diagnosing asthma in early childhood is challenging due to limited routine assessments. Wheezing, though common in asthma, is not exclusive to the condition. A predictive model applicable before age two could have significant clinical utility, but current models fall short.
Data from the Canadian Healthy Infant Longitudinal Development (CHILD) study were used to develop longitudinal logistic and zero-inflated Poisson (ZIP) models. Wheezing severity was analyzed using individual- and area-level random effects, covering 60 of Manitoba’s 96 regional health authority districts (RHADs). These models examined factors influencing wheezing frequency and severity and the role of location within Manitoba.
Maternal asthma (OR: 3.31, 95\% CI: 1.87–5.81) and smoking (OR: 2.85, 95% CI: 0.98–7.46) significantly increased odds of wheezing. Living near a farm reduced wheezing odds (OR: 0.70, 95% CI: 0.23–1.82) but increased rates among those already wheezing (RR: 2.45, 95% CI: 1.11–5.93). Within Manitoba, Gimli, Hanover, Spruce Woods, and St. Pierre had the highest predicted proportions and rates of wheezing.
Maternal health history, particularly asthma and smoking, significantly impacts wheezing in Manitoba’s children. Areas like Gimli and Spruce Woods show persistent wheezing hotspots, warranting further study to validate these findings.

------------------------------------------------------------------------------------------------------------------------------------------------------
Note that the dataset used cannot be provided due to copyright and for privacy of the particpants involved in the CHILD cohort study, see link for more details https://childstudy.ca/for-researchers/data-access/. 
Note individual-level participant data was used for this study which fall under controlled access data. 
All code and findings are copyright of Charanpal Singh.
------------------------------------------------------------------------------------------------------------------------------------------------------
Title: Modeling Childhood Wheezing in Small Areas in Manitoba

Author: Charanpal Singh and Mahmoud Torabi

E-mail: Charanpal.Singh@umanitoba.ca and Mahmoud.Torabi@umanitoba.ca

Software: R version 4.1.3

The "Data_cleaning_and_exploration.R" is the code used to clean the real dataset from the CHILD study and was used to create data lists suitable for the models to use in "Data_Analysis.R" and to make some of the plots in "Figures.R". The script "Figures.R" contains all the plots in order by chapter. Note it is not advised to run the "Data_Analysis.R" all at once since this would take a long time, but simply choose the model you wish to run in a separate script. The order the scripts should be run is "Data_cleaning_and_exploration.R, "Data_Analysis.R" and then finally "Figures.R". There is a seperate folder for the  simulation section and its contains its own readme file, the simualtions were perfomed on a cluster network by making multiple R scripts which were run in parallel.

