Masters Thesis Repository

This Repository contains my final defense paper and all relevant R code.\
Note that the dataset used cannot be provided due to copyright and for privacy of the particpants involved in the CHILD cohort study.\
All code and findings are copyright of Charanpal Singh.

Title: Modeling Childhood Wheezing in Small Areas in Manitoba

Author: Charanpal Singh and Mahmoud Torabi

E-mail: Charanpal.Singh@umanitoba.ca and Mahmoud.Torabi@umanitoba.ca

Software: R version 4.1.3

The "Data_cleaning_and_exploration.R" is the code used to clean the real dataset from the CHILD study and was used to create data lists suitable for the models to use in "Data_Analysis.R" and to make some of the plots in "Figures.R". The script "Figures.R" contains all the plots in order by chapter. Note it is not advised to run the "Data_Analysis.R" all at once since this would take a long time, but simply choose the model you wish to run in a separate script. The order the scripts should be run is "Data_cleaning_and_exploration.R, "Data_Analysis.R" and then finally "Figures.R". There is a seperate folder for the  simulation section and its contains its own readme file, the simualtions were perfomed on a cluster network by making multiple R scripts which were run in parallel.

