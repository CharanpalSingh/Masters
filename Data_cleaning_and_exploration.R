rm(list=ls())
library(foreign)
library(sf)
library(tidyverse)
library(tmap)
library(colorspace)
library(rgdal)
library(readxl)
library(dplyr)
library(imputeTS)
library(forestmangr)
library(tidyr)
library(car)
library(xtable)
library(openintro)
library(janitor) #clean colunm names
library(zoo) #na.aggerate function
library(purrr) # contengency table across columns
library(gtsummary) #tbl summary 
library(ggplot2)
library(forcats)

setwd("your_path")

#postal codes and regions database 

DBF1=read.dbf("...\\combined96_PCCF_2016.dbf")

#test=read.dbf("...\\shapefile\\combined.dbf")

B=which(DBF1[,23]==1)
DBF2=DBF1[c(B),]
DBF3=DBF2[,c(2,22,21,35)]

Manitoba96=read.csv(file="...\\Re__Convert_postal-codes_to_health_regions\\96regions.csv", header=TRUE)  

#5 health regions in Manitoba
FiveHealthRegions=read.dbf("..\\reshapefilefor5healthregions\\RHA2012.dbf")

#import datasets into R 

Health_1Y <- read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Child Health/cleaned_Q198CHLTH1Y.xlsx")
Health_18M <- read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Child Health/cleaned_Q218CHLTH18M.xlsx")
Health_3M <- read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Child Health/cleaned_Q233CHLTH3M.xlsx")
Health_6M <- read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Child Health/cleaned_Q234CHLTH6M.xlsx")
Health_2Y <- read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Child Health/cleaned_Q299CHLTH2Y.xlsx")
Health_2HY <- read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Child Health/cleaned_Q302CHLTH2HY.xlsx")
Health_3Y <- read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Child Health/cleaned_Q377CHLTH3Y.xlsx")
Health_4Y <- read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Child Health/cleaned_Q404CHLTH4Y.xlsx")
Health_5Y <- read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Child Health/cleaned_Q452CHLTH5Y.xlsx")

Home_enviro_3M=read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Home enviroment/cleaned_Q114HENV3M.xlsx")
Home_enviro_3M_2ndrequest=read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Data request of C99/20201125 - Charanpal Singh (second request)/CP99 Q114 HENV3M.xlsx")
Home_enviro_3M=read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Home enviroment/cleaned_Q114HENV3M.xlsx")
Home_enviro_6M=read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Home enviroment/cleaned_Q165HENV6M.xlsx")
Home_enviro_1Y=read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Home enviroment/cleaned_Q173HENV1Y.xlsx")

Home_enviro_1Y_2ndrequest=read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Data request of C99/20201125 - Charanpal Singh (second request)/CP99 Q173 HENV1Y.xlsx")
Home_enviro_18M=read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Home enviroment/cleaned_Q215HENV18M.xlsx")
Home_enviro_2Y=read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Home enviroment/cleaned_Q280HENV2Y.xlsx")
Home_enviro_2HY=read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Home enviroment/cleaned_Q353HENV2HY.xlsx")
Home_enviro_3Y=read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Home enviroment/cleaned_Q376HENV3Y.xlsx")
Home_enviro_4Y=read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Home enviroment/cleaned_Q403HENV4Y.xlsx")
Home_enviro_5Y=read_excel("C:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis work/Home enviroment/cleaned_Q442HENV5Y.xlsx")

Clinical1Y=read_excel("C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\Clinical\\cleaned_Q192CHCLA1Y.xlsx")
Clinical3Y=read_excel("C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\Clinical\\cleaned_Q378CHCLA3Y.xlsx")
Clinical5Y=read_excel("C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\Clinical\\cleaned_Q454CHCLA5Y.xlsx")

MotherHealth1Y=read_excel("C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Data request of C99\\cleaned_Q182MHLTH1Y.xlsx")

MotherHealth1Y2ndRequest=read_excel("C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Data request of C99\\20201125 - Charanpal Singh (second request)\\CP99 Q182 MHLTH1Y.xlsx")

MotherHealth1Y2ndRequest=MotherHealth1Y2ndRequest[order(MotherHealth1Y2ndRequest$SubjectNumber),]


MotherHealth1Y=merge(MotherHealth1Y,MotherHealth1Y2ndRequest)
FatherHealth1Y=read_excel("C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Data request of C99\\cleaned_Q93PRNFH18W.xlsx")
FatherHealth1Y2ndRequest=read_excel("C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Data request of C99\\20201125 - Charanpal Singh (second request)\\CP99 Q93 PRNFH18W.xlsx")
FatherHealth1Y=merge(FatherHealth1Y,FatherHealth1Y2ndRequest)

MotherHealthPrenatal=read_excel('C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Data request of C99\\20210805 (Prenatal mother)- Charanpal Singh\\Q91 PRNMH18WK_cleaned.xlsx')

#SES
SES18WK=read_excel("C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Data request of C99\\20201125 - Charanpal Singh (second request)\\CP99 Q100 SES18WK.xlsx")
SES1Y=read_excel("C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\SES\\cleaned_Q176SES1Y.xlsx")
SES3Y=read_excel("C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\SES\\cleaned_Q371SES3Y.xlsx")
SES4Y=read_excel("C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\SES\\cleaned_Q408SES4Y.xlsx")
SES5Y=read_excel("C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\SES\\cleaned_Q445SES5Y.xlsx")

#order by subject id
Health_3M=Health_3M[order(Health_3M$SubjectNumber),]
Health_6M=Health_6M[order(Health_6M$SubjectNumber),]
Health_18M=Health_18M[order(Health_18M$SubjectNumber),]
Health_1Y=Health_1Y[order(Health_1Y$SubjectNumber),]
Health_2Y=Health_2Y[order(Health_2Y$SubjectNumber),]
Health_2HY=Health_2HY[order(Health_2HY$SubjectNumber),]
Health_3Y=Health_3Y[order(Health_3Y$SubjectNumber),]
Health_4Y=Health_4Y[order(Health_4Y$SubjectNumber),]
Health_5Y=Health_5Y[order(Health_5Y$SubjectNumber),]

Home_enviro_3M=Home_enviro_3M[order(Home_enviro_3M$SubjectNumber),]
Home_enviro_3M_2ndrequest=Home_enviro_3M_2ndrequest[order(Home_enviro_3M_2ndrequest$SubjectNumber),]
Home_enviro_3M=merge(Home_enviro_3M,Home_enviro_3M_2ndrequest)
Home_enviro_6M=Home_enviro_6M[order(Home_enviro_6M$SubjectNumber),]
Home_enviro_1Y=Home_enviro_1Y[order(Home_enviro_1Y$SubjectNumber),]
Home_enviro_1Y_2ndrequest=Home_enviro_1Y_2ndrequest[order(Home_enviro_1Y_2ndrequest$SubjectNumber),]
Home_enviro_1Y=merge(Home_enviro_1Y,Home_enviro_1Y_2ndrequest)
Home_enviro_18M=Home_enviro_18M[order(Home_enviro_18M$SubjectNumber),]
Home_enviro_2Y=Home_enviro_2Y[order(Home_enviro_2Y$SubjectNumber),]
Home_enviro_2HY=Home_enviro_2HY[order(Home_enviro_2HY$SubjectNumber),]
Home_enviro_3Y=Home_enviro_3Y[order(Home_enviro_3Y$SubjectNumber),]
Home_enviro_4Y=Home_enviro_4Y[order(Home_enviro_4Y$SubjectNumber),]
Home_enviro_5Y=Home_enviro_5Y[order(Home_enviro_5Y$SubjectNumber),]

Clinical1Y=Clinical1Y[order(Clinical1Y$SubjectNumber),]
Clinical3Y=Clinical3Y[order(Clinical3Y$SubjectNumber),]
Clinical5Y=Clinical5Y[order(Clinical5Y$SubjectNumber),]

Clinical3Y=Clinical3Y[order(Clinical3Y$SubjectNumber),]
Clinical5Y=Clinical5Y[order(Clinical5Y$SubjectNumber),]

MotherHealth1Y=MotherHealth1Y[order(MotherHealth1Y$SubjectNumber),]
MotherHealthPrenatal=MotherHealthPrenatal[order(MotherHealthPrenatal$SubjectNumber),]
FatherHealth1Y=FatherHealth1Y[order(FatherHealth1Y$SubjectNumber),]

SES18WK=SES18WK[order(SES18WK$SubjectNumber),]
SES1Y=SES1Y[order(SES1Y$SubjectNumber),]
SES3Y=SES3Y[order(SES3Y$SubjectNumber),]
SES4Y=SES4Y[order(SES4Y$SubjectNumber),]
SES5Y=SES5Y[order(SES5Y$SubjectNumber),]

#In the last 3 months, has baby had a wheezing noise (whistling sound) coming from his/her chest
#either WITH a cold or WITHOUT a cold? subquestions are if yes are how many episodes.eg 20_1

wheezing_3M=Health_3M[c("SubjectNumber","CHLTH3MQ20","CHLTH3MQ20_1")]
colnames(wheezing_3M) <- c("SubjectNumber","Wheezed_3M","Count_3M")
wheezing_6M=Health_6M[c("SubjectNumber","CHLTH6MQ17","CHLTH6MQ17_1")]
colnames(wheezing_6M) <- c("SubjectNumber","Wheezed_6M","Count_6M")
wheezing_1Y=Health_1Y[c("SubjectNumber","CHLTH1YQ17","CHLTH1YQ17_1")]
colnames(wheezing_1Y) <- c("SubjectNumber","Wheezed_1Y","Count_1Y")
wheezing_18M=Health_18M[c("SubjectNumber","CHLTH18MQ5","CHLTH18MQ5_1")]
colnames(wheezing_18M) <- c("SubjectNumber","Wheezed_18M","Count_18M")
wheezing_2Y=Health_2Y[c("SubjectNumber","CHLTH2YQ6","CHLTH2YQ6_1")]
colnames(wheezing_2Y) <- c("SubjectNumber","Wheezed_2Y","Count_2Y")
wheezing_2HY=Health_2HY[c("SubjectNumber","CHLTH2HYQ6","CHLTH2HYQ6_1")]
colnames(wheezing_2HY) <- c("SubjectNumber","Wheezed_2HY","Count_2HY")
wheezing_3Y=Health_3Y[c("SubjectNumber","CHLTH3YQ4","CHLTH3YQ4_1")]
colnames(wheezing_3Y) <- c("SubjectNumber","Wheezed_3Y","Count_3Y")
wheezing_4Y=Health_4Y[c("SubjectNumber","CHLTH4YQ6","CHLTH4YQ6_1")]
colnames(wheezing_4Y) <- c("SubjectNumber","Wheezed_4Y","Count_4Y")
wheezing_5Y=Health_5Y[c("SubjectNumber","CHLTH5YQ6","CHLTH5YQ6_1")]
colnames(wheezing_5Y) <- c("SubjectNumber","Wheezed_5Y","Count_5Y")

#wheezing scale 

wheezing_scale_3M=Health_3M[c("SubjectNumber","CHLTH3MQ25")]
wheezing_scale_6M=Health_6M[c("SubjectNumber","CHLTH6MQ21")]
wheezing_scale_1Y=Health_1Y[c("SubjectNumber","CHLTH1YQ21")]
wheezing_scale_18M=Health_18M[c("SubjectNumber","CHLTH18MQ14")]
wheezing_scale_2Y=Health_2Y[c("SubjectNumber","CHLTH2YQ10")]
wheezing_scale_2HY=Health_2HY[c("SubjectNumber","CHLTH2HYQ10")]

colnames(wheezing_scale_3M) <- c("SubjectNumber","severity_3M")
colnames(wheezing_scale_6M) <- c("SubjectNumber","severity_6M")
colnames(wheezing_scale_1Y) <- c("SubjectNumber","severity_1Y")
colnames(wheezing_scale_18M) <- c("SubjectNumber","severity_18M")
colnames(wheezing_scale_2Y) <- c("SubjectNumber","severity_2Y")
colnames(wheezing_scale_2HY) <- c("SubjectNumber","severity_2HY")

#Child had any colds and how many times
Colds_3M=Health_3M[c("SubjectNumber","CHLTH3MQ4","CHLTH3MQ5")]
Colds_6M=Health_6M[c("SubjectNumber","CHLTH6MQ1","CHLTH6MQ2")]
Colds_1Y=Health_1Y[c("SubjectNumber","CHLTH1YQ1","CHLTH1YQ2")]
Colds_18M=Health_18M[c("SubjectNumber","CHLTH18MQ1","CHLTH18MQ1_1")]
Colds_2Y=Health_2Y[c("SubjectNumber","CHLTH2YQ3","CHLTH2YQ3_1")]
Colds_2HY=Health_2HY[c("SubjectNumber","CHLTH2HYQ3","CHLTH2HYQ3_1")]
Colds_3Y=Health_3Y[c("SubjectNumber","CHLTH3YQ1","CHLTH3YQ1_1")]
Colds_4Y=Health_4Y[c("SubjectNumber","CHLTH4YQ3","CHLTH4YQ3_1")]
Colds_5Y=Health_5Y[c("SubjectNumber","CHLTH5YQ3","CHLTH5YQ3_1")]
colnames(Colds_3M) <- c("SubjectNumber","Cold_3M","CountCold_3M")
colnames(Colds_6M) <- c("SubjectNumber","Cold_6M","CountCold_6M")
colnames(Colds_1Y) <- c("SubjectNumber","Cold_1Y","CountCold_1Y")
colnames(Colds_18M) <- c("SubjectNumber","Cold_18M","CountCold_18M")
colnames(Colds_2Y) <- c("SubjectNumber","Cold_2Y","CountCold_2Y")
colnames(Colds_2HY) <- c("SubjectNumber","Cold_2HY","CountCold_2HY")
colnames(Colds_3Y) <- c("SubjectNumber","Cold_3Y","CountCold_3Y")
colnames(Colds_4Y) <- c("SubjectNumber","Cold_4Y","CountCold_4Y")
colnames(Colds_5Y) <- c("SubjectNumber","Cold_5Y","CountCold_5Y")

#Anyone smoking at home (Binary)

Smoking_3M=Home_enviro_3M[c("SubjectNumber","HENV3MQ59")]
Smoking_6M=Home_enviro_6M[c("SubjectNumber","HENV6MQ20")]
Smoking_1Y=Home_enviro_1Y[c("SubjectNumber","HENV1YQ59")]
Smoking_18M=Home_enviro_18M[c("SubjectNumber","HENV18MQ11")]
Smoking_2Y=Home_enviro_2Y[c("SubjectNumber","HENV2Y__Q12")]
Smoking_2HY=Home_enviro_2HY[c("SubjectNumber","HENV2HYQ12")]
Smoking_3Y=Home_enviro_3Y[c("SubjectNumber","HENV3YQ32")]
Smoking_4Y=Home_enviro_4Y[c("SubjectNumber","HENV4YQ12")]
Smoking_5Y=Home_enviro_5Y[c("SubjectNumber","HENV5YQ31")]

colnames(Smoking_3M) <- c("SubjectNumber","Smoking_3M")
colnames(Smoking_6M) <- c("SubjectNumber","Smoking_6M")
colnames(Smoking_1Y) <- c("SubjectNumber","Smoking_1Y")
colnames(Smoking_18M) <- c("SubjectNumber","Smoking_18M")
colnames(Smoking_2Y) <- c("SubjectNumber","Smoking_2Y")
colnames(Smoking_2HY) <- c("SubjectNumber","Smoking_2HY")
colnames(Smoking_3Y) <- c("SubjectNumber","Smoking_3Y")
colnames(Smoking_4Y) <- c("SubjectNumber","Smoking_4Y")
colnames(Smoking_5Y) <- c("SubjectNumber","Smoking_5Y")

#Clincal Assements 
Asthma3Y=Clinical3Y[c("SubjectNumber","CHCLA3YQ31",	"CHCLA3YQ31_1","CHCLA3YQ31_2")]
colnames(Asthma3Y) <- c("SubjectNumber","Asthma_3Y",	"Viral Asthma_3Y","Multi-trigger Asthma_3Y")

Asthma5Y=Clinical5Y[c("SubjectNumber","CHCLA5YQ41","CHCLA5YQ41_1","CHCLA5YQ41_2")]
colnames(Asthma5Y) <- c("SubjectNumber","Asthma_5Y",	"Viral Asthma_5Y","Multi-trigger Asthma_5Y")

#Mother Health
#2nd data request is  c("MHLTH1YQ17","MHLTH1YQ17_1","MHLTH1YQ18")
MomQuestions1Y=MotherHealth1Y[c("SubjectNumber","MHLTH1YQ3","MHLTH1YQ28","MHLTH1YQ17","MHLTH1YQ17_1","MHLTH1YQ18")]
colnames(MomQuestions1Y)=c("SubjectNumber","Mom_Wheezed1Y","Mom_Smoking1Y","Mom_Asthma","DiagnosedMom_Asthma","TreatMom_Asthma")

MomQuestions18W=MotherHealthPrenatal[c("SubjectNumber","PRNMH18WQ6","PRNMH18WQ20","PRNMH18WQ35")]
colnames(MomQuestions18W) =c("SubjectNumber","Mom_Wheezed18W","Mom_Asthma18W","Mom_Smoking18W")
  
Mom_Wheezed1Y_table=table(MomQuestions1Y$Mom_Wheezed1Y)
Mom_Smoked1Y_table=table(MomQuestions1Y$Mom_Smoking1Y)
Mom_Wheezed18W_table=table(MomQuestions18W$Mom_Wheezed18W)
Mom_Smoked18W_table=table(MomQuestions18W$Mom_Smoking18W)
Mom_Asthma18W_table=table(MomQuestions18W$Mom_Asthma18W)

#Father Health
#2nd data request is  c("PRNFH18WQ19","PRNFH18WQ19_1","PRNFH18WQ20"")
DadQuestions1Y=FatherHealth1Y[c("SubjectNumber","PRNFH18WQ5","PRNFH18WQ31","PRNFH18WQ19","PRNFH18WQ19_1","PRNFH18WQ20")]
colnames(DadQuestions1Y)=c("SubjectNumber","Dad_Wheezed1Y","Dad_Smoked1Y","Dad_Asthma","DiagnosedDad_Asthma","TreatDad_Asthma")
Dad_Wheezed1Y=table(DadQuestions1Y$Dad_Wheezed1Y)
Dad_Smoked1Y=table(DadQuestions1Y$Dad_Smoked1Y)



#SES scores by simple mean imputation (not used)
SESQuestions18WK=SES18WK[c("SubjectNumber","SES18WKQ2","SES18WKQ4","SES18WKQ9")]
colnames(SESQuestions18WK)=c("SubjectNumber","MomEducation","DadEducation","Income")

table(SESQuestions18WK$MomEducation)
table(SESQuestions18WK$DadEducation)
table(SESQuestions18WK$Income)



SESQuestions18WK=SESQuestions18WK[order(SESQuestions18WK$SubjectNumber),]


#impute  prefer not to say response (11) 
SESQuestions18WK$Income[SESQuestions18WK$Income == 11] <- NA
SESQuestions18WK_imputed=na_mean(SESQuestions18WK)
SESQuestions18WK_imputed=round_df(SESQuestions18WK_imputed,rf="floor")

#create SES score placeholder, later the scores will be replaced by SDO averaged by area 
SESQuestions18WK_imputed=SESQuestions18WK_imputed[order(SESQuestions18WK_imputed$SubjectNumber),]
SES_factors=SESQuestions18WK_imputed[,2:4]

fa=factanal(SES_factors,1,scores = "regression")


SES_Scores=cbind(SESQuestions18WK_imputed,fa$scores)
colnames(SES_Scores)=c("SubjectNumber","MomEducation","DadEducation","Income","Score")




#furry pets

FurryPets3M=Home_enviro_3M[c("SubjectNumber","HENV3MQ17")]
colnames(FurryPets3M)=c("SubjectNumber","Furrypets3M")
FurryPets6M=Home_enviro_6M[c("SubjectNumber","HENV6MQ8")]
colnames(FurryPets6M)=c("SubjectNumber","Furrypets6M")
FurryPets1Y=Home_enviro_1Y[c("SubjectNumber","HENV1YQ17")]
colnames(FurryPets1Y)=c("SubjectNumber","Furrypets1Y")
FurryPets18M=Home_enviro_18M[c("SubjectNumber","HENV18MQ4")]
colnames(FurryPets18M)=c("SubjectNumber","Furrypets18M")
FurryPets2Y=Home_enviro_2Y[c("SubjectNumber","HENV2Y__Q4")]
colnames(FurryPets2Y)=c("SubjectNumber","Furrypets2Y")
FurryPets2HY=Home_enviro_2HY[c("SubjectNumber","HENV2HYQ4")]
colnames(FurryPets2HY)=c("SubjectNumber","Furrypets2HY")

FurryPets=merge(FurryPets3M,FurryPets6M)
FurryPets=merge(FurryPets,FurryPets1Y)
FurryPets=merge(FurryPets,FurryPets18M)
FurryPets=merge(FurryPets,FurryPets2Y)
FurryPets=merge(FurryPets,FurryPets2HY)


#Is your home within 100 metres of any of the following ?

#Major Highway/Artery
Highway3M=Home_enviro_3M[c("SubjectNumber","HENV3MQ14a")]
colnames(Highway3M)=c("SubjectNumber","Highway3M")
table(Highway3M$Highway3M)
#Factory
Factory3M=Home_enviro_3M[c("SubjectNumber","HENV3MQ14c")]
colnames(Factory3M)=c("SubjectNumber","Factory3M")
#Major/prolonged construction activity
Construction3M=Home_enviro_3M[c("SubjectNumber","HENV3MQ14h")]
colnames(Construction3M)=c("SubjectNumber","Construction3M")
#Body of water
Water3M=Home_enviro_3M[c("SubjectNumber","HENV3MQ14b")]
colnames(Water3M)=c("SubjectNumber","Water3M")
#Farm
Farm3M=Home_enviro_3M[c("SubjectNumber","HENV3MQ14d")]
colnames(Farm3M)=c("SubjectNumber","Farm3M")

Farm1Y=Home_enviro_1Y[c("SubjectNumber","HENV1YQ14d")]
colnames(Farm1Y)=c("SubjectNumber","Farm1Y")


#gather postal codes from follow-up  with the most unique codes. (2Y)

postal_codes_2Y=Home_enviro_2Y[c("SubjectNumber","HENV2Y__Q1")]
colnames(postal_codes_2Y) <- c("SubjectNumber","POSTAL")

#remove missing values (for checking)
postal_codes_2Y_trimmed=postal_codes_2Y[-grep('^\\d+$', postal_codes_2Y$POSTAL),]
#add in regions
Manitoba_Post=merge(Manitoba96,DBF3,by="Region")
Relaxed_Postal=merge(Manitoba_Post,postal_codes_2Y ,by="POSTAL")

#combining data
location=Relaxed_Postal[,c('SubjectNumber','Region','Number','POSTAL')]
location=location[order(location$Number),]
temp=merge(location,wheezing_3M)
temp=merge(temp,wheezing_6M)
temp=merge(temp,wheezing_1Y)
temp=merge(temp,wheezing_18M)
temp=merge(temp,wheezing_2Y)
temp=merge(temp,wheezing_2HY)
temp=merge(temp,wheezing_3Y)
temp=merge(temp,wheezing_4Y)
merged_postal_wheezing_relaxed=merge(temp,wheezing_5Y)
merged_postal_wheezing_relaxed=merged_postal_wheezing_relaxed[,c(1:4,seq(5, 22, 2),seq(6, 22, 2))]
dim(merged_postal_wheezing_relaxed)

merged_outcome_postal_relaxed=merge(merged_postal_wheezing_relaxed,wheezing_scale_3M)
merged_outcome_postal_relaxed=merge(merged_outcome_postal_relaxed,wheezing_scale_6M)
merged_outcome_postal_relaxed=merge(merged_outcome_postal_relaxed,wheezing_scale_1Y)
merged_outcome_postal_relaxed=merge(merged_outcome_postal_relaxed,wheezing_scale_18M)
merged_outcome_postal_relaxed=merge(merged_outcome_postal_relaxed,wheezing_scale_2Y)
merged_outcome_postal_relaxed=merge(merged_outcome_postal_relaxed,wheezing_scale_2HY)

#add wheezing episode counts

merged_outcome_postal_relaxed=merge(merged_outcome_postal_relaxed,wheezing_scale_2HY)

wheezingScaleCounts=cbind(table(merged_outcome_postal_relaxed[,23])[1:3],
table(merged_outcome_postal_relaxed[,24])[1:3],
table(merged_outcome_postal_relaxed[,25])[1:3],
table(merged_outcome_postal_relaxed[,26])[1:3],
table(merged_outcome_postal_relaxed[,27])[1:3],
table(merged_outcome_postal_relaxed[,28])[1:3])
wheezingScaleCounts=as.data.frame(wheezingScaleCounts)
colnames(wheezingScaleCounts) <-paste('F/U', 1:6,sep="-")
wheezingScaleCounts[4,] = c(dim(merged_outcome_postal_relaxed)[1]-colSums(wheezingScaleCounts))
rownames(wheezingScaleCounts)=c("1","2","3","Missing/Skipped")
write.excel(wheezingScaleCounts)

####################################create merged data set s######################################################################################################################

Final_table_relaxed=merge(merged_outcome_postal_relaxed,Asthma3Y)
Final_table_relaxed=merge(Final_table_relaxed,Asthma5Y)
Final_table_relaxed=merge(Final_table_relaxed,MomQuestions18W)
Final_table_relaxed=merge(Final_table_relaxed,MomQuestions1Y)

Final_table_relaxed=merge(Final_table_relaxed,DadQuestions1Y)

Final_table_relaxed=merge(Final_table_relaxed,FurryPets)
Final_table_relaxed=merge(Final_table_relaxed,Factory3M)
Final_table_relaxed=merge(Final_table_relaxed,Farm3M)
Final_table_relaxed=merge(Final_table_relaxed,Farm1Y)
Final_table_relaxed=merge(Final_table_relaxed,Water3M)
Final_table_relaxed=merge(Final_table_relaxed,Construction3M)
Final_table_relaxed=merge(Final_table_relaxed,Highway3M)
Final_table_relaxed=merge(Final_table_relaxed,SESQuestions18WK)
Final_table_relaxed=merge(Final_table_relaxed,SES_Scores)
save(Final_table_relaxed,file=".../Final_table_relaxed.RData")


######################################################data cleaning##############################################


#if wheezing =0 then so does wheezing severity
data_relaxed_cleaned=Final_table_relaxed
data_relaxed_cleaned$severity_3M[data_relaxed_cleaned$Wheezed_3M==0] <-0
data_relaxed_cleaned$severity_6M[data_relaxed_cleaned$Wheezed_6M==0] <-0
data_relaxed_cleaned$severity_1Y[data_relaxed_cleaned$Wheezed_1Y==0] <-0
data_relaxed_cleaned$severity_18M[data_relaxed_cleaned$Wheezed_18M==0] <-0
data_relaxed_cleaned$severity_2Y[data_relaxed_cleaned$Wheezed_2Y==0] <-0
data_relaxed_cleaned$severity_2HY[data_relaxed_cleaned$Wheezed_2HY==0] <-0


###############if wheezing =0 then so does wheezing episodes#########################

data_relaxed_cleaned$Count_3M[data_relaxed_cleaned$Wheezed_3M==0] <-0
data_relaxed_cleaned$Count_6M[data_relaxed_cleaned$Wheezed_6M==0] <-0
data_relaxed_cleaned$Count_1Y[data_relaxed_cleaned$Wheezed_1Y==0] <-0
data_relaxed_cleaned$Count_18M[data_relaxed_cleaned$Wheezed_18M==0] <-0
data_relaxed_cleaned$Count_2Y[data_relaxed_cleaned$Wheezed_2Y==0] <-0
data_relaxed_cleaned$Count_2HY[data_relaxed_cleaned$Wheezed_2HY==0] <-0
data_relaxed_cleaned$Count_3Y[data_relaxed_cleaned$Wheezed_3Y==0] <-0
data_relaxed_cleaned$Count_4Y[data_relaxed_cleaned$Wheezed_4Y==0] <-0
data_relaxed_cleaned$Count_5Y[data_relaxed_cleaned$Wheezed_5Y==0] <-0
 
## Generating frequency data for all the tables in section 3.3.1#############

Final_table_relaxed %>% select( -c(Region,POSTAL)) %>% tbl_summary()

## Generating frequency data for all the tables in section 3.3.2#############
data_relaxed_cleaned %>% select( -c(Region,POSTAL)) %>% tbl_summary()

###Impute data by the five health regions in Manitoba########

data_cleaned_parental_asthma=data_relaxed_cleaned

#if mom never had asthma at the 12 months then also never diagnosed  or treated or asthma
#dianosed
data_cleaned_parental_asthma$DiagnosedMom_Asthma[data_cleaned_parental_asthma$Mom_Asthma==0] <-0
#treated
data_cleaned_parental_asthma$TreatMom_Asthma[data_cleaned_parental_asthma$Mom_Asthma==0] <-0

#if dad never had astma at the 12 months then also never diagnosed  or treated or asthma
#dianosed
data_cleaned_parental_asthma$DiagnosedDad_Asthma[data_cleaned_parental_asthma$Dad_Asthma==0] <-0
#treated
data_cleaned_parental_asthma$TreatDad_Asthma[data_cleaned_parental_asthma$Dad_Asthma==0] <-0

################tables for parental asthma 
table(data_cleaned_parental_asthma$Mom_Asthma)
table(data_cleaned_parental_asthma$DiagnosedMom_Asthma)
table(data_cleaned_parental_asthma$TreatMom_Asthma)

table(data_cleaned_parental_asthma$Dad_Asthma)
table(data_cleaned_parental_asthma$DiagnosedDad_Asthma)
table(data_cleaned_parental_asthma$TreatDad_Asthma)

#####################Impute the dataset by the health five regions in MB ############################################
data_imputed_by_regions=data_cleaned_parental_asthma
names(data_imputed_by_regions)[names(data_imputed_by_regions) == 'Number'] <- 'Area'

data_imputed_by_regions[data_imputed_by_regions == 999] <- NA
data_imputed_by_regions[data_imputed_by_regions == 8888] <- NA
data_imputed_by_regions[data_imputed_by_regions == 888] <- NA

#data_imputed_by_regions <- clean_names(data_imputed_by_regions)

data_imputed_by_regions_SDO=data_imputed_by_regions%>% mutate( SDO = case_when(str_sub(Region,1,1) == "S" ~ "Southern Health",
                                       str_sub(Region,1,1) == "N" ~ "Northern Regional Health Authority",
                                       str_sub(Region,1,1) == "I" ~ "Interlake-Eastern Regional Health Authority",
                                       str_sub(Region,1,1) == "W" & str_sub(Region,2,2) != "E" ~ "Winnipeg Regional Health Authority",
                                       str_sub(Region,1,2) == "WE" ~ "Prairie Mountain Health"))

data_imputed_by_regions_SDO$SDO=as.factor(data_imputed_by_regions_SDO$SDO)

##########Mean impuation by five Health regions

# S3 method for default
#data_imputed_by_regions_SDO=na.aggregate(data_imputed_by_regions_SDO, by #=factor(data_imputed_by_regions_SDO$SDO) , FUN = function(x) mean(as.numeric(as.character(x))),na.rm = FALSE)


#data_imputed_by_regions_SDO=round_df(data_imputed_by_regions_SDO,rf="floor")
#table(data_imputed$Area)
#length(unique(data_imputed$Area))


data_imputed_by_regions_mean <- data_imputed_by_regions_SDO %>%  group_by(SDO) %>%
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)))

#data_imputed_by_regions_mean[, c(1,3,5:64)] <- sapply(data_imputed_by_regions_mean[, c(1,3,5:64)], as.numeric)

data_imputed_by_regions_mean=round_df(data_imputed_by_regions_mean,rf="floor")
#was converting to charactor
data_imputed_by_regions_mean$POSTAL=data_imputed_by_regions_SDO$POSTAL

#generate missing SES Questions by health region then generate new SES scores
SDO_Subjects=cbind(data_imputed_by_regions_SDO$SubjectNumber,data_imputed_by_regions_SDO$SDO,data_imputed_by_regions_SDO$Area)
colnames(SDO_Subjects)=c("SubjectNumber","SDO","Area")

#impute  prefer not to say response (11)

SESQuestions18WK$Income[SESQuestions18WK$Income == 11] <- NA

SESQuestions18WK_SDO=merge(SESQuestions18WK,SDO_Subjects)

SESQuestions18WK_SDO_imputed <- SESQuestions18WK_SDO %>%  group_by(SDO) %>%
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)))

SESQuestions18WK_SDO_imputed=round_df(SESQuestions18WK_SDO_imputed,rf="floor")
#create SES score 
SESQuestions18WK_SDO_imputed=SESQuestions18WK_SDO_imputed[order(SESQuestions18WK_SDO_imputed$SubjectNumber),]
SES_factors_SDO=SESQuestions18WK_SDO_imputed[,2:4]
fa=factanal(SES_factors_SDO,1,scores = "regression")


SES_Scores_SDO=cbind(SESQuestions18WK_SDO_imputed[,1:4],fa$scores)
colnames(SES_Scores_SDO)=c("SubjectNumber","MomEducation","DadEducation","Income","Score")
#replace the previous SES score by new SES scores 
data_imputed_by_regions_mean$Score=SES_Scores_SDO$Score

#recode the area identifers, remember what you changed these to for the map later on

#test <- recode(imputed_data$Area, "1:5=1:5; 7=6; 8=7; 10:12=8:10; 14:17=11:14; 19:25=15:21; 28=22; 32:33=23:24; 37:39=25:27; 47:48=28:29; 50=30; 54:60=31:37; 62:64=38:40; 71:72=41:42; 74=43; else=NA")

# imputed_SDO_coded will be coded for areas
imputed_SDO_coded=data_imputed_by_regions_mean
#imputed data in wide format areas note recoded, used for for contincency tables 
#winnipeg <25
#none needed to recode

#outside Winnipeg >25
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 27] <- 26
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 28] <- 27
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 29] <- 28

imputed_SDO_coded$Area[imputed_SDO_coded$Area == 31] <- 29
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 32] <- 30
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 33] <- 31
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 34] <- 32

imputed_SDO_coded$Area[imputed_SDO_coded$Area == 37] <- 33
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 38] <- 34
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 39] <- 35
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 40] <- 36

imputed_SDO_coded$Area[imputed_SDO_coded$Area == 43] <- 37
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 44] <- 38
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 45] <- 39

imputed_SDO_coded$Area[imputed_SDO_coded$Area == 47] <- 40
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 48] <- 41

imputed_SDO_coded$Area[imputed_SDO_coded$Area == 50] <- 42

imputed_SDO_coded$Area[imputed_SDO_coded$Area == 52] <- 43
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 53] <- 44
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 54] <- 45
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 55] <- 46
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 56] <- 47
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 57] <- 48
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 58] <- 49
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 59] <- 50
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 60] <- 51
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 61] <- 52
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 62] <- 53
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 63] <- 54
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 64] <- 55

imputed_SDO_coded$Area[imputed_SDO_coded$Area == 71] <- 56
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 72] <- 57

imputed_SDO_coded$Area[imputed_SDO_coded$Area == 74] <- 58
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 86] <- 59
imputed_SDO_coded$Area[imputed_SDO_coded$Area == 94] <- 60

imputed_SDO_coded_sorted_Area=imputed_SDO_coded[order(imputed_SDO_coded$Area),]
#table(data_imputed$Area)
#table(imputed_SDO_coded$Area)


"Severity"=c(imputed_SDO_coded$severity_3M,imputed_SDO_coded$severity_6M,imputed_SDO_coded$severity_1Y,imputed_SDO_coded$severity_18M,imputed_SDO_coded$severity_2Y,imputed_SDO_coded$severity_2HY)
Severity=as.integer(Severity)

Count=c(imputed_SDO_coded$Count_3M,imputed_SDO_coded$Count_6M,imputed_SDO_coded$Count_1Y,imputed_SDO_coded$Count_18M,imputed_SDO_coded$Count_2Y,imputed_SDO_coded$Count_2HY)

Count=as.integer(Count)

"Severity_TwoFollowUps"=c(imputed_SDO_coded$severity_3M,imputed_SDO_coded$severity_1Y)
Severity_TwoFollowUps=as.integer(Severity_TwoFollowUps)

Severity=replace(Severity, Severity==3, 1)
Severity=replace(Severity, Severity==2, 1)
table(Severity)

Severity_TwoFollowUps=replace(Severity_TwoFollowUps, Severity_TwoFollowUps==3, 1)
Severity_TwoFollowUps=replace(Severity_TwoFollowUps, Severity_TwoFollowUps==2, 1)
table(Severity_TwoFollowUps)
FurryPets=c(imputed_SDO_coded$Furrypets3M,imputed_SDO_coded$Furrypets6M,imputed_SDO_coded$Furrypets1Y,imputed_SDO_coded$Furrypets18M,imputed_SDO_coded$Furrypets2Y,imputed_SDO_coded$Furrypets2HY)



#remember don't add from imputed if intended to keep decimal places (SES)
data_long_SDO <- data.frame("SubjectNumber"=rep(imputed_SDO_coded$SubjectNumber,6),"Region"=rep(data_relaxed_cleaned$Region,6),"Area"=rep(imputed_SDO_coded$Area,6),Severity,"Asthma_Young"=rep(imputed_SDO_coded$Asthma_3Y,6),Count,"Asthma_Older"=rep(imputed_SDO_coded$Asthma_5Y,6),"Mom_wheezed"=rep(imputed_SDO_coded$Mom_Wheezed1Y,6),"Mom_smoked"=rep(imputed_SDO_coded$Mom_Smoking1Y,6),"Dad_wheezed"=rep(imputed_SDO_coded$Dad_Wheezed1Y,6),"Dad_smoked"=rep(imputed_SDO_coded$Mom_Smoking1Y,6),"Mom_Asthma"=rep(imputed_SDO_coded$Mom_Asthma,6),"DiagnosedMom_Asthma"=rep(imputed_SDO_coded$DiagnosedMom_Asthma,6),"TreatMom_Asthma"=rep(imputed_SDO_coded$TreatMom_Asthma,6),FurryPets,"Factory"=rep(imputed_SDO_coded$Factory3M,6),"Farm"=rep(imputed_SDO_coded$Farm3M,6),"Highway"=rep(imputed_SDO_coded$      Highway3M,6),"Construction"=rep(imputed_SDO_coded$Construction3M,6),"Water"=rep(imputed_SDO_coded$Water3M,6),"Dad_Asthma"=rep(imputed_SDO_coded$Dad_Asthma,6),"DiagnosedDad_Asthma"=rep(imputed_SDO_coded$DiagnosedDad_Asthma,6),"TreatDad_Asthma"=rep(imputed_SDO_coded$TreatDad_Asthma,6),"SES"=rep(imputed_SDO_coded$Score,6),"Mom_wheezed18W"=rep(imputed_SDO_coded$Mom_Wheezed18W,6),"Mom_Smoked18W"=rep(imputed_SDO_coded$Mom_Smoking18W,6),"Mom_Asthma18W"=rep(imputed_SDO_coded$Mom_Asthma18W,6))


#data_long <- data.frame("SubjectNumber"=rep(imputed_SDO_coded$SubjectNumber,6),"Region"=rep(imputed_SDO_coded$Region,6),"Area"=rep(imputed_SDO_coded$Area,6),Severity)

#simlify covaraites 

data_long_SDO$Asthma_Young=replace(data_long_SDO$Asthma_Young, data_long_SDO$Asthma_Young==2, 1)
data_long_SDO$Asthma_Older=replace(data_long_SDO$Asthma_Older, data_long_SDO$Asthma_Older==2, 1)


dataModelBuilding_SDO=list(K=60,I=690,J=6,N=4140,
                           y=Severity,
                           Count=data_long_SDO$Count,
                           TreatDadAsthma=data_long_SDO$TreatDad_Asthma,
                           TreatMomAsthma=data_long_SDO$TreatMom_Asthma,
                           DiagnosedDadAsthma=data_long_SDO$DiagnosedDad_Asthma,
                           DiagnosedMomAsthma=data_long_SDO$DiagnosedMom_Asthma,
                           MomSmoked=data_long_SDO$Mom_smoked,
                           MomSmoked18W=data_long_SDO$Mom_Smoked18W,
                           MomWheezed=data_long_SDO$Mom_wheezed,
                           MomWheezed18W=data_long_SDO$Mom_wheezed18W,
                           DadSmoked=data_long_SDO$Dad_smoked,
                           SES=data_long_SDO$SES,
                           DadAsthma=data_long_SDO$Dad_Asthma,
                           DadWheezed=data_long_SDO$Dad_wheezed,
                           MomAsthma=data_long_SDO$Mom_Asthma,
                           MomAsthma18W=data_long_SDO$Mom_Asthma18W,
                           FurryPets=data_long_SDO$FurryPets,
                           Factory=data_long_SDO$Factory,
                           Farm=data_long_SDO$Farm,
                           Highway=data_long_SDO$Highway,
                           Water=data_long_SDO$Water,
                           Contruction=data_long_SDO$Construction,
                           AsthmaYoung=data_long_SDO$Asthma_Young,
                           AsthmaOlder=data_long_SDO$Asthma_Older,
                           
                           
                           #dummy variables for individual ,follow-up and area
                           
                            
                           
                           indexI= rep(1:690,6),
                           indexJ=rep(1:6, each =690),
                           indexK=data_long_SDO$Area
                           
)
save(dataModelBuilding_SDO,file=".../dataModelBuilding_SDO.RData")

dataModelBuilding_SDO_One_Follow_up=list(K=60,I=690,
                           y=Severity[1:690],
                           Count=data_long_SDO$Count[1:690],
                           TreatDadAsthma=data_long_SDO$TreatDad_Asthma[1:690],
                           TreatMomAsthma=data_long_SDO$TreatMom_Asthma[1:690],
                           DiagnosedDadAsthma=data_long_SDO$DiagnosedDad_Asthma[1:690],
                           DiagnosedMomAsthma=data_long_SDO$DiagnosedMom_Asthma[1:690],
                           MomSmoked=data_long_SDO$Mom_smoked[1:690],
                           MomSmoked18W=data_long_SDO$Mom_Smoked18W[1:690],
                           MomWheezed=data_long_SDO$Mom_wheezed[1:690],
                           MomWheezed18W=data_long_SDO$Mom_wheezed18W[1:690],
                           DadSmoked=data_long_SDO$Dad_smoked[1:690],
                           SES=data_long_SDO$SES[1:690],
                           DadAsthma=data_long_SDO$Dad_Asthma[1:690],
                           DadWheezed=data_long_SDO$Dad_wheezed[1:690],
                           MomAsthma=data_long_SDO$Mom_Asthma[1:690],
                           MomAsthma18W=data_long_SDO$Mom_Asthma18W[1:690],
                           FurryPets=data_long_SDO$FurryPets[1:690],
                           Factory=data_long_SDO$Factory[1:690],
                           Farm=data_long_SDO$Farm[1:690],
                           Highway=data_long_SDO$Highway[1:690],
                           Water=data_long_SDO$Water[1:690],
                           Contruction=data_long_SDO$Construction[1:690],
                           AsthmaYoung=data_long_SDO$Asthma_Young[1:690],
                           AsthmaOlder=data_long_SDO$Asthma_Older[1:690],
                           
                           
                           #area
                           indexK=data_long_SDO$Area[1:690]
                           
)
save(dataModelBuilding_SDO_One_Follow_up,file=".../dataModelBuilding_SDO_One_Follow_up.RData")


dataTwoFollow_ups_SDO=list(K=60,I=690,J=2,N=1380,
   y=Severity_TwoFollowUps,
   MomAsthma=c(imputed_SDO_coded$Mom_Asthma18W,imputed_SDO_coded$Mom_Asthma),
   MomSmoked=c(imputed_SDO_coded$Mom_Smoking18W,imputed_SDO_coded$Mom_Smoking1Y),
   FurryPets=c(imputed_SDO_coded$Furrypets3M,imputed_SDO_coded$Furrypets1Y),
   Farm=c(imputed_SDO_coded$Farm3M,imputed_SDO_coded$Farm1Y),

     indexI= rep(1:690,2),
     indexJ=rep(1:2, each =690),
     indexK=rep(imputed_SDO_coded$Area,2)                 
)
save(dataTwoFollow_ups_SDO,file=".../dataTwoFollow_ups_SDO.RData")

###############Data for area level models#########################
responses=as.data.frame(cbind(dataModelBuilding_SDO_One_Follow_up$indexK,dataModelBuilding_SDO_One_Follow_up$y,dataModelBuilding_SDO_One_Follow_up$Count,rep(1,690)))
colnames(responses)=c("Area","wheezing_severity","wheezing_episodes","sample_size")
uncoded=c(27,28,29,31,32,33,34,37,38,39,40,43,44,45,47,48,50,52,53,54,55,56,57,58,59,60:64,71,72,74,86,94)
#coded=26:60

#create sample size vector and see response aggregated across areas
#don't want to sum areas thats why we subset
responses_aggregated=aggregate(subset(responses, select=-c(Area)), by=list(Area=responses$Area), FUN=sum)
colnames(responses_aggregated)=c("Area","wheezing_severity_sum","wheezing_episodes_sum","sample_size")
#responses_aggregated$Area[responses_aggregated$Area[26:60]]=uncoded
print(xtable(digits = 0,responses_aggregated),include.rownames=FALSE)
#print(xtable(digits = 0,responses_aggregated))


#environmental
environmental=as.data.frame(cbind(dataModelBuilding_SDO_One_Follow_up$indexK,dataModelBuilding_SDO_One_Follow_up$Highway,dataModelBuilding_SDO_One_Follow_up$Water,dataModelBuilding_SDO_One_Follow_up$Factory,dataModelBuilding_SDO_One_Follow_up$Farm,dataModelBuilding_SDO_One_Follow_up$Contruction))
colnames(environmental)=c("Area","Highway","Water","Factory","Farm","Construction")
environmental_aggregated=aggregate(subset(environmental, select=-c(Area)), by=list(Area=environmental$Area), FUN=sum)
colnames(environmental_aggregated)=c("Area","Highway_sum","Water_sum","Factory_sum","Farm_sum","Construction_sum")

#parental health
parental_health=as.data.frame(cbind(dataModelBuilding_SDO_One_Follow_up$indexK,dataModelBuilding_SDO_One_Follow_up$MomSmoked18W,dataModelBuilding_SDO_One_Follow_up$MomWheezed18W,dataModelBuilding_SDO_One_Follow_up$MomAsthma18W,dataModelBuilding_SDO_One_Follow_up$DadSmoked,dataModelBuilding_SDO_One_Follow_up$DadWheezed,dataModelBuilding_SDO_One_Follow_up$DadAsthma))
colnames(parental_health)=c("Area","MomSmoked18W","MomWheezed18W","MomAsthma18W","DadSmoked","DadWheezed","DadAsthma")
parental_health_aggregated=aggregate(subset(parental_health, select=-c(Area)), by=list(Area=parental_health$Area), FUN=sum)
colnames(parental_health_aggregated)=c("Area","MomSmoked18W_sum","MomWheezed18W_sum","MomAsthma18W_sum","DadSmoked_sum","DadWheezed_sum","DadAsthma_sum")

#SES 
SES_df=as.data.frame(cbind(dataModelBuilding_SDO_One_Follow_up$indexK,dataModelBuilding_SDO_One_Follow_up$SES))
colnames(SES_df)=c("Area","SES")
SES_df_aggregate=aggregate(subset(SES_df, select=-c(Area)), by=list(Area=SES_df$Area), FUN=mean)
colnames(SES_df_aggregate)=c("Area","SES_mean")

#SES area level 

response_environmental_left_join=left_join(responses_aggregated,environmental_aggregated)
res_envir_health_left_join=left_join(response_environmental_left_join,parental_health_aggregated)
final_left_join=left_join(res_envir_health_left_join,SES_df_aggregate)

#create proportions of all individual covariates except SES (since mean values are used)
area_level_dataset= final_left_join %>%
  mutate(across(c(5:15),.fns = ~./sample_size))

SES_SDO_averaged_first=aggregate(subset(SESQuestions18WK_SDO_imputed, select=-c(Area,SubjectNumber)), by=list(Area=SESQuestions18WK_SDO_imputed$Area), FUN=mean)
SES_factors_SDO_averaged_first=SES_SDO_averaged_first[,2:4]
fa_averaged_first=factanal(SES_factors_SDO_averaged_first,1,scores = "regression")
SES_SDO_area_level=cbind(1:60,SES_SDO_averaged_first[,2:4],fa_averaged_first$scores)
colnames(SES_SDO_area_level)=c("Area","MomEducation","DadEducation","Income","Score")

save(dataModelBuilding_SDO_Area_Level,file=".../Area Level Models/dataModelBuilding_SDO_Area_Level.RData")
#percentages
dataModelBuilding_SDO_Area_Level_percent=list(K=60,
                                      y_area=area_level_dataset$wheezing_severity_sum,
                                      Count_area=area_level_dataset$wheezing_episodes_sum,
                                      MomSmoked18W=area_level_dataset$MomSmoked18W_sum*100,
                                      MomWheezed18W=area_level_dataset$MomWheezed18W_sum*100,
                                      MomAsthma18W=area_level_dataset$MomAsthma18W_sum*100,
                                      DadSmoked=area_level_dataset$DadSmoked_sum*100,
                                      DadAsthma=area_level_dataset$DadAsthma_sum*100,
                                      DadWheezed=area_level_dataset$DadWheezed_sum*100,
                                      
                                      SES=area_level_dataset$SES_mean,
                                      #SES averaged first
                                      MomEducation=SES_SDO_area_level$MomEducation,
                                      DadEducation=SES_SDO_area_level$DadEducation,
                                      Income =SES_SDO_area_level$Income,
                                      SES_averaged_first= SES_SDO_area_level$Score,
                                      
                                      
                                      Factory=area_level_dataset$Factory_sum*100,
                                      Farm_3M=area_level_dataset$Farm_sum*100,
                                      Highway=area_level_dataset$Highway_sum*100,
                                      Water=area_level_dataset$Water_sum*100,
                                      Construction=area_level_dataset$Construction_sum*100,
                                      
                                      #area
                                      Area=area_level_dataset$Area,
                                      SampleSize=area_level_dataset$sample_size
                                      
)
save(dataModelBuilding_SDO_Area_Level_percent,file=".../Area Level Models/dataModelBuilding_SDO_Area_Level_percent.RData")

