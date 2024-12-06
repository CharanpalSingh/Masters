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

setwd("your_path")

load("Final_table_relaxed")
load("dataModelBuilding_SDO_Area_Level_percent.RData")
Manitoba96=read.csv(file="...\\96regions.csv", header=TRUE)

#5 health regions in Manitoba
FiveHealthRegions=read.dbf("...\\reshapefilefor5healthregions\\RHA2012.dbf")

shape = st_read("...\\shapefile\\combined.shp")
shape=merge(shape,Manitoba96)

#######################figures 3.2- 3.8 #########################################################

#replace codes with NA for mappings purposes
Final_table_relaxed_no_codes=Final_table_relaxed
Final_table_relaxed_no_codes[Final_table_relaxed_no_codes == 999] <- NA
Final_table_relaxed_no_codes[Final_table_relaxed_no_codes == 8888] <- NA
Final_table_relaxed_no_codes[Final_table_relaxed_no_codes == 888] <- NA

data=Final_table_relaxed_no_codes %>%
  select(Region,Wheezed_3M,Wheezed_6M,Wheezed_1Y,Wheezed_18M,Wheezed_2Y,Wheezed_2HY,Wheezed_3Y,Wheezed_4Y,Wheezed_5Y) %>% 
  group_by(Region) %>%
  summarise(across(everything(), ~sum(., na.rm = T))) 

data2=Final_table_relaxed_no_codes %>%
  select(Region,Wheezed_3M,Wheezed_6M,Wheezed_1Y,Wheezed_18M,Wheezed_2Y,Wheezed_2HY,Wheezed_3Y,Wheezed_4Y,Wheezed_5Y) %>% 
  group_by(Region) %>%
  count(Region)

map_data = left_join(shape,data)%>% mutate(Area_Winnipeg_3M= ifelse(Wheezed_3M > 0, as.character(Number), ""))%>%mutate(Area_3M = ifelse(Wheezed_3M > 0 & Number>25, as.character(Number), "")) %>% 
  mutate(Area_Winnipeg_6M= ifelse(Wheezed_6M > 0,as.character(Number), ""))%>%mutate(Area_6M = ifelse(Wheezed_6M > 0 & Number>25,     as.character(Number), ""))%>%
  mutate(Area_Winnipeg_1Y= ifelse(Wheezed_1Y > 0, as.character(Number), ""))%>%mutate(Area_1Y = ifelse(Wheezed_1Y > 0 & Number>25, as.character(Number), ""))%>% 
  mutate(Area_Winnipeg_18M= ifelse(Wheezed_18M > 0, as.character(Number), ""))%>%mutate(Area_18M = ifelse(Wheezed_18M > 0 & Number>25, as.character(Number), ""))%>% 
  mutate(Area_Winnipeg_2Y= ifelse(Wheezed_2Y > 0, as.character(Number), ""))%>%mutate(Area_2Y = ifelse(Wheezed_2Y > 0 & Number>25,    as.character(Number), "")) %>% 
  mutate(Area_Winnipeg_2HY= ifelse(Wheezed_2HY > 0, as.character(Number), ""))%>%mutate(Area_2HY = ifelse(Wheezed_2HY > 0 & Number>25,    as.character(Number), "")) %>% 
  mutate(Area_Winnipeg_3Y= ifelse(Wheezed_3Y > 0, as.character(Number), ""))%>%mutate(Area_3Y = ifelse(Wheezed_3Y > 0 & Number>25,    as.character(Number), "")) %>% 
  mutate(Area_Winnipeg_4Y= ifelse(Wheezed_4Y > 0, as.character(Number), ""))%>%mutate(Area_4Y = ifelse(Wheezed_4Y > 0 & Number>25,    as.character(Number), "")) %>% 
  mutate(Area_Winnipeg_5Y= ifelse(Wheezed_5Y > 0, as.character(Number), ""))%>%mutate(Area_5Y = ifelse(Wheezed_5Y > 0 & Number>25,    as.character(Number), ""))

map_data_all=left_join(shape,data2) %>% mutate(Area_Winnipeg= ifelse(n > 0, as.character(Number), ""))%>%mutate(Area = ifelse(n > 0 & Number>25, as.character(Number), ""))  

#WPG Only
map_data_Winnipeg=map_data %>% filter(str_sub(Region,1,1) == "W" & str_sub(Region,2,2) != "E" & Region != "WP21")

map_data <- st_make_valid(map_data)

map_data_all_Winnipeg=map_data_all %>% filter(str_sub(Region,1,1) == "W" & str_sub(Region,2,2) != "E" & Region != "WP21")

map_data_all <- st_make_valid(map_data_all)

tmap_options(check.and.fix = TRUE)
par(mfrow=c(1,2))

tmap_mode("plot")

#distribution of all participants
map_particpants=tm_shape(map_data_all) +
  tm_fill("n",title="All particpants" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area", size = 0.5)
  tm_layout(title = "All recurited ", title.position = c("right","bottom"))

  map_particpants
  
map_particpants_Winnipeg=tm_shape(map_data_all_Winnipeg) +
  tm_fill("n",title="" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_Winnipeg", size = 0.5)
  
  
  
map_3M=tm_shape(map_data) +
  tm_fill("Wheezed_3M",title="Wheezing counts 3 months" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_3M", size = 0.5)
  tm_layout(title = "Wheezing", title.position = c("right","bottom"))



  Winnipeg_map_3M=tm_shape(map_data_Winnipeg) +
  tm_fill("Wheezed_3M",title="" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_Winnipeg_3M", size = 0.5)
  
  
  map_6M=tm_shape(map_data) +
  tm_fill("Wheezed_6M",title="Wheezing counts 6 months" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_6M", size = 0.5)
  tm_layout(title = "Wheezing", title.position = c("right","bottom"))
  
  Winnipeg_map_6M=tm_shape(map_data_Winnipeg) +
  tm_fill("Wheezed_6M",title="" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_Winnipeg_6M", size = 0.5)
  
  
  map_1Y=tm_shape(map_data) +
  tm_fill("Wheezed_1Y",title="Wheezing counts 1 year" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_1Y", size = 0.5)
  tm_layout(title = "Wheezing", title.position = c("right","bottom"))
  
  Winnipeg_map_1Y=tm_shape(map_data_Winnipeg) +
  tm_fill("Wheezed_1Y",title="" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_Winnipeg_1Y", size = 0.5)
  
  
  
  
  map_18M=tm_shape(map_data) +
  tm_fill("Wheezed_18M",title="Wheezing counts 18 months" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_18M", size = 0.5)
  tm_layout(title = "Wheezing", title.position = c("right","bottom"))


#WPG
 Winnipeg_map_18M=tm_shape(map_data_Winnipeg) +
  tm_fill("Wheezed_18M",title="" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_Winnipeg_18M", size = 0.5)
  
 
   map_2Y=tm_shape(map_data) +
  tm_fill("Wheezed_2Y",title="Wheezing counts 2 years" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_2Y", size = 0.5)
  tm_layout(title = "Wheezing", title.position = c("right","bottom"))
  
  Winnipeg_map_2Y=tm_shape(map_data_Winnipeg) +
  tm_fill("Wheezed_2Y",title="" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_Winnipeg_2Y", size = 0.5)
  
  
   map_2HY=tm_shape(map_data) +
  tm_fill("Wheezed_2HY",title="Wheezing counts 2.5 years" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_2HY", size = 0.5)
  tm_layout(title = "Wheezing", title.position = c("right","bottom"))
  
  Winnipeg_map_2HY=tm_shape(map_data_Winnipeg) +
  tm_fill("Wheezed_2HY",title="" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_Winnipeg_2HY", size = 0.5)
  
  
   map_3Y=tm_shape(map_data) +
  tm_fill("Wheezed_3Y",title="Wheezing counts 3 years" ,n=3,style="jenks",palette =c("white","yellow","blue")) +
  tm_borders() +tm_text("Area_3Y", size = 0.5)
  tm_layout(title = "Wheezing", title.position = c("right","bottom"))
  
  Winnipeg_map_3Y=tm_shape(map_data_Winnipeg) +
  tm_fill("Wheezed_3Y",title="" ,n=3,style="jenks",palette =c("white","yellow","blue")) +
  tm_borders() +tm_text("Area_Winnipeg_3Y", size = 0.5)
  
  map_4Y=tm_shape(map_data) +
  tm_fill("Wheezed_4Y",title="Wheezing counts 4 years" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_4Y", size = 0.5)
  tm_layout(title = "Wheezing", title.position = c("right","bottom"))
  
  Winnipeg_map_4Y=tm_shape(map_data_Winnipeg) +
  tm_fill("Wheezed_4Y",title="" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_Winnipeg_4Y", size = 0.5)
  
   map_5Y=tm_shape(map_data) +
  tm_fill("Wheezed_5Y",title="Wheezing counts 5 years" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_5Y", size = 0.5)
  tm_layout(title = "Wheezing", title.position = c("right","bottom"))
  
  Winnipeg_map_5Y=tm_shape(map_data_Winnipeg) +
  tm_fill("Wheezed_5Y",title="" ,n=4,style="jenks",palette =c("white","yellow","blue","red")) +
  tm_borders() +tm_text("Area_Winnipeg_5Y", size = 0.5)
  

final_maps_all=tmap_arrange(map_particpants,  map_particpants_Winnipeg) 
tmap_save(final_maps_all,height = 1671,width=2280, filename = "C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\plots\\Area_Level_predictions\\combine_map_particpants.jpg")

final_maps_3M=tmap_arrange(map_3M,  Winnipeg_map_3M) 
tmap_save(final_maps_3M,height = 1671,width=2280, filename = "C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\plots\\Area_Level_predictions\\combine_plot_3M.jpg")

final_maps_6M=tmap_arrange(map_6M,  Winnipeg_map_6M) 
tmap_save(final_maps_6M,height = 1671,width=2280, filename = "C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\plots\\Area_Level_predictions\\combine_plot_6M.jpg")

final_maps_1Y=tmap_arrange(map_1Y,  Winnipeg_map_1Y) 
tmap_save(final_maps_1Y,height = 1671,width=2280, filename = "C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\plots\\Area_Level_predictions\\combine_plot_1Y.jpg")

final_maps_18M=tmap_arrange(map_18M,  Winnipeg_map_18M) 
tmap_save(final_maps_18M,height = 1671,width=2280, filename = "C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\plots\\Area_Level_predictions\\combine_plot_18M.jpg")

final_maps_2Y=tmap_arrange(map_2Y,  Winnipeg_map_2Y) 
tmap_save(final_maps_2Y,height = 1671,width=2280, filename = "C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\plots\\Area_Level_predictions\\combine_plot_2Y.jpg")

final_maps_2HY=tmap_arrange(map_2HY,  Winnipeg_map_2HY) 
tmap_save(final_maps_2HY,height = 1671,width=2280, filename = "C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\plots\\Area_Level_predictions\\combine_plot_2HY.jpg")

final_maps_3Y=tmap_arrange(map_3Y,  Winnipeg_map_3Y) 
tmap_save(final_maps_3Y,height = 1671,width=2280, filename = "C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\plots\\Area_Level_predictions\\combine_plot_3Y.jpg")

final_maps_4Y=tmap_arrange(map_4Y,  Winnipeg_map_4Y) 
tmap_save(final_maps_4Y,height = 1671,width=2280, filename = "C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\plots\\Area_Level_predictions\\combine_plot_4Y.jpg")

final_maps_5Y=tmap_arrange(map_5Y,  Winnipeg_map_5Y) 
tmap_save(final_maps_4Y,height = 1671,width=2280, filename = "C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis work\\plots\\Area_Level_predictions\\combine_plot_5Y.jpg")

#######################figure 5.1 #########################################################

load("...\\Model_Buildling_3_percent_with_p.RData")
uncoded_areas=c(1:25,27,28,29,31,32,33,34,37,38,39,40,43,44,45,47,48,50,52,53,54,55,56,57,58,59,60:64,71,72,74,86,94)
logit_model=summary(model_Model_Buildling_3_percent_with_p)
predictions_proportions=round(signif(logit_model$statistics,3),2)[6:65,1:2]
predictions_df=as.data.frame(cbind(uncoded_areas,predictions_proportions))
raw_proportions=dataModelBuilding_SDO_Area_Level_percent$y_area/dataModelBuilding_SDO_Area_Level_percent$SampleSize
raw_proportions_df=as.data.frame(cbind(uncoded_areas,round(signif(raw_proportions,3),2)))
row.names(raw_proportions_df)=NULL
row.names(predictions_df)=NULL
colnames(raw_proportions_df)=c("Number","proportion_raw")
colnames(predictions_df)=c("Number","p_k","SD")

#creating map data by left joining with shape file. Don't want to show WPG area numbers in MB map, create two Area columns
map_data_predictions_p = left_join(shape,predictions_df) %>% mutate(Area_Winnipeg= ifelse(p_k > 0, as.character(Number), ""))%>%mutate(Area = ifelse(p_k > 0 & Number>25, as.character(Number), ""))

#WPG Only
map_data_predictions_p_Winnipeg=map_data_predictions_p %>% filter(str_sub(Region,1,1) == "W" & str_sub(Region,2,2) != "E" & Region != "WP21")


map_predictions_p <- tm_shape(map_data_predictions_p) + 
  tm_polygons("p_k", style = "jenks", n = 7,title = "Predicted proportion of wheezing" ,palette =c("white","yellow","green","orange","blue","purple","red"),labels=c("0.02 to <0.04","0.04 to <0.07","0.07 to <0.10","0.10 to <0.14","0.14 to <0.20","0.20 to <0.31","0.31 to 0.58")) +tm_text("Area", size = 0.5)
  tm_legend(outside = TRUE)
  
   
  map_predictions_p_Winnipeg <- tm_shape(map_data_predictions_p_Winnipeg) + 
  tm_polygons("p_k", style = "fixed", breaks=c(0.04,0.07,0.10,0.14,0.19),title = "Predicted proportion of wheezing", palette =c("yellow","green","orange","blue"),labels=c("0.04 to <0.07","0.07 to <0.10","0.10 to <0.14","0.14 to 0.19")) + tm_text("Area_Winnipeg", size = 0.5)
  tm_legend(outside = TRUE)

final_maps_p=tmap_arrange(map_predictions_p, map_predictions_p_Winnipeg) 
final_maps_p
tmap_save(final_maps_p,height = 1671,width=2280, filename = "...\\map_predictions_p.jpg")


#######################figure 5.2 #########################################################


# raw proportions
map_data_raw_proportions = left_join(shape,raw_proportions_df) %>% mutate(Area_Winnipeg= ifelse(proportion_raw > 0, as.character(Number), ""))%>%mutate(Area = ifelse(proportion_raw > 0 & Number>25, as.character(Number), ""))

#WPG raw Only
map_data_raw_proportions_Winnipeg=map_data_raw_proportions %>% filter(str_sub(Region,1,1) == "W" & str_sub(Region,2,2) != "E" & Region != "WP21")


map_raw_proportions <- tm_shape(map_data_raw_proportions) + 
  tm_polygons("proportion_raw", style = "fixed", n = 6,breaks=c(0.00,0.01,0.07,0.12,0.14,0.25,0.38,1.00),palette=c("white","yellow","green","orange","blue","purple","red"),labels=c("0.00 to <0.01","0.01 to <0.07","0.07 to <0.12","0.12 to <0.14","0.14 to <0.25","0.25 to <0.38","0.38 to 1.00"),title = "Wheezing proportions") +tm_text("Area", size = 0.5)
   
  map_raw_proportions_Winnipeg <- tm_shape(map_data_raw_proportions_Winnipeg) + 
  tm_polygons("proportion_raw", style = "fixed",breaks=c(0.00,0.01,0.07,0.12,0.14,0.25,0.33), n = 6,palette=c("white","yellow","green","orange","blue","purple"),labels=c("0.00 to <0.01","0.01 to <0.07","0.07 to <0.12","0.12 to <0.14","0.14 to <0.25","0.25 to 0.33"),title = " Wheezing proportions") + tm_text("Area_Winnipeg", size = 0.5)

  map_raw_proportions_Winnipeg 

final_maps_p=tmap_arrange(map_raw_proportions, map_raw_proportions_Winnipeg) 
final_maps_p
tmap_save(final_maps_p,height = 1671,width=2280, filename = "...\map_raw_proportions.jpg")


#######################figure 5.3 #########################################################

#ZIP model
load("...\\ZIP_Model_Building_2_logit_covariate_No_SES_lambda.RData")
uncoded_areas=c(1:25,27,28,29,31,32,33,34,37,38,39,40,43,44,45,47,48,50,52,53,54,55,56,57,58,59,60:64,71,72,74,86,94)
ZIP_model=summary(model_ZIP_Model_Building_2_logit_covariate_No_SES)
predictions_lambda=round(signif(ZIP_model$statistics,3),2)[4:63,1:2]
raw_counts=dataModelBuilding_SDO_Area_Level_percent$Count_area
raw_counts_df=as.data.frame(cbind(uncoded_areas,raw_counts))
lambda_df=as.data.frame(cbind(uncoded_areas,predictions_lambda))
row.names(raw_counts_df)=NULL
row.names(lambda_df)=NULL
colnames(raw_counts_df)=c("Number","counts_raw")
colnames(lambda_df)=c("Number","lambda_k","SD")

# ZIP model map of lambda's
#creating map data by left joining with shape file. Don't want to show WPG area numbers in MB map, create two Area columns
map_data_predictions_lambda = left_join(shape,lambda_df) %>% mutate(Area_Winnipeg= ifelse(lambda_k > 0, as.character(Number), ""))%>%mutate(Area = ifelse(lambda_k > 0 & Number>25, as.character(Number), ""))

#WPG Only
map_data_predictions_lambda_Winnipeg=map_data_predictions_lambda %>% filter(str_sub(Region,1,1) == "W" & str_sub(Region,2,2) != "E" & Region != "WP21")

map_predictions_lambda <- tm_shape(map_data_predictions_lambda) + 
  tm_polygons("lambda_k", style = "jenks", n = 6,title = "Predicted episodes of wheezing" ,palette =c("white","yellow","green","orange","blue","red"),labels=c("0.53 to <2.03","2.03 to <3.37","3.37 to <4.88","4.88 to <8.37","8.37 to <13.80","13.80 to 21.50")) +tm_text("Area", size = 0.5)
  tm_legend(outside = TRUE)
  
   map_predictions_lambda
    

  map_predictions_lambda_Winnipeg <- tm_shape(map_data_predictions_lambda_Winnipeg) + 
  tm_polygons("lambda_k",  style = "fixed", breaks=c(1.24,2.03,4.88,8.37,13.80,21.50), n=4,title = "Predicted episodes of wheezing", palette =c("white","yellow","orange","blue","red"),labels=c("1.24 to <2.03","2.03 to <4.88","4.88 to <8.37","8.37 to <13.80","13.80 to 21.50")) + tm_text("Area_Winnipeg", size = 0.5)
  tm_legend(outside = TRUE)

final_maps_p=tmap_arrange(map_predictions_lambda, map_predictions_lambda_Winnipeg) 
final_maps_p
tmap_save(final_maps_p,height = 1671,width=2280, filename = "...\\map_predictions_lambda.jpg")

#######################figure 5.4 #########################################################

#raw counts map data
map_data_raw_counts = left_join(shape,raw_counts_df) %>% mutate(Area_Winnipeg= ifelse(counts_raw > 0, as.character(Number), ""))%>%mutate(Area = ifelse(counts_raw > 0 & Number>25, as.character(Number), ""))

#WPG Only
map_data_raw_counts_Winnipeg=map_data_raw_counts %>% filter(str_sub(Region,1,1) == "W" & str_sub(Region,2,2) != "E" & Region != "WP21")


map_raw_counts <- tm_shape(map_data_raw_counts) + 
  tm_polygons("counts_raw", style = "fixed",breaks=c(0,1,3,6,12,16,24),labels=c("0 to < 1","1 to < 3","3 to < 6","6 to < 12","12 to < 16","16 to 24") ,n = 6,title = "Episodes of wheezing" ,palette =c("white","yellow","green","orange","blue","red")) +tm_text("Area", size = 0.5)
  tm_legend(outside = TRUE)

  
   map_raw_counts
   
  map_raw_counts_Winnipeg <- tm_shape(map_data_raw_counts_Winnipeg) + 
  tm_polygons("counts_raw", style = "fixed",breaks=c(0,1,3,6,12,16,24),labels=c("0 to < 1","1 to < 3","3 to < 6","6 to < 12","12 to < 16","16 to 24") ,n=6,title = "Episodes of wheezing", palette =c("white","yellow","green","orange","blue","red")) + tm_text("Area_Winnipeg", size = 0.5)
  tm_legend(outside = TRUE)

  map_raw_counts_Winnipeg

final_maps_p=tmap_arrange(map_raw_counts,map_raw_counts_Winnipeg) 
final_maps_p
tmap_save(final_maps_p,height = 1671,width=2280, filename = "...\\map_raw_counts.jpg")


#######################Figure 6.1 and Table 6.1 #########################################################

Final_table_relaxed_subset=Final_table_relaxed%>%
  select(Wheezed_3M,Wheezed_6M,Wheezed_1Y,Wheezed_18M,Wheezed_2Y,Wheezed_2HY,Wheezed_3Y,Wheezed_4Y,Wheezed_5Y)%>%
  filter(Final_table_relaxed$Wheezed_3M==1)%>% tbl_summary()

Final_table_relaxed_subset_6M=Final_table_relaxed%>%
  select(Wheezed_3M,Wheezed_6M,Wheezed_1Y,Wheezed_18M,Wheezed_2Y,Wheezed_2HY,Wheezed_3Y,Wheezed_4Y,Wheezed_5Y)%>%
  filter(Final_table_relaxed$Wheezed_6M==1)%>% tbl_summary()

Final_table_relaxed_subset_1Y=Final_table_relaxed%>%
  select(Wheezed_3M,Wheezed_6M,Wheezed_1Y,Wheezed_18M,Wheezed_2Y,Wheezed_2HY,Wheezed_3Y,Wheezed_4Y,Wheezed_5Y)%>%
  filter(Final_table_relaxed$Wheezed_1Y==1)%>% tbl_summary()

Final_table_relaxed_subset_18M=Final_table_relaxed%>%
  select(Wheezed_3M,Wheezed_6M,Wheezed_1Y,Wheezed_18M,Wheezed_2Y,Wheezed_2HY,Wheezed_3Y,Wheezed_4Y,Wheezed_5Y)%>%
  filter(Final_table_relaxed$Wheezed_18M==1)%>% tbl_summary()

Final_table_relaxed_subset_2Y=Final_table_relaxed%>%
  select(Wheezed_3M,Wheezed_6M,Wheezed_1Y,Wheezed_18M,Wheezed_2Y,Wheezed_2HY,Wheezed_3Y,Wheezed_4Y,Wheezed_5Y)%>%
  filter(Final_table_relaxed$Wheezed_2Y==1)%>% tbl_summary()

Final_table_relaxed_subset_2HY=Final_table_relaxed%>%
  select(Wheezed_3M,Wheezed_6M,Wheezed_1Y,Wheezed_18M,Wheezed_2Y,Wheezed_2HY,Wheezed_3Y,Wheezed_4Y,Wheezed_5Y)%>%
  filter(Final_table_relaxed$Wheezed_2HY==1)%>% tbl_summary()

Final_table_relaxed_subset_3Y=Final_table_relaxed%>%
  select(Wheezed_3M,Wheezed_6M,Wheezed_1Y,Wheezed_18M,Wheezed_2Y,Wheezed_2HY,Wheezed_3Y,Wheezed_4Y,Wheezed_5Y)%>%
  filter(Final_table_relaxed$Wheezed_3Y==1)%>% tbl_summary()

Final_table_relaxed_subset_4Y=Final_table_relaxed%>%
  select(Wheezed_3M,Wheezed_6M,Wheezed_1Y,Wheezed_18M,Wheezed_2Y,Wheezed_2HY,Wheezed_3Y,Wheezed_4Y,Wheezed_5Y)%>%
  filter(Final_table_relaxed$Wheezed_4Y==1)%>% tbl_summary()

Final_table_relaxed_subset_all_wheezing=Final_table_relaxed%>%
  select(SubjectNumber,Wheezed_3M,Wheezed_6M,Wheezed_1Y,Wheezed_18M,Wheezed_2Y,Wheezed_2HY,Wheezed_3Y,Wheezed_4Y,Wheezed_5Y)%>%
  filter(if_all(contains("Wheezed"),~ .== 1) )



df=data.frame(follow_up=factor(c("3 months","6 months","1 year","18 months","2 years","2.5 years","3 years","4 years","5 years")),wheezing_3M=c(63,17,17,17,19,14,12,11,11),wheezing_6M=c(NA,62,21,16,15,10,8,9,9),
              wheezing_1Y=c(NA,NA,73,20,19,16,15,10,14),wheezing_18M=c(NA,NA,NA,67,23,20,12,16,12),
              wheezing_2Y=c(NA,NA,NA,NA,78,24,18,18,16),wheezing_2HY=c(NA,NA,NA,NA,NA,58,14,16,13),
              wheezing_3Y=c(NA,NA,NA,NA,NA,NA,38,11,11),wheezing_4Y=c(NA,NA,NA,NA,NA,NA,NA,55,15))

wheezing_trend_plot=ggplot(df, aes(x=fct_inorder(follow_up),group=1))  + labs(x = "Follow-up after birth",y="Children wheezing episodes")+
  geom_line(aes(y = wheezing_3M,colour="3 months")) +geom_line(aes(y = wheezing_6M), colour = "6 months")+
  geom_line(aes(y = wheezing_1Y,colour = "1 year"))+
  geom_line(aes(y = wheezing_18M,colour = "18 months"))+
   geom_line(aes(y = wheezing_2Y, colour = "2 years"))+geom_line(aes(y = wheezing_2HY,colour = "2.5 years"))+
   geom_line(aes(y = wheezing_3Y,colour = "3 years"))+geom_line(aes(y = wheezing_4Y,colour = "4 years"))+
  scale_color_manual(name = "Child began wheezing at:", 
                     values = c("3 months" = "blue", "6 months" = "red","1 year" = "yellow","18 months" = "green",
                                "2 years"="purple","2.5 years"="orange","3 years"="black","4 years"="brown"))


ggsave(wheezing_trend_plot,height = 1671,width=2280,units = "px", filename = "...\\wheezing_trends.jpg")
