# Manipulate data For POC  
  # We don't want to distract with the Canada vs. USA thing so there have been some manipulations 
    #1: Centriods for Region1 have been moved west into 4XW and Region 2 from the states in to Newfoundland 
    #2: Region names have all been changed from Canada and USA to Region 1 and Region 2 Respectively 
    #3: Distance to border
      # The Hague Line has been replaced with a theoretical administrative  border (which, based on NAFO divisions, runs between the Scotian shelf and Newfoundland ) for POC only. 
      # Distance to border values were not recalculated based on the new COG values, but trends are similar so it should be sufficient for POC.
      # HOWEVER, because the COG for Region 1 is now west of the border, and Region 2 is now east of it, 
          # the signs need to be reversed 
          # the timeline on region1 needs to be reversed to that it approaches the border, the direction for Region 2 is appropriately drifting away 
# Notes
#A. Path to new files: Data/Data_SHinyApp_Proof_of_Concept/NewData_NoCountries/_manipulated.csv
#B. path to new administrative border shapefile:  Data/Data_SHinyApp_Proof_of_Concept/NewData_NoCountries/TheorheticalBorder_Shapefile/AdministrativeBorder.shp



library(ggplot2)
library(patchwork)
library(here)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
#library(rnaturalearthhires)
library(dplyr)
library(geosphere)
library(ggpmisc)

POC_COG<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_COG.csv"))
str(POC_COG)

#COG
#1: move the centroids, put Canada in 4Xw, and USA in 3ps, 
# move COG for USA up into Newfoundland (based it off of a leading edge poing that is relatively west)
# move COG for Canada over into 4Xw
library(dplyr)

POC_COG <- POC_COG %>%
  mutate(
    centroid_longitude = case_when(
      Region == "USA"    ~ centroid_longitude + 12.28,
      Region == "Canada" ~ centroid_longitude - 2.84,
      TRUE ~ centroid_longitude
    ),
    centroid_latitude = case_when(
      Region == "USA"    ~ centroid_latitude + 2.81,
      Region == "Canada" ~ centroid_latitude - 1.40,
      TRUE ~ centroid_latitude
    )
  )

#2: replace Canada/USA with Region 1 and 2
POC_COG <- POC_COG %>%
  mutate(
    Region = case_when(
      Region == "Canada" ~ "Region1",
      Region == "USA"    ~ "Region2",
      TRUE ~ Region
    ))
#he is only using the longitude and latitude for the POC so i will take the others out 
POC_COG <- POC_COG[, 1:4]
write.csv(POC_COG,(here::here("Data/Data_SHinyApp_Proof_of_Concept/NewData_NoCountries/POC_COG_manipulated.csv")), row.names = FALSE)

#CONTINUED...all data need to have country names removed 
#Abundance
POC_Abundance<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_Abundance.csv"))
POC_Abundance <- POC_Abundance %>%
  mutate(
    Region = case_when(
      Region == "Canada" ~ "Region1",
      Region == "USA"    ~ "Region2",
      TRUE ~ Region
    ))
write.csv(POC_Abundance,(here::here("Data/Data_SHinyApp_Proof_of_Concept/NewData_NoCountries/POC_Abundance_manipulated.csv")), row.names = FALSE)

#Area Occupied
POC_AO<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_AreaOccupied.csv"))
POC_AO <- POC_AO %>%
  mutate(
    Region = case_when(
      Region == "Canada" ~ "Region1",
      Region == "USA"    ~ "Region2",
      TRUE ~ Region
    ))
write.csv(POC_AO,(here::here("Data/Data_SHinyApp_Proof_of_Concept/NewData_NoCountries/POC_AreaOccupied_manipulated.csv")), row.names = FALSE)

#Abundance weighted depth 
POC_AWD<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_AWD.csv"))
POC_AWD <- POC_AWD %>%
  mutate(
    Region = case_when(
      Region == "Canada" ~ "Region1",
      Region == "USA"    ~ "Region2",
      TRUE ~ Region
    ))
write.csv(POC_AWD,(here::here("Data/Data_SHinyApp_Proof_of_Concept/NewData_NoCountries/POC_AWD_manipulated.csv")), row.names = FALSE)


#3. distance to border
  # First, rename the Regions
  # Second, reverse the sighs
  # Third, reverse the time series for Region1
POC_DtoB<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_DtoB.csv"))
POC_DtoB <- POC_DtoB %>%
  mutate(
    Region = case_when(
      Region == "Canada" ~ "Region1",
      Region == "USA"    ~ "Region2",
      TRUE ~ Region
    ),
    across(c(Dist_Mean, Dist_Med, Dist_Q5, Dist_Q95), ~ .x * -1)
  )
POC_DtoB <- POC_DtoB %>%
  group_by(Region) %>%                       # Group by Region
  mutate(
    Dist_Mean   = if_else(Region == "Region1", rev(Dist_Mean), Dist_Mean),
    Dist_Med    = if_else(Region == "Region1", rev(Dist_Med), Dist_Med),
    Dist_Q5     = if_else(Region == "Region1", rev(Dist_Q5), Dist_Q5),
    Dist_Q95    = if_else(Region == "Region1", rev(Dist_Q95), Dist_Q95)
  ) %>%
  ungroup()

write.csv(POC_DtoB,(here::here("Data/Data_SHinyApp_Proof_of_Concept/NewData_NoCountries/POC_DtoB_manipulated.csv")), row.names = FALSE)
