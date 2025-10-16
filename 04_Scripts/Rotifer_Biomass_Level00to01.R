## LOAD PACKAGES ================================================

library(dplyr)         # manipulating data
library(data.table)    # for fread() - super fast load-in of data
library(tidyr)         # for replace_na()
library(readxl)        # reading Excel data
library(magrittr)      # for %<>% - doble pipe function, reads and saves from/to same object
library(readxl)
library(tidyverse)

## CLEAR ENVIRONMENT ============================================

rm(list = ls(all.names=TRUE))



#read in the ZEMS excel data sheet, assuming the data are in "Sheet1"
my_data<-(read.csv("01_Level00_Data/02_rotifer_measure/LT1_S1_09_08_2023.csv"))

#keep only the needed data columns
new_data<-my_data[c(1:3,5,8,30:33)]

#calculate the expansion coefficient
new_data$subSampleExpansionCoef <-new_data$subSampleTotal/new_data$amountSubSampled


#change date back to m/d/y format
new_dateCollect=format(as.POSIXct(new_data$dateCollect, format = "%Y_%m_%d"), format = "%m/%d/%y")
new_data$dateCollect<-new_dateCollect
#new_processDate=format(as.POSIXct(new_data$processDate, format = "%Y-%m-%d"), format = "%m/%d/%y")
#new_data$processDate <- new_processDate

#rename the columns for the analysis part of R script below
colnames(new_data)<-c("amountSubSampled_ml","subSampleTotal_ml","subSampleExpansionCoef","sampleID",
                      "dateCollect","species",
                      "organismCount","length_mm","width_mm")

#put the length measurements as numbers instead of characters
new_data$length_mm<-as.numeric(new_data$length_mm)
new_data$width_mm<-as.numeric(new_data$width_mm)





#sort the data by species and then length
new_data<-new_data[order(new_data$species,new_data$length_mm),]

#This indicates if we have any missing length values, and if so, which species#
#make note in QAQC notes and change later in BBEdit
new_data$species[is.na(new_data$length_mm)]

#change working directory to where want to save the QAQC file as CSV
setwd("02_Level02_Data/Rotifer_Biomas") #change this to the directory where your dataset is saved 
#create character string of the date sample was collected to put in the CSV filename
junkdate<-format(as.POSIXct(new_data$dateCollect[1], format = "%m/%d/%y"), format = "%d%b%Y")
#write the CSV file
write.csv(new_data,file = paste0("SHEZOOFUNS_Rotifer_", unique(new_data$sampleID),"_",junkdate,"_HMC",".csv"),row.names=FALSE, quote=FALSE)




####THIS SECTION CALCULATED DENSITY, BIOMASS, AND L-F DISTRIBUTIONS OF ALL SAMPLES
## CLEAR ENVIRONMENT ============================================

rm(list = ls(all.names=TRUE))

## Make a list of all the files in the folder for loop to read
datafiles <- list.files(path = "/Users/Hanna/OneDrive/Internship_RUB/SHEZOOFUNS Rotifer Samples", pattern = "SHEZOOFUNS_Rotifer_.*csv", full.names = TRUE)

## Now use your list to read in each data file
ZPData <- do.call(rbind, lapply(datafiles, function(i) {
  ## print filename in console
  print(i)
  
  ## load data and combine into data frame using do.call()
  fread(file = i, fill=TRUE)
}))


library(stringr)         # to use the str_replace_all function
#Rename other taxa to match names in length-mass file used later

ZPData$species<-str_replace_all(ZPData$species,'Calanoid copepodid','Calanoid Copepodite')
ZPData$species<-str_replace_all(ZPData$species,'Cyclopoid copepodid','Cyclopoid Copepodite')

##change from um to mm 
##ZPData <- ZPData %>% mutate(length_mm = (length_mm/1000), width_mm = (width_mm/1000)) 


## For lengths measured by Victoria Giacchino, they were incorrectly measured as total length (to end of setae)
## To correct this, Rosie Chapina measured total length and correct length for each of the major species
## This section brings in the regression coefficients (species,intercept,slope) from those data and applies them to 
## the total lengths to generate the correct lengths to be used for mass estimates
## NOTE THIS SHOULD ONLY BE DONE FOR THOSE SAMPLES PROCESSED BY GIACCHINO
#length_coef<-read.csv("Length_Corr_Coef_Superior2018.csv")
#length_coef$species<-as.character(length_coef$species)
#ZPData <- left_join(ZPData, length_coef) %>% mutate(corr_length = intercept+slope*length_mm)
# Remove unnecessary columns
#ZPData <- ZPData%>%mutate(slope=NULL,intercept=NULL,sex=NULL,length_mm=NULL)
# Rename the corrected length to "length_mm" to avoid confusion later
#setnames(ZPData,"corr_length","length_mm")


#identification of limnocalanus and limnocalanus copepodites was inconsistent. L-F graph
#show two distinct groups with a wide gap between. Conway (1977) suggests 1.8 mm as a threshold
#between CV/CVI and < CIV copepodite stages, which lines up well with our distribution
#therefore, this next part searches for all cases where "Limnocalanus copepodites" were >= 1.8 mm
#and renames them "Limnocalanus", and searches for all cases where "Limnocalanus" was < 1.8 mm
#and renames them "Limnocalanus copepodites"

#ZPData <- ZPData %>%
#mutate(species = ifelse(species=="Limnocalanus Copepodite" & length_mm >= 1.8, "Limnocalanus", species))

#ZPData <- ZPData %>%
#mutate(species = ifelse(species=="Limnocalanus" & length_mm < 1.8, "Limnocalanus Copepodite", species))

##Exclude any Keratella- they have a different formula
#ZPData<-ZPData[which(ZPData$species != "Keratella"),]

## Establish the length bins for length-frequency distribution
breaks = seq(0,3.0,by=0.2)





## CALCULATE INDIVIDUAL DRY WEIGHT ==============================

## Notes:
##  Calculate biomass per individual using L:W equations 
##  This follows the formula "= [(length3 x FF) + (%BV x length3 x FF)] x 10-6 x WW : DW" 
##  where g = biomass of individual 
##  Length = total length in m
##  FF = species specific formula factor (see Appendix 2)
##  % BV = volume of appendages as a percent of body biovolume (see Appendix 2)
##  10-6 = conversion to wet weight; assuming a density of 1
##  WW:DW = wet weight to dry weight conversion (see 8.2.2) 
##  See EPA SOP 2016

## Load LW coefficeints
lw <- read.csv("C:/Users/Hanna/OneDrive/Internship_RUB/rotifer_LM_table.csv", header = TRUE)

## Join LW coefficents to resepctive species
zoop.lw <- left_join(ZPData, lw, by = "species")

## Calculate individual dry weight using ln(length), ln(A), and B.

zoop.lw <- zoop.lw %>% 
  mutate(ind_dry_wt_ug= case_when(
    group == "Trichocerca" ~ (((length_mm * width_mm * FF) + (percent_BV * length_mm * width_mm * FF)) * 1e-6 * WWDW),
    group == "Rotifera" ~ ((length_mm^3 * FF + (percent_BV * length_mm^3 * FF)) * 1e-6 * WWDW),
    group == "Rotifera_2" ~ (FF* (length_mm^3)),
    TRUE ~ NA_real_))

## Summarize by samplID and species
sum.sample.species <- zoop.lw %>% group_by(sampleID, species) %>%
  mutate(n.measured = length(length_mm[!is.na(length_mm)])) %>%
  summarise(
    #mesh.um=mean(netMesh_um),
    #netradius.m=mean(netDiameter_m)/2, 
    #towdepth.m=mean(towDepthStart_m)-mean(towDepthEnd_m),
    volume.L= 7, 
    n.counted = sum(organismCount), n.measured  = mean(n.measured),
    mean.ind.dry.wt.ug   = mean(ind_dry_wt_ug, na.rm = TRUE),
    sd.ind.dry.wt.ug     = sd(ind_dry_wt_ug, na.rm = TRUE),
    mean.ind.length.mm  = mean(length_mm, na.rm = TRUE),
    sd.ind.length.mm   = sd(length_mm, na.rm = TRUE),
    density.L=n.counted*mean(subSampleExpansionCoef)/volume.L,
    biom.dry.ug.L=density.L*mean.ind.dry.wt.ug)

## Nauplii were not measured. So assume their mean body mass is 0.2 ug dry.
## Fill in this value in table, and multiply by density to estimate biomass
## If nauplii lengths were recorded, do not do this next step
#sum.sample.species$mean.ind.dry.wt.ug[sum.sample.species$species=="Nauplii"]=0.2
#sum.sample.species$biom.dry.ug.L[sum.sample.species$species=="Nauplii"]=
#sum.sample.species$mean.ind.dry.wt.ug[sum.sample.species$species=="Nauplii"]*
#sum.sample.species$density.L[sum.sample.species$species=="Nauplii"]
## End of fix for no nauplii lengths

length.freq<-zoop.lw %>% group_by(sampleID,species) %>% summarise(n=n()) 

zoop.summary <- left_join(sum.sample.species, length.freq)

#write.csv(zoop.summary %>% select(-netradius.m,-volume.L), "/Users/jdstockw/Desktop/UVM/LS Cisco Research/Zooplankton - Yule/Superior Zoop Data 2018/Superior Zoops 2018 R Code/SuperiorZoop_Yule_2018.csv", row.names = F)
write.csv(zoop.summary %>% select(-volume.L), "/Users/Hanna/OneDrive/Internship_RUB/R-Output_rot.csv", row.names = F, quote = T)

write.csv(ZPData,"C:/Users/Hanna/OneDrive/Internship_RUB/Correct_Output_rot.csv", row.names = F, quote = F)