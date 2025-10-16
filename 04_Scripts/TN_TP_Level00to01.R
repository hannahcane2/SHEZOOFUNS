
##Created 10/16/2025 by HMC##
##Initial Organization of TNTP Data from Nutrient Data Sheet###  


##Load packages
library(dplyr)         # manipulating data
library(data.table)    # for fread() - super fast load-in of data
library(tidyr)         # for replace_na()
library(readxl)        # reading Excel data
library(magrittr)      # for %<>% - doble pipe function, reads and saves from/to same object
library(readxl)
library(tidyverse)



# Set the path to the folder (adjust as needed)
folder_path <- "/git_hub/SHEZOOFUNS/SHEZOOFUNS/01_Level00_Data/03_TNTP"   

# List all CSV files in that folder
file_list <- list.files(path = folder_path, pattern = ".csv", full.names = TRUE)

# Read them all into a list
data_list <- lapply(file_list, read.csv)
  
##Merge data based on sample number 
merged_data <- Reduce(function(x, y) merge(x, y, by = "Sample_Number", all = FALSE), data_list)

# Check results
head(merged_data)
##Merge data based on sample number 
merged_data <- Reduce(function(x, y) merge(x, y, by = "Sample_Number", all = FALSE), data_list)

# Check results
head(merged_data)

##only selecting columns that are needed
TNTP_data <- merged_data %>%
  select(L_Tron, T_Ment, Week, "Date..DD.MM.YYYY.", "Nitrogen..Results.mg.N.liter.", "Phosphate..Results.mg.P.Liter.")

##Changing names of columns
TNTP_data <- rename(merged_data,
                    Nitrogen_mgpL = "Nitrogen..Results.mg.N.liter.",
                    Phosphate_mgpL = "Phosphate..Results.mg.P.Liter.",
                    Date = "Date..DD.MM.YYYY.")


write.csv(TNTP_data, "git_hub/SHEZOOFUNS/01_Level02_Data/TNTP_data.csv", row.names = FALSE)

ggplot(TNTP_data, aes(x = Week, y = Nitrogen_mgpL, color = T_Ment, group = T_Ment)) +
  geom_point() +      # points for each measurement
  labs(
    x = "Week",
    y = "Total Nitrogen (mg/L)",
    color = "Treatment"
  ) +
  theme_minimal()
