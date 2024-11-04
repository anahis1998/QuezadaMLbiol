install.packages("rgbif") #for downloading the data from GBIF
library(rgbif)

# Establishing working directory
getwd()
setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitWorkflows")

# Set up species and country filters
myspecies <- "Scalesia"
country <- "ECU" 
species_key <- name_backbone(name = "Scalesia")$usageKey

# Submit a download request for occurrences of this species
download_request <- occ_download(pred("taxonKey", species_key), 
                                 format = "SIMPLE_CSV", 
                                 pred_in("gadm",GADM_ids),
                                 pred("hasCoordinate", TRUE),
                                 pred("hasGeospatialIssue", FALSE),
                                 user = "anahi1998.",
                                 pwd = "m@ripos@2024",
                                 email = "anahiquezada@ku.edu"
)

d <- occ_download_get('0020425-241024112534372') %>%
  occ_download_import()
write.csv(d, "scalesia.csv", row.names = FALSE)

f <- occ_download_get('0020431-241024112534372')%>%
  occ_download_import()
write.csv(f, "scalesia1.csv", row.names=FALSE)
library(dplyr)
library(ggplot2)
head(d)
str(d)
ggplot(f, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_point() +
  labs(title = "Scalesia pedunculata Occurrences",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()
nrow(d)
