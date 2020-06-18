library(dplyr)
library(tidyr)
library(sf)
library(readxl)
library(tools)


setwd("/Users/chengren/Documents/GitHub/Cov-19")

# Source code to jitter any duplicate coords (locations)
source("0_geoR_jitter.R")
setwd("/Users/chengren/Documents/GitHub/Cov-19/Data_input")
df <- read.csv('SITE_HCC_FCT_DET.csv')
# only for migrant
#df_mg <- df%>%filter(Migrant.Health.Centers.HRSA.Grant.Subprogram.Indicator == "Y")
health_clinics <- df
health_clinics$ID <- 1:nrow(health_clinics)
health_clinics$Site.Name <- toTitleCase(tolower(as.character(health_clinics$Site.Name)))
health_clinics$Site.Name <- gsub('Gvhc|Ghvc', 'GVHC', health_clinics$Site.Name)
health_clinics$Site.Name <- gsub('Csvs', 'CSVS', health_clinics$Site.Name)
health_clinics$Site.Name <- gsub('Chcrr', 'CHCRR', health_clinics$Site.Name)
colnames(health_clinics) <- gsub("\\.\\.\\.\\.|\\.\\.", ".", colnames(health_clinics))
health_clinics[c("Geocoding.Artifact.Address.Primary.X.Coordinate","Geocoding.Artifact.Address.Primary.Y.Coordinate")] <- jitterDupCoords(health_clinics[c("Geocoding.Artifact.Address.Primary.X.Coordinate",
                                                                                                                                                           "Geocoding.Artifact.Address.Primary.Y.Coordinate")], 
                                                                                                                                          min=0.00001, max=0.00009)
health_clinics$full_address <- paste0(health_clinics$Site.Address, ", ", 
                                      health_clinics$Site.City, ", ",
                                      health_clinics$Site.State.Abbreviation, " ", 
                                      health_clinics$Site.Postal.Code)
###################################
# Reformat to create website URL
####################################

health_clinics$website <- health_clinics$Site.Web.Address
health_clinics$website <- gsub('None|NA|N/A','',health_clinics$website)
health_clinics$website <- ifelse(substring(health_clinics$website, 1, 4) == "http"|
                                   health_clinics$website=="", 
                                 health_clinics$website, paste0("http://", health_clinics$website))
health_clinics$website <- tolower(health_clinics$website)
health_clinics$website[grep("\\bGVHC",health_clinics$Site.Name)]<- "http://www.gvhc.org"
health_clinics$website[grep("\\bnot apply|\\bnot applicable",health_clinics$website)]<- ""
# Format Zip Codes
health_clinics$zipcode <- substr(health_clinics$Site.Postal.Code, 1, 5)
#get county code
table(health_clinics$County.or.County.Equivalent.Federal.Information.Processing.Standard.Code)
health_keep_cols <- c(
  "ID",                      # need a unique id
  "Site.Name",
  "full_address",
  "Site.Telephone.Number",
  "Federally.Qualified.Health.Center.FQHC.Look.Alike.Organization.Site.Administrator.Contact.Email.Address",
  "website",
  #"Mission.Statement",
  "Geocoding.Artifact.Address.Primary.X.Coordinate",
  "Geocoding.Artifact.Address.Primary.Y.Coordinate",
  #"health_services",
  #"language_filter",
  #"service_access",
  #"Site.City",
  "zipcode",
  # "Migrant.Health.Centers.HRSA.Grant.Subprogram.Indicator",
  # "Serving.Uninsured",
  "Operating.Hours.per.Week"
) 
health_clinics <- health_clinics[c(health_keep_cols)]
# Rename the columns
colnames(health_clinics) <- c("ID", "org", "address", "phone", "email", "website", #"mission",
                              "lon", "lat", #"services", "language_support", "service_access", #"city", 
                              "zipcode","capacity_hours"
                              #"HRSA", "uninsured", 
)
round(colSums(is.na(health_clinics))/nrow(health_clinics)*100,2)
#drop lon is NA
health_clinics <- health_clinics[!is.na(health_clinics$lon), ]
round(colSums(is.na(health_clinics))/nrow(health_clinics)*100,2)
#clinics_preload <- health_clinics
#clinics_preload$type <- "health"
#write out
setwd("/Users/chengren/Documents/GitHub/Cov-19/Data_output")
write.csv(health_clinics,"1_health_clinics_national.csv",row.names = FALSE)


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
################
#Do NOT RUN FOLLOW!!!!!!!!!!!!!!!!!!!!!
#if you are not combine this to final data
###############
# Read in the census data
#census_data <- read_sf("./preprocess_census_data/msi_tractdata_acs2017.shp")


load("~/Downloads/bimi_msi-health_clinic_national/PREPROCESS_SHINY_DATA/preprocess_census_data/census_tract_data_processed.Rdata")

# # Simplify the tract polygons, while preserving the topology for close-up view 
##census_data <- st_transform(census_data, crs = "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# census_data <- st_simplify(census_data, preserveTopology = T, dTolerance = 100)
##census_data <- st_transform(census_data, crs = "+proj=longlat +datum=WGS84")

# Make clinics an sf object with same crs as census data
clinics_preload <- clinics_preload %>% 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(census_data), remove = F)

st_crs(census_data) == st_crs(clinics_preload)

# get the census vars for each clinic and join it to the clinics data
clinics_preload <- st_join(clinics_preload, census_data)

# Remove any clinics with no GEOID - that means it is not in our counties of interest
clinics_preload <- clinics_preload[!is.na(clinics_preload$GEOID),]

# Set any NAs in the clinics data to an empty string so it displays nice
clinics_preload <- clinics_preload %>% mutate_if(is.character, ~replace_na(., ""))

# Update filter lists since some clinics may have been filtered out
## NOTE: we should just copy the above section to here and run once!
#health_language_support <- capitalize(unique(unlist(strsplit(trimws(clinics_preload[clinics_preload$type=='health',]$language_support), split = "\\, | & ")))) ## Revised to include , "/" delimited compound values
#legal_language_support <-  capitalize(unique(unlist(strsplit(trimws(clinics_preload[clinics_preload$type=='legal',]$language_support), split = "\\, | & "))))  ## Revised to include , "/" delimited compound values

#health_services <- capitalize(unique(unlist(strsplit(clinics_preload[clinics_preload$type=='health',]$services, split = "\\, | & "))))                      ## OK
#legal_services <- capitalize(trimws(unique(unlist(strsplit(trimws(clinics_preload[clinics_preload$type=='legal',]$services), split = "\\, | & ")))))        ## OK

#health_service_access <- capitalize(unique(unlist(strsplit(trimws(clinics_preload[clinics_preload$type=='health',]$service_access), split = "\\, | & "))))   ## OK
#legal_service_access <- capitalize(unique(unlist(strsplit(trimws(clinics_preload[clinics_preload$type=='legal',]$service_access), split = "\\, | & "))))     ## OK

#######################################################
# CLEAN UP - all but clinics_preload and select lists
#######################################################
# Clean the environment by keeping only the relevant inputs for the interactive app

keep_objects <- c("census_data","clinics_preload")
                  #"health_language_support","health_service_access", "health_services", 
                 # "legal_language_support", "legal_service_access","legal_services")

rm(list = setdiff(ls(), keep_objects))
####################################
# SAVE DATA FOR CENSUS APP
####################################
# Save the environment to be read when app starts
save.image("/Users/chengren/Documents/GitHub/bimi_msi-health_clinic_national/CENSUS/census_app_data_processed.Rdata")

# Remove all objects
rm(list = ls())
