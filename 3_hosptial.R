library(dplyr)
library(tidyr)
library(sf)
library(readxl)
library(tools)


setwd("/Users/chengren/Documents/GitHub/Cov-19/")

# Source code to jitter any duplicate coords (locations)
source("0_geoR_jitter.R")
setwd("/Users/chengren/Documents/GitHub/Cov-19/Data_input")
df <- read.csv('Hospitals.csv')
# only for migrant
#df_mg <- df%>%filter(Migrant.Health.Centers.HRSA.Grant.Subprogram.Indicator == "Y")
hospitals <- df
hospitals$id <- 1:nrow(hospitals)
hospitals$NAME <-  toTitleCase(tolower(as.character(hospitals$NAME)))
hospitals$TYPE <-  toTitleCase(tolower(as.character(hospitals$TYPE)))
hospitals[c("X","Y")] <- jitterDupCoords(hospitals[c("X","Y")],min=0.00001, max=0.00009)
 
hospitals$ADDRESS<-  toTitleCase(tolower(as.character(hospitals$ADDRESS)))
hospitals$CITY<-  toTitleCase(tolower(as.character(hospitals$CITY)))    
hospitals$full_address <- paste0(hospitals$ADDRESS, ", ", 
                                      hospitals$CITY, ", ",
                                      hospitals$STATE, " ", 
                                      hospitals$ZIP)
hospitals$full_address<-  toTitleCase(as.character(hospitals$full_address))
###################################
# Reformat to create website URL
####################################

hospitals$website <- hospitals$WEBSITE
hospitals$website <- gsub('None|NA|N/A|NOT AVAILABLE','',hospitals$website)
hospitals$website <- ifelse(substring(hospitals$website, 1, 4) == "http"|
                                   hospitals$website=="", 
                                 hospitals$website, paste0("http://", hospitals$website))
###################################
# telephone
####################################
hospitals$telephone <- hospitals$TELEPHONE
hospitals$telephone <- gsub('None|NA|N/A|NOT AVAILABLE','',hospitals$telephone)
###################################
# Format Zip Codes
##################################### 
hospitals$zipcode <- substr(hospitals$ZIP, 1, 5)

health_keep_cols <- c(
  "id",                      # need a unique id
  "NAME",
  "full_address",
  "telephone",
  #"Federally.Qualified.Health.Center.FQHC.Look.Alike.Organization.Site.Administrator.Contact.Email.Address",
  "website",
  #"Mission.Statement",
  "X",
  "Y",
  #"health_services",
  #"language_filter",
  #"service_access",
  #"Site.City",
  "zipcode",
  # "Migrant.Health.Centers.HRSA.Grant.Subprogram.Indicator",
  # "Serving.Uninsured",
  # "Capacity.Hours.of.Operation.per.Week"
  "BEDS",
  "TYPE"
) 
hospitals <- hospitals[c(health_keep_cols)]
# Rename the columns
colnames(hospitals) <- c("ID", "org", "address", "phone", "website", #"mission",
                              "lon", "lat", #"services", "language_support", "service_access", #"city", 
                              "zipcode","beds_capacity","type"
                              #"HRSA", "uninsured", "capacity_hours"
)
round(colSums(is.na(hospitals))/nrow(hospitals)*100,2)
#drop lon is NA
hospitals <- hospitals[!is.na(hospitals$lon), ]
round(colSums(is.na(hospitals))/nrow(hospitals)*100,2)
#write out
setwd("/Users/chengren/Documents/GitHub/Cov-19/Data_output")
write.csv(hospitals,"3_hospitals_national.csv",row.names = FALSE)


