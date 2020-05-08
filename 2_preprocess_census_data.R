#
# This file is part of the code for the BIMI Spatial Inequities Shinydashboard Clinics+ Census data web mapper.
# This should be the first of 2 R files run to prepare the data for the mapper.
# This code:
#  1. fetches census data for the all tract based on variables in the input file "msi_cenvars_lut.csv"
#  2. reformats the census vars that are returned
#  3  creates new vars for the mapper
#  4 saves the output to an Rdata file that is then read in by the file "process.R"
#
# Authors: Patty Frontiera (pattyf@berkeley.edu) & Denys Dukhovnov (denys_dukhovnov@berkeley.edu) 
#

library(tidycensus)
library(tidyr)
library(dplyr)
library(sf)
library(stringr)

# SET WD
setwd("/Users/chengren/Documents/GitHub/Cov-19")

#Set Options
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Load census key - Patty's key, apply for your own at: 
my_census_api_key <- "f2d6f4f743545d3a42a67412b05935dc7712c432"

# Make tidycensus aware of census key
census_api_key(my_census_api_key)

# Identify county or counties of interest
#my_counties <- c("001", "075", "013", "041", "055", "081", "085", "095", "097") 
# Alameda, SF, Contra Costa, Marin County, Napa, 
# San Mateo, Santa Clara,  Solano,  Sonoma, 
# Removing santa cruz ("087"")
states <- c('AL','AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 'GA')
states1<- c('HI','IA', 'KS', 'KY', 'LA', 'ME', 'MD')
states2 <- c('MA', 'MI',  'MN', 'MS', 'MO', 'MT', 'NE', 'NV','NH')           
states3 <- c('NJ', 'NM', 'NY', 'NC', 'ND','OH', 'OK', 'OR', 'PA')
states4 <- c('PR','RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA','WA', 'WV', 'WI', 'WY')
# Identify census variable table names
## Take a look at the data frame to find the names of census tables
## This will give you the table and variable codes
## Use "View(cenvar_table)" and filter within the table to get the codes for a specific variable
## uncomment to explore
#cenvar_table <-load_variables(year=2017, dataset = "acs5", cache=T)
#View(cenvar_table) 
# For details see the google doc named: MSI - ACS Tables and Measures

# Load cenvar lookup table of vars of interest
#my_cenvar_df <-read.csv("msi_acs_var_lookup.csv", strip.white = T, stringsAsFactors = F)
my_cenvar_df <-read.csv("Data_input/msi_cenvars_lut.csv", strip.white = T, stringsAsFactors = F)

# Fetch the ACS data
ct_data <- get_acs(geography = "county", 
                         variables = my_cenvar_df$my_cen_vars, 
                         year=2018, 
                         survey="acs5",
                         state = states, 
                         #county = my_counties, 
                         geometry = T,
                         keep_geo_vars = T)
ct_data1 <- get_acs(geography = "county", 
                      variables = my_cenvar_df$my_cen_vars, 
                      year=2018, 
                      survey="acs5",
                      state = states1, 
                      #county = my_counties, 
                      geometry = T,
                      keep_geo_vars = T)
ct_data2 <- get_acs(geography = "county", 
                       variables = my_cenvar_df$my_cen_vars, 
                       year=2018, 
                       survey="acs5",
                       state = states2, 
                       #county = my_counties, 
                       geometry = T,
                       keep_geo_vars = T)
ct_data3 <- get_acs(geography = "county", 
                       variables = my_cenvar_df$my_cen_vars, 
                       year=2018, 
                       survey="acs5",
                       state = states3, 
                       #county = my_counties, 
                       geometry = T,
                       keep_geo_vars = T)
ct_data4 <- get_acs(geography = "county", 
                       variables = my_cenvar_df$my_cen_vars, 
                       year=2018, 
                       survey="acs5",
                       state = states4, 
                       #county = my_counties, 
                       geometry = T,
                       keep_geo_vars = T)
ct_data5 <- get_acs(geography = "county", 
                       variables = my_cenvar_df$my_cen_vars, 
                       year=2018, 
                       survey="acs5",
                       state = c('ID','IL'), 
                       #county = my_counties, 
                       geometry = T,
                       keep_geo_vars = T)# do not know why 'ID'/ILis different from others 
ct_data6 <- get_acs(geography = "county", 
                       variables = my_cenvar_df$my_cen_vars, 
                       year=2018, 
                       survey="acs5",
                       state = c('IN'), 
                       #county = my_counties, 
                       geometry = T,
                       keep_geo_vars = T)#
# Reformat the data table so that 
# (1) we only keep the columns of interest - including NAME, GEOID and geometry
# (2) each estimate variable is in its own column
# ORDER MATTERS - infile must have cenvars in cenvar order!!!!
ct_data_c <- rbind(ct_data,ct_data1,
                      ct_data2,ct_data3,
                      ct_data4,ct_data5,
                      ct_data6)
#tract_data_c <- rbind(tract_data,tract_data1,
#                      tract_data2,tract_data3,
#                      tract_data4,tract_data5,
#                      tract_data6)
tract_data_c1 <- ct_data_c %>%
  select("NAME.y","GEOID","ALAND","variable","estimate") %>%
  spread(key=variable, value=estimate)

#Remove rows with no land are
tract_data_c1  <- tract_data_c1 [!is.na(tract_data_c1 $ALAND),]

# Copy for debugging
tract_data_c2 <- tract_data_c1 
# rename columns   
colnames(tract_data_c2) <- c("NAME","GEOID", "land_area",my_cenvar_df$my_cen_var_names,"geometry")
#tract_data_c2$NAME <- gsub(", California","",tract_data2$NAME)

# Remove rows with no people
tract_data_c2 <- tract_data_c2[tract_data_c2$tract_totpop!=0,]

# Change units for land_area from sq meters to sq km
tract_data_c2$p_land_areakm2 <- tract_data_c2$land_area / (1000 * 1000)
tract_data_c2$p_land_acreage <- tract_data_c2$land_area / 4046.856
tract_data_c2$p_land_sqmiles <- tract_data_c2$land_area / 2589988.1103

#tfix <- tract_data2[tract_data2$GEOID == "06041122000",]
# Create Calculated columns
## Note: keeping colnames short due to shapefile limits
tract_data_c3 <- tract_data_c2 %>%
  mutate (
    # Immigrant noncitizens in the total pop
    p_noncit_totpop = tract_totpop,
    p_noncit_count = tract_fborn_noncit,
    p_noncit = ifelse(tract_totpop==0, 0, round(100 * (tract_fborn_noncit/tract_totpop),1)),
    p_noncit_density = ifelse(tract_totpop==0, 0, round((tract_fborn_noncit / p_land_sqmiles),1)),
    
    # Immigrants in the total pop
    p_fborn_totpop = tract_totpop,
    p_fborn_count = tract_fborn,
    p_fborn =   ifelse(tract_totpop==0, 0, round(100 * (tract_fborn / tract_totpop), 1)),
    p_fborn_density =  ifelse(tract_totpop==0, 0, round((tract_fborn / p_land_sqmiles),1)),
    
    # Recent Immigrants in the total pop
    p_recent_totpop = tract_totpop,
    p_recent_count = tract_post2010_fborn,
    p_recent =  ifelse(tract_totpop==0, 0, round(100 * (tract_post2010_fborn / tract_totpop),1)),
    p_recent_density = ifelse(tract_totpop==0, 0, round((tract_post2010_fborn / p_land_sqmiles),1)),
    
    # Immigrants living 150% below poverty in the Population for whom poverty has been determined
    p_fb_pov_totpop = poverty_totpop,
    p_fb_pov_count = (poverty_100below_fborn + poverty_100to149below_fborn),
    p_fb_pov =  ifelse(poverty_totpop==0, 0, round(100 * ((poverty_100below_fborn + poverty_100to149below_fborn) / poverty_totpop),1)),
    p_fb_pov_density =  ifelse(poverty_totpop==0, 0, round( ((poverty_100below_fborn + poverty_100to149below_fborn) / p_land_sqmiles),1)),
    
    # Immigrants with no health insurance in the Civilian Noninstitutionalized Population
    p_fb_nohi_totpop = nohealth_totpop,
    p_fb_nohi_count = (nohealth_fborn_nat + nohealth_fborn_noncit),
    p_fb_nohi = ifelse(nohealth_totpop==0, 0, round(100 * (nohealth_fborn_nat + nohealth_fborn_noncit) / nohealth_totpop,1)),
    p_fb_nohi_density = ifelse(nohealth_totpop==0, 0, round(((nohealth_fborn_nat + nohealth_fborn_noncit) / p_land_sqmiles),1)),
    #
    p_fb_noncit_nohi_totpop = nohealth_totpop,
    p_fb_noncit_nohi_count = nohealth_fborn_noncit,
    p_fb_noncit_nohi = ifelse(nohealth_totpop==0, 0, round(100 * (nohealth_fborn_noncit / nohealth_totpop),1)),
    p_fb_noncit_nohi_density =  ifelse(nohealth_totpop==0, 0, round((nohealth_fborn_noncit / p_land_sqmiles),1)),
    
    # Limited english speakers by language spoken at home in the population over 5
    # limited_english_totpop variable is the denominator
    p_lim_eng_totpop = limited_english_totpop,
    #spanish
    p_lim_eng_spanish_count = limited_english_spanish,
    p_lim_eng_spanish = ifelse(limited_english_totpop==0, 0, round(100 * (limited_english_spanish / limited_english_totpop),1)),
    p_lim_eng_spanish_density = ifelse(limited_english_totpop==0, 0, round((limited_english_spanish / p_land_sqmiles),1)),
    #chinese
    p_lim_eng_chinese_count = limited_english_chinese,
    p_lim_eng_chinese = ifelse(limited_english_totpop==0, 0, round(100 * (limited_english_chinese / limited_english_totpop),1)),
    p_lim_eng_chinese_density = ifelse(limited_english_totpop==0, 0, round((limited_english_chinese / p_land_sqmiles),1)),
    #tagalog
    p_lim_eng_tagalog_count = limited_english_tagalog,
    p_lim_eng_tagalog = ifelse(limited_english_totpop==0, 0, round(100 * (limited_english_tagalog / limited_english_totpop),1)),
    p_lim_eng_tagalog_density = ifelse(limited_english_totpop==0, 0, round((limited_english_tagalog / p_land_sqmiles),1)),
    #vitenamese
    p_lim_eng_vietnamese_count = limited_english_vietnamese,
    p_lim_eng_vietnamese = ifelse(limited_english_totpop==0, 0, round(100 * (limited_english_vietnamese / limited_english_totpop),1)),
    p_lim_eng_vietnamese_density = ifelse(limited_english_totpop==0, 0, round((limited_english_vietnamese / p_land_sqmiles),1)),
    #korean
    p_lim_eng_korean_count = limited_english_korean,
    p_lim_eng_korean = ifelse(limited_english_totpop==0, 0, round(100 * (limited_english_korean / limited_english_totpop),1)),
    p_lim_eng_korean_density = ifelse(limited_english_totpop==0, 0, round((limited_english_korean / p_land_sqmiles),1)),
    
    #other - needed for determining quantile bins only
    p_lim_eng_allgroups_count = (limited_english_spanish + limited_english_chinese + limited_english_tagalog + 
                                     limited_english_vietnamese + limited_english_korean +
                                     limited_english_french +  limited_english_german + limited_english_russian +
                                    limited_english_other_indoeuropean +  limited_english_other_api +
                                   limited_english_arabic +  limited_english_other),
    
    p_lim_eng_allgroups = ifelse(limited_english_totpop==0, 0, round(100 * (p_lim_eng_allgroups_count / limited_english_totpop),1))
  )

# Transforming CRS from NAD83 to WGS84 ("+proj=longlat +datum=WGS84")
tract_data_c3 <- st_transform(tract_data_c3, 4326) 

# WRITE output to CSV
write_sf(tract_data_c3,"msi_countydata_acs2018.csv", layer_options = "GEOMETRY=AS_WKT", delete_layer=TRUE, delete_dsn=TRUE)
         
# Save vars to be mapped only
county_data <- select(tract_data_c3, "NAME", "GEOID", starts_with("p_"))
county_data$state=str_split_fixed(county_data$NAME, ", ",2)[,2]
# Remove temp objects
keep_objects <- c("county_data")
#"health_language_support","health_service_access", "health_services", 
# "legal_language_support", "legal_service_access","legal_services")

rm(list = setdiff(ls(), keep_objects))

# Write Census Data to an Rdata file (census)
save.image("county_data_processed.Rdata")

# remove objects from session
rm(list = ls())

############state data
my_cenvar_df <-read.csv("Data_input/msi_cenvars_lut.csv", strip.white = T, stringsAsFactors = F)
state_data <- get_acs(geography = "state", 
                   variables = my_cenvar_df$my_cen_vars, 
                   year=2018, 
                   survey="acs5",
                   #state = states, 
                   #county = my_counties, 
                   geometry = T,
                   keep_geo_vars = T)
tract_data_c1 <- state_data %>%
  select("NAME.y","GEOID","ALAND","variable","estimate") %>%
  spread(key=variable, value=estimate)
#Remove rows with no land are
tract_data_c1  <- tract_data_c1 [!is.na(tract_data_c1 $ALAND),]

# Copy for debugging
tract_data_c2 <- tract_data_c1 
# rename columns   
colnames(tract_data_c2) <- c("NAME","GEOID", "land_area",my_cenvar_df$my_cen_var_names,"geometry")
#tract_data_c2$NAME <- gsub(", California","",tract_data2$NAME)

# Remove rows with no people
tract_data_c2 <- tract_data_c2[tract_data_c2$tract_totpop!=0,]

# Change units for land_area from sq meters to sq km
tract_data_c2$p_land_areakm2 <- tract_data_c2$land_area / (1000 * 1000)
tract_data_c2$p_land_acreage <- tract_data_c2$land_area / 4046.856
tract_data_c2$p_land_sqmiles <- tract_data_c2$land_area / 2589988.1103

#tfix <- tract_data2[tract_data2$GEOID == "06041122000",]
# Create Calculated columns
## Note: keeping colnames short due to shapefile limits
tract_data_c3 <- tract_data_c2 %>%
  mutate (
    # Immigrant noncitizens in the total pop
    p_noncit_totpop = tract_totpop,
    p_noncit_count = tract_fborn_noncit,
    p_noncit = ifelse(tract_totpop==0, 0, round(100 * (tract_fborn_noncit/tract_totpop),1)),
    p_noncit_density = ifelse(tract_totpop==0, 0, round((tract_fborn_noncit / p_land_sqmiles),1)),
    
    # Immigrants in the total pop
    p_fborn_totpop = tract_totpop,
    p_fborn_count = tract_fborn,
    p_fborn =   ifelse(tract_totpop==0, 0, round(100 * (tract_fborn / tract_totpop), 1)),
    p_fborn_density =  ifelse(tract_totpop==0, 0, round((tract_fborn / p_land_sqmiles),1)),
    
    # Recent Immigrants in the total pop
    p_recent_totpop = tract_totpop,
    p_recent_count = tract_post2010_fborn,
    p_recent =  ifelse(tract_totpop==0, 0, round(100 * (tract_post2010_fborn / tract_totpop),1)),
    p_recent_density = ifelse(tract_totpop==0, 0, round((tract_post2010_fborn / p_land_sqmiles),1)),
    
    # Immigrants living 150% below poverty in the Population for whom poverty has been determined
    p_fb_pov_totpop = poverty_totpop,
    p_fb_pov_count = (poverty_100below_fborn + poverty_100to149below_fborn),
    p_fb_pov =  ifelse(poverty_totpop==0, 0, round(100 * ((poverty_100below_fborn + poverty_100to149below_fborn) / poverty_totpop),1)),
    p_fb_pov_density =  ifelse(poverty_totpop==0, 0, round( ((poverty_100below_fborn + poverty_100to149below_fborn) / p_land_sqmiles),1)),
    
    # Immigrants with no health insurance in the Civilian Noninstitutionalized Population
    p_fb_nohi_totpop = nohealth_totpop,
    p_fb_nohi_count = (nohealth_fborn_nat + nohealth_fborn_noncit),
    p_fb_nohi = ifelse(nohealth_totpop==0, 0, round(100 * (nohealth_fborn_nat + nohealth_fborn_noncit) / nohealth_totpop,1)),
    p_fb_nohi_density = ifelse(nohealth_totpop==0, 0, round(((nohealth_fborn_nat + nohealth_fborn_noncit) / p_land_sqmiles),1)),
    #
    p_fb_noncit_nohi_totpop = nohealth_totpop,
    p_fb_noncit_nohi_count = nohealth_fborn_noncit,
    p_fb_noncit_nohi = ifelse(nohealth_totpop==0, 0, round(100 * (nohealth_fborn_noncit / nohealth_totpop),1)),
    p_fb_noncit_nohi_density =  ifelse(nohealth_totpop==0, 0, round((nohealth_fborn_noncit / p_land_sqmiles),1)),
    
    # Limited english speakers by language spoken at home in the population over 5
    # limited_english_totpop variable is the denominator
    p_lim_eng_totpop = limited_english_totpop,
    #spanish
    p_lim_eng_spanish_count = limited_english_spanish,
    p_lim_eng_spanish = ifelse(limited_english_totpop==0, 0, round(100 * (limited_english_spanish / limited_english_totpop),1)),
    p_lim_eng_spanish_density = ifelse(limited_english_totpop==0, 0, round((limited_english_spanish / p_land_sqmiles),1)),
    #chinese
    p_lim_eng_chinese_count = limited_english_chinese,
    p_lim_eng_chinese = ifelse(limited_english_totpop==0, 0, round(100 * (limited_english_chinese / limited_english_totpop),1)),
    p_lim_eng_chinese_density = ifelse(limited_english_totpop==0, 0, round((limited_english_chinese / p_land_sqmiles),1)),
    #tagalog
    p_lim_eng_tagalog_count = limited_english_tagalog,
    p_lim_eng_tagalog = ifelse(limited_english_totpop==0, 0, round(100 * (limited_english_tagalog / limited_english_totpop),1)),
    p_lim_eng_tagalog_density = ifelse(limited_english_totpop==0, 0, round((limited_english_tagalog / p_land_sqmiles),1)),
    #vitenamese
    p_lim_eng_vietnamese_count = limited_english_vietnamese,
    p_lim_eng_vietnamese = ifelse(limited_english_totpop==0, 0, round(100 * (limited_english_vietnamese / limited_english_totpop),1)),
    p_lim_eng_vietnamese_density = ifelse(limited_english_totpop==0, 0, round((limited_english_vietnamese / p_land_sqmiles),1)),
    #korean
    p_lim_eng_korean_count = limited_english_korean,
    p_lim_eng_korean = ifelse(limited_english_totpop==0, 0, round(100 * (limited_english_korean / limited_english_totpop),1)),
    p_lim_eng_korean_density = ifelse(limited_english_totpop==0, 0, round((limited_english_korean / p_land_sqmiles),1)),
    
    #other - needed for determining quantile bins only
    p_lim_eng_allgroups_count = (limited_english_spanish + limited_english_chinese + limited_english_tagalog + 
                                   limited_english_vietnamese + limited_english_korean +
                                   limited_english_french +  limited_english_german + limited_english_russian +
                                   limited_english_other_indoeuropean +  limited_english_other_api +
                                   limited_english_arabic +  limited_english_other),
    
    p_lim_eng_allgroups = ifelse(limited_english_totpop==0, 0, round(100 * (p_lim_eng_allgroups_count / limited_english_totpop),1))
  )

# Transforming CRS from NAD83 to WGS84 ("+proj=longlat +datum=WGS84")
tract_data_c3 <- st_transform(tract_data_c3, 4326) 

# WRITE output to CSV
write_sf(tract_data_c3,"msi_statedata_acs2018.csv", layer_options = "GEOMETRY=AS_WKT", delete_layer=TRUE, delete_dsn=TRUE)

# Save vars to be mapped only
state_data <- select(tract_data_c3, "NAME", "GEOID", starts_with("p_"))

# Remove temp objects
keep_objects <- c("state_data")
#"health_language_support","health_service_access", "health_services", 
# "legal_language_support", "legal_service_access","legal_services")

rm(list = setdiff(ls(), keep_objects))

# Write Census Data to an Rdata file (census)
save.image("state_data_processed.Rdata")
