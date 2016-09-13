library(readxl)
library(dplyr)
library(tidyr)
library(dplyr)
library(ggmap)
library(knitr)
library(stringr)
library(ggplot2)
library(rgdal)
library(sp)
library(maptools)
library(scales)
asthma <- read.csv("https://data.ct.gov/api/views/javn-ujwr/rows.csv?accessType=DOWNLOAD", stringsAsFactors=F)


towntracts <- readOGR(dsn="maps", layer="census_tracts")

# creating a copy
towntracts_only <- towntracts

# turn the shapefile into a dataframe that can be worked on in R

towntracts <- fortify(towntracts, region="GEOID10")



townborders <- readOGR(dsn="maps", layer="ctgeo")
townborders_only <- townborders
townborders<- fortify(townborders, region="NAME10")

#names(asthma)[names(asthma) == 'Census.Tract'] <- 'id'
#asthma$id <- ifelse(nchar(asthma$Census.Tract) ==5, paste0("090010", asthma$Census.Tract), paste0("09009", asthma$Census.Tract))

tracts2towns <- read.csv("data/tracts_to_towns.csv", stringsAsFactors=F)

tracts2towns$id <- str_sub(tracts2towns$tract, 6,10 )
colnames(tracts2towns) <- c("id", "town", "Census.Tract")

asthma$Census.Tract <- as.character(asthma$Census.Tract)

asthma <- left_join(asthma, tracts2towns)

asthma$id <- as.character(asthma$id)
asthma$id <- paste0("0", asthma$id)

pol <- read_excel("data/2011nata_national_resp_by_tract_poll.xlsx", sheet=1)

pol2 <- read_excel("data/2011nata_national_resp_by_tract_source.xlsx", sheet=1)

ct_pol <- subset(pol, State=="CT")

ct_pol <- ct_pol %>%
  select(Tract, `Point (includes railyards) Respiratory HI`, `Total Respiratory HI`) %>%
  filter(Tract!="00000000000")

colnames(ct_pol) <- c("id", "point", "total.respiratory")

asthma2 <- left_join(asthma, ct_pol)
