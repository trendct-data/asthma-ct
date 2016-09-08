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

total_map <- left_join(towntracts, asthma)

total_map2 <- gather(total_map, "category", "n", 10:19)

total_map2$n <- as.numeric(total_map2$n)

tm_ct <- ggplot() +
  geom_polygon(data = total_map2, aes(x=long, y=lat, group=group, fill=n), color = "black", size=0.2) +
  geom_polygon(data = townborders, aes(x=long, y=lat, group=group, fill=total), color = "black", fill=NA, size=0.5) +
  coord_map() +
  facet_wrap(~category, ncol=2) +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Asthma in Connecticut", fill="")
print(tm_ct)



