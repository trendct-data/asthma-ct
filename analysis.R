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

asthma$Census.Tract <- ifelse(nchar(asthma$Census.Tract)==5, asthma$Census.Tract, str_sub(asthma$Census.Tract, 2,6 ))

asthma$ugh <- paste(asthma$Town, asthma$Census.Tract)
tracts2towns$ugh <- paste(tracts2towns$town, tracts2towns$Census.Tract)
tracts2towns$ugh <- str_trim(tracts2towns$ugh)

asthma <- left_join(asthma, tracts2towns, by="ugh")

asthma$id <- as.character(asthma$id)


asthma$id <- paste0("0", asthma$id)

total_map <- left_join(towntracts, asthma, by="id")
total_map$ugh <- NULL
total_map$town <- NULL
total_map$Census.Tract.y <- NULL

total_map2 <- gather(total_map, "category", "n", 10:19)

total_map2$n <- as.numeric(total_map2$n)

tm_ct <- ggplot() +
  geom_polygon(data = total_map2, aes(x=long, y=lat, group=group, fill=n), color = "black", size=0.2) +
  geom_polygon(data = townborders, aes(x=long, y=lat, group=group, fill=total), color = "black", fill=NA, size=0.5) +
  coord_map() +
  facet_wrap(~category, ncol=1) +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Asthma in Connecticut", fill="")
print(tm_ct)

##


pol <- read_excel("data/2011nata_national_resp_by_tract_poll.xlsx", sheet=1)

pol2 <- read_excel("data/2011nata_national_resp_by_tract_source.xlsx", sheet=1)

ct_pol <- subset(pol, State=="CT")

ct_pol <- ct_pol %>%
  select(Tract, `Point (includes railyards) Respiratory HI`, `Total Respiratory HI`) %>%
  filter(Tract!="00000000000")

colnames(ct_pol) <- c("id", "point", "total.respiratory")

total_map3 <- total_map[!duplicated(total_map$id),]

total_map3 <- left_join(total_map3, ct_pol)

total_map3$Total.Number.of.Cases <- as.numeric(total_map3$Total.Number.of.Cases)
total_map3$Total.Rate.per.10.000 <- as.numeric(total_map3$Total.Rate.per.10.000)
total_map3$Number.of.Cases...19.Years.Old <- as.numeric(total_map3$Number.of.Cases...19.Years.Old)
total_map3$Rate.per.10.000....19.Years.Old <- as.numeric(total_map3$Rate.per.10.000....19.Years.Old)
total_map3$Number.of.Cases...20.Years.Old <- as.numeric(total_map3$Number.of.Cases...20.Years.Old)
total_map3$Rate.per.10.000...20.Years.Old <- as.numeric(total_map3$Rate.per.10.000...20.Years.Old)


cor(total_map3$Total.Number.of.Cases, total_map3$point, use="complete.obs")
cor(total_map3$Total.Number.of.Cases, total_map3$total.respiratory, use="complete.obs")

cor(total_map3$Total.Rate.per.10.000, total_map3$point, use="complete.obs")
cor(total_map3$Total.Rate.per.10.000, total_map3$total.respiratory, use="complete.obs")

cor(total_map3$Number.of.Cases...19.Years.Old, total_map3$point, use="complete.obs")
cor(total_map3$Number.of.Cases...19.Years.Old, total_map3$total.respiratory, use="complete.obs")

cor(total_map3$Rate.per.10.000....19.Years.Old, total_map3$point, use="complete.obs")
cor(total_map3$Rate.per.10.000....19.Years.Old, total_map3$total.respiratory, use="complete.obs")

cor(total_map3$Number.of.Cases...20.Years.Old, total_map3$point, use="complete.obs")
cor(total_map3$Number.of.Cases...20.Years.Old, total_map3$total.respiratory, use="complete.obs")

cor(total_map3$Rate.per.10.000...20.Years.Old, total_map3$point, use="complete.obs")
cor(total_map3$Rate.per.10.000...20.Years.Old, total_map3$total.respiratory, use="complete.obs")

#write.csv(total_map3, "total_map3.csv")
