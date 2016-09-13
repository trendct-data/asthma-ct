
# Analyzing asthma and pollution in Connecticut by census tract

This repository contains source data and analysis code for the Trend CT story:

* [In Connecticut, nearly one in ten have asthma](http://trendct.org/2016/09/13/asthma-in-connecticut/)

# Walkthroughs

* Exploratory visualizations of asthma-related emergencies and pollutants- [[RMarkdown](https://github.com/trendct-data/asthma-ct/blob/master/index.Rmd)] [[HTML](http://trendct-data.github.io/asthma-ct/)]

### The Data

* The data used in this analysis was either imported from the state of Connecticut's [open data portal](https://data.ct.gov/Health-and-Human-Services/Combined-Asthma-Emergency-Department-and-Hospitali/javn-ujwr) or downloaded from the Environmental Protection Agency National Air Toxics Assessment site.

### What's in this repo

* `data` [folder](https://github.com/trendct-data/tree/master/asthma-ct/data) with raw data and modified data.
* `analysis.R` is an R script creating some quick map visualizations of asthma-related emergencies by census tract in Connecticut.
* `respiratory.R` is an R script creating some quick map visualizations of level of pollutants by census tract in Connecticut.
* `maps` [folder](https://github.com/trendct-data/tree/master/asthma-ct/maps) contains shapefiles of census tracts and town borders. 

----

### Philosophy

We believe in open data. Our scripts may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon. 

Check out the reproducible scripts and data behind many of our other stories in our [central repo](https://github.com/trendct/data)

Follow us out on Twitter [@TrendCT](http://www.trendct.org) and on [Facebook/TrendCT](https://www.facebook.com/trendct/).

### Usage

If you use our data or methodology, please give us a shout out in your story. It'd also be nice if you gave me a heads up: [@abtran](http://www.twitter.com/abtran) or abtran@trendct.org.
0
