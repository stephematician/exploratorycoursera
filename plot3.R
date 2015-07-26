## Course project for Exploratory Data Analysis - plot3
# Author: Stephen Wade
# Date: 26/07/2015
#
# Generate a plot using ggplot2 which shows which of the four sources has
# experienced a decrease in emissions over 1999-2008
#
# For this I chose to use a simply point + linear regression plot to demonstrate
# if there is any trend in the observed total pm2.5 measurements
#
# download_data.R is available on
# https://www.github.com/stephematician/exploratorycoursera

library(ggplot2)
library(dplyr)

# I run a script called download_data.R that downloads the data and puts it in
# a ./data subdirectory.
source('download_data.R')

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

BaC_data <- subset(NEI, fips == "24510")
gBaC_summary <- subset(NEI, fips == "24510") %>% 
    group_by(year, type) %>%
    summarise_each(funs(sum), Emissions)

# scales=free is quite important here when demonstrating linear trends
g <- ggplot(data=gBaC_summary, aes(x=year, y=Emissions))
g + geom_smooth(metho="lm",
                alpha=1,
                color="black",
                fill="antiquewhite") +
    geom_point(stat="identity",
               size=4,
               fill="cadetblue",
               shape=21,
               color="black") +
    facet_wrap(~type,
               ncol=2,
               scales="free") +
    theme_bw() +
    scale_x_continuous(breaks=unique(final_totals$year)) +
    labs(x=NULL,
         title="Total emissions for Baltimore City by type of source")

dev.copy(png, width=560, height=560, filename='plot3.png', bg='transparent')
dev.off()
