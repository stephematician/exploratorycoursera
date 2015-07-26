## Course project for Exploratory Data Analysis - plot5
# Author: Stephen Wade
# Date: 26/07/2015
#
# Generate a plot using ggplot2 which shows which of the four sources has
# experienced a decrease in emissions over 1999-2008
#
# For this I chose to use a simply point + linear regression plot to demonstrate
# if there is any trend in the observed total pm2.5 measurements, using the 
# EI.Sector as a facet to break the data down into light/heavy vehicle usage,
# and gasoline/diesel usage.
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

vehicle_idx <- grep("Vehicle",SCC$EI.Sector, ignore.case=TRUE)
vehicle_SCC <- slice(SCC, vehicle_idx) %>% select(SCC, SCC, EI.Sector)
vehicle_SCC$EI.Sector <- droplevels(vehicle_SCC$EI.Sector)

splitlevels <- strsplit(levels(vehicle_SCC$EI.Sector), " ")
newlevels = sapply(splitlevels, function(x) {return(paste(x[5], x[6], x[4], sep=""))})
levels(vehicle_SCC$EI.Sector) <- newlevels

NEI_vehicle <- subset(NEI, SCC %in% vehicle_SCC$SCC) %>%
               mutate(SCC = factor(SCC, levels=levels(vehicle_SCC$SCC))) %>%
               filter(fips=="24510") %>%
               left_join(vehicle_SCC, by='SCC') %>%
               group_by(year, EI.Sector)

sub_totals <- summarise_each(NEI_vehicle, funs(sum), Emissions)

all_totals <- group_by(sub_totals, year) %>%
              summarise(Emissions=sum(Emissions)) %>%
              mutate(EI.Sector=as.factor('Total'))
final_totals <- rbind(sub_totals, all_totals)

# scales=free is quite important here when demonstrating linear trends
g <- ggplot(data=final_totals, aes(x=year, y=Emissions))
g + geom_smooth(metho="lm",
                alpha=1,
                color="black",
                fill="antiquewhite") +
    geom_point(stat="identity",
               size=4,
               fill="cadetblue",
               shape=21,
               color="black") +
    facet_wrap(~EI.Sector,
               ncol=2,
               scales="free") +
    theme_bw() +
    scale_x_continuous(breaks=unique(final_totals$year)) +
    labs(x=NULL,
         title="Total emissions for Baltimore City vehicles by vehicle type")

dev.copy(png, width=560, height=840, filename='plot5.png', bg='transparent')
dev.off()
