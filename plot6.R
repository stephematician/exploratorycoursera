## Course project for Exploratory Data Analysis - plot6
# Author: Stephen Wade
# Date: 26/07/2015
#
# Generate a plot using ggplot2 which shows which of the four sources has
# experienced a decrease in emissions over 1999-2008
#
# For this I chose to use a simply point + linear regression plot to demonstrate
# if there is any trend in the observed total pm2.5 measurements, using the 
# EI.Sector as a facet to break the data down into light/heavy vehicle usage,
# and gasoline/diesel usage, and the Baltimore City data is overlaid on
# Los Angeles
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
               mutate(SCC = factor(SCC, levels=levels(vehicle_SCC$SCC)),
                      fips = factor(fips)) %>%
               filter(fips=="24510" | fips=="06037") %>%
               left_join(vehicle_SCC, by='SCC') %>%
               group_by(year, EI.Sector, fips)

sub_totals <- summarise_each(NEI_vehicle, funs(sum), Emissions)

all_totals <- group_by(sub_totals, year, fips) %>%
              summarise(Emissions=sum(Emissions)) %>%
              mutate(EI.Sector=as.factor('Total'))
final_totals <- rbind(sub_totals, all_totals) %>%
                group_by(EI.Sector, fips)
final_totals <- mutate(final_totals, Emissions = Emissions/max(Emissions)) %>%
                group_by() %>%
                mutate(county=factor(fips))
levels(final_totals$county) <- c("LA",
                               "Baltimore")

# scales=free is quite important here when demonstrating linear trends
g <- ggplot(data=final_totals, aes(x=year,
                                   y=Emissions,
                                   group=county,
                                   fill=county))
g + geom_smooth(method="lm",
                alpha=0.25,
                color="black",
                level=0.75) +
    geom_point(stat="identity",
               size=4,
               shape=21,
               alpha=0.75,
               color="black") +
    facet_wrap(~EI.Sector,
               ncol=2,
               scales="free") +
    theme_bw() +
    scale_fill_manual(values=c("cadetblue","coral")) +
    scale_x_continuous(breaks=unique(final_totals$year)) +
    coord_cartesian(ylim=c(-0.2,1.2)) +
    scale_y_continuous(breaks=c(0,1)) +
    theme(legend.justification=c(1,0), legend.position=c(0.9,0.1))
    labs(x=NULL,
         title="Normalised emissions for Baltimore versus LA")

dev.copy(png, width=560, height=840, filename='plot6.png', bg='transparent')
dev.off()
