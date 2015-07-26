## Course project for Exploratory Data Analysis - plot4
# Author: Stephen Wade
# Date: 26/07/2015
#
# Generate a plot using ggplot2 which shows how across the states, the emissions
# from coal-combustion related sources have changed from 1999-2008
#
# The question asked to show what has happened 'across' the United States.
# This seemed open to interpretation, so I have chosen to plot with
# states being a facet, with the total emissions by state plotted as points
# with the lm linear trend overlaid. The total for USA is also included as a
# final panel.
#
# As per http://www.epa.gov/envirofw/html/codes/state.html the first two
# digits are the state code. I placed these into a .csv called
# state_codes_swade.csv
#
# All the supporting code, e.g. download_data.R is available on 
# https://www.github.com/stephematician/exploratorycoursera

library(ggplot2)
library(dplyr)

data('state')

# I run a script called download_data.R that downloads the data and puts it in
# a ./data subdirectory.
source('download_data.R')

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
state_codes <- read.csv('./data/state_codes_swade.csv',
                        header=TRUE)

coal_comb_idx <- grep("Fuel Comb.*Coal",SCC$EI.Sector)
coal_SCC <- slice(SCC, coal_comb_idx) %>% select(SCC, SCC)

NEI_coal <- subset(NEI, SCC %in% coal_SCC$SCC) %>%
            mutate(stfip = floor(ifelse(is.na(as.numeric(fips)),
                                              0,
                                              as.numeric(fips)) / 1000)) %>%
            left_join(state_codes, by=c('stfip'='ansicode')) %>%
            group_by(state,year)

coal_totals <- summarise_each(NEI_coal, funs(sum), Emissions)

usa_totals <- group_by(coal_totals, year) %>%
              summarise(Emissions=sum(Emissions)) %>%
              mutate(state=as.factor('USA (total)'))
final_totals <- rbind(coal_totals, usa_totals)
final_totals <- final_totals[-which(is.na(final_totals$state)),]

# scales=free is quite important here when demonstrating linear trends
g <- ggplot(data=final_totals,
            aes(x=year, y=Emissions))
g + geom_smooth(metho="lm",
                alpha=1,
                color="black",
                fill="antiquewhite") +
    geom_point(stat="identity",
               size=3,
               fill="cadetblue",
               shape=21,
               color="black"
               ) +
    facet_wrap(~state,
               ncol=8,
               scales="free") +
    theme_bw() +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())# +
    labs(x=NULL,
         title="Emissions from coal burning by state and USA total")

dev.copy(png, width=2400, height=1600, filename='plot4.png', bg='transparent')
dev.off()
