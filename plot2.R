## Course project for Exploratory Data Analysis - plot2
# Author: Stephen Wade
# Date: 26/07/2015
#
# Generate a plot using the base plotting system which shows the total PM2.5
# emissions from all sources for Balitmore, from 1999 to 2008.
#
# For this I chose to use a simple barplot, including some information about
# the distribution via percentiles (0-90, 90-99, and 99-100). As can be seen, 
# the observations in the 99th percentile count for a large portion of the
# variance over the years.
#
# The number of observations is also clearly growing with time.
#
# download_data.R are available on
# https://www.github.com/stephematician/exploratorycoursera


library(dplyr)

# I run a script called download_data.R that downloads the data and puts it in
# a /data/ subdirectory.

source('download_data.R')

NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")



mBaC_data <- subset(NEI, fips == "24510") %>%
             mutate(emis_quart = cut(Emissions,
                                     quantile(Emissions,
                                              c(0,0.9,0.99)),
                                     include.lowest=TRUE)) %>%
             group_by(year, emis_quart)

BaC_total_emissions <- summarise_each(mBaC_data, funs(sum), Emissions)
BaC_count_emissions <- count(mBaC_data, year)

# A bit of a hack, but I'm in a rush.
BaC_matrix <- matrix(BaC_total_emissions$Emissions, nrow=3, ncol=4,
                     dimnames=list(c('0-90%', '90-99%', '99-100%'),
                                   c('1999','2002','2005','2008')))

bx <- barplot(BaC_matrix,
        beside=FALSE,
        main="Total PM2.5 emissions in Baltimore City",
        col=c("cadetblue","antiquewhite","coral"),
        xlab="Year",
        ylab="PM2.5",
        sub="(###) is number of measurements",
        legend=TRUE)
num_obs_lab <- sapply(BaC_count_emissions$n, function(x){sprintf("(%i)",x)})
text(bx,
     y=apply(BaC_matrix,2,sum)*0.9,
     labels=num_obs_lab)

# plot in 4:3 as the legend gets cropped badly otherwise.
dev.copy(png, width=720, filename='plot2.png', bg='transparent')
dev.off()
