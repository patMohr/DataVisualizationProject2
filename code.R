setwd("/Users/patm12/documents/coursera/dataVisualization/project2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

pm25PerYear=aggregate(Emissions ~ year, NEI, sum)

png("plot1.png")
plot(pm25PerYear$year,pm25PerYear$Emissions/1000000,type="l",main="Total PM2.5 per Year",xlab="Year",ylab="PM2.5 (Million tons)",col="green")
points(pm25PerYear$year,pm25PerYear$Emissions/1000000,pch=21,col = "green",bg="yellow")
text(2004,7,"PM2.5 levels have fallen considerably from 1999 to 2008")
dev.off()

#########

setwd("/Users/patm12/documents/coursera/dataVisualization/project2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

baltimore=NEI[NEI$fips == 24510,]
pm25PerYear=aggregate(Emissions ~ year, baltimore, sum)
png("plot2.png")
plot(pm25PerYear$year,pm25PerYear$Emissions/1000,type="l",main="Total PM2.5 per Year for Baltimore",xlab="Year",ylab="PM2.5 (Thousand tons)",col="green")
points(pm25PerYear$year,pm25PerYear$Emissions/1000,pch=21,col = "green",bg="yellow")
text(2003,2,"PM2.5 levels in Baltimore have fallen from 1999 to 2008")
dev.off()

#########
library(ggplot2)
setwd("/Users/patm12/documents/coursera/dataVisualization/project2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

baltimore=NEI[NEI$fips == 24510,]
pm25PerYear=aggregate(Emissions ~ year + type, baltimore, sum)
png("plot3.png")
ggplot(pm25PerYear, aes(x=year, y=Emissions)) + geom_line() + geom_point() + ggtitle("Total PM2.5 in Baltimore by Type (tons)") + facet_wrap( ~ type, ncol=2)
dev.off()

##############
library(ggplot2)
setwd("/Users/patm12/documents/coursera/dataVisualization/project2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

SCC$EI.Sector=as.character(SCC$EI.Sector)
coal=unique(SCC$EI.Sector)
coal=coal[grep("Coal",coal)]
coalID=as.character(unique(SCC[SCC$EI.Sector %in% coal,"SCC"]))
#use these IDs to get all the PM2.5 data for coal related emissions
coalSet=NEI[NEI$SCC %in% coalID,]

pm25PerYear=aggregate(Emissions ~ year, coalSet, sum)
png("plot4.png")
ggplot(pm25PerYear, aes(x=year, y=Emissions)) + geom_line() + geom_point() + ggtitle("Total Coal related PM2.5 Emissions (tons)")
dev.off()

##############
library(ggplot2)
setwd("/Users/patm12/documents/coursera/dataVisualization/project2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

SCC$EI.Sector=as.character(SCC$EI.Sector)
mobile=unique(SCC$EI.Sector)
mobile=mobile[grep("Mobile",mobile)]
mobileID=as.character(unique(SCC[SCC$EI.Sector %in% mobile,"SCC"]))
#use these IDs to get all the PM2.5 data for coal related emissions
baltimoreMobile=NEI[NEI$SCC %in% mobileID & NEI$fips == 24510,]

pm25PerYear=aggregate(Emissions ~ year, baltimoreMobile, sum)
png("plot5.png")
ggplot(pm25PerYear, aes(x=year, y=Emissions)) + geom_line() + geom_point() + ggtitle("Total Mobile related PM2.5 Emissions in Baltimore (tons)")
dev.off()

##############
library(ggplot2)
setwd("/Users/patm12/documents/coursera/dataVisualization/project2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

SCC$EI.Sector=as.character(SCC$EI.Sector)
mobile=unique(SCC$EI.Sector)
mobile=mobile[grep("Mobile",mobile)]
mobileID=as.character(unique(SCC[SCC$EI.Sector %in% mobile,"SCC"]))
#use these IDs to get all the PM2.5 data for coal related emissions
baltLA=NEI[NEI$SCC %in% mobileID & NEI$fips %in% c("24510","06037"),]
baltLA$county=ifelse(baltLA$fips=="24510","Baltimore","Los Angeles County")

pm25PerYear=aggregate(Emissions ~ year + county, baltLA, sum)
png("plot6.png")
ggplot(pm25PerYear, aes(x=year, y=Emissions)) + geom_line() + geom_point() + ggtitle("Total Mobile Related PM2.5 Emissions \n in Baltimore vs Los Angeles County (tons)") + facet_wrap( ~ county, ncol=2)
dev.off()
