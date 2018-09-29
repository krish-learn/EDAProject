# Exploratory Data Analysis - Course Project 2

#The overall goal of this assignment is to explore the National Emissions Inventory database and see what 
#it say about fine particulate matter pollution in the United states over the 10-year period 1999-2008.


unzip("exdata%2Fdata%2FNEI_data.zip", exdir = getwd())

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## _______________________________________________________________________________________
## Q1 - Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
## Using the base plotting system, make a plot showing the ## total PM2.5 emission from 
## all sources for each of the years 1999, 2002, 2005, and 2008.
## _______________________________________________________________________________________

Totals <- aggregate(Emissions ~ year,NEI, sum)
barplot(
    (Totals$Emissions)/10^6,
    names.arg=Totals$year,
    xlab="Year",
    ylab="PM2.5 Emissions",
    main="Total PM2.5 Emissions across US"
  )

dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()
## _______________________________________________________________________________________
## Q1 answer - The plot shows clear drop in total emissions from 1999 TO 2008
## _______________________________________________________________________________________

## _______________________________________________________________________________________
## Q2 - Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
## (\color{red}{\verb|fips == "24510"|}fips=="24510") from 1999 to 2008? Use the 
## base plotting system to make a plot answering this question.
## _______________________________________________________________________________________

bltNEI <- NEI[NEI$fips == "24510",]
bltTotals <- aggregate(Emissions ~ year,bltNEI, sum)

barplot(
  (bltTotals$Emissions)/10^6,
  names.arg=bltTotals$year,
  xlab="Year",
  ylab="PM2.5 Emissions",
  main="Total Baltimore PM2.5 Emissions over years"
)
dev.copy(png, file="plot2.png", height=480, width=480)
dev.off()

## _______________________________________________________________________________________
## Q2 answer - The plot shows clear drop in total emissions in BALTIMORE from 1999 TO 2008
## _______________________________________________________________________________________



## _______________________________________________________________________________________
## Q3 - Of the four types of sources indicated by the \color{red}{\verb|type|}type 
## (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases
## in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions
## from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.
## _______________________________________________________________________________________


library(ggplot2)

ggp <- ggplot(bltNEI,aes(year,Emissions,fill=type)) +
  geom_bar(stat="identity") +
  theme_bw() + guides(fill=FALSE)+
  facet_grid(.~type,scales = "free",space="free") + 
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))

print(ggp)

dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()

## _______________________________________________________________________________________
## Q4 - Across the United States, how have emissions from coal combustion-related sources 
## changed from 1999-2008?
## _______________________________________________________________________________________


SCCcoal <- SCC[grepl("coal", SCC$Short.Name, ignore.case = T),]
NEIcoal <- NEI[NEI$SCC %in% SCCcoal$SCC, ]
coalTotals <- aggregate(Emissions ~ year,NEIcoal, sum)
barplot(
  (coalTotals$Emissions)/10^6,
   names.arg=coalTotals$year,
   xlab="Year",
   ylab="PM2.5 Emissions",
   main="Total PM2.5 Emissions over years for Source Type COAL"
  )

dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()

## _______________________________________________________________________________________
## Q5 - How have emissions from motor vehicle sources changed from 1999-2008 in 
## Baltimore City?
## _______________________________________________________________________________________

NEIbltmotor <- NEI[NEI$fips == "24510" & NEI$type == "ON-ROAD", ]
bltmotorTotals <- aggregate(Emissions ~ year,NEIbltmotor, sum)
bltmotorTotals <- aggregate(Emissions ~ year,NEIbltmotor, sum)
barplot(
  (bltmotorTotals$Emissions)/10^6,
   names.arg=bltmotorTotals$year,
   xlab="Year",
   ylab="PM2.5 Emissions",
   main="Total Baltimore PM2.5 Emissions from MOTOR VEHICLES over years"
  )

dev.copy(png, file="plot5.png", height=480, width=480)
dev.off()
## _______________________________________________________________________________________
## Q6 - Comparing emissions from motor vehicle sources in Baltimore City (fips == "24510") with emissions from 
## motor vehicle sources in Los Angeles County, California (fips == "06037"),
## _______________________________________________________________________________________

NEIbltmotor$city <- "Baltimore"
NEImotorLA <- NEI[NEI$fips == "06037" & NEI$type == "ON-ROAD", ]
NEImotorLA$city <- "Los Angeles"
LAandBlt <- rbind(NEImotorLA, NEIbltmotor)

ggp <- ggplot(LAandBlt, aes(x=factor(year), y=Emissions, fill=city)) +
  geom_bar(aes(fill=year),stat="identity") +
  facet_grid(scales="free", space="free", .~city) +
  guides(fill=FALSE) + theme_bw() +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission")) + 
  labs(title=expression("PM2.5 Motor Vehicle Emissions in Baltimore & LA, 1999-2008"))

print(ggp)

dev.copy(png, file="plot6.png", height=480, width=480)
dev.off()

