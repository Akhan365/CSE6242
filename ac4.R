## ----setup, include=FALSE, echo=FALSE------------------------------------
knitr::opts_chunk$set(echo = TRUE)
setwd("C:/Users/eelsayed/Google Drive/CSE 6242/2017 Spring")
library(rworldmap) 
library(rworldxtra) 
library(geosphere)
library(maps)
library(ggmap)
library(plyr)

## ----data, include=FALSE, echo=FALSE-------------------------------------
rawDataExist <- TRUE
# Airports #
# 1 - Get airports data from https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat?_sm_au_=isVDZ77Sn0kTV1q6 and save it to airports.csv file
# 2 - Load airports.csv into R
if(file.exists("airports.csv")){
airports <- read.csv(file="airports.csv", header = FALSE, sep=",")

# 3 - Assign names to columns based on http://openflights.org/data.html 
# Airport ID	Unique OpenFlights identifier for this airport.
# Name	Name of airport. May or may not contain the City name.
# City	Main city served by airport. May be spelled differently from Name.
# Country	Country or territory where airport is located. See countries.dat to cross-reference to ISO 3166-1 codes.
# IATA	3-letter IATA code. Null if not assigned/unknown.
# ICAO	4-letter ICAO code. Null if not assigned.
# Latitude	Decimal degrees, usually to six significant digits. Negative is South, positive is North.
# Longitude	Decimal degrees, usually to six significant digits. Negative is West, positive is East.
# Altitude	In feet.
# Timezone	Hours offset from UTC. Fractional hours are expressed as decimals, eg. India is 5.5.
# DST	Daylight savings time. One of E (Europe), A (US/Canada), S (South America), O (Australia), Z (New Zealand), N (None) or U (Unknown). See also: Help: Time
# Tz database time zone	Timezone in "tz" (Olson) format, eg. "America/Los_Angeles".
# Type	Type of the airport. Value "airport" for air terminals, "station" for train stations, "port" for ferry terminals and "unknown" if not known. In airports.csv, only type=airport is included.
# Source	Source of this data. "OurAirports" for data sourced from OurAirports, "Legacy" for old data not matched to OurAirports (mostly DAFIF), "User" for unverified user contributions. In airports.csv, only source=OurAirports is included.
colnames(airports) <- c("AirportID", "Name", "City", "Country", "IATO", "ICAD", "Latitude", "Longitude", "Altitude", "Timezone", 
                        "DST", "Tz", "Type", "Source")
airports$AirportID = as.character(airports$AirportID)
airports$Name = as.character(airports$Name)
airports$City = as.character(airports$City)
airports$Country = as.character(airports$Country)
airports$IATO = as.character(airports$IATO)
airports$ICAD = as.character(airports$ICAD)
airports$Timezone = as.character(airports$Timezone)
airports$DST = as.character(airports$DST)
airports$Type = as.character(airports$Tz)
airports$Tz = as.character(airports$Type)
airports$Source = as.character(airports$Source)
}else{
  rawDataExist <- FALSE
}
# Airlines #
# 1 - Get airlines data from https://raw.githubusercontent.com/jpatokal/openflights/master/data/airlines.dat?_sm_au_=isVDZ77Sn0kTV1q6 and save it to airlines.csv file
# 2 - Load airlines.csv into R
if(file.exists("airlines.csv")){
airlines <- read.csv(file="airlines.csv", header = FALSE, sep=",")

# 3 - Assign names to columns based on http://openflights.org/data.html 
# Airline ID	Unique OpenFlights identifier for this airline.
# Name	Name of the airline.
# Alias	Alias of the airline. For example, All Nippon Airways is commonly known as "ANA".
# IATA	2-letter IATA code, if available.
# ICAO	3-letter ICAO code, if available.
# Callsign	Airline callsign.
# Country	Country or territory where airline is incorporated.
# Active	"Y" if the airline is or has until recently been operational, "N" if it is defunct. This field is not reliable: in particular, major airlines that stopped flying long ago, but have not had their IATA code reassigned (eg. Ansett/AN), will incorrectly show as "Y".
colnames(airlines) <- c("AirlineID", "Name", "Alias", "IATA", "ICAO","Callsign","Country", "Active")
airlines$AirlineID = as.character(airlines$AirlineID)
airlines$Name = as.character(airlines$Name)
airlines$Alias = as.character(airlines$Alias)
airlines$IATA = as.character(airlines$IATA)
airlines$ICAO = as.character(airlines$ICAO)
airlines$Callsign = as.character(airlines$Callsign)
airlines$Country = as.character(airlines$Country)
airlines$Active = as.character(airlines$Active)
}else{
  rawDataExist <- FALSE
}
# Routes #
# 1 - Get routes data from https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat?_sm_au_=isVH66qWCH5tTDQr and save it to routes.csv file
# 2 - Load routes.csv into R
if(file.exists("routes.csv")){
routes <- read.csv(file="routes.csv", header = FALSE, sep=",")

# 3 - Assign names to columns based on http://openflights.org/data.html 
# Airline	2-letter (IATA) or 3-letter (ICAO) code of the airline.
# Airline ID	Unique OpenFlights identifier for airline (see Airline).
# Source airport	3-letter (IATA) or 4-letter (ICAO) code of the source airport.
# Source airport ID	Unique OpenFlights identifier for source airport (see Airport)
# Destination airport	3-letter (IATA) or 4-letter (ICAO) code of the destination airport.
# Destination airport ID	Unique OpenFlights identifier for destination airport (see Airport)
# Codeshare	"Y" if this flight is a codeshare (that is, not operated by Airline, but another carrier), empty otherwise.
# Stops	Number of stops on this flight ("0" for direct)
# Equipment	3-letter codes for plane type(s) generally used on this flight, separated by spaces
colnames(routes) <- c("Airline", "AirlineID", "SourceAirport", "SourceAirportID", "DestinationAirport", "DestinationAirportID","CodeShare","Stops", "Equipment")
routes$Airline = as.character(routes$Airline)
routes$AirlineID = as.character(routes$AirlineID)
routes$SourceAirport = as.character(routes$SourceAirport)
routes$SourceAirportID = as.character(routes$SourceAirportID)
routes$DestinationAirport = as.character(routes$DestinationAirport)
routes$DestinationAirportID = as.character(routes$DestinationAirportID)
routes$CodeShare = as.character(routes$CodeShare)
routes$Equipment = as.character(routes$Equipment)
}else{
  rawDataExist <- FALSE
}
if(rawDataExist){
USAirports = airports[airports$Country == "United States",]
USAirports$NumRoutes <- 0

for(j in 1:nrow(USAirports)){
  nRoutes = 0
  cat(j)
  for(i in 1:nrow(routes)){
    if(
      (routes[i,"SourceAirport"] == USAirports[j, "IATO"]) ||
      (routes[i,"SourceAirport"] == USAirports[j, "IATO"]) ||
      (routes[i,"DestinationAirport"] == USAirports[j, "IATO"]) ||
      (routes[i,"DestinationAirport"] == USAirports[j, "IATO"]) 
    )
    {
      nRoutes = nRoutes + 1
      cat(".")
    }
  }
  print(nRoutes)
  USAirports[j, "NumRoutes" ] = nRoutes
}
write.csv(USAirports, file = "USAirports.csv" )
}

## ----analysis------------------------------------------------------------
if(!rawDataExist){ # Try load the preprocessed data frame
  USAirports <- read.csv(file = "USAirports.csv", sep=",")
}
USAirports <- USAirports[order(-USAirports$NumRoutes),]
Top10USAirports <- USAirports[1:10,]

chart <- ggplot() + 
  borders("usa", colour="gray50", fill="gray50") +
  geom_point(aes(x=Top10USAirports$Longitude, y=Top10USAirports$Latitude) ,color="blue", size=3) + 
  ggtitle("Figure 1 - Top 10 most connected airports in USA")
plot(chart)

