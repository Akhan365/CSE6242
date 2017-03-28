# install.packages("dismo")
#install.packages("rgdal")
#install.packages("XML")
library(dismo)

mymap <- gmap("France")  # choose whatever country
plot(mymap)

# Map type
mymap <- gmap("France", type = "satellite" )
plot(mymap)

# Zoom level
mymap <- gmap("France", type = "satellite", exp = 3)
plot(mymap)

Save the map as a file in your working directory for future use
mymap <- gmap("France", type = "satellite", filename = "France.gmap")

mymap <- gmap("Europe")
plot(mymap)

select.area <- drawExtent()
# now click 2 times on the map to select your region
mymap <- gmap(select.area)
plot(mymap)
# See ?gmap for many other possibilities
############################################################################################
install.packages("RgoogleMaps")
library(RgoogleMaps)

# Get base maps from Google (a file will be saved in your working directory)
newmap <- GetMap(center = c(36.7, -5.9), zoom = 10, destfile = "newmap.png", maptype = "satellite")


# Now using bounding box instead of center coordinates:
newmap2 <- GetMap.bbox(lonR = c(-5, -6), latR = c(36, 37), destfile = "newmap2.png", maptype = "terrain")

# Try different maptypes
newmap3 <- GetMap.bbox(lonR = c(-5, -6), latR = c(36, 37), destfile = "newmap3.png", maptype = "satellite")
# Now plot data onto these maps, e.g. these 3 points
PlotOnStaticMap(lat = c(36.3, 35.8, 36.4), lon = c(-5.5, -5.6, -5.8), zoom = 10, cex = 4, pch = 19, col = "red", FUN = points, add = F)
############################################################################################
install.packages("googleVis")
library(googleVis)

data(Exports)    # a simple data frame
Geo <- gvisGeoMap(Exports, locationvar="Country", numvar="Profit", options=list(height=400, dataMode='regions')) 
plot(Geo)

data(Andrew)
M1 <- gvisMap(Andrew, "LatLong", "Tip",
              options=list(showTip=TRUE, showLine=F, enableScrollWheel=TRUE, 
                           mapType='satellite', useMapTypeControl=TRUE, width=800,height=400))
plot(M1)

## Example with address, here UK post-code and some html code in tooltip

df <- data.frame(Postcode=c("EC3M 7HA", "EC2P 2EJ"),
                 Tip=c("<a href='http://www.lloyds.com'>Lloyd's</a>", 
                       "<a href='http://www.guildhall.cityoflondon.gov.uk/'>Guildhall</a>"))

M2 <- gvisMap(df, "Postcode", "Tip",
              options=list(showTip=TRUE, mapType='normal',
                           enableScrollWheel=TRUE))

plot(M2)

## Change mapping icons
M3 <- gvisMap(df, "Postcode", "Tip",
              options=list(showTip=TRUE, mapType='normal',
                           enableScrollWheel=TRUE,
                           icons=paste0("{",
                                        "'default': {'normal': 'http://icons.iconarchive.com/",
                                        "icons/icons-land/vista-map-markers/48/",
                                        "Map-Marker-Ball-Azure-icon.png',\n",
                                        "'selected': 'http://icons.iconarchive.com/",
                                        "icons/icons-land/vista-map-markers/48/",
                                        "Map-Marker-Ball-Right-Azure-icon.png'",
                                        "}}")))

plot(M3)
########################################################################################################
install.packages("rworldmap")
library(rworldmap)

newmap <- getMap(resolution = "coarse")  # different resolutions available
plot(newmap)

mapCountryData()
mapCountryData(mapRegion = "europe")
mapGriddedData()
mapGriddedData(mapRegion = "europe")


mapCountryData()
data("countryExData",envir=environment(),package="rworldmap")
sPDF <- joinCountryData2Map(countryExData
                            , joinCode = "ISO3"
                            , nameJoinColumn = "ISO3V10")
mapCountryData( sPDF, nameColumnToPlot="BIODIVERSITY")

#user defined map colour scheme for categorical data              
mapParams <- mapCountryData(nameColumnToPlot='GEO3major'
                            , catMethod='categorical'
                            , addLegend='FALSE'
                            , colourPalette=c('white','green','red','yellow','blue','black') 
                            )
#changing legendText
mapParams$legendText <- c('antarctic','africa','oceania'
                          ,'americas','s.asia','eurasia')              
do.call( addMapLegendBoxes, c(mapParams,x='bottom',title="Region",horiz=TRUE))

##showing how rworldmap can be used with the classInt and RColorBrewer packages
install.packages("classInt")
library(classInt)
library(RColorBrewer)
#getting example data and joining to a map
data("countryExData",envir=environment(),package="rworldmap")
sPDF <- joinCountryData2Map(countryExData,joinCode = "ISO3",nameJoinColumn = "ISO3V10")
#getting class intervals using a 'jenks' classification in classInt package
classInt <- classIntervals( sPDF$EPI, n=5, style="jenks")
catMethod = classInt$brks
#getting a colour scheme from the RColorBrewer package
colourPalette <- brewer.pal(5,'RdPu')
#calling mapCountryData with the parameters from classInt and RColorBrewer
mapParams <- mapCountryData( sPDF, nameColumnToPlot="EPI", addLegend=FALSE
                             , catMethod = catMethod, colourPalette=colourPalette )
do.call(addMapLegend, c(mapParams
                        ,legendLabels="all"
                        ,legendWidth=0.5
                        ,legendIntervals="data"))
#################################################################################################

install.packages('ggmap')
library(ggmap)

## load data
lib='/...directory with CSV crime file.../'
setwd(lib)
boston=read.csv('Crime_Incident_Reports.csv')

## subset to drug crimes
drugs=boston[which(boston$INCIDENT_TYPE_DESCRIPTION=='DRUG CHARGES' & boston$Year=='2014'),]

## plot drug and shooting crimes
bos_plot=ggmap(get_map('Boston, Massachusetts',
                       zoom=13,
                       source='google',
                       maptype='terrain'))


ggplot() +  
  geom_polygon(data = bostonMap, aes(x = long, y = lat, group = group)) + 
  geom_point(data = bostonData2, aes(x = LON, y = LAT), color = "red")

# convert the points data frame to class SpatialPointsDataFrame.
coordinates(bostonData)<-~LON+LAT