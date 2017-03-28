# Mass Shootings
# packages used: rworldmap rworldxtra
# data source: www.motherjones.com
library(rworldmap)
library(rworldxtra)
US <- getMap(resolution = "high")

plot(US,xlim=c(-125,-65),ylim=c(39,39), asp=1.31803)
title(main="Mass Shootings 1982-2013")

points(d$longitude,d$latitude,col="red",cex=d$Fatalities*.25)

text(-69.31142,37.21232,"Newtown")
text(-72.41394,30.22957,"Virginia Tech")
text(-111.04308,38.55200,"San Ysidro \n McDonald's Massacre")
text(-89.72780,25.9,"Luby's Massacre")

#using locator() -- add lines from circles to labels
points(c(-77.67630,-72.99422),c(36.08547,31.16065),type='l')
points(c(-71.71729, -69.05702),c(39.79927,37.94237),type='l')
points(c(-96.51104, -92.68024),c(29.62669,26.23582),type='l')
points(c(-115.8778, -111.4086),c(33.98637, 36.73135),type='l')

#################################################################################

# Airline Data
# Packages: rworldmap rworldxtra geosphere
library(rworldmap)
library(rworldxtra)
library(geosphere)
library(maps)
# Source: OpenFlights.org; flowingdata.com

# plot world map
map("world", col="grey15", fill=TRUE, bg="Black")

#create 100 shades of blue
pal <- colorRampPalette(c("#f2f2f2", "Blue"))
colors <- pal(100)

#plot each route
attach(gs)
for(i in 1:length(S_Long)){
  inter <- gcIntermediate(cbind(gs[i,]$S_Long, gs[i,]$S_Lat), 
                             cbind(gs[i,]$D_Long, gs[i,]$D_Lat), n=100)
  
  index <- round( (Dest_Count/max(Dest_Count))*length(colors))
  
  lines(inter, col=colors[index], lwd=.2)
}
title(main="American Airline Routes",col.main="Blue")
