getwd()
setwd("C:\\Users\\Dell\\Music\\R-assignment")
irish<-read.csv("Irish Towns Co Ordinates.csv")
View(irish)

library(sqldf)
ir_county<-sqldf("select count(DISTINCT name), county from irish group by county")
View(ir_county)
ir_galway<-sqldf("select name, easting, northing, latitude, longitude from irish where county == 'Galway'")
View(ir_galway)

library(dplyr)
library(leaflet)
leaflet()%>%addTiles()             
leaflet(ir_galway)%>%addTiles()%>%addCircles(lng = ~longitude,lat = ~latitude, popup = ~name)

