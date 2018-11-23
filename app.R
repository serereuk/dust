
library(shiny)
if (!require(leaflet)) install.packages("leaflet")
if (!require(dplyr)) install.packages("dplyr")
if (!require(data.table)) install.packages("data.table")
if (!require(circular)) install.packages("circular")
#if (!require(sp)) install.packages("sp")

library(leaflet)
library(dplyr)
library(data.table)
library(circular)
library(timeDate)
library(sp)
ds <- fread("ex2016.txt", data.table = F)
ds$d <- as.timeDate(substr(ds$d,1,10), zone = "")@Data
ds$w.direction <- circular(ds$`풍향(deg)`, type = "angles", units = "degree",template = "geographics")
ds <- ds %>% mutate(start.x = 125.5596, start.y = 36.6254, id = 1:nrow(ds))
line.length <- 0.1
arrow.length <- 0.3
arrow.angle <- 120

end.xy.df <- data.frame(end.x=NA,end.y=NA,end.arrow.x=NA,end.arrow.y=NA)

for (i in c(1:nrow(ds))){
  
  #coordinates of end points for wind lines (the initial points are the ones where data was observed)
  if (ds$w.direction[i] <= 90) {
    end.x <- ds$start.x[i] + (cos((90 - ds$w.direction[i]) * 0.0174532925) * line.length)
  } else if (ds$w.direction[i] > 90 & ds$w.direction[i] <= 180) {
    end.x <- ds$start.x[i] + (cos((ds$w.direction[i] - 90) * 0.0174532925) * line.length)
  } else if (ds$w.direction[i] > 180 & ds$w.direction[i] <= 270) {
    end.x <- ds$start.x[i] - (cos((270 - ds$w.direction[i]) * 0.0174532925) * line.length)
  } else {end.x <- ds$start.x[i] - (cos((ds$w.direction[i] - 270) * 0.0174532925) * line.length)}
  
  if (ds$w.direction[i] <= 90) {
    end.y <- ds$start.y[i] + (sin((90 - ds$w.direction[i]) * 0.0174532925) * line.length)
  } else if (ds$w.direction[i] > 90 & ds$w.direction[i] <= 180) {
    end.y <- ds$start.y[i] - (sin((ds$w.direction[i] - 90) * 0.0174532925) * line.length)
  } else if (ds$w.direction[i] > 180 & ds$w.direction[i] <= 270) {
    end.y <- ds$start.y[i] - (sin((270 - ds$w.direction[i]) * 0.0174532925) * line.length)
  } else {end.y <- ds$start.y[i] + (sin((ds$w.direction[i] - 270) * 0.0174532925) * line.length)}
  
  #coordinates of end points for arrowhead leg lines (the initial points are the previous end points)
  end.arrow.x <- end.x + (cos((ds$w.direction[i] + arrow.angle) * 0.0174532925) * arrow.length)
  end.arrow.y <- end.y - (sin((ds$w.direction[i] + arrow.angle) * 0.0174532925) * arrow.length)
  
  end.xy.df <- rbind(end.xy.df,c(end.x,end.y,end.arrow.x,end.arrow.y)) 
}

end.xy <- end.xy.df[-1,]
ds <- data.frame(ds,end.xy)


lines <- data.frame(cbind(lng=c(ds$start.x,ds$end.x,ds$end.arrow.x),
                          lat=c(ds$start.y,ds$end.y,ds$end.arrow.y),
                          id=c(rep(ds$id,3))))

lines.list <- list()
for (i in c(1:max(lines$id))){
  line <- subset(lines,lines$id==i)
  line <- as.matrix(line[,c(1:2)])
  line <- Line(line) #object of class 'Line'
  lines.list[[i]] <- Lines(list(line), ID = i) #list of 'objects'Lines' 
}

sp.lines <- SpatialLines(lines.list) #object of class 'SpatialLines'
proj4string(sp.lines) <- CRS("+init=epsg:4326") #define CRS

#Convert CRS to geographic coordinates (http://spatialreference.org/ref/epsg/4326/)
#for overlaying on OpenStreetMaps tiles in Leaflet
sp.lines <- spTransform(sp.lines, CRS("+init=epsg:4326"))

rownames(ds) = ds$id
#Join wind variables (id, speed, direction and date) to object of class 'SpatialLines'
sp.lines.df <- SpatialLinesDataFrame(sp.lines, ds[,c("id","풍속.m.s.","w.direction","d")]) #object of class 'SpatialLinesDataFrame'

library(leaflet)

#popup settings
labels <- paste0("ID: ",sp.lines.df@data$id,
                 "<br>Wind speed: ",sp.lines.df@data$풍속.m.s.,
                 "Wind direct: ",sp.lines.df@data$w.direction,
                 "Date: ", sp.lines.df@data$d)

#pallete settings
pal <- colorNumeric(palette = colorRampPalette(c("red", "blue"))(5),
                    domain = 0:max(sp.lines.df@data$풍속.m.s.))

#Create object fo class 'leaflet' 'htmlwidget'
m <- leaflet(sp.lines.df) %>%
  addTiles()  %>% addProviderTiles(providers$Esri.OceanBasemap)  %>% # add default OpenStreetMap map tiles
  addPolylines(color = ~pal(풍속.m.s.), opacity=1, weigh = 3, popup = labels) %>%
  addLegend("bottomright", pal = pal, values = ~풍속.m.s.,
            title = "Wind speed <br> (km/h)",
            opacity = 1) %>%
  fitBounds(sp.lines.df@bbox[1,1], sp.lines.df@bbox[2,1], sp.lines.df@bbox[1,2], sp.lines.df@bbox[2,2])


#User interface (UI) settings
ui <- fluidPage(leafletOutput("m.dynamic"),
                h1("서해 바람 방향 2016년 기준 저고도에서의 영향 확인"),
                absolutePanel(top = 20,
                              right = 20,
                              height = "800px",
                              draggable = TRUE,
                              sliderInput("range",
                                          "Time of data collection:",
                                          min = min(sp.lines.df@data$d),
                                          max = max(sp.lines.df@data$d),
                                          value = min(sp.lines.df@data$d),
                                          step = 3600,
                                          animate=TRUE)))

#Name @coords slot of SpatialLinesDataFrame: 'lng' and 'lat'
#task necessary for 'observer' within 'server' function
for (i in c(1:max(sp.lines.df@data$id))) {
  colnames(sp.lines.df@lines[[i]]@Lines[[1]]@coords) <- c("lng","lat")
}

#Server logic
server <- function(input, output){
  filteredData <- reactive({
    sp.lines.df[sp.lines.df@data$d == input$range[1],]
  })
  output$m.dynamic <- renderLeaflet({
    leaflet(sp.lines.df) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addLegend("bottomright",pal = pal, values = ~풍속.m.s., title = "Wind speed <br> (m/s)", opacity = 0.9) %>%
      fitBounds(sp.lines.df@bbox[1,1], sp.lines.df@bbox[2,1], sp.lines.df@bbox[1,2], sp.lines.df@bbox[2,2])
  })
  observe({
    leafletProxy("m.dynamic", data = filteredData()) %>%
      clearShapes() %>%
      addPolylines(color = ~pal(풍속.m.s.), opacity=1, weigh = 3, popup = labels)
  })
}

# Complete app with UI and server components
shinyApp(ui = ui, server = server)

