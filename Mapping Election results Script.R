# Let's get started

Sys.info()

sessionInfo()

getwd()


## first and foremost
Sys.setenv(language="en")

## clean old workspace
rm(list = ls())

## load packages----
pac <- "pacman" %in% rownames(installed.packages())
if(!pac) install.packages("pacman")
library(pacman)
rm(pac)
p_load("tidyverse", "data.table", "readxl", "leaflet", "ggmap", "giscoR",
       "ggplot2", "ggrepel", "sf", "rnaturalearth", "rnaturalearthdata", "widgetframe",
       "rgdal", "geojsonio", "leaflet.extras", "scales", "htmlwidgets", "htmltools", 
       "leaflet.providers")

# Load Geodata----

file.choose()

WahlkreiseGeo <- readOGR("C:\\Fun Fun Fun\\Projekte\\Leaflet Mapping Wahlergebnisse\\Mapping Election results\\Input\\Bundeswahlleiter\\generalisiert\\Geometrie_Wahlkreise_20DBT.shp", 
                         use_iconv = TRUE, encoding = "UTF-8")

#

WahlkreiseGeo <- spTransform(WahlkreiseGeo, CRS("+proj=longlat"))


# Load election results----

file.choose()

Votes <- fread("C:\\Fun Fun Fun\\Projekte\\Leaflet Mapping Wahlergebnisse\\Mapping Election results\\Input\\kerg2(1).csv", 
                       skip = 9, header = TRUE, fill = TRUE, dec = ".",
                       stringsAsFactors = FALSE)

# Preprocessing Data----

Wahlkreise <- Votes %>% 
  filter(Gebietsart == "Wahlkreis", 
         Gruppenart == "Partei") %>% 
  select(3:5, 8:11, 13)

str(Wahlkreise)

# change character to numeric variable, i.e. Prozent column

Wahlkreise$Prozent <- as.numeric(gsub(",", ".", Wahlkreise$Prozent))

# replace occuring NA to 0, i.e. 0 percent of votes

Wahlkreise$Prozent[is.na(Wahlkreise$Prozent)] <- 0

# Round election results to 2 digits (decimal places)

Wahlkreise$Prozent <- round(Wahlkreise$Prozent, digits = 2)

#
## Create characteristic/ values of the variables as own variables (wide format)----
#

Wahlkreise1 <- Wahlkreise %>% 
  select(-Gruppenreihenfolge) %>% 
  filter(Stimme == 1)

#

Wahlkreise2 <- Wahlkreise %>% 
  select(-Gruppenreihenfolge) %>% 
  filter(Stimme == 2)

#

WahlkreiseWide1 <- pivot_wider(Wahlkreise1, names_from = Gruppenname, 
                              values_from = Prozent) 


#

WahlkreiseWide2 <- pivot_wider(Wahlkreise2, names_from = Gruppenname, 
                               values_from = Prozent) 


# Mapping results per Party-----

# merge geodata to election results

colnames(WahlkreiseWide1)[3] <- "WKR_NAME"

colnames(WahlkreiseWide2)[3] <- "WKR_NAME"

#

WahlkreiseGeo1 <- merge(WahlkreiseGeo, WahlkreiseWide1, by = "WKR_NAME")

WahlkreiseGeo2 <- merge(WahlkreiseGeo, WahlkreiseWide2, by = "WKR_NAME")


# save election results per 'Wahlkreis' in a wide format as a df

save(WahlkreiseWide1, file = "Wahlkreise1Wide.Rda")

save(WahlkreiseWide2, file = "Wahlkreise2Wide.Rda")

# save combined geo- and structural data

save(WahlkreiseGeo1, file = "WahlkreiseGeo1.Rda")

save(WahlkreiseGeo2, file = "WahlkreiseGeo2.Rda")

# Visualisation of Election results per Party----

# create color palettes per Party 

# SPD

palSPD1 <- colorBin(c("#f9a099", "#a90c00"), domain=WahlkreiseGeo1$SPD, 
                    pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo1$colorSPD <- palSPD1(WahlkreiseGeo1$SPD)


palSPD2 <-  colorBin(c("#f9a099", "#a90c00"), domain=WahlkreiseGeo2$SPD,
                     pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo2$colorSPD <- palSPD2(WahlkreiseGeo2$SPD)

# CDU

palCDU1 <- colorBin(c("#d0d0d0", "#161515"), domain=WahlkreiseGeo1$CDU,
                    pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo1$colorCDU <- palCDU1(WahlkreiseGeo1$CDU)

palCDU2 <- colorBin(c("#d0d0d0", "#161515"), domain=WahlkreiseGeo2$CDU,
                    pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo2$colorCDU <- palCDU2(WahlkreiseGeo2$CDU)

# CSU

palCSU1 <- colorBin(c("#86c1c1", "#0d8383"), domain=WahlkreiseGeo1$CSU,
                    pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo1$colorCSU <- palCSU1(WahlkreiseGeo1$CSU)

palCSU2 <- colorBin(c("#86c1c1", "#0d8383"), domain=WahlkreiseGeo2$CSU,
                    pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo2$colorCSU <- palCSU2(WahlkreiseGeo2$CSU)

# AFD

palAFD1 <- colorBin(c("#b49d84", "#693a08"), domain=WahlkreiseGeo1$AfD,
                    pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo1$colorAFD <- palAFD1(WahlkreiseGeo1$AfD)

palAFD2 <- colorBin(c("#b49d84", "#693a08"), domain=WahlkreiseGeo2$AfD,
                    pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo2$colorAFD <- palAFD2(WahlkreiseGeo2$AfD)

# FDP 

palFDP1 <- colorBin(c("#f5f789", "#e6c61f"), domain=WahlkreiseGeo1$FDP,
                    pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo1$colorFDP <- palFDP1(WahlkreiseGeo1$FDP)

palFDP2 <- colorBin(c("#f5f789", "#e6c61f"), domain=WahlkreiseGeo2$FDP,
                    pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo2$colorFDP <- palFDP2(WahlkreiseGeo2$FDP)

# Die Linke

palLinke1 <- colorBin(c("#fa88ec", "#ac0c97"), domain=WahlkreiseGeo1$`DIE LINKE`,
                      pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo1$colorLinke <- palLinke1(WahlkreiseGeo1$`DIE LINKE`)

palLinke2 <- colorBin(c("#fa88ec", "#ac0c97"), domain=WahlkreiseGeo2$`DIE LINKE`,
                      pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo2$colorLinke <- palLinke2(WahlkreiseGeo2$`DIE LINKE`)

# Grüne

palGrüne1 <- colorBin(c("#8fcf8f", "#1f9e1f"), domain=WahlkreiseGeo1$GRÜNE,
                      pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo1$colorGruene <- palGrüne1(WahlkreiseGeo1$GRÜNE)

palGrüne2 <- colorBin(c("#8fcf8f", "#1f9e1f"), domain=WahlkreiseGeo2$GRÜNE,
                      pretty = TRUE, na.color = "#FAEBD7")
WahlkreiseGeo2$colorGruene <- palGrüne2(WahlkreiseGeo2$GRÜNE)


#
## create map with polygons (election results per party and election district)
#

# First Votes

FigureElection1 <- leaflet(WahlkreiseGeo1, 
                options=leafletOptions(zoomControl=F)) %>%
  addProviderTiles("OpenStreetMap.DE", group = "OpenStreetMap.DE") %>% 
  setView(lng=10, lat=51, zoom=5.35) %>%
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Erststimmen SPD: ", WahlkreiseGeo1$SPD), 
              fillOpacity = 0.95, fillColor = ~colorSPD,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Erststimmen SPD") %>% 
 addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Erststimmen CDU: ", WahlkreiseGeo1$CDU), 
              fillOpacity = 0.95, fillColor = ~colorCDU,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Erststimmen CDU") %>% 
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Erststimmen CSU: ", WahlkreiseGeo1$CSU), 
              fillOpacity = 0.95, fillColor = ~colorCSU,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Erststimmen CSU") %>% 
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Erststimmen Grüne: ", WahlkreiseGeo1$GRÜNE), 
              fillOpacity = 0.95, fillColor = ~colorGruene,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Erststimmen Grüne") %>% 
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Erststimmen FDP: ", WahlkreiseGeo1$FDP), 
              fillOpacity = 0.95, fillColor = ~colorFDP,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Erststimmen FDP") %>% 
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Erststimmen Die Linke: ", WahlkreiseGeo1$`DIE LINKE`), 
              fillOpacity = 0.95, fillColor = ~colorLinke,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Erststimmen Die Linke") %>% 
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Erststimmen AFD: ", WahlkreiseGeo1$AfD), 
              fillOpacity = 0.95, fillColor = ~colorAFD,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Erststimmen AfD") %>% 
  addLayersControl(baseGroups = "OpenStreetMap.DE",
                   overlayGroups = c("Erststimmen SPD",
                                     "Erststimmen CDU",  
                                     "Erststimmen CSU", 
                                     "Erststimmen Grüne", 
                                     "Erststimmen FDP", 
                                     "Erststimmen Die Linke", 
                                     "Erststimmen AfD"), 
                   options = layersControlOptions(collapsed = T))
  


# Add Legend per Layer Group

FigureElection1 <- FigureElection1 %>% 
  addLegend("bottomright", pal = palSPD1, values = ~WahlkreiseGeo1$SPD,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Erststimmen der SPD </br> je Wahlkreis (2021)", 
            group = "Erststimmen SPD") %>% 
  addLegend("bottomright", pal = palCDU1, values = ~WahlkreiseGeo1$CDU,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Erststimmen der CDU </br> je Wahlkreis (2021)", 
            group = "Erststimmen CDU", 
            na.label = "Partei im Bundesland Bayern nicht vertreten") %>% 
  addLegend("bottomright", pal = palCSU1, values = ~WahlkreiseGeo1$CSU,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Erststimmen der CSU </br> je Wahlkreis (2021)", 
            group = "Erststimmen CSU", 
            na.label = "Partei im restlichen Bundesgebiet nicht vertreten") %>% 
  addLegend("bottomright", pal = palGrüne1, values = ~WahlkreiseGeo1$GRÜNE,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Erststimmen der Grünen </br> je Wahlkreis (2021)", 
            group = "Erststimmen Grüne") %>% 
  addLegend("bottomright", pal = palFDP1, values = ~WahlkreiseGeo1$FDP,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Erststimmen der FDP </br> je Wahlkreis (2021)", 
            group = "Erststimmen FDP") %>% 
  addLegend("bottomright", pal = palLinke1, values = ~WahlkreiseGeo1$`DIE LINKE`,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Erststimmen der Linken </br> je Wahlkreis (2021)", 
            group = "Erststimmen Die Linke") %>% 
  addLegend("bottomright", pal = palAFD1, values = ~WahlkreiseGeo1$AfD,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Erststimmen der AFD </br> je Wahlkreis (2021)", 
            group = "Erststimmen AfD")


# hide layer groups in a default view 

FigureElection1 <- FigureElection1 %>% 
  hideGroup(c("Erststimmen AfD", "Erststimmen Die Linke", "Erststimmen FDP", 
              "Erststimmen Grüne", "Erststimmen CSU", "Erststimmen CDU")) 
  
FigureElection1

save(FigureElection1, file = "LeafletErststimmen.Rda")
  
# save as a html widget  
  

FigureElection1 <- prependContent(FigureElection1, 
                       HTML("<style> .leaflet-container { background-color: white } </style> "))

FigureElection1 <- prependContent(FigureElection1, 
                       HTML("<style> .leaflet-popup-content-wrapper { height: 100px; width: 200px; background-color: gray91 } </style> "))

#


saveWidget(
  frameableWidget(
    FigureElection1), 
  file = paste0(
    getwd(),
    "/Output/",
    Sys.Date(),
    "_MultilayerErststimmen.html"),
  selfcontained=F,
  title="_MultilayerErststimmen.html"
)


#  Second Votes


FigureElection2 <- leaflet(WahlkreiseGeo2, 
                           options=leafletOptions(zoomControl=F)) %>%
  addProviderTiles("OpenStreetMap.DE", group = "OpenStreetMap.DE") %>% 
  setView(lng=10, lat=51.25, zoom=5.5) %>%
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Zweitstimmen SPD: ", WahlkreiseGeo2$SPD), 
              fillOpacity = 0.95, fillColor = ~colorSPD,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Zweitstimmen SPD") %>% 
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Zweitstimmen CDU: ", WahlkreiseGeo2$CDU), 
              fillOpacity = 0.95, fillColor = ~colorCDU,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Zweitstimmen CDU") %>% 
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Zweitstimmen CSU: ", WahlkreiseGeo2$CSU), 
              fillOpacity = 0.95, fillColor = ~colorCSU,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Zweitstimmen CSU") %>% 
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Zweitstimmen Grüne: ", WahlkreiseGeo2$GRÜNE), 
              fillOpacity = 0.95, fillColor = ~colorGruene,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Zweitstimmen Grüne") %>% 
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Zweitstimmen FDP: ", WahlkreiseGeo2$FDP), 
              fillOpacity = 0.95, fillColor = ~colorFDP,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Zweitstimmen FDP") %>% 
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Zweitstimmen Die Linke: ", WahlkreiseGeo2$`DIE LINKE`), 
              fillOpacity = 0.95, fillColor = ~colorLinke,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Zweitstimmen Die Linke") %>% 
  addPolygons(weight = 1, 
              color = "grey", 
              label = ~WKR_NAME, 
              popup = paste("Zweitstimmen AFD: ", WahlkreiseGeo2$AfD), 
              fillOpacity = 0.95, fillColor = ~colorAFD,
              highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE), 
              group = "Zweitstimmen AfD") %>% 
  addLayersControl(baseGroups = "OpenStreetMap.DE",
                   overlayGroups = c("Zweitstimmen SPD",
                                     "Zweitstimmen CDU",  
                                     "Zweitstimmen CSU", 
                                     "Zweitstimmen Grüne", 
                                     "Zweitstimmen FDP", 
                                     "Zweitstimmen Die Linke", 
                                     "Zweitstimmen AfD"), 
                   options = layersControlOptions(collapsed = T))


# Add Legend per Layer Group

FigureElection2 <- FigureElection2 %>% 
  addLegend("bottomright", pal = palSPD2, values = ~WahlkreiseGeo2$SPD,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Zweitstimmen der SPD </br> je Wahlkreis (2021)", 
            group = "Zweitstimmen SPD") %>% 
  addLegend("bottomright", pal = palCDU2, values = ~WahlkreiseGeo2$CDU,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Zweitstimmen der CDU </br> je Wahlkreis (2021)", 
            group = "Zweitstimmen CDU", 
            na.label = "Partei im Bundesland Bayern nicht vertreten") %>% 
  addLegend("bottomright", pal = palCSU2, values = ~WahlkreiseGeo2$CSU,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Zweitstimmen der CSU </br> je Wahlkreis (2021)", 
            group = "Zweitstimmen CSU", 
            na.label = "Partei im restlichen Bundesgebiet nicht vertreten") %>% 
  addLegend("bottomright", pal = palGrüne2, values = ~WahlkreiseGeo2$GRÜNE,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Zweitstimmen der Grünen </br> je Wahlkreis (2021)", 
            group = "Zweitstimmen Grüne") %>% 
  addLegend("bottomright", pal = palFDP2, values = ~WahlkreiseGeo2$FDP,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Zweitstimmen der FDP </br> je Wahlkreis (2021)", 
            group = "Zweitstimmen FDP") %>% 
  addLegend("bottomright", pal = palLinke2, values = ~WahlkreiseGeo2$`DIE LINKE`,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Zweitstimmen der Linken </br> je Wahlkreis (2021)", 
            group = "Zweitstimmen Die Linke") %>% 
  addLegend("bottomright", pal = palAFD2, values = ~WahlkreiseGeo2$AfD,
            opacity = 1,
            labFormat = labelFormat(suffix = " %"),
            title = "Zweitstimmen der AFD </br> je Wahlkreis (2021)", 
            group = "Zweitstimmen AfD")


# hide layer groups in a default view 

FigureElection2 <- FigureElection2 %>% 
  hideGroup(c("Zweitstimmen AfD", "Zweitstimmen Die Linke", "Zweitstimmen FDP", 
              "Zweitstimmen Grüne", "Zweitstimmen CSU", "Zweitstimmen CDU")) 

FigureElection2


# save as a html widget  

FigureElection2 <- prependContent(FigureElection2, 
                                  HTML("<style> .leaflet-container { background-color: white } </style> "))

FigureElection2 <- prependContent(FigureElection2, 
                                  HTML("<style> .leaflet-popup-content-wrapper { height: 100px; width: 200px; background-color: gray91 } </style> "))

#


saveWidget(
  frameableWidget(
    FigureElection2), 
  file = paste0(
    getwd(),
    "/Output/",
    Sys.Date(),
    "_MultilayerZweitstimmen.html"),
  selfcontained=F,
  title="_MultilayerZweitstimmen.html"
)


# Finito

