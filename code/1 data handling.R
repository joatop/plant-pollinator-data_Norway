library(downloader)
library(sf)
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(readxl)
library(tmap)
library(tmaptools)



#### load species data ####

### download ANO data from kartkatalogen to P-drive
# url <- "https://nedlasting.miljodirektoratet.no/naturovervaking/naturovervaking_eksport.gdb.zip"
download(url, dest="P:/41201785_okologisk_tilstand_2022_2023/data/naturovervaking_eksport.gdb.zip", mode="w") 
unzip ("P:/41201785_okologisk_tilstand_2022_2023/data/naturovervaking_eksport.gdb.zip", 
       exdir = "P:/41201785_okologisk_tilstand_2022_2023/data/naturovervaking_eksport.gdb")

st_layers(dsn = "P:/41201785_okologisk_tilstand_2022_2023/data/naturovervaking_eksport.gdb")


### upload data from P-drive
## ANO
ANO.sp <- st_read("P:/41201785_okologisk_tilstand_2022_2023/data/Naturovervaking_eksport.gdb",
                  layer="ANO_Art")
ANO.geo <- st_read("P:/41201785_okologisk_tilstand_2022_2023/data/Naturovervaking_eksport.gdb",
                   layer="ANO_SurveyPoint")
head(ANO.sp)
head(ANO.geo)

## ASO data from 2022
excel_sheets("P:/41201785_okologisk_tilstand_2022_2023/data/ASO/Semi-naturlig_eng_S123_2022.xlsx")

ASO.sp <- read_excel("P:/41201785_okologisk_tilstand_2022_2023/data/ASO/Semi-naturlig_eng_S123_2022.xlsx", 
                     sheet = "transektregistreringer_4")
ASO.geo <- read_excel("P:/41201785_okologisk_tilstand_2022_2023/data/ASO/Semi-naturlig_eng_S123_2022.xlsx",
                      sheet = "surveyPoint_0")
head(ASO.sp)
head(ASO.geo)

## GRUK
excel_sheets("P:/41201785_okologisk_tilstand_2022_2023/data/GRUK/GRUKdata_2020-2022_GJELDENDE.xlsx")
GRUK.variables <- read_excel("P:/41201785_okologisk_tilstand_2022_2023/data/GRUK/GRUKdata_2020-2022_GJELDENDE.xlsx", 
                             sheet = 2)
GRUK.species <- read_excel("P:/41201785_okologisk_tilstand_2022_2023/data/GRUK/GRUKdata_2020-2022_GJELDENDE.xlsx", 
                           sheet = 3)

# condition evaluation for 2021 data
excel_sheets("P:/41201785_okologisk_tilstand_2022_2023/data/GRUK/NNF_GRUK_GJELDENDE.xls")
GRUK2021.condition <- read_excel("P:/41201785_okologisk_tilstand_2022_2023/data/GRUK/NNF_GRUK_GJELDENDE.xls", 
                                 sheet = 1)

## artskart data
# all verified plant species observations with at least 50m precision for 2013-2023
artskart <- read.csv("C:/Users/joachim.topper/Downloads/19.Oct.2023 12-01_not NotRecovered, not Absent, Validated_2013+_-50_Polygon_/artskart_sp.csv",header=T,sep=";",dec=",")
head(artskart)

## NiN data (Naturtypekartlegging etter Miljødirektoratets instruks)
### Import NiN data
nin <- st_read("R:/GeoSpatialData/Habitats_biotopes/Norway_Miljodirektoratet_Naturtyper_nin/Original/versjon20221231/Natur_Naturtyper_nin_norge_med_svalbard_25833/Natur_Naturtyper_NiN_norge_med_svalbard_25833.gdb")

### Import region- og Norgeskart
nor <- st_read("input/outlineOfNorway_EPSG25833.shp")%>%
  st_as_sf() %>%
  st_transform(crs = st_crs(nin))

reg <- st_read("input/regions.shp")%>%
  st_as_sf() %>%
  st_transform(crs = st_crs(nin))

# change region names to something R-friendly
reg$region
reg$region <- c("Northern Norway","Central Norway","Eastern Norway","Western Norway","Southern Norway")

regnor <- st_intersection(reg,nor)

# get region info into the nin-object
names(reg)
names(nin)
nin <- st_join(nin[,c(3,12,19,23,24,36)],  reg[,c(2,3)], left=TRUE)

#### data handling ####

### artskart
# get rid of some columns not necessary for this work
names(artskart)
artskart <- artskart[,c("validScientificName",
                        "dateTimeCollected",
                        "coordinatePrecision",
                        "latitude","longitude",
                        "east","north",
                        "geometry")]
# making it into a spatial object with the same crs as nin and regnor
artskart <- artskart %>%
  st_as_sf(coords = c("longitude","latitude"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84") %>%
  st_transform(crs = st_crs(nin))

# plotting artskart observations and nin sites on the Norway map
tm_shape(regnor) +
  tm_fill("GID_0", labels = "", title = "", legend.show = FALSE, alpha=0) +
  tm_borders() +
  tm_shape(artskart) +
  tm_dots(scale = 1.5,col='orange', legend.show = FALSE, alpha=0.5) +
  tm_layout(
    main.title = "Artskart observations",
    main.title.size = 1.2, legend.position = c("right", "bottom"),
    legend.text.size = 1.3, legend.title.size = 1.4
  ) +
  tm_shape(nin) +
  tm_borders(col="purple")

# filtering for only those artskart data that are within nin polygons
artskart_nin <- st_filter(artskart, nin)

tm_shape(regnor) +
  tm_fill("GID_0", labels = "", title = "", legend.show = FALSE, alpha=0) +
  tm_borders() +
  tm_shape(artskart_nin) +
  tm_dots(scale = 1.5,col='orange', legend.show = FALSE, alpha=0.5) +
  tm_layout(
    main.title = "Artskart observations",
    main.title.size = 1.2, legend.position = c("right", "bottom"),
    legend.text.size = 1.3, legend.title.size = 1.4
  )

# spatial join to get species and nin/region information together
artskart_nin <- st_join(artskart_nin, nin, left = TRUE)

