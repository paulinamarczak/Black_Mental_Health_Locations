# app.r
# black health alliance contract
# shiny app
# paulina marczak
setwd("C:\\Users\\Paulina\\Documents\\black_health_alliance\\app_dir1")

# # --------------------- Install R Packages ---------------------
# install.packages("futile.logger")
# install.packages("broom")
# install.packages("sp")
# install.packages("rgdal")
# install.packages("sf")
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("sqldf")
# install.packages("ggplot2")
# install.packages("spdplyr")
# install.packages("dplyr")
# install.packages("ggiraph")
# install.packages("htmltools")
# install.packages("raster")
# install.packages("BAMMtools")
# install.packages("tibble")
# install.packages("rsconnect")
# install.packages("BAMMtools")
# install.packages('rsconnect')
# install.packages('plyr')
# install.packages('opendatatoronto')

# --------------------- R Packages ---------------------

library(futile.logger)
library(broom)
library(sp)
library(rgdal)
library(sf)
library(shiny)
library(leaflet)
library(sqldf)
library(ggplot2)
library(plyr)
library(spdplyr)
library(dplyr)
library(ggiraph)
library(htmltools)
library(htmlwidgets)
library(raster)
library(BAMMtools)
library(tibble)
library(rsconnect)
library(opendatatoronto)
library(stringr)
library(shinyWidgets)


# ---------------------deploy on server ---------------------
#http://www.kimberlycoffey.com/blog/2016/2/13/mlz90wjw0k76446xkg262prvjp0l8u
#how to deploy a shiny server on amazon AWS

# --------------------- Shapefiles and Geocoding---------------------
flog.info("Reading Shapefiles")

#toronto
#KEEP THIS FOR NOW, MIGHT NOT NEED LATER BECAUSE I HAVE SHAPES OF WARDS IN wards_toronto
shape <- readOGR("shapefiles/wards.shp")
wards <- spTransform(shape, CRS("+init=epsg:4326")) 
# df_wards <- ggplot2::fortify(wards, region = "OBJECTID")  # make SpatialPolygonDataFrame shapes
# head(df_wards)

#toronto with black density stats
shape2 <- readOGR("shapefiles/Neighbourhoods.shp")
wards_toronto <- spTransform(shape2, CRS("+init=epsg:4326")) 
wards_toronto$BlkDensity<- wards_toronto$BlkDensity%>%round(0)
#remove anything with format "space (number)" to blank
wards_toronto$FIELD_7<-str_replace(wards_toronto$FIELD_7, " \\s*\\([^\\)]+\\)", "")
#remove anything with format "space space (number)" to blank (for Mimico)
wards_toronto$FIELD_7<-str_replace(wards_toronto$FIELD_7, " \\s*\\s*\\([^\\)]+\\)", "")

# df_wards_toronto <- ggplot2::fortify(wards_toronto)  # make SpatialPolygonDataFrame shapes

#peel
#https://data.peelregion.ca/datasets/census2016-wards20182022-ethnicityimmigrationaboriginal
shape <- readOGR("shapefiles/Census2016_Wards20182022_EthnicityImmigrationAboriginal.shp")
wards_peel <- spTransform(shape, CRS("+init=epsg:4326")) 
wards_peel$area_sqkm <- area(wards_peel) / 1000000
wards_peel$BlkDensity<-wards_peel$VM_Black/wards_peel$area_sqkm
wards_peel$BlkDensity<- wards_peel$BlkDensity%>%round(0)

# df_wards_peel <- ggplot2::fortify(wards, region = "OBJECTID")  # make SpatialPolygonDataFrame shapes
# head(df_wards)

#durham
# https://opendata.durham.ca/datasets/municipal-boundaries
shape <- readOGR("shapefiles/Municipal_Boundaries.shp")
wards_durham <- spTransform(shape, CRS("+init=epsg:4326")) 
wards_durham$area_sqkm <- area(wards_durham) / 1000000

#tie in aspatial black density with spatial areas to create cloropleth
files1 <- list.files(path=getwd(), pattern=glob2rx("Census*"), full.names=TRUE, recursive=FALSE)

#Function to extract a list of each municipality and respective Black people number
#Extract self-reported Black VM from census data
#from https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/page.cfm?Lang=E&Geo1=CSD&Code1=3518009&Geo2=CD&Code2=3518&SearchText=whitby&SearchType=Begins&SearchPR=01&B1=Visible%20minority&TABID=1&type=0

list_of_mun<- lapply(files1, function(x) {
			    read<-read.table(x, sep = ',', nrow=1, header= FALSE, fill=TRUE)
			    read1<-read.table(x, sep = ',', skip=7, nrow=1, header= FALSE, fill=TRUE)
			    durham<-read$V4
			    durham_first<-gsub(",.*$", "", durham)
			    durham_density<-read1$V4
			    durham_df<-data.frame("_xNAME_"= durham_first, "Num_Black"=durham_density)%>%bind_rows()
			    return(durham_df)
})

dataframe_of_mun<-bind_rows(list_of_mun)
dataframe_of_mun$X_xNAME_<-as.factor(dataframe_of_mun$X_xNAME_)
dataframe_of_mun$proper_order<- factor(c(403, 408, 407, 406, 404, 401, 402, 405), ordered = TRUE)
dataframe_of_mun$proper_order <- factor(dataframe_of_mun$proper_order, levels = c(403, 408, 407, 406, 404, 401, 402, 405))

#Now combine dataframe of Black people with, for example, wards_durham
#do a table join based on identical columns 'NAME' to get Black Density for Durham
#PROBLEM- IT OVERWRITES ORDER IN BAD WAY.
#SORT DATAFRAME BEFORE THE MERGE.
wards_durham@data<-merge(x = wards_durham@data, y = dataframe_of_mun, by.x = "OBJECTID", by.y= "proper_order")
names(wards_durham)[8]<- "Num_Black"
wards_durham$BlkDensity<-wards_durham$Num_Black/wards_durham$area_sqkm
wards_durham$BlkDensity<- wards_durham$BlkDensity%>%round(0)
# df_wards_durham <- ggplot2::fortify(wards_durham, region = "OBJECTID")  # make SpatialPolygonDataFrame shapes

#geocode csv of black/mainstream service providers
shape2<- read.csv("sna1.csv")
names(shape2)[names(shape2) == "Information.Hubs"] <- "InformationHub"

#replace individual URLs that didnt work
#these just dont work
# www.rosaliehall.com -> ""
# https://www.wingsofpassion.ca/ -> ""
shape2[shape2=="campus-health-centre/mental-health-services"] <- "https://durhamcollege.ca/student-life/health-and-wellness/campus-health-and-wellness-centre"
shape2[shape2=="www.hopecministries.com"] <- "https://www.hopecministries.com/"
shape2[shape2=="www.etobicokechildren.com"] <- " http://www.etobicokechildren.com/"
shape2[shape2=="http://www.yorktownfamilyservices.com/"] <- "https://www.yorktownfamilyservices.com/"
shape2[shape2=="adventureplace.ca"] <- "http://adventureplace.ca/"
shape2[shape2=="www.nygh.on.ca"] <- "https://www.nygh.on.ca/"
shape2[shape2=="skylarkyouth.org"] <- "https://www.skylarkyouth.org/"
shape2[shape2=="www.centrefranco.org"] <- "https://centrefranco.org/fr/"
shape2[shape2=="www.aht.ca"] <- "http://www.aht.ca/"
shape2[shape2=="sherbourne.on.ca"] <-"https://sherbourne.on.ca/"
shape2[shape2=="www.parcyouth.com"] <-"https://www.parcyouth.com/"
shape2[shape2=="bbyos.org"] <- "https://bbyos.org/"
shape2[shape2=="www.neighbourhoodlink.org"] <- "https://neighbourhoodlink.org/"
shape2[shape2=="careachc.ca"] <- "https://www.careachc.ca/"
shape2[shape2=="CMHA@cmhadurham.org"] <- "https://cmhadurham.ca/"

#identified as wgs84, crs 4326
shape2<-shape2[!(!is.na(shape2$Datum) & shape2$Datum==""), ]
events_sf <- shape2 %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
events_transformed_with_lat_lon <- cbind(events_sf, st_coordinates(events_sf))
head(events_transformed_with_lat_lon)

#separate map layers for black/ nonblack
black_focused_health<- filter(events_transformed_with_lat_lon, Type=="Black")
mainstream_health<- filter(events_transformed_with_lat_lon, Type=="Mainstream")

#Transportation
#first batch of subway stations from Mel
subway<- read.csv("ttc_coordinates.csv")
head(subway)
subway<- subway%>%
	dplyr::rename(LOCATION_N = Subway_Station)%>%
	add_column("STATUS"= "Existing", .after= "LOCATION_N")%>%
	add_column("TECHNOLOGY"= "Subway", .after= "STATUS")%>%
	add_column("NAME"= "NA", .after= "TECHNOLOGY")%>%
	add_column("optional"= "NA", .after= "Longitude")%>%
	dplyr::rename(coords.x1= Longitude)%>%
	dplyr::rename(coords.x2= Latitude)%>%
	subset(select=c(1,2,3,4,6,5,7))



#Second batch of subway stations bus stations/LRT stops in Toronto and GTA
subway2<-readOGR("shapefiles/FRTN_POINTS.shp")
subway2 <- spTransform(subway2, CRS("+init=epsg:4326")) 

#subset to subways and existing
subway2<-subway2[subway2@data$STATUS== "Existing",]

#remove inaccurate stations and replace with mel's accurate ones
inaccurate_list<-c('Downsview Park', 'Finch West', 'York University', 'Pioneer Village', 'Highway 407', 'Vaughan Metropolitan Centre')
subway4<-subway2[ ! subway2@data$LOCATION_N %in% inaccurate_list, ]

## try merging when theyre dataframes
subway4<-data.frame(subway4)
#merge mels list and open data
subway4<-rbind(subway4, subway)
head(subway)
head(subway4)
#make subways spatial
subway_sf2 <- subway4 %>%
  st_as_sf(coords = c("coords.x1", "coords.x2"), crs = 4326)
subway_transformed_with_lat_lon2 <- cbind(subway_sf2, st_coordinates(subway_sf2))

#split into individual layers for legend and icon purposes.
subway <- filter(subway_transformed_with_lat_lon2, TECHNOLOGY=="Subway")
busses<-filter(subway_transformed_with_lat_lon2, TECHNOLOGY=="BRT")
GORAIL<-filter(subway_transformed_with_lat_lon2, TECHNOLOGY=="GO Rail")
UPEXPRESS<-filter(subway_transformed_with_lat_lon2, TECHNOLOGY=="UP Express - 15-Min")
LRTBRT<-filter(subway_transformed_with_lat_lon2, TECHNOLOGY=="LRT / BRT")

#get bus stops
#should work

#does not work because api is shut down right now (as of march 2020)
#use local download instead
#SO SLOW
# bus_toronto<- read.csv("OpenData_TTC_Schedules/stops.txt")
# head(bus_toronto)
# events_bus <- bus_toronto %>%
#   st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
# bus_transformed_with_lat_lon <- cbind(events_bus, st_coordinates(events_bus))
# bus_transformed_with_lat_lon$stop_name<-str_to_title(bus_transformed_with_lat_lon$stop_name)


# routes_and_schedules <- search_packages("TTC Routes and Schedules") %>%
#   list_package_resources() %>%
#   filter(name =="TTC Routes and Schedules Data") %>%
#   get_resource()

# str(routes_and_schedules, max.level = 1)

#----------Icons and Styling---------#

#black_density function based on total wards in map so far
#change to iterative list sometime?
#CHANGE TO QUANTILES.
Black_density_scale<- function (wards1, wards2, wards3) {
	first<-round(wards1[['BlkDensity']],2)
	second<-round(wards2[['BlkDensity']],2)
	third<-round(wards3[['BlkDensity']],2)
	total_density_vector<-(c(first,second,third))
	#jenks<-getJenksBreaks(total_density_vector, 6, subset= NULL)
	#jenks<-quantile(total_density_vector,probs = c(0, 0.25, .5, .75, 1, NA))
	jenks<-quantile(total_density_vector,probs = c(0, 0.2, .4, .6, .8, 1))
	jenks<-round(jenks, 0)
	return(jenks) 
}
list_of_ward_densities<-c(wards_toronto$BlkDensity, wards_durham$BlkDensity, wards_peel$BlkDensity)
list_of_ward_densities<-round(list_of_ward_densities, 0)
#used for getting legend density in addLegend, and range/breaks of map colours.

bins<-Black_density_scale(wards_toronto, wards_durham, wards_peel)
#used for mapping cloropleth

#use this colour scale #1F193E
colour_palette<- c(	'#bfb8e3'
					,'#9d95c7'
					,'#70689c'
					,'#453d70' 
					, '#1F193E'
					)
pal <- colorBin(colour_palette, list_of_ward_densities, bins = bins, pretty= FALSE)

##add custom legend for markers
#FOR NOW TURN THIS OFF BECAUSE IM TRYING TO PUT THEM IN THE DROPDOWN.

#example from https://stackoverflow.com/questions/37862467/leaflet-legend-for-custom-markers-in-r
#function from https://stackoverflow.com/questions/47064921/leaflet-legend-for-addawesomemarkers-function-with-icons

# legend html generator:
# markerLegendHTML <- function(IconSet) {
#     # container div:
#     legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:0px; margin: 0;'"

#     n <- 1
#     # add each icon for font-awesome icons icons:
#     #change icon to align with text in legend???
#     #transparent? https://stackoverflow.com/questions/49099987/use-marker-icon-with-only-awesome-fonts-no-surrounding-balloon
#     #not sure with css
#     for (Icon in IconSet) {
#         if (Icon[["library"]] == "fa") {
#         legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 45px'>",
#                              "<div style='position: relative; display: inline-block; width: 36px; height: 50px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
#                                "<i style='margin-left: 5px; margin-top: 11px; 'class= 'fa fa-",Icon[["icon"]]," fa-inverse'></i>",
#                              "</div>",
#                              "<p style='position: relative; top: 10px; display: inline-block; ' >", names(IconSet)[n] ,"</p>",
#                            "</div>")    
#         }
#         n<- n + 1
#     }
#     paste0(legendHtml, "</div>")
# }

# #change size of icon only if not awesomeIcon https://stackoverflow.com/questions/52202980/modify-the-size-of-r-leaflet-markers

# IconSet <- awesomeIconList(
#   "Mainstream"   = makeAwesomeIcon(icon= 'medkit', markerColor = 'orange', iconColor = '#674B2D', library = "fa"),
#   "Black-focused" = makeAwesomeIcon(icon= 'medkit', markerColor = 'green', iconColor = '#3F5A33', library = "fa")
# )

#Icons for ancillary data

#markers are svg? but can only use png still idk why
#https://github.com/lvoogdt/Leaflet.awesome-markers/pull/74
#somene showed how you can do svg here but...https://github.com/ilyankou/Leaflet.IconMaterial

# icon.glyphicon_black <- makeAwesomeIcon(
# 	icon= 'medkit', 
# 	library="fa",
#  	markerColor = 'green', 
#  	iconColor = '#3F5A33')

icon.glyphicon_black <- makeIcon(
	iconUrl = "www/noun_Healthcare_1296604.svg",
  	iconRetinaUrl= "www/noun_Healthcare_1296604.svg",
   iconWidth = 15, iconHeight = 15 #,
)


# icon.glyphicon_nonblack <- makeAwesomeIcon(
# 	icon= 'medkit', 
# 	library="fa",
#  	markerColor = 'orange', 
#  	iconColor = '#674B2D')


icon.glyphicon_nonblack <- makeIcon(
	iconUrl = "www/noun_Healthcare_1886661.svg",
  	iconRetinaUrl= "www/noun_Healthcare_1886661.svg",
   iconWidth = 15, iconHeight = 15 #,
)
# icon.ion_subway <- makeAwesomeIcon(
# 	icon = 'subway', 
# 	markerColor = 'blue', 
# 	iconColor = '#FFFFFF',
# 	library='fa'
# 			)
#dont need separate subway icons for now##
	############icon.ion_subway_OD <- makeAwesomeIcon(
	############	icon = 'subway', 
	############	markerColor = 'blue', 
	############	iconColor = '#FFFFFF',
	############	library='fa')

#try this instead of awesome icons

#SUBWAY
{icon.ion_subway <- makeIcon(
	  iconUrl = "www/noun_Subway_342475_black.svg",
	  iconRetinaUrl= "www/noun_Subway_342475_black.svg",
	   iconWidth = 10, iconHeight = 10 #,
	  #className= "icon_subway_class_work" ##,
	  # #iconAnchorX = 22, iconAnchorY = 94
	  # # shadowUrl = "~//icons//noun_Subway",
	  # # shadowWidth = 50, shadowHeight = 64,
	  # # shadowAnchorX = 4, shadowAnchorY = 62
	)

	#BUS STOP OR STATION
	icon.ion_bus <- makeIcon(
		iconUrl = "www/noun_Bus_2558229_black.svg",
	  	iconRetinaUrl= "www/noun_Bus_2558229_black.svg",
	   iconWidth = 10, iconHeight = 10 #,
	)

	#GORAIL
	icon.ion_gorail <- makeIcon(
		iconUrl = "www/noun_Rail_2931084_black.svg",
	  	iconRetinaUrl= "www/noun_Rail_2931084_black.svg",
	   iconWidth = 10, iconHeight = 10 #,
	)

	#UNION PEARSON
	icon.ion_up <- makeIcon(
		iconUrl = "www/noun_expresstrain_2699167_black.svg",
	  	iconRetinaUrl= "www/noun_expresstrain_2699167_black.svg",
	   iconWidth = 10, iconHeight = 10 #,
	)
	#LRT/BRT
	icon.ion_train <- makeIcon(
		iconUrl = "www/noun_Train_18268_black.svg",
	  	iconRetinaUrl= "www/noun_Train_18268_black.svg",
	   iconWidth = 10, iconHeight = 10 #,
	)
}


# icon.glyphicon_black <- makeIcon(
#   iconUrl = "icons//noun_MentalHealth.svg",
#   iconWidth = 35, iconHeight = 40)
# icon.glyphicon_nonblack <- makeIcon(
#   iconUrl = "icons//noun_MentalHealth.svg",
#   iconWidth = 35, iconHeight = 40)

#black density labels
{
	labels <- sprintf(
	  "<strong>%s</strong><br/>%g Black people / km<sup>2</sup>",
	  wards_toronto$FIELD_7, wards_toronto$BlkDensity
	) %>% lapply(htmltools::HTML)

	labels_peel<- sprintf(
	  "<strong>%s</strong><br/>%g Black people / km<sup>2</sup>",
	  wards_peel$Municipal_, wards_peel$BlkDensity
	) %>% lapply(htmltools::HTML)

	labels_durham<- sprintf(
	  "<strong>%s</strong><br/>%g Black people / km<sup>2</sup>",
	  wards_durham$LABEL_NAME , wards_durham$BlkDensity
	) %>% lapply(htmltools::HTML)

}
  

#https://rstudio.github.io/leaflet/shiny.html
padding <- "padding:10px;"  # adds a margin of whitespace around app regions 

# Choices for drop-downs
vars <- c(
  "Simple pop-up" = "simple",
  "Detailed pop-up" = "detailed"
)

#define icons for legend
groups <- c("Black-Focused Healthcare Locations" <- 
			"<img src= 'https://raw.githubusercontent.com/paulinamarczak/paulinamarczak/bf214c66194e4fd1a88c09277ac2db6d45229e2a/icons/noun_Healthcare_1296604.svg' alt= 'test' height='20' width='20' style='padding:10px;' > Black-Focused Healthcare Locations"
			,"Mainstream Healthcare Locations" <-
			"<img src= 'https://raw.githubusercontent.com/paulinamarczak/paulinamarczak/bf214c66194e4fd1a88c09277ac2db6d45229e2a/icons/noun_Healthcare_1886661.svg' alt= 'test' height='20' width='20'> Mainstream Healthcare Locations"
			,"Subway station" <-
			"<img src= 'https://raw.githubusercontent.com/paulinamarczak/paulinamarczak/bf214c66194e4fd1a88c09277ac2db6d45229e2a/icons/noun_Subway_342475_black.svg' alt= 'test' height='20' width='20'> Subway station"
			,"Bus stop/station" <-
			"<img src= 'https://raw.githubusercontent.com/paulinamarczak/paulinamarczak/master/icons/noun_Bus_2558229_black.svg' alt= 'test' height='20' width='20'> Bus stop/station"
			,"GORail Station" <-
			"<img src= 'https://raw.githubusercontent.com/paulinamarczak/paulinamarczak/bf214c66194e4fd1a88c09277ac2db6d45229e2a/icons/noun_Rail_2931084_black.svg' alt= 'test' height='20' width='20'> GORail Station"
			,"Union-Pearson Express" <-
			"<img src= 'https://raw.githubusercontent.com/paulinamarczak/paulinamarczak/bf214c66194e4fd1a88c09277ac2db6d45229e2a/icons/noun_expresstrain_2699167_black.svg' alt= 'test' height='20' width='20'> Union-Pearson Express"
			,"Light Rail/ Bus Rapid Transit"<-
			"<img src= 'https://raw.githubusercontent.com/paulinamarczak/paulinamarczak/master/icons/noun_Train_18268_black.svg' alt= 'test' height='20' width='20'> Light Rail/ Bus Rapid Transit"
			,"Black Density" <-
			"<img src= 'https://github.com/paulinamarczak/paulinamarczak/blob/master/icons/black_pop_density.png?raw=true' alt= 'black population density' height='20' width='20'> Black Population Density"
)

#bottomleft mainstream vs black legend
#try this tm https://stackoverflow.com/questions/10001294/how-to-position-image-next-to-text-with-padding
html_legend <- "<img src='https://raw.githubusercontent.com/paulinamarczak/paulinamarczak/bf214c66194e4fd1a88c09277ac2db6d45229e2a/icons/noun_Healthcare_1296604.svg'  alt= 'Black-focused healthcare icon' height='30' width='30' style='padding: 7px; '> Black-focused <br/> healthcare location<br/>
<img src='https://raw.githubusercontent.com/paulinamarczak/paulinamarczak/bf214c66194e4fd1a88c09277ac2db6d45229e2a/icons/noun_Healthcare_1886661.svg' alt= 'Black-focused healthcare icon' height='30' width='30' style='padding: 7px; '> Mainstream <br/> healthcare location"


# Define UI ----
#helpful layout guide here: https://shiny.rstudio.com/articles/layout-guide.html

#use this example to get a layout that goes over the main panel.
#https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/ui.R#L3
ui <- fluidPage(
	titlePanel("", windowTitle= "Black-Focused Healthcare Locations"),
    div(class="outer",
      tags$head(
      	
        # Include our custom CSS
        includeCSS("www/styles.css"),
        #https://www.w3schools.com/howto/tryit.asp?font=Advent%20Pro finally did it
        tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Nunito&display=swap"),
        tags$style("*{font-family: Nunito, sans-serif;}",
		        	".leaflet .legend {
		                 font-family: Nunito, sans-serif;
		                 }",
		                 ) #close style
      ), #close tags
      leafletOutput("miti_map", width="100%", height="100%"),

      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = "50%", left = "auto", right = "auto", bottom = "50%",
        #width = 254, height = 70,
        #width = "10%", height = "5%",
         width = "10vw", height = "7vh",

        #https://stackoverflow.com/questions/44159168/how-to-style-an-single-individual-selectinput-menu-in-r-shiny
        #selectInput("popup_view", "", vars, selected= "simple")
        tags$b(
        materialSwitch(inputId = "popup_view_test", label = "Show survey results in pop-up", status = "primary", width= '100%')
        ),
        ) #close absolute panel
      )
    )

# Define server logic ----
server <- function(input, output, session) {	
	output$miti_map <- renderLeaflet({

	flog.info("Rendering output$miti_map")

	leaflet() %>%
	#layer order
	addMapPane("layer1", zIndex=420) %>% 
	addMapPane("layer2",zIndex=410)%>%
	addMapPane("layer3",zIndex=400)%>% 
	addMapPane("layer4",zIndex=390)%>%
	addMapPane("layer5",zIndex=380)%>% 

	addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite (Recomended)") %>%
	addTiles(group = "OpenStreetMap") %>%
	addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
	
	addScaleBar(position = "bottomright", options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE,
			updateWhenIdle = TRUE)) %>%

			#add polygons being the wards and the black density
	  addPolygons(
	  	data= wards_toronto,
	  	group= groups[8], 
	  	weight= 2,
	  	opacity = 1,
	  	color= "white",
	  	fillColor= ~pal(wards_toronto$BlkDensity),
	  	dashArray = "3",
	  	fillOpacity = 0.8,
	  	highlight = highlightOptions(
		    weight = 5,
		    color = "#666",
		    dashArray = "",
		    fillOpacity = 0.8,
		    bringToFront = TRUE),
	  	label= labels,
	  	labelOptions= labelOptions(
	  		style = list("font-weight" = "normal", padding = "3px 8px"),
	    	textsize = "15px",
	    	direction = "auto"))%>%

	addPolygons(
	  	data= wards_peel,
	  	group= groups[8], 
	  	weight= 2,
	  	opacity = 1,
	  	color= "white",
	  	fillColor= ~pal(wards_peel$BlkDensity),
	  	dashArray = "3",
	  	fillOpacity = 0.8,
	  	highlight = highlightOptions(
		    weight = 5,
		    color = "#666",
		    dashArray = "",
		    fillOpacity = 0.8,
		    bringToFront = TRUE),
	  	label= labels_peel,
	  	labelOptions= labelOptions(
	  		style = list("font-weight" = "normal", padding = "3px 8px"),
	    	textsize = "15px",
	    	direction = "auto"))%>%

	addPolygons(
	  	data= wards_durham,
	  	group= groups[8], 
	  	weight= 2,
	  	opacity = 1,
	  	color= "white",
	  	fillColor= ~pal(wards_durham$BlkDensity),
	  	dashArray = "3",
	  	fillOpacity = 0.8,
	  	highlight = highlightOptions(
		    weight = 5,
		    color = "#666",
		    dashArray = "",
		    fillOpacity = 0.8,
		    bringToFront = TRUE),
	  	label= labels_durham,
	  	labelOptions= labelOptions(
	  		style = list("font-weight" = "normal", padding = "3px 8px"),
	    	textsize = "15px",
	    	direction = "auto"))%>%

	#add markers for transit stops
	addMarkers(
		data= subway, ~X, ~Y,
		#group="Subway station",
		#try group with HTML tags
		group=groups[3],
		options = pathOptions(pane = "layer3"),
		icon = icon.ion_subway,
		popup = paste0(
	  					"Subway station: ", subway$LOCATION_N)) %>%

  	addMarkers(
	  	data=busses, ~X, ~Y,
	  	#group="Bus stop/station",
	  	#try group with HTML tags
		group=groups[4],
	  	icon= icon.ion_bus,
	  	popup = paste0(
	  					"Bus station: ", busses$LOCATION_N)) %>%

  	addMarkers(
	  	data=GORAIL, ~X, ~Y,
	  	#group="GORail Station",
	  	#try group with HTML tags
		group=groups[5],
	  	icon= icon.ion_gorail,
	  	popup = paste0(
	  					"GORail Station: ", GORAIL$LOCATION_N)) %>%

  	addMarkers(
	  	data=UPEXPRESS, ~X, ~Y,
	  	#group="Union-Pearson Express",
	  	#try group with HTML tags
		group=groups[6],
	  	icon= icon.ion_up,
	  	popup = paste0(
	  					"UP Express Station: ", UPEXPRESS$LOCATION_N)) %>%

  	#addAwesomeMarkers(
  	addMarkers(
	  	data=LRTBRT, ~X, ~Y,
	  	#group="Light Rail/ Bus Rapid Transit",
		group=groups[7],
	  	icon= icon.ion_train,
	  	popup = paste0(
	  					"LRT/BRT Station: ", LRTBRT$LOCATION_N)) %>%

	addMarkers(data =black_focused_health, ~X, ~Y, clusterOptions =
                                                    markerClusterOptions()) %>%

  	hideGroup(groups[3]) %>%
  	hideGroup(groups[4]) %>%
  	hideGroup(groups[5]) %>%
  	hideGroup(groups[6]) %>%
	hideGroup(groups[7]) %>%

	addLayersControl(
		baseGroups = c("Toner Lite (Recommended)","OpenStreetMap", "Toner"),
		overlayGroups = groups,
        options = layersControlOptions(collapsed = TRUE)) %>%

	#set default extent to Toronto
	setView(-79.3832,43.6532, zoom = 10) %>%

 	addControl(html = html_legend, position = "bottomleft")%>%

	addLegend(
		"bottomright", pal = pal, values = bins,
		title = "Black Population Density <br>(people per km<sup>2</sup>)", opacity = 1) %>%
 
	#add a "locate me" button
	addEasyButton(easyButton(
	    icon="fa-crosshairs", title="Locate Me",
	    onClick=JS("function(btn, map)
	    	{ map.locate(
	    		{setView: true}
	    		); 
	    		}")
	     )) #close easy button
 })

	observe({
		  
		    if (input$popup_view_test == "FALSE") {

		    leafletProxy("miti_map") %>%
		    #https://stackoverflow.com/questions/33143169/how-to-get-layer-to-top-in-shiny-leaflet-map
		    clearGroup(group = c(groups[1], groups[2])) %>% 
		    clearMarkerClusters() %>%
			      addMarkers(
				  	data=black_focused_health, ~X, ~Y, 
				  	# group= "Black-Focused Healthcare Locations",
				  	group= groups[1],
				  	icon= icon.glyphicon_black,
				  	#clusters should be same colours as icons
				  	#https://stackoverflow.com/questions/47507854/coloring-clusters-by-markers-inside
				  	#https://esri.github.io/esri-leaflet/examples/styling-clusters.html
				  	#LOOK AT TODAY esri
				  	# clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE
				  	# )#close markercluster
				  	#,
				  	options = pathOptions(pane = "layer1"),
				  	#clusterId = "cluster",
				  	popup = paste0(
				  					"Name: ", black_focused_health$Name,
				  					"<br>",
				  					"Services Provided: ", black_focused_health$Services,
				  					"<br>",
				  					"Information Hub: ", black_focused_health$Information,
				  					"<br>",
				  					"Phone: ", black_focused_health$Phone,
				  					"<br>",
				  					"Email: ",
				  					"<a href='"
				  					, paste0("mailto:", black_focused_health$Email)
				  					, "' target='_blank'>"
				  					, paste0(black_focused_health$Email, "</a>") ,
				  					"<br>",
				  					"Website: ",
				  					"<a href='"
				                 	, black_focused_health$Website
				                 	, "' target='_blank'>"
				                 	, paste0(black_focused_health$Website, "</a>") ,
				  					"<br>",
				  					"Address: ",
				  					"<a href='"
				  					, paste0("https://www.google.com/maps/search/?api=1&query=", black_focused_health$Y, "+", black_focused_health$X)
				  					, "' target='_blank'>"
				  					, paste0(black_focused_health$Address, "</a>") ,
				  					"<br>",
				  					"Region: ", black_focused_health$Region,
				  					"<br>",
				  					"Province: ", black_focused_health$Province,
				  					"<br>",
				  					"Latitude: ", black_focused_health$Y,
				  					"<br>",
				  					"Longitude: ", black_focused_health$X,
				  					"<br>",
				  					"Type: ", black_focused_health$Type,
				  					"<br>"
				  					))%>% #close first addMarkers call

				# htmlwidgets::onRender("function (cluster) {    
				# 	    var childCount = cluster.getChildCount(); 
				# 	    var c = ' marker-custom-';  
				# 	    if (childCount < 10) {  
				# 	      c += 'large';  
				# 	    } else if (childCount < 40) {  
				# 	      c += 'medium';  
				# 	    } else { 
				# 	      c += 'small';  
				# 	    }    
				# 	    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
				# 	  }") %>%
			# 	htmlwidgets::onRender("function(el,x) {
			#   map = this;  
			  
			#   var style = document.createElement('style');
			#   style.type = 'text/css';
			#   style.innerHTML = '.red, .red div { background-color: rgba(255,0,0,0.6); }'; // set both at the same time
			#   document.getElementsByTagName('head')[0].appendChild(style);


			#   var cluster = map.layerManager.getLayer('cluster','cluster'); 
			#   cluster.options.iconCreateFunction = function(c) {
			#     var markers = c.getAllChildMarkers();
			#     var priority = {
			#      'green': 0,
			#      'orange': 1,
			#      'red': 2
			#     };
			#     var highestRank = 0; // defaults to the lowest level to start
			                        
			#     markers.forEach(function(m) {
			#     var color = m.options.icon.options.markerColor;
			                        
			#     // check each marker to see if it is the highest value
			#     if(priority[color] > highestRank) {
			#        highestRank = priority[color];  
			#      }                      
			#   })
			                        
			#   var styles = [
			#     'marker-cluster-small', // green
			#     'marker-cluster-large',  // orange
			#     'red' // red
			#   ]
			                        
			#   var style = styles[highestRank];
			#   var count = markers.length;
			                        
			#    return L.divIcon({ html: '<div><span>'+count+'</span></div>', className: 'marker-cluster ' + style, iconSize: new L.Point(40, 40) });
			#  }
			# }") %>%

			  addMarkers(
			     data=mainstream_health, ~X, ~Y, 
				  	#group= "Mainstream Healthcare Locations",
				  	group= groups[2],
				  	icon= icon.glyphicon_nonblack,
				  	clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE
				  	)
				  	,
				  	options = pathOptions(pane = "layer2"),
				  	popup = paste0(
				  					"Name: ", mainstream_health$Name,
				  					"<br>",
				  					"Services Provided: ", mainstream_health$Services,
				  					"<br>",
				  					"Information Hub: ", mainstream_health$Information,
				  					"<br>",
				  					"Phone: ", mainstream_health$Phone,
				  					"<br>",
				  					"Email: ",
				  					"<a href='"
				  					, paste0("mailto:", mainstream_health$Email)
				  					, "' target='_blank'>"
				  					, paste0(mainstream_health$Email, "</a>") ,
				  					"<br>",
				  					"Website: ",
				  					"<a href='"
				                 	, mainstream_health$Website
				                 	, "' target='_blank'>"
				                 	, paste0(mainstream_health$Website, "</a>") ,
				  					"<br>",
				  					"Address: ",
				  					"<a href='"
				  					, paste0("https://www.google.com/maps/search/?api=1&query=", mainstream_health$Y, "+", mainstream_health$X)
				  					, "' target='_blank'>"
				  					, paste0(mainstream_health$Address, "</a>") ,
				  					#try https://stackoverflow.com/questions/2660201/what-parameters-should-i-use-in-a-google-maps-url-to-go-to-a-lat-lon
				  					#http://maps.google.com/maps?z=12&t=m&q=loc:38.9419+-78.3020
				  					#requires decimal format
				  					"<br>",
				  					"Region: ", mainstream_health$Region,
				  					"<br>",
				  					"Province: ", mainstream_health$Province,
				  					"<br>",
				  					"Latitude: ", mainstream_health$Y,
				  					"<br>",
				  					"Longitude: ", mainstream_health$X,
				  					"<br>",
				  					"Type: ", mainstream_health$Type,
				  					"<br>"
				  					 ))    
		      
		    }

		    else {
		    
		     leafletProxy("miti_map", data = black_focused_health) %>%
			      clearPopups() %>%
			      #clear markers but you have to use clearGroup function otherwise the clustering will doubleup
			      clearGroup(group = c(groups[1], groups[2])) %>% 
			     	addMarkers(
					  	data=black_focused_health, ~X, ~Y, 
					  	#group= "Black-Focused Healthcare Locations",
					  	group= groups[1],
					  	icon= icon.glyphicon_black,
					  	# clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE
					  	# 	)
					  	# ,
					  	options = pathOptions(pane = "layer1"),
					  	popup = paste0(
					  					"Name: ", black_focused_health$Name,
					  					"<br>",
					  					"Services Provided: ", black_focused_health$Services,
					  					"<br>",
					  					"Information Hub: ", black_focused_health$Information,
					  					"<br>",
					  					"Phone: ", black_focused_health$Phone,
					  					"<br>",
					  					"Email: ",
					  					"<a href='"
					  					, paste0("mailto:", black_focused_health$Email)
					  					, "' target='_blank'>"
					  					, paste0(black_focused_health$Email, "</a>") ,
					  					"<br>",
					  					"Website: ",
					  					"<a href='"
					                 	, black_focused_health$Website
					                 	, "' target='_blank'>"
					                 	, paste0(black_focused_health$Website, "</a>") ,
					  					"<br>",
					  					"Address: ",
					  					"<a href='"
					  					, paste0("https://www.google.com/maps/search/?api=1&query=", black_focused_health$Y, "+", black_focused_health$X)
					  					, "' target='_blank'>"
					  					, paste0(black_focused_health$Address, "</a>") ,
					  					"<br>",
					  					"Region: ", black_focused_health$Region,
					  					"<br>",
					  					"Province: ", black_focused_health$Province,
					  					"<br>",
					  					"Latitude: ", black_focused_health$Y,
					  					"<br>",
					  					"Longitude: ", black_focused_health$X,
					  					"<br>",
					  					"Type: ", black_focused_health$Type,
					  					"<br>",
					  					"Social Network Analysis Survey: ", black_focused_health$SNA
					  					)) %>%
			     	addMarkers(
			     		data=mainstream_health, ~X, ~Y, 
			     		group= groups[2],
			     		icon= icon.glyphicon_nonblack,
			     		clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE
			     			)
			     		,
			     		options = pathOptions(pane = "layer2"),
			     		popup = paste0(
				  					"Name: ", mainstream_health$Name,
				  					"<br>",
				  					"Services Provided: ", mainstream_health$Services,
				  					"<br>",
				  					"Information Hub: ", mainstream_health$Information,
				  					"<br>",
				  					"Phone: ", mainstream_health$Phone,
				  					"<br>",
				  					"Email: ",
				  					"<a href='"
				  					, paste0("mailto:", mainstream_health$Email)
				  					, "' target='_blank'>"
				  					, paste0(mainstream_health$Email, "</a>") ,
				  					"<br>",
				  					"Website: ",
				  					"<a href='"
				                 	, mainstream_health$Website
				                 	, "' target='_blank'>"
				                 	, paste0(mainstream_health$Website, "</a>") ,
				  					"<br>",
				  					"Address: ",
				  					"<a href='"
				  					, paste0("https://www.google.com/maps/search/?api=1&query=", mainstream_health$Y, "+", mainstream_health$X)
				  					, "' target='_blank'>"
				  					, paste0(mainstream_health$Address, "</a>") ,
				  					"<br>",
				  					"Region: ", mainstream_health$Region,
				  					"<br>",
				  					"Province: ", mainstream_health$Province,
				  					"<br>",
				  					"Latitude: ", mainstream_health$Y,
				  					"<br>",
				  					"Longitude: ", mainstream_health$X,
				  					"<br>",
				  					"Type: ", mainstream_health$Type,
				  					"<br>",
				  					"Social Network Analysis Survey: ", mainstream_health$SNA
				  					 ))
		      
		    }

		}) #close observe
}

shinyApp(ui = ui, server = server)



#///////////////////////////EXAMPLE//////////////TESTING/////////////#
