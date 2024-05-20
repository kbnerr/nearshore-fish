
#Map of CCT sample locations for reports/presentations
#Author: Taylor Cubbage 
#Date: 4/5/2024

#Install necessary packages
install.packages(ggmap)
library(ggmap)

#setting working directory - update where you saved the CCT data in your computer
setwd("C:/Users/tlcubbage/OneDrive - State of Alaska/Documents/working from home/Cutthroat Trout/2021-23 CCT Project/Sampling")

#Opening spreadsheet
cct <- read.csv("CCT24.csv")
str(cct)
View(cct)

#fixing some data categories
cct$Lon <- as.numeric(cct$Lon)
cct$Year <- as.character(cct$Year)
cct$CCT <- as.character(cct$CCT)
cct$Likelihood <- factor(cct$Likelihood, levels = c("0.0-0.25", "0.26-0.50", "0.51-0.75", "0.76-1.0"))

####Making the map with stadia maps
#First go to this website and make a free account: https://client.stadiamaps.com/signup/
#Once you have an account, go to "manage properties" tab at the top of the screen, scroll down and 
#find your unique API key. Enter it below where my API key is. I think you could honestly leave
#my API key in there and use it if the website doesn't work for some reason.

#setting up API key with Stadia maps
register_stadiamaps("204ae593-dba5-48a2-8879-681d8a6970b4", write = TRUE)

#making border to plot. Lat/lon of your study area bounds
PWS_border <- c(-148.8,59.7, -145.4, 61.3)

#making a label for PWS in the map and its associated lat/lon where I want it displayed
pws <- data.frame(
  name = "Prince William\nSound",
  lat = -146.9,
  lon = 60.55)

#Making the map. Zoom controls things like details of the map if you use a more detailed terrain layer like "stamen_terrain".
#Larger the zoom, the longer time it takes the map to load though.
#Try different map types, you can see a list of options by searching the "get_stadiamap()" command
#in R's help tab.
map <- get_stadiamap(PWS_border, zoom = 8, maptype = "stamen_toner_lite")

#this is plotting the map with ggmap so you can actually see it
#geom point plots the cutthroat locations, size is by likelihood of finding them, color is presence/absence, "CCT" in my datasheet,
#and scale_color_viridis is a colorblind friendly palette that applies the color. Alpha changes the transparency 
#to make the dots slightly see-through. The theme stuff just removes the axis and title labels, but you can keep them if you like.
#Sorry if I am over explaining, you probably know these :)

ggmap(map)+
  geom_point(data = cct, aes (x = Lon, y = Lat, color = CCT, size = Likelihood))+
  scale_color_viridis_d(begin = 0, end = 0.4, alpha = 0.4)+
  geom_text(data = pws, size = 4, aes(x = lat, y = lon, label = name, fontface = 3))+
  theme(axis.title = element_blank(), 
        axis.text  = element_blank(),
        axis.ticks = element_blank())

#And that's it, go forth and make beautiful maps!
  

