library(pacman) #load in this to make things easy
pacman::p_load("lubridate","pdftools","jsonlite","leaflet",install = T)

setwd("~/Desktop/SICSS/SICSS-data-sessions/Day1")


#############################################################################
# scrape basic information
#############################################################################

# navigate to https://www.phillypolice.com/ois/
#  - view page source
#  - save page as html
a <- scan(file="Officer Involved Shootings _ Philadelphia Police Department.html",
          what="",sep="\n")

a[1:20] #lines 1-20
a[6000:6020] #lines 6k-6020

# extract the incident ID
i <- grep("o\\.title", a)
head(a[i])
gsub("<[^>]*>","",a[i]) #removing html tags replace with nothing

# two lines later is the location
gsub("<[^>]*>","",a[i+2]) #removing html tags ok replace things two lines later 

# three lines later is the date
gsub("<[^>]*>","",a[i+3]) #removing html tags ok replace 3 lines later 

# also get the URL with the text description
gsub('.* href="([^"]*)".*',"\\1",a[i])

# pack it all into a data frame
ois <- data.frame(id=gsub("<[^>]*>","",a[i]),
                  location=gsub("<[^>]*>","",a[i+2]),
                  date    =gsub("<[^>]*>","",a[i+3]),
                  url     =gsub('.* href="([^"]*)".*',"\\1",a[i]))

# communicate to R that date column are really dates
ois$date <- mdy(ois$date)

# Let's just work shootings since 2016 (data are a little cleaner)
ois <- subset(ois, !is.na(date) & date >= "2016-01-01")

# check for duplicates
any(duplicated(ois$id))
subset(ois, duplicated(id))
subset(ois, id=="17-13") #drop the duplicate 
# remove duplicate
ois <- subset(ois, !duplicated(id))

# another duplicate is missing ID
subset(ois, grepl("Carlisle", location))
ois <- subset(ois, id!="") #removing one with the duplicate ID 

#############################################################################
# get text descriptions of incidents
#############################################################################

# try with the first incident
ois$url[1] # try viewing in browser, view source
a <- scan(ois$url[1], what="", sep="\n")

iStart <- grep("entry-content clearfix", a) + 1 # header wrapper
iEnd   <- grep("\\.entry-content", a)       - 1 # footer wrapper
a <- paste(a[iStart:iEnd], collapse="\n")
a <- gsub("<[^>]*>", "", a)
a

# also remove spaces, tabs, line feeds at the beginning and end
gsub("^[[:space:]]*|[[:space:]]*$", "", "\t \t text to keep \n\n\r\t  ")
a <- gsub("^[[:space:]]*|[[:space:]]*$", "", a)
a

# Now all incidents
ois$text <- NA
for(i in 1:nrow(ois))
{
  a <- scan(ois$url[i], what="", sep="\n")
  iStart <- grep("entry-content clearfix", a) + 1
  iEnd   <- grep("\\.entry-content", a)       - 1
  
  if(length(iEnd)>0 && length(iStart)>0 && (iEnd-iStart > 1))
  {
    a <- paste(a[iStart:iEnd], collapse="\n")
    a <- gsub("<[^>]*>", "", a)
    a <- gsub("^[[:space:]]*|[[:space:]]*$", "", a)
    ois$text[i] <- a
  } else
  {
    cat("No text for ",ois$id[i],"\n") #tells us that theres no text descriptions 
  }
}

ois[1,]


# some appear to be missing descriptions
subset(ois, is.na(text))

# https://ois.sites.phillypolice.com/16-42/
# https://ois.sites.phillypolice.com/20-09/
# but there is another source in pdf format
# http://www.phillypolice.com/assets/crime-maps-stats/officer-involved-shootings/16-42.pdf
# http://www.phillypolice.com/assets/crime-maps-stats/officer-involved-shootings/20-09.pdf

a <- pdf_text("http://www.phillypolice.com/assets/crime-maps-stats/officer-involved-shootings/16-42.pdf")
ois$text[ois$id=="16-42"] <- a
a <- pdf_text("http://www.phillypolice.com/assets/crime-maps-stats/officer-involved-shootings/20-09.pdf")
ois$text[ois$id=="20-09"] <- a




#############################################################################
# map location of incidents
#############################################################################

# fix weird HTML characters
grep("&", ois$location, value=TRUE)
ois$location <- gsub("&amp;","and",ois$location)

# put "blocks" at the midpoint
grep("[Bb]lock", ois$location, value=TRUE)
ois$location <- gsub("00 [Bb]lock( of)?", "50", ois$location) #puts it in the middle of the block (50 = middle of the block) and accounting for upper and lower case brackets
ois$location <- gsub("[Uu]nit [Bb]lock( of)?", "50", ois$location) # and some that are the unit block of 
# additional cleanup
ois$location <- gsub("[Nn]ear ", "", ois$location)




# ArcGIS online geocoder
#    Note spelling error here!        ****
a <- gsub(" +", "\\%20", "3718 Locust Wall, Philadelphia, PA")
a <- paste0("https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?f=json&singleLine=",
            a,
            "&outFields=Match_addr,Addr_type")
geoResults <- fromJSON(a)
geoResults
geoResults$candidates$location$x
geoResults$candidates$location$y
geoResults$candidates$score
geoResults$candidates$attributes$Addr_type
geoResults$candidates$attributes$Match_addr

# a function for using ArcGIS geocoder service
geocodeARCGIS <- function(address)
{
  a <- gsub(" +", "\\%20", address)
  a <- paste0("https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?f=json&singleLine=",
              a,
              "&outFields=Match_addr,Addr_type")
  
  geoResults <- jsonlite::fromJSON(a)
  geoResults <- list(coords=geoResults$candidates$location[1,],
                     score=geoResults$candidates$score[1],
                     type=geoResults$candidates$attributes$Addr_type[1],
                     match=geoResults$candidates$attributes$Match_addr[1])
  return( geoResults )
}

geocodeARCGIS("3718 Locust Walk, Philadelphia, PA")
geocodeARCGIS("37th St and Locust Walk, Philadelphia, PA")


gcPenn <- geocodeARCGIS("3718 Locust Walk, Philadelphia, PA")

leaflet() |>
  addTiles() |>
  setView(lng=gcPenn$coords$x, lat=gcPenn$coords$y, zoom=18) |>
  addCircleMarkers(lng=gcPenn$coords$x,
                   lat=gcPenn$coords$y)


# check locations that are blank
i <- grep("[Ww]ithheld",ois$location)
ois[i,]

# fix the missing address
ois$location[ois$id=="16-18"] <- "3250 Wellington Street"
# this one is not really a police shooting
ois <- subset(ois, id!="16-26")

# examine other addresses
ois[,c("id","location")]

subset(ois, id %in% c("16-30","16-10","17-08"), select=c("id","location","text"))

# this is not a PPD shooting
ois <- subset(ois, id!="17-08")
# two locations, let's use the first one
ois$location[ois$id=="16-10"] <- "5750 N. Broad Street"
# pick the location where the police shooting occurred
ois$location[ois$id=="16-30"] <- "4850 Sansom Street"

# add the city
ois$location <- paste0(ois$location,", Philadelphia, PA")


# time to geocode for each observation 
a <- geocodeARCGIS(ois$location[1])

ois$addrtype <- ois$score <- ois$addrmatch <- ois$lat <- ois$lon <- NA
for(i in 1:nrow(ois))
{
  print(i)
  print(ois$location[i])
  a <- geocodeARCGIS(ois$location[i])
  
  ois$lon[i]       <- a$coords$x
  ois$lat[i]       <- a$coords$y
  ois$score[i]     <- a$score
  ois$addrtype[i]  <- a$type
  ois$addrmatch[i] <- a$match
}

# check for signs of geocoding problems
table(ois$addrtype)
subset(ois, addrtype=="StreetName")

i <- which(ois$addrtype=="StreetName")

leaflet(ois[i,c("lon","lat")]) |>
#  setView(lng=ois$lon[i], lat=ois$lat[i], zoom=16) |>
  addTiles() |>
  addCircleMarkers(~lon, ~lat,
                   radius=3, stroke=FALSE,
                   fillOpacity = 1) |>
  addPopups(~lon, ~lat, ois$location[i])


# lookup on Google Maps for #21-14
ois[ois$id=="21-14", c("lat","lon")] <- c(39.975984, -75.203309) #adjusting something that isnt correct


# spot check a few
i <- sample(1:nrow(ois), size=1)
ois$location[i]
leaflet(ois[i,c("lon","lat")]) |>
  setView(lng=ois$lon[i], lat=ois$lat[i], zoom=16) |>
  addTiles() |>
  addCircleMarkers(~lon, ~lat,
                   radius=3, stroke=FALSE,
                   fillOpacity = 1) |>
  addPopups(~lon, ~lat, ois$location[i])


# generate city plot
leaflet(ois[,c("lon","lat")]) |>
   addTiles() |>
   addCircleMarkers(~lon, ~lat,
                    radius=3, stroke=FALSE,
                    fillOpacity = 1,
                    popup = paste("<b>",ois$location,"</b><br>",ois$text),
                    popupOptions = popupOptions(autoClose = TRUE, 
                                                closeOnClick = FALSE))
  

############################################################################
# Ask ChatGPT to answer questions about text
############################################################################
#   models evolving fast https://platform.openai.com/docs/models/overview
#   requires an OpenAI account https://platform.openai.com/login
#   also requires billing information
#   reviewing 95 descriptions cost 10 cents

library(httr)
# Define the API endpoint and key
GPTurl <- "https://api.openai.com/v1/chat/completions"
key        <- ""

# Define the text to send to the model
gptQuery <- paste("Did the police transport the person they shot to the hospital in this incident? Just answer yes, no, or unknown:",
              ois$text[1])

# ask GPT
response <- POST(GPTurl,
                 add_headers(Authorization = paste0("Bearer ", key)),
                 body = list(model = "gpt-3.5-turbo",
                             max_tokens = 1,  # force yes/no
                             temperature = 0, # force best answer/no "creativity"
                                              # default 0.7,
                             messages = list(list(role="user",
                                                  content=gptQuery))),
                 encode = "json")
# extract response
content(response)$choices[[1]]$message$content



ois$transport <- NA
for(i in 1:nrow(ois))
{
  gptQuery <- paste("Did the police transport the person they shot to the hospital in this incident? Just answer yes, no, or unknown:",
                    ois$text[i])
  response <- POST(GPTurl,
                   add_headers(Authorization = paste0("Bearer ", key)),
                   body = list(model = "gpt-3.5-turbo",
                               max_tokens = 1,
                               temperature = 0,
                               messages = list(list(role="user",
                                                    content=gptQuery))),
                   encode = "json")
  ois$transport[i] <- content(response)$choices[[1]]$message$content
  print(ois[i,c("id","transport")])
}


table(tolower(ois$transport))
with(subset(ois, tolower(transport)=="no"), text)
with(subset(ois, tolower(transport)=="unknown"), text[1:3])




ois$transportDetail <- NA
for(i in 1:nrow(ois))
{
  gptQuery <- paste("Did the police transport the person they shot to the hospital in this incident?:",
                    ois$text[i])
  response <- POST(GPTurl,
                   add_headers(Authorization = paste0("Bearer ", key)),
                   body = list(model = "gpt-3.5-turbo",
                               max_tokens = 100,
                               temperature = 0,
                               messages = list(list(role="user",
                                                    content=gptQuery))),
                   encode = "json")
  ois$transportDetail[i] <- content(response)$choices[[1]]$message$content
  print(ois[i,c("id","transportDetail")])
}

t(ois[,c("id","transport","transportDetail")])

subset(ois, id=="21-15")
