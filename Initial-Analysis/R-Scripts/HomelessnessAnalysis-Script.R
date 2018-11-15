library(dplyr)
library(stringr)
library(stringi)
library(ggmap)


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Read the dataset that has the CoC GeoCodes, so that it can be linked into other datasets
cocgeocodes <- read.csv("../Datasets/COCNumWithGeoCodes.csv")

#Read Homeless dataset, that has info on shelter beds etc. across all CoCs
HomelessData <- read.csv("../Datasets/CoCGranteeAreasWithGeoCodes2.csv")

#Filter to compile dataset that are Balance of State vs. other jurisdictions
BOSHomelessData <- filter (HomelessData, str_detect(COCNAME, 'Balance of State'))
NonBOSHomelessData <- filter(HomelessData, !str_detect(COCNAME, 'Balance of State'))

#Read dataset that lists # of homeless adults, veterans and youth 
hdata <- read.csv("../Datasets/HomelessData2016_Mod.csv")
hdata$lon = NULL
hdata$lat = NULL

#Merge Lat and Lon details from CoCGeoCodes dataset; capture only non-Balance of State jurisdictions
hdata <- merge(hdata, cocgeocodes, by.x="CoCNumber", by.y="COCNUM")
hdata <- filter(hdata, !str_detect(CoCName, 'Balance of State'))
#hdata <- hdata %>% na.omit()
hdata <- filter (hdata, !is.na(hdata$lat))
hdata$region = substr(hdata$CoCNumber, 1, 2)

#Read the US Map
usa_center = as.numeric(geocode("United States"))
USAMap = get_googlemap(center=usa_center, scale=2, zoom=4)

#Use this to see where there are higher homeless counts
hdatatop35 <- tbl_df(hdata) %>% 
  top_n(35, TotalHomeless2016)

#Homeless veterans
hvdatatop35 <- tbl_df(hdata) %>% 
  top_n(35, HomelessVeterans2016)

#Homeless youth under age of 25
hyouthtop35 <- tbl_df(hdata) %>% 
  top_n(35, HomelessUnder25Youth2016)

#ALL Homeless Counts
hdata$AllHomelessCounts = hdata$TotalHomeless2016 + hdata$HomelessVeterans2016 + hdata$HomelessUnder25Youth2016
AllHomelessTop35 <- tbl_df(hdata) %>% 
  top_n(35, AllHomelessCounts)

#Exclude Hawaii, so that the US map shows up better
hdatatop35 <- subset(hdatatop35, !(region == "HI"))
hvdatatop35 <- subset(hvdatatop35, !(region == "HI"))
hyouthtop35 <- subset(hyouthtop35, !(region == "HI"))
AllHomelessTop35 <- subset(AllHomelessTop35, !(region == "HI"))


us <- map_data("state")

#draw a blank map of the United States
gg <- ggplot()
gg <- gg + geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region),
                    fill="white", color="black")  #, size=0.15)
gg <- gg + 
  geom_point(aes(x=lon, y=lat, size=AllHomelessCounts, colour="red", alpha=0.8), data=AllHomelessTop35) + 
  geom_point() +
  labs(x='Longitude', y='Latitude') +
  ggtitle("Homeless Counts by CoC (Top 35)") +
  scale_size_continuous(name="Homeless Counts", range = c(2,12)) +
  theme(legend.position="none")

gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(axis.text = element_blank())
gg


COLByCities <- read.csv("../Datasets/COL_ByCities.csv")
COLByCities$UrbanArea <- gsub(", ", " ", COLByCities$UrbanArea)

COLByCities$State <- COLByCities$UrbanArea
COLByCities$State <- substrRight(COLByCities$State, 2)
COLByCities <- group_by(COLByCities, State)
COLByCities <- summarise(COLByCities, COLStateAvg=mean(COLIndex), HousingIndex=mean(Housing))


us <- map_data("state")
usstates <- read.csv("../Datasets/StateNames.csv")
usstates <- usstates %>%
  add_rownames("region") %>%
  mutate(region=tolower(StateName))

# Merge US States and Cost of Living by cities datasets for color-coding the US Map
usstates$StateName=NULL
usstates <- merge(usstates, COLByCities, by.x="State", by.y="State")

#Plot a blank US Map
BlankUSMap <- ggplot()
BlankUSMap <- BlankUSMap + geom_map(data=us, map=us,
                                    aes(x=long, y=lat, map_id=region), 
                                    fill="white", color="black")

#US Map, color coded based on the COL Index in the state
COLMap <- BlankUSMap + geom_map(data=usstates, map=us, 
                                      aes(fill=HousingIndex, map_id=region), color="#ffffff", size=0.15) 

COLMap <- COLMap + scale_fill_continuous(low='gray94', high='thistle3', guide='colorbar')

COLMap <- COLMap + labs(x=NULL, y=NULL) + 
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) +
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank())
#COLMap

#draw map of the United States, filled based on Housing Index
homelessmap <- COLMap + 
  geom_point(aes(x=lon, y=lat, size=AllHomelessCounts, colour="red", alpha=0.8), data=AllHomelessTop35) + 
  labs(x='Longitude', y='Latitude') +
  ggtitle("Homeless Counts by CoC (Top 35)") +
  scale_size_continuous(name="Homeless Counts", range = c(2,12), guide = FALSE) +
  scale_alpha(guide=FALSE) +
  scale_colour_discrete(guide=FALSE)
# use this to remove all legends
#  theme(legend.position="none")

homelessmap <- homelessmap + labs(x=NULL, y=NULL) + 
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank())

homelessmap

#Map top 35 jurisdictions according to homeless adults counts
homelessadultsmap <- COLMap + 
  geom_point(aes(x=lon, y=lat, size=TotalHomeless2016, colour="red", alpha=0.8), data=hdatatop35) + 
  geom_point() +
  labs(x='Longitude', y='Latitude') +
  ggtitle("Homeless Adults Counts - Top 35") +
  scale_size_continuous(name="Homeless Adults Count", range = c(2,12), guide=FALSE) +
  scale_alpha(guide=FALSE) +
  scale_colour_discrete(guide=FALSE)
# use this to remove all legends
#  theme(legend.position="none")

homelessadultsmap <- homelessadultsmap + labs(x=NULL, y=NULL) + 
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank())
homelessadultsmap

homelessvetsmap <- COLMap + 
  geom_point(aes(x=lon, y=lat, size=HomelessVeterans2016, colour="red", alpha=0.8), data=hvdatatop35) + 
  geom_point() +
  labs(x='Longitude', y='Latitude') +
  ggtitle("Homeless Veterans Counts - Top 35") +
  scale_size_continuous(name="Homeless Veterans Count", range = c(2,12), guide=FALSE) +
  scale_alpha(guide=FALSE) +
  scale_colour_discrete(guide=FALSE)
# use this to remove all legends
#  theme(legend.position="none")

homelessvetsmap <- homelessvetsmap + labs(x=NULL, y=NULL) + 
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank())
homelessvetsmap

HomelessYouthMap <- COLMap + 
  geom_point(aes(x=lon, y=lat, size=HomelessUnder25Youth2016, colour="red", alpha=0.8), data=hyouthtop35) + 
  geom_point() +
  labs(x='Longitude', y='Latitude') +
  ggtitle("Homeless Youth (< 25) Counts - Top 35") +
  scale_size_continuous(name="Homeless Youth (under 25) Count", range = c(2,12), guide=FALSE) +
  scale_alpha(guide=FALSE) +
  scale_colour_discrete(guide=FALSE)
# use this to remove all legends
#  theme(legend.position="none")

HomelessYouthMap <- HomelessYouthMap + labs(x=NULL, y=NULL) + 
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank())
HomelessYouthMap

# mapping TOP 35 jurisdictions based on counts of homeless Adults, Veterans and Youth in the same map
AdultsVetsHomelessMap <- COLMap +
  geom_point(aes(x=lon, y=lat, size=TotalHomeless2016, colour="Adults", alpha=0.8), data=hdatatop35) + 
  geom_point(aes(x=lon, y=lat, size=HomelessVeterans2016, colour="Veterans", alpha=0.8), data=hvdatatop35) +
  geom_point(aes(x=lon, y=lat, size=HomelessVeterans2016, colour="Youth", alpha=0.8), data=hyouthtop35) +
  scale_size_continuous(name="Homeless Count", range = c(2,10), guide=FALSE) +
  labs(x='Longitude', y='Latitude') +
  ggtitle("Homeless Counts by Category (Adults, Veterans, Youth)") +
  scale_alpha(guide=FALSE) +
  scale_colour_discrete(name="Category")
  #theme(legend.position="none")

AdultsVetsHomelessMap <- AdultsVetsHomelessMap + 
  labs(x=NULL, y=NULL) + 
  theme(panel.border = element_blank()) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(axis.text = element_blank())
AdultsVetsHomelessMap

#Read Grants dataset

