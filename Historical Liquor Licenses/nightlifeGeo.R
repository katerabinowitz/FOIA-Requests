require(dplyr)
require(rgdal)
require(ggmap)

setwd("/Users/katerabinowitz/Documents/DataLensDCOrg/nightlifeTimeline")

### Double to single address
### Double to single address
### Double to single address
addressFix <- allTogetherNow %>% subset(grepl("-", address)) %>%
  distinct(license, .keep_all = TRUE) %>%
  mutate(address = gsub("2346|3502|1064|3058|3226|2442| 529|3712|
                        |1815|1212|1412|1123|2342|02|C-1|1402|2422| -|-", "",address),
         address = ifelse("3277 M ST NW 00C10", "3277 M ST NW", address)) %>%
  select(license, address)

llFinal <- allTogetherNow %>% left_join(addressFix, by="license") %>%
  mutate(address = paste0(ifelse(is.na(address.y), address.x, address.y), ", Washington DC")) %>%
  select(-address.x, -address.y) %>%
  arrange(license, year)

### Geocode addresses
### Geocode addresses
### Geocode addresses
geoAddress <- llFinal %>% distinct(license, .keep_all = TRUE) 
LatLong <- geocode(geoAddress$address, source="google")

geoCoded <- cbind(geoAddress, LatLong)

llFinal <- llFinal %>% select(license, year)

llFinal <- inner_join(llFinal, geoCoded2, by="license")

### Assign neighborhoods
### Assign neighborhoods
### Assign neighborhoods
hood = readOGR(dsn="http://ec2-54-235-58-226.compute-1.amazonaws.com/storage/f/2013-05-12T03%3A50%3A18.251Z/dcneighorhoodboundarieswapo.geojson", layer="OGRGeoJSON")

latLonHood <- llFinal %>% select(lon, lat)

hoodPoints <- SpatialPoints(latLonHood, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
HoodID <- over(hoodPoints, hood)

liquorFinal <- cbind(llFinal, HoodID) %>% 
              mutate(year = ifelse(year.x==8, "2008", 
                              ifelse(year.x==9, "2009", 
                                ifelse(year.x==10, "2010", 
                                  ifelse(year.x==11, "2011", 
                                    ifelse(year.x==12, "2012", 
                                      ifelse(year.x==13, "2013", 
                                        ifelse(year.x==14, "2014", 
                                          ifelse(year.x==15, "2015", "2016"))))))))) %>% 
              select(-year.x, -year.y, -id)

write.csv(liquorFinal, "nightlife0816.csv")
