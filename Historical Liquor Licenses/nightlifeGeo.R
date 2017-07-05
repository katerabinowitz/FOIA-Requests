require(dplyr)
require(tidyr)
require(rgdal)
require(geojsonio)
require(ggmap)

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

llFinal <- inner_join(llFinal, geoCoded, by="license")

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

#add year open variable (2008 means 2008 or earlier)
yrOpen <- liquorFinal %>% group_by(license) %>%
  arrange(year) %>%
  slice(1) %>%
  rename(firstYr = year) %>%
  select(license, firstYr)

liquorFinal <- liquorFinal %>% left_join(yrOpen, by="license")

write.csv(liquorFinal, "./nightlife0816.csv")

geojson_write(liquorFinal, geometry = "point",
              file = "nightlife.geojson", overwrite = TRUE)

#neighborhood cluster summaries
cluster = readOGR(dsn="https://opendata.arcgis.com/datasets/f6c703ebe2534fc3800609a07bad8f5b_17.geojson", layer="OGRGeoJSON")
latLon <- liquorFinal %>% select(lon, lat)

points <- SpatialPoints(latLon, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
clusterID <- over(points, cluster)

nlCluster <- cbind(liquorFinal, clusterID)
nlCount <- nlCluster %>% group_by(NBH_NAMES, year) %>%
                         summarise(licenseN = n()) %>%
                         subset(year %in% c(2008,2016))

nl16 <- nl %>% subset(year==2016 & licenseN > 9) %>%
  select(NBH_NAMES)

nl10 <- nl16 %>% left_join(nl, by="NBH_NAMES") %>% select(-X)
fill <- data.frame("Edgewood, Bloomingdale, Truxton Circle, Eckington", 2008, 0)
colnames(fill) <- c("NBH_NAMES", "year", "licenseN")
nlCount <- rbind(nl10, fill) %>% arrange(NBH_NAMES, year, licenseN)

nlGeo <- nlCount %>% spread(year, licenseN)

colnames(nlGeo) <- c("NBH_NAMES", "yr08", "yr16")

nlGeo$NBH_SHORT <- c("Cleveland, Woodley Park", "Palisades, Spring Valley", "Adams Morgan, Kalorama", "Chevy Chase", "Takoma, Brightwood", 
                     'West End, Foggy Bottom', 'Brookland, Brentwood', 'Glover Park, McLean Gardens', 'Friendship Heights, Tenleytown', 'Georgetown', 
                     'Ivy City, Trinidad', 'Bloomingdale, Eckington', 'Petworth, Brightwood Park', 'Navy Yard', 'Capitol Hill (SE)', 
                     'Dupont Circle, K St.','Columbia Heights, Mt. Pleasant', 'Logan Circle', 'Captiol Hill (NE), H St.', 'Chinatown, Downtown', 'Shaw')

nlGeo <- nlGeo %>% mutate(NBH_VSHORT = ifelse(NBH_SHORT == "Bloomingdale, Eckington", "Bloomingdale", 
                                               ifelse(NBH_SHORT == "Dupont Circle, K St.", "Dupont, K St.",
                                                      ifelse(NBH_SHORT == "Chinatown, Downtown", "Chinatown", NBH_SHORT))))

write.csv(nlGeo, "nightlifeHoodCount0816.csv", row.names = FALSE)

nlCluster<-merge(cluster,nlGeo,by="NBH_NAMES",all.x=TRUE)
writeOGR(nlCluster, 'nlCluster.geojson','nlCluster', driver='GeoJSON',check_exists = FALSE)
