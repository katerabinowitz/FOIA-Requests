require(dplyr)
require(tidyr)
require(stringdist)

files = list.files(pattern="*.csv")

#file names as abcYYMM
#file names as abcYYMM
#file names as abcYYMM
names = c("abc1110","abc0511","abc0611","abc1011","abc0812","abc0912","abc0709","abc1404","abc1412","abc1608","abc1408","abc1507","abc1212",
          "abc1204","abc1207","abc1308")

for (i in 1:length(files)){
  assign(names[i], 
         read.csv(files[i], stringsAsFactors=FALSE, strip.white=TRUE)
  )}

#only starting in 2008 are there standard IDs and classification types
#keep datasets that conform to annual reporting
rm(abc0511, abc0611, abc0709, abc1204, abc1207, abc1404, abc1412)

#standardize columns and variable names
#standardize columns and variable names
#standardize columns and variable names
abc0812 <- abc0812 %>%  subset(grepl(" CN | CR | CT | DN | DR | DT", Description)) %>%
  rename(license = License, orgName = Applicant.Name, name08 = Trade.Name, type08 = Description) %>%
  mutate(license=gsub( "\\.|/|\\-|\"|\\s", "",license),
         address08=(paste(Street.Number, Street.Name, ST.Type, Quad))) %>%
  select(license, name08, address08, type08)


abc0912 <- abc0912 %>%  subset(grepl(" CN | CR | CT | DN | DR | DT", Description)) %>%
  rename(license = License, orgName = Applicant.Name, name09 = Trade.Name, type09 = Description) %>%
  mutate(license=gsub( "\\.|/|\\-|\"|\\s", "",license),
         address09=(paste(Street.., Street.Name, Street.Type, Quad))) %>%
  select(license, name09, address09, type09)


abc1011 <- abc1011 %>%  subset(grepl(" CN | CR | CT | DN | DR | DT", Description)) %>%
  rename(license = License, orgName = Applicant, name10 = Trade.Name, type10 = Description) %>%
  mutate(license=gsub( "\\.|/|\\-|\"|\\s", "",license),
         streetType=ifelse(Street.Type=="AVENUE","AVE",
                           ifelse(Street.Type=="BOULEVARD","BLVD",
                                  ifelse(Street.Type=="COURT","CT",
                                         ifelse(Street.Type=="DRIVE","DR",
                                                ifelse(Street.Type=="PLACE","PL",
                                                       ifelse(Street.Type=="ROAD","RD",
                                                              ifelse(Street.Type=="STREET","ST",Street.Type))))))),
         address10=(paste(Street.Number,Street.Name,streetType,Quadrant))) %>%
         select(license, name10, address10, type10) 


abc1110 <- abc1110 %>%  subset(grepl("Nightclub|Restaurant|Tavern", TYPE)) %>%
  rename(license = LICENSE.NUMBER, orgName = APPLICANT.NAME, name11 = LICENSEE.TRADE.NAME, type11 = TYPE) %>%
  mutate(license=gsub( "\\.|/|\\-|\"|\\s", "",license),
         address11= sub("\\\n.*", "", ADDRESS)) %>%
  select(license, name11, address11, type11)
 

abc1212 <- abc1212 %>%  subset(grepl("Nightclub|Restaurant|Tavern", Establishment.Type)) %>%
  rename(license = License.Number, name12 = Trade.Name, orgName = Organization.Name, type12 = Establishment.Type) %>%
  mutate(license=gsub( "\\.|/|\\-|\"|\\s", "",license),
         address12= sub("\\\n.*", "", Address)) %>%
  select(license, name12, address12, type12)
  

abc1308 <- abc1308 %>%  subset(grepl("Nightclub|Restaurant|Tavern", DESCRIPTION)) %>%
  rename(license = LICENSE, orgName = APPLICANT, name13 = TRADE.NAME, type13 = DESCRIPTION, status = STATUS) %>%
  mutate(license=gsub( "\\.|/|\\‐|\"|\\s|-", "",license),
         address13= sub("\\\n.*", "", ADDRESS)) %>%
  select(license, name13, address13, type13)


abc1408 <- abc1408 %>%  subset(grepl("Nightclub|Restaurant|Tavern", Description)) %>%
  rename(license = License.., status = Status, orgName = Applicant, name14 = Trade.Name, type14 = Description) %>%
  mutate(license=gsub( "\\.|/|\\-|\"|\\s", "",license),
         address14=paste(Street.., Street.Name, Type, Quad)) %>%
  select(license, name14, address14, type14)

  
abc1507 <- abc1507 %>%  subset(grepl("Nightclub|Restaurant|Tavern", Description)) %>%
  rename(license = License, status = Status, orgName = Applicant, name15 = TradeName, type15=Description) %>%
  mutate(license=gsub( "\\.|/|\\-|\"|\\s", "",license),
         address15=paste(StreetNumber, StreetName, StreetType, Quadrant)) %>%
  select(license, name15, address15, type15)
  

abc1608 <- abc1608 %>%  subset(grepl("Nightclub|Restaurant|Tavern", Type)) %>%
  rename(status = Status, orgName = Entity.Name, name16 = Trade.Name, type16 = Type) %>%
  mutate(license=gsub( "\\.|/|\\-|\"|\\s", "",License..),
         address16= sub("\\,.*", "", Address)) %>%
  select(license, name16, address16, type16)


### gather annual reports into single dataframe
### gather annual reports into single dataframe
### gather annual reports into single dataframe
license <- rbind(abc0812[1], abc0912[1], abc1011[1], abc1110[1], abc1212[1], abc1308[1], abc1408[1], abc1507[1], abc1608[1])

licenseUnq <- as.data.frame(license[!duplicated(license$license),])
colnames(licenseUnq) <- "license"

All <- licenseUnq %>% left_join(abc0812, "license") %>%
                      left_join(abc0912, "license") %>%
                      left_join(abc1011, "license") %>%
                      left_join(abc1110, "license") %>%
                      left_join(abc1212, "license") %>%
                      left_join(abc1308, "license") %>%
                      left_join(abc1408, "license") %>%
                      left_join(abc1507, "license") %>%
                      left_join(abc1608, "license")

allT <- All %>% gather(key, value, -license)

allN <- allT %>% subset(grepl("name",key) & !(is.na(value))) %>%
                 mutate(year = substr(key,(nchar(key)+1)-2,nchar(key))) %>%
                 select(-key) %>%
                 rename(name = value)

allA <- allT %>% subset(grepl("address",key)) %>%
                 mutate(year = substr(key,(nchar(key)+1)-2,nchar(key))) %>%
                 select(-key) %>%
                 rename(address = value)

allTy <- allT %>% subset(grepl("type",key)) %>%
                  mutate(year = substr(key,(nchar(key)+1)-2,nchar(key)))%>%
                  select(-key) %>%
                  rename(type = value)%>%
                  subset(!(is.na(type)))

nlTime <- allN %>% inner_join(allA, c("license","year")) %>%
                   inner_join(allTy, c("license","year")) %>%
                   arrange(license, year)

### manual review of license numbers - identify true restaurant changes under same license
### manual review of license numbers - identify true restaurant changes under same license
### manual review of license numbers - identify true restaurant changes under same license
write.csv(nlTime,"LL0816.csv", row.names=FALSE)