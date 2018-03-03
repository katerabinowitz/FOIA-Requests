require(dplyr)
require(tidyr)
require(stringdist)

### read in file w manual cleanup of restaurant change under same license number
### read in file w manual cleanup of restaurant change under same license number
### read in file w manual cleanup of restaurant change under same license number
ll <- read.csv("LL0816_filled.csv", stringsAsFactors=FALSE, strip.white=TRUE)

### remove non-nightlife and hold years
### remove non-nightlife and hold years
### remove non-nightlife and hold years
remove <- ll %>% subset(grepl("1720|Cosi|Chipotle|Bon Appetit M|We Work|Giant|Whole Foods|International Trade Center|Sports Club|Bar 21|
                              |JBS Ventures, LLC|Jose|Fuel Pizza|3001 M Street NW, LP|Apartments|World Bank|No trade name|No Trade Name|
                              |Law Group|Restaurant Associates|Eastbanc|Heat|INTERNATIONAL HOUSE OF PONG",
                              ll$name)) %>% 
  distinct(license, .keep_all = TRUE)
ll <- ll %>% subset(!(license %in% remove$license))

ll <- ll %>% subset(!(grepl("TBD|etermine|ETERMINE|NONE|formerly|FORMERLY|Formerly|afekeeping",name)))

### break out multiple businesses under same address/license/time
### break out multiple businesses under same address/license/time
### break out multiple businesses under same address/license/time
double <- ll %>% subset(grepl("/",name)) %>%
  mutate(ct = lengths(regmatches(name, gregexpr("/", name)))) %>%
  subset(name  != "Custom Fuel/Fuel Pizza")

double2 <- double %>% subset(ct>1) %>%
  distinct(license, .keep_all = TRUE) %>%
  select(license, ct) %>%
  left_join(ll, by="license") %>%
  mutate(name=gsub("Ardeo/Bardeo|Ardeo/Bard eo","Ardeo & Bardeo", name),
         name=gsub("Cobalt/ 30 Degrees|Cobalt/30°|Cobalt/ 30\nDegrees","Cobalt",name),
         
         name = ifelse(name=="Napoleon/Hierarchy/Cru","Napoleon",
                  ifelse(name=="Ben's Chili Bowl / Ben's Upstairs/Ten 01","Ben's Chili Bowl / Ben's Upstairs",
                    ifelse(name=="Acqua al 2/Suna/Harold Black Bar" | name=="Acqua al\n2/Suna/Har","Acqua al 2/Harold Black Bar",
                      ifelse(name=="Suna","Acqua al 2/Suna/Harold Black Bar",
                        ifelse(name=="Cobalt/Lev", "Cobalt/Level One",
                          ifelse(license=="ABRA081092" & year==14, "M.I.A.",
                            ifelse(license=="ABRA081092" & year==13,"M.I.A",
                             ifelse(license=="ABRA081092" & year==12,"Lupe/M.I.A.",
                               ifelse(license=="ABRA081092" & year==11,"Spot/Lupe/M.I.A.",
                                ifelse(license=="ABRA081092" & year==10, "Spot/Lupe", 
                                 ifelse(name=="Hanks Oyster Bar (Duplicate License 07/15/2014)","Hanks Oyster Bar",
                                  ifelse(name=="Darlington House: Cantina/Kitchen/Library","Darlington House",name)))))))))))))

double <- double %>% subset(!(license %in% double2$license))
double1 <- rbind(double,double2) %>% 
  mutate(name=="Avenue Suites/A Bar","A Bar", name)

double1a <- double1 %>%
  mutate(name = gsub("(.*)/.*", "\\1",name), 
         license2 = license) %>%
  select(-ct)

double1b <- double1 %>% 
  subset(grepl("/",name, fixed=TRUE)) %>%
  mutate(name = gsub("^.*\\/","", name), 
         license2 = paste0(license, ".1")) %>%
  select(-ct)

double <- rbind(double1a, double1b)

rm(double1, double1a, double1b, double2, remove)

### break out new businesses under same license
### break out new businesses under same license
### break out new businesses under same license
llNoDouble <- ll %>% subset(!(paste0(license,year) %in% paste0(double$license,double$year)))

change <- llNoDouble %>% subset(X==1) %>%
  select(license) %>%
  left_join(llNoDouble, by="license") %>%
  arrange(license, year) %>%
  distinct(license, year, .keep_all = TRUE) %>%
  mutate(nChange=ifelse(license=="ABRA000882" & year==11, 1,
                    ifelse(license=="ABRA000882" & year==13, NA,
                      ifelse(license=="ABRA000882" & year==15, 2,
                        ifelse(license=="ABRA060401" & year==15, 2,
                          ifelse(license=="ABRA060678" & year==16, 2, 
                            ifelse(license=="ABRA072734" & year==13, 2,
                              ifelse(license=="ABRA072743" & year==16, 2, 
                                ifelse(license=="ABRA073188" & year==12, 2, 
                                  ifelse(license=="ABRA075284" & year==10, 1,
                                    ifelse(license=="ABRA075284" & year==15, 2, 
                                      ifelse(license=="ABRA077455" & year==11, 2, 
                                        ifelse(license=="ABRA082451" & year==14, 2,
                                          ifelse(license=="ABRA082451" & year==16, 3,
                                            ifelse(license=="ABRA090239" & year==14, NA,
                                             X)))))))))))))),
         yrChg = ifelse(!(is.na(nChange)), year, NA))%>%
  filter(!(license %in% c("ABRA017458","ABRA095913") & year %in% c(12,13,14)))


change1 <- change %>% filter(nChange==1) %>%
  select(nChange, yrChg, license) %>% 
  rename(nChange1 = nChange, yrChg1 = yrChg)

change2 <- change %>% filter(nChange==2) %>%
  select(nChange, yrChg, license) %>%
  rename(nChange2 = nChange, yrChg2 = yrChg)

change <- change %>%
  left_join(change1, by='license') %>%
  left_join(change2, by='license') %>%
  mutate(new = ifelse(is.na(nChange) & !(is.na(yrChg2)) & year > yrChg2, nChange2,
                      ifelse(is.na(nChange) & !(is.na(yrChg1)) & year > yrChg1, nChange1, 
                             nChange)),
         license2 = ifelse(is.na(new), license, paste0(license, ".", new))) %>%
  select(-nChange, -nChange1, -nChange2, -yrChg, -yrChg1, -yrChg2, -new)

rm(change1, change2)

### combine for updated license that reflects multiple and new businesses, consistent names + addresses
### combine for updated license that reflects multiple and new businesses, consistent names + addresses
### combine for updated license that reflects multiple and new businesses, consistent names + addresses
licenseUpdate <- llNoDouble %>% subset(!(license %in% change$license))

licenseUpdate <- licenseUpdate %>% bind_rows(change) %>%
  bind_rows(double) %>%
  mutate(license = ifelse(is.na(license2), license, license2)) %>%
  select(-license2, -X)

# 2013 report had a character limit on business names, so populate w first year if last year is 2013
last <- licenseUpdate %>% group_by(license) %>%
  arrange(year) %>%
  slice(n()) %>%
  subset(year != 13)

first <- licenseUpdate %>% subset(!(license %in% last$license)) %>%
  group_by(license) %>%
  arrange(year) %>%
  slice(1) 

singleRec <- first %>% rbind(last) %>%
  select(-year)

licenseUpdate <- licenseUpdate %>% select(license, year)

remove(first, last, change, double)

### Correct for restaurants that have different license numbers over time
### Correct for restaurants that have different license numbers over time
### Correct for restaurants that have different license numbers over times
license <- licenseUpdate %>% merge(singleRec, by="license") %>%
  arrange(address, license, year) %>%
  mutate(address = ifelse(name=="Osteria Al Volo", "1790 Columbia Rd NW", address), 
         name = ifelse(name=="Temporary Works","hogo", tolower(name)), 
         name = gsub("elroy  \\(the\\)", "the elroy", name),
         licenseLag = lag(license), 
         addressLag = lag(address),
         nameLag = lag(name),
         nameShort = substr(name, 1, 10), 
         nameLagShort = substr(nameLag, 1, 10),
         licenseChg = ifelse(license==licenseLag, 0, 1),
         addressChg = ifelse(address==addressLag, 0, 1), 
         nameChg = ifelse(name==nameLag, 0, 1), 
         SD = stringdist(nameShort, nameLagShort, method="dl")) %>%
  select(-addressLag)

diffName1 <- license %>% subset(licenseChg==1 & addressChg==0 & SD < 5) %>%
  subset(!(name %in% c("aggio","umaya", "umi", "nido", "19th", "sip","mova")))

diffName <- license %>% subset(licenseChg==1 & addressChg==0 & nameChg==1 & SD > 4) %>%
  subset(name %in% c("d c noodles", "axum's level x lounge", "bossa brazilian bistro", "saki","umi japanese cuisine","sax")) %>%
  rbind(diffName1) %>%
  select(license, licenseLag, nameLag)

allTogetherNow <- license %>% left_join(diffName, by="license") %>%
  rename(licenseLag = licenseLag.y, nameLag = nameLag.y) %>%
  select(license, licenseLag, year, name, nameLag, address) %>%
  mutate(license = ifelse(is.na(licenseLag), license, licenseLag), 
         name = ifelse(is.na(nameLag), name, nameLag)) %>%
  select(license, year, name, address) %>%
  arrange(license, year)

remove(diffName, diffName1, singleRec)