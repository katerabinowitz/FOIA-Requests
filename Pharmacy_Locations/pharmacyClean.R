library(stringr)
library(ggmap)
library(rgdal)
setwd("/Users/katerabinowitz/Documents/DataLensDC/FOIA-Requests/Pharmacy Locations")

### Read in and prep data###
### Read in and prep data###
### Read in and prep data###
pharm13<-read.csv("tabulaDCPharm13.csv",
         fill = FALSE, strip.white = TRUE,stringsAsFactors=FALSE)
pharm16Name<-read.csv("tabulaDCPharm16.csv",
                  fill = FALSE, strip.white = TRUE,stringsAsFactors=FALSE, 
                  header=F,col.names="PHARMACY.NAME",skip=1, nrows=160)
#address unreliable-shows corporate HQ for many-only used where '13 does provide & double checked
pharm16Add<-read.csv("tabulaDCPharm16.csv",
                     fill = FALSE, strip.white = TRUE,stringsAsFactors=FALSE, 
                     header=F,col.names="ADDRESS",skip=162)
pharm16<-cbind(pharm16Name,pharm16Add)

#correct 2013 pharmacy PDF read error
pharm13<-subset(pharm13, pharm13$LICENSE.NUMBER != "LICENSE NUMBER" & 
                  pharm13$STREET.ADDRESS != "")

name13<-subset(pharm13, pharm13$PHARMACY.NAME!="")
noname13<-subset(pharm13, pharm13$PHARMACY.NAME=="")

idname<-t(as.data.frame(str_split(noname13$LICENSE.NUMBER," ",n=2)))
newname13<-cbind(noname13[c(3:7)],idname)
colnames(newname13)[c(6,7)]<-c("LICENSE.NUMBER","PHARMACY.NAME")

pharmacy13<-rbind(name13,newname13)

#remove multiple pharmacies in single hospital
pharmacy13<-subset(pharmacy13, !(pharmacy13$LICENSE.NUMBER %in% 
            c("RX1200020","RX1000417","RX1200007","RX1000427","RX1200012","RX1200006","")))

#prepare for matching by creating for more uniform names
pharm16$PHARMACY.NAME<-ifelse(pharm16$PHARMACY.NAME=="FOER'S  PHARMACY","FOER'S  PHARMACYK",
                              pharm16$PHARMACY.NAME)
pharm16$PHARMACY.NAME<-gsub("NAI SATURN EASTERN LLC DBA ","",pharm16$PHARMACY.NAME)
pharm16$PHARMACY.NAME<-gsub("[[:punct:]]", " ", pharm16$PHARMACY.NAME)
pharm16$PHARMACY.NAME<-gsub(" ","",pharm16$PHARMACY.NAME)

pharmacy13$PHARMACY.NAME<-gsub("[[:punct:]]", " ",pharmacy13$PHARMACY.NAME)
pharmacy13$PHARMACY.NAME<-gsub(" ","",pharmacy13$PHARMACY.NAME)

### Merge and remerge to catch all ###
### Merge and remerge to catch all ###
### Merge and remerge to catch all ###
pharm<-merge(pharm16,pharmacy13,by="PHARMACY.NAME")[c(1,3:8)]

#remaining
out13<-subset(pharmacy13,!(pharmacy13$PHARMACY.NAME %in% pharm$PHARMACY.NAME))
out16<-subset(pharm16,!(pharm16$PHARMACY.NAME %in% pharm$PHARMACY.NAME))

#manual 2013 pharmacy cleanup - NA for closed/true address duplicates/central jail pharmacy
pharm<-pharm[order(pharm$STREET.ADDRESS),]
out13$status<-c("NA","NA","CVSPHARMACY2733","CHILDRENSNATIONALMEDICALCENTERPHARMACY","NA",
          "FortLincolnPharmacyMedicalEquipmentLLC","GWMEDICALFACULTYASSOCIATESPHARMACY",
          "GENOAaQoLHEALTHCARECOMPANYLLC","GEORGEWASHINGTONUNIVERSITYHOSPITALPHARMACY","NA",
          "HSTREETCAREPHARMACYANDWELLNESSCENTERLLC","HOWARDUNIVERSITYHOSPITALPHARMACY",
          "KAISERPERMANENTENORTHWESTDCPHARMACY","NA","NA","NA","PSYCHIATRICINSTITUTEOFWASHINGTON",
          "NA","RITEAID3351","ALEXPHARMLLCDBAPALISADESPHARMACY","NA","NA","DCACAPITOLHILLLTACHLLC",
          "DCAHADLEYLTACHLLC","NA","NA","NA")
out13<-subset(out13,out13$status!="NA")

#remerge
pharm2<-merge(out13, out16, by.x="status",by.y="PHARMACY.NAME")[c(1:2,4:8)]
colnames(pharm2)[c(1)]<-"PHARMACY.NAME"

#clean up pharmacies that are opened after 2013 report and remove those that are closed / hospital / jail
out162<-subset(out16, !(out16$PHARMACY.NAME %in% pharm2$PHARMACY.NAME))
out162<-subset(out162,!(grepl("KAISERPERMANENTE|MEDSTAR|FOERSPHARMACYK|SIBLEY|CENTRALDETENTION|MORTON|AIDSHEALTHCARE|GOODHOPECARE",out162$PHARMACY.NAME)))

out162$ADDRESS<-ifelse(out162$ADDRESS=="ONE CVS DR","3700 NEWARK ST NW",
                       ifelse(out162$ADDRESS=="645 HST NE","645 H ST NE",
                            ifelse(out162$ADDRESS=="4551 FORBES BLVD","3830 Georgia Ave NW",
                              out162$ADDRESS)))
out162$ADDRESS<-gsub("  "," ",out162$ADDRESS)

pharm<-pharm[c(1,3)]
pharm2<-pharm2[c(1,3)]
colnames(out162)[c(2)]<-"STREET.ADDRESS"
pharmacy<-rbind(pharm,pharm2,out162)


### geocode and add wards ###
### geocode and add wards ###
### geocode and add wards ###
pharmGeo<-geocode(paste0(pharmacy$STREET.ADDRESS,", Washington DC"))

ward<-readOGR("http://opendata.dc.gov/datasets/a4442c906559456eb6ef3ea0898fe994_32.geojson",
             "OGRGeoJSON")
addAll<-SpatialPoints(pharmGeo, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
pharmWard <- over(addAll, ward)[c(4)]

pharmacy<-cbind(pharmacy,pharmGeo,pharmWard)
pharmacy<-pharmacy[order(pharmacy$WARD_ID),]

table(pharmacy$WARD_ID)

#output
write.csv(pharmacy,"DCPharmacyLocations2016.csv", row.names=FALSE)

