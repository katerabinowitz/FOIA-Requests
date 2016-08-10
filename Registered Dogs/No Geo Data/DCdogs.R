setwd("/Users/katerabinowitz/Documents/DataLensDC/FOIA-Requests/Registered Dogs/No Geo Data")

files = list.files(pattern="*.csv")
data = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE,fill = FALSE,
                                                            strip.white = TRUE,header=F) [c(1,4:5,7,10,12,14,17,19)]))

r <- 1:nrow(data)
data0<-data[r%%3==0,-c(2,7)]
colnames(data0)<-c("animalN","name","DOB","species","breed","color","gender")
data1<-data[r%%3==1,-c(5,7,9)] 
colnames(data1)<-c("licenseN","licenseStatus","statusDT","licenseType","licenseIssuer","licenseCost")
data2<-data[r%%3==2,-c(2,5)] 
colnames(data2)<-c("licenseLength","expireD","renewal","rabiesVac","vacDate","reVacD","provider")
rightData<-cbind(data0,data1,data2)

write.csv(rightData,"licensedDogs.csv")
unq<-rightData[!duplicated(rightData$animalN),]
# --> look into licenseStatus

#Names
name<-as.data.frame(table(unq$name))
name<-name[order(-name$Freq),]

#Breeds
breed<-as.data.frame(table(unq$breed))
breed<-breed[order(-breed$Freq),]