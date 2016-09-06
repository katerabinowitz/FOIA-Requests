##What I requested:
>All licensed pharmacies as of August 1, 2016 in Washington, DC including license number, pharmacy name, and address. 
>Essentially, I would like the most up to date version possible of this document:
>http://doh.dc.gov/publication/list-licensed-dc-pharmacies
>If you are unable to create the report as of August 1, 2016 please let me know the latest date possible the report can be created. 

##What I received:
This seemed like a relatively easy request given that older data was already published. I was wrong.  

First I received [an image PDF](https://github.com/katerabinowitz/FOIA-Requests/blob/master/Pharmacy%20Locations/FOIA%20Request%20-%20DC%20Pharmacies%20(Rabinowitz)%202016-FOIA-04908.pdf). 

After some follow up they [kindly provided a native PDF,](https://github.com/katerabinowitz/FOIA-Requests/blob/master/Pharmacy%20Locations/DC%20Resident%20Pharmacies_8%205%202016.pdf), the format of which is bit iffy. *Please note I deleted the last four pages of the PDF because it included personal email addresses of the pharmacy contacts.* 

The address is for headquarters, not the individual pharmacy location (for instance, CVS in Woonsocket, RI). DOH indicated I should match this new file with the 2013 document online for addresses. *Shortly thereafter, the 2013 document was taken offline*

##What I've done (so far):
Used [Tabula](http://tabula.technology/) to get the data to a Tabular format. [Merged and filled out](https://github.com/katerabinowitz/FOIA-Requests/blob/master/Pharmacy%20Locations/pharmacyClean.R) the data in R. Created a .csv of [all DC pharmacies with location data.](https://github.com/katerabinowitz/FOIA-Requests/blob/master/Pharmacy%20Locations/DCPharmacyLocations2016.csv)
