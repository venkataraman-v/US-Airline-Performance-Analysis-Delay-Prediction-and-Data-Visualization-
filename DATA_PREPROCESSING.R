
multmerge <- function(mypath) {
        filenames=list.files(path=mypath, full.names=TRUE)
        rbindlist(lapply(filenames, function(x){read.csv(x, stringsAsFactors = F, sep=',')}))
}

# all_final_flights_2019 <- multmerge("C:/Users/narra/Documents/ProjectRShinyGroup10/Project-RShiny-Group10/data")
# write.csv(all_final_flights_2019,'merged_flights.csv')
all_final_flights_2019 <- read.csv("merged_flights.csv")
all_final_flights_2019<-subset(all_final_flights_2019, OP_UNIQUE_CARRIER %in% c('AA','DL','WN','NK'))
all_final_flights_2019<-subset(all_final_flights_2019, ORIGIN_STATE_NM %in% c('New York','California','Washington','Texas','Alaska','Hawaii','Florida','Massachusetts','Illinois')
                               & DEST_STATE_NM %in% c('New York','California','Washington','Texas','Alaska','Hawaii','Florida','Massachusetts','Illinois'))



carrier_code <- read.csv("L_UNIQUE_CARRIERS.csv_")
names(carrier_code)[1]<-"OP_UNIQUE_CARRIER"
names(carrier_code)[2]<-"AIRLINE_NAME"

db_flights <-merge(x=all_final_flights_2019,y=carrier_code,by="OP_UNIQUE_CARRIER",all.x = TRUE)


location<-read.csv("location.csv", stringsAsFactors = FALSE)
location<- location[!is.na(location), c(2,4,5)]
location<-location[!duplicated(location$DISPLAY_AIRPORT_CITY_NAME_FULL),]


db_flights <- merge(x = db_flights, y = location, by.x = "ORIGIN_CITY_NAME", by.y="DISPLAY_AIRPORT_CITY_NAME_FULL", all.x=TRUE)
db_flights <- merge(x = db_flights, y = location, by.x = "DEST_CITY_NAME", by.y="DISPLAY_AIRPORT_CITY_NAME_FULL",suffix = c("_ORIGIN", "_DEST"), all.x=TRUE)

db_flights <- db_flights[!is.na(db_flights$ORIGIN_CITY_NAME)&!is.na(db_flights$DEP_DELAY)&!is.na(db_flights$ORIGIN_CITY_NAME),]
