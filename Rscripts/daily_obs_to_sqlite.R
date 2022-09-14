library(readxl)
library(tidyr)
library(readr)
library(dplyr)
library(RSQLite)

# NAME OF FILE 
file <- "~/Documents/Bistand/BMD2022/5DAY_AV2022.xlsx"

# READ EXCEL FILE
# temp <- read_excel(file, sheet = "Temp-2022)")

# READ EXCEL FILE, BUT ONLY COLUMNS A TO AF. THESE CONTAIN MAXIMUM TEMPERATURE
tmax <- read_excel(file, sheet = "Temp-2022)", range = cell_cols("A:AF"))

# STATIONS. DOUBLE CHECK THAT ALL STATIONS ARE IN COLUMN 3 TO 45.
stations <- unique(tmax[,1])
stations <- as.vector(t(stations[3:45,]))

# IDENTIFY THE ROWS IN THE FIRST COLUMN THAT HAVE CONTAIN THE STRING "Stations"
# THIS IS THE START OF WHERE DATA FOR EACH MONTH IS IN THE EXCEL SHEET
startofmonth <- which(tmax[,1] == "Stations")

# EXTRACT JANUARY DATA
tmax.jan <- tmax[startofmonth[1]:startofmonth[2], ]

# CHANGE COLUMN NAMES TO BE THE SAME AS THE FIRST ROW
colnames(tmax.jan) <- tmax.jan[1, ]

# FILTER SO THAT ONLY THE ROWS IN THE STATIONS COLUMN THAT CONTAIN A 
# STATION NAME IS KEPT
tmax.jan <- tmax.jan %>% filter(Stations %in% stations)

# PRINT CLASS OF EACH COLUMN
print(sapply(tmax.jan, class) )

# MAKE ALL COLUMNS CONTAINING TEMPERATURE DATA NUMERIC
tmax.jan[ , 2:32] <- apply(tmax.jan[ , 2:32], 2, function(x) as.numeric(x))
print(sapply(tmax.jan, class) )

# CHANGE TABLE TO KEEP ONE COLUMN WITH THE DATES AND ONE WITH THE MAXIMUM TEMPERATURE
tmax.jan <- pivot_longer(tmax.jan, # table to convert
                         cols = 2:32, # columns that contain the temperature data
                         names_to = "Date", # the column names are now in a column called Date
                         values_to = "Tmax" # the temperature data are now in a column called Tmax
                         )
# CHANGE THE DATES IN Date COLUMN TO BE DATES WITH YEAR AND MONTH
tmax.jan$Date <- as.Date(paste("2022-01", tmax.jan$Date, sep = "-"))


# DO THE SAME FOR ALL MONTHS IN A LOOP

# CREATE EMPTY OBJECT
tmax.all <- NULL

for (i in 1:12){
  tmax.tmp <- tmax[startofmonth[i]:startofmonth[i+1], ]
  colnames(tmax.tmp) <- tmax.tmp[1, ]
  
  # THE MONTHS HAVE A DIFFERENT NUMBER OF DAYS. WE REMOVE THE EXCESS COLUMNS 
  tmax.tmp <- tmax.tmp[names(tmax.tmp) != "NA"]
  
  tmax.tmp <- tmax.tmp %>% filter(Stations %in% stations)
  
  tmax.tmp[ , 2:dim(tmax.tmp)[2]] <- apply(tmax.tmp[ , 2:dim(tmax.tmp)[2]], 2, function(x) as.numeric(x))
  
  tmax.tmp <- pivot_longer(tmax.tmp, 
                           cols = 2:dim(tmax.tmp)[2], # dimension 2 is width of table
                           names_to = "Date", 
                           values_to = "Tmax")
  
  tmax.tmp$Date <- as.Date(paste("2022", # year
                                 i,  # month  
                                 tmax.tmp$Date, #day
                                 sep = "-"))
  
  # UPDATE tmax.all TO CONTAIN 
  tmax.all <- rbind(tmax.all, tmax.tmp)
}

# CREATE A COLUMN WITH STATION ID WITH ONLY THE STATION NUMBER
tmax.all$SID <- as.numeric(gsub(".*?([0-9]+).*", "\\1", tmax.all$Stations))

# CREATE A COLUMN CALLED validdate WHICH KEEPS DATES AS A NUMERIC VALUE
tmax.all$validdate <- as.numeric(as.POSIXct(tmax.all$Date, tz = "UTC" ))

# STATION LOCATIONS
locations <- read_excel("~/Documents/Bistand/BMD2022/data/OBS/All_station_BMD.xlsx")
colnames(locations)[match(c("St_ID", "Lat", "Lon"), names(locations))] <- c("SID", "lat", "lon")

# MERGE STATION INFORMATION WITH Tmax TABLE
tmax.all <- merge(tmax.all, locations, by = "SID")

# HARP REQUIRES ELEVATION...
tmax.all$elev <- 0

# NEED TO DO THE SAME FOR MINIMUM TEMPERATURE AND DAILY PRECIPITATION LATER

SYNOP <- tmax.all
# t.all <- merge(tmax.all, tmin.all,  by = c("SID","datetime"), all = T)
# SYNOP <- merge(t.all, precip,  by = c("SID","datetime"), all = T)

# EXTRACTING REQUIRED COLUMNS
SYNOP <- SYNOP[c("validdate", "SID", "lat", "lon", "elev", "Tmax")]
# SYNOP <- SYNOP[c("validdate", "SID", "lat", "lon", "elev", "Tmax", "Tmin", "AccPcp24h")]

# METADATA
SYNOP_params <- data.frame(
  parameter = c( "Tmax"),
  accum_hours=c(0), 
  units=c("degC")
)
# SYNOP_params <- data.frame(
#   parameter = c( "Tmax", "Tmin", "AccPcp24h"),
#   accum_hours=c(0, 0, 24), 
#   units=c("degC", "degC", "kg/m^2")
# )

# CHANGE OUTPUT PATH TO BE IN A CONVENIENT PLACE 
path.out <- "/home/leneo/Documents/Bistand/BMD2022/dailydata/OBSTABLE/"

# FILENAME SHOULD BE KEPT LIKE THIS FOR COMPLIANCE WITH HARP
filename <- paste0(path.out,"OBSTABLE_2022.sqlite")

# WRITE TO SQLITE
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)
dbWriteTable(
  db,
  "SYNOP",
  SYNOP,
  overwrite = T
)

dbWriteTable(
  db,
  "SYNOP_params",
  SYNOP_params,
  overwrite = T
)

dbDisconnect(db)



