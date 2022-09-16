library(readxl)
library(tidyr)
library(readr)
library(dplyr)

my_read_obs <- function(file, sheet, year, parameter, cell_columns, set_trace_amount = F){
  # READ EXCEL FILE, BUT ONLY COLUMNS A TO AF. THESE CONTAIN MAXIMUM TEMPERATURE
  obs <- read_excel(file, sheet = sheet, range = cell_cols(cell_columns))
  
  # STATIONS. DOUBLE CHECK THAT ALL STATIONS ARE IN COLUMN 3 TO 45.
  stations <- unique(obs[,1])
  stations <- as.vector(t(stations[3:45,]))
  
  # IDENTIFY THE ROWS IN THE FIRST COLUMN THAT HAVE CONTAIN THE STRING "Stations"
  # THIS IS THE START OF WHERE DATA FOR EACH MONTH IS IN THE EXCEL SHEET
  startofmonth <- which(obs[,1] == "Stations")
  
  # CREATE EMPTY OBJECT
  obs.all <- NULL
  
  for (i in 1:12){
    if (i < length(startofmonth)){
      obs.tmp <- obs[startofmonth[i]:startofmonth[i+1], ]
    }else{
      obs.tmp <- obs[startofmonth[i]:dim(obs)[1],]
    }
    
    colnames(obs.tmp) <- obs.tmp[1, ]
    
    # THE MONTHS HAVE A DIFFERENT NUMBER OF DAYS. WE REMOVE THE EXCESS COLUMNS 
    obs.tmp <- obs.tmp[! names(obs.tmp) %in% c("NA", NA)]
    
    obs.tmp <- obs.tmp %>% filter(Stations %in% stations)
    
    if (set_trace_amount == T){
      obs.tmp[obs.tmp == "T"] <- "0"
    }
    
    obs.tmp[ , 2:dim(obs.tmp)[2]] <- apply(obs.tmp[ , 2:dim(obs.tmp)[2]], 2, function(x) as.numeric(x))
    
    obs.tmp <- pivot_longer(obs.tmp, 
                            cols = 2:dim(obs.tmp)[2], # dimension 2 is width of table
                            names_to = "Date", 
                            values_to = parameter)
    
    obs.tmp$Date <- as.Date(paste(year, # year
                                  i,  # month  
                                  obs.tmp$Date, #day
                                  sep = "-"))
    
    # UPDATE obs.all TO CONTAIN 
    obs.all <- rbind(obs.all, obs.tmp)
  }
  return(obs.all)
}

# EXAMPLES OF USE
# file <- "~/Documents/Bistand/BMD2022/5DAY_AV2022.xlsx"
# tmax <- my_read_obs(file, sheet = "Temp-2022)", year = "2022", 
#                     parameter = "Tmax", cell_columns = "A:AF", 
#                     set_trace_amount = F)
# 
# tmin <- my_read_obs(file, sheet = "Temp-2022)", 
#                     year = "2022", parameter = "Tmin", cell_columns = "AK:BP", 
#                     set_trace_amount = F)
# 
# file2 <- "~/Documents/Bistand/BMD2022/Monsoon_com_new-2022.xlsx"
# pcp <- my_read_obs(file2, sheet = "2022", 
#                     year = "2022", parameter = "AccPcp24h", cell_columns = "A:AF", 
#                     set_trace_amount = T)
# 

