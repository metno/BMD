# FUNCTION FOR FINDING MAX, MIN OR MEAN TEMPERATURE FROM HARP FORECAST OBJECT
## fcst - HARP FORECAST OBJECT
## fcst_func - EITHER "min", "max" or "mean"

T2m_to_min_max_mean <- function(fcst, fcst_func){
  
  # ADD COLUMN WITH DATES
  fcst <- mutate(fcst, date = as.Date(validdate))
  
  fcst <- purrr::map(
    fcst,
    ~group_by(
      .x, SID, fcdate, date
    ) %>%
      summarize(
        across(
          matches("_det$"),
          get(fcst_func) # find daily maximum, minimum or mean
        ),
        n = n(),
        .groups = "drop"
      ) %>% 
      filter(
        across(
          matches("n$"))
        == 8) # but only if we have data for the full day. 3 hourly data * 8 = 24 hours
  ) %>%
    harpPoint:::new_harp_fcst() %>%
    mutate(validdate = lubridate::as_datetime(date,tz="UTC")) %>%
    mutate(leadtime = as.integer(
      difftime(
        validdate, fcdate,units="hours")
    ) + 24
    ) %>%
    mutate(fcst_cycle = format(fcdate, "%H")) %>%
    mutate(parameter = paste0("T",fcst_func)) %>%
    mutate(units = "degC")
}
####################
## EXAMPLE OF USE ##
####################

# READ FORECAST TEMPERATURE (3 HOURLY)
t2m <- read_point_forecast(
  start_date = 20200401,
  end_date   = 20200531,
  lead_time  = seq(0,72,3),
  fcst_model = "ec",
  fcst_type  = "det",
  parameter  = "T2m",
  by         = "1d",
  file_path  = "~/Documents/BMD2022/data/FCTABLE/deterministic" # must correspond to path.out in fcsttosqlite.R
)

## CONVERT FORECAST TEMPERATURE FROM KELVIN TO DEGREES CELSIUS
t2m <- scale_point_forecast(t2m, -273.15, new_units = "degC")

## REMOVE MISSING DATA FROM EC
t2m$ec <- t2m$ec %>% filter(ec_det<10^36)

## FIND MAXIMUM TEMPERATURE
tmax.fcst <- T2m_to_min_max_mean(t2m, "max")

