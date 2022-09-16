library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

# NAME OF OBSERVATION FILE
file.obs <- "Observed_data.xlsx"

# READ OBSERVATION FILE
obs <- read_excel(file.obs)

colnames(obs) <- obs[2,]

obs <- obs[-c(1,2)]

class(obs$`Dry_Bulb_Temperature(deg. C)`)
obs$`Dry_Bulb_Temperature(deg. C)` <- as.numeric(class(obs$`Dry_Bulb_Temperature(deg. C)`))
class(obs$`Dry_Bulb_Temperature(deg. C)`)

obs2 <- read_excel(file.obs, skip=1)
