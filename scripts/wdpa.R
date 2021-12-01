library(wdpar)
library(dplyr)

if (!require("remotes")) install.packages("remotes")
remotes::install_github("prioritizr/wdpar")

wdpa_global <- wdpa_fetch("global", wait = TRUE, download_dir = "/Users/Ibrito/Documents/MPAs_ClimateChange/wdpa",
                          force_download = TRUE, verbose = TRUE)

saveRDS(wdpa_global, "/Users/Ibrito/OneDrive - Conservation International Foundation/Documents/MPAs_ClimateChange/wdpa_global.rds")


wdpa_global_0 <- wdpa_global %>%
  filter(MARINE == 0)
nrow(wdpa_global_0)
wdpa_global_1 <- wdpa_global %>%
  filter(MARINE == 1)
nrow(wdpa_global_1)
wdpa_global_2 <- wdpa_global %>%
  filter(MARINE == 2)
nrow(wdpa_global_2)

wdpa_global_3 <- wdpa_global %>%
  filter(MARINE != 0)

saveRDS(wdpa_global_3, "/Users/Ibrito/OneDrive - Conservation International Foundation/Documents/MPAs_ClimateChange/wdpa_global_marine.rds")
