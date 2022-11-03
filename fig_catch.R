library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)
library(ggaqua) # https://github.com/maxlindmark/ggaqua
####
## This script will prepare a figure with a time series of catches of salmon by Swedish fishers
## both recreational and commercial in the Bailtc sea and rivers entering the Baltic sea.
## The data for freshwater is taken from (the excel-dump of) fishdb, the data for the Baltic
## sea is taken from the WGBAST catch database.




## You can chang the location of the files if you want to run on "private" copies of the data
Data_root_folder <- "//storage-dh.slu.se/restricted$/Lax/Data"
WGBAST_folder <- "WGBAST_CatchDatabase"
Fishdata_folder <- "fishdb"

WGBAST_database <- file.path(Data_root_folder, WGBAST_folder, "WGBAST_Catch_Latest.xlsx")
Fishdata_database <- file.path(Data_root_folder, Fishdata_folder, "fishdata-latest.xlsx")

# Try to automate the start year to use. If wrong just hard code it.
first_year <- as.numeric(format(Sys.Date(), "%Y")) - 19
#first_year <- 2003

river_data <- read_xlsx(Fishdata_database, sheet = "river_sum")

sal_ts_river <- river_data %>% filter(maf == "SAL", ## Salmon
                       year >= first_year,
                       landed, ## Only landed fish
                       haronr < 86001) ## From Mörrum and further north

landed_per_year_river  <- sal_ts_river %>%
  group_by(year, fcat) %>%
  summarize(N = sum(num_fish), .groups = "drop") %>%
  mutate(fcat = if_else(fcat == "Other", "Recreational", fcat))

##### Extract and summarise COMM landings for FISHERY C and O from WGBAST database
sea_data <- read_xlsx(WGBAST_database, sheet = "Catch data")

sal_ts_sea <- sea_data %>%
  filter(COUNTRY == "SE", YEAR >= first_year, SPECIES == "SAL",
         FISHERY %in% c("C", "O"), F_TYPE %in% c("COMM", "RECR"))

landed_per_year_sea <- sal_ts_sea %>%
  rename(year = YEAR, fcat = F_TYPE) %>%
  group_by(year, fcat) %>%
  summarise(N = round(sum(NUMB), 0), .groups = "drop") %>%
  mutate(fcat = case_when(
    fcat == "COMM" ~ "Commercial",
    fcat == "RECR" ~ "Recreational"
  ))

#### Put it together, create figure and data table
landed_per_year <- landed_per_year_river %>%
  bind_rows(landed_per_year_sea) %>%
  group_by(year, fcat) %>%
  summarise(N = sum(N), .groups = "drop")

landed_wide  <- landed_per_year %>%
  pivot_wider(names_from=fcat, values_from=c(N))

write_xlsx(landed_per_year, "catch_long.xlsx")
write_xlsx(landed_wide, "catch_wide.xlsx")

## Fix things in data to make the figure look OK
fig_data <- landed_per_year  %>%
  mutate(fcat = case_when(
    fcat == "Brood" ~ "Avelsfiske",
    fcat == "Commercial" ~ "Yrkesfiske",
    fcat == "Recreational" ~ "Fritidsfiske",
    TRUE ~ "Okänt fiske. FIXA!"))


y_axis <- "Fångster (antal)"
##################### FÖR FLERA SERIER stapeldiagram
pal <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#E69F00", "#D55E00")
fig_catch <- ggplot(fig_data, aes(x = year, y = N, fill = fcat)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = y_axis) +
  scale_fill_manual(values = pal) +
  labs(x = NULL) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = guide_legend(nrow = 2,
                             title = "",
                             keywidth = 0.1,
                             keyheight = 0.03,
                             default.unit = "inch")) +
  theme_aqua() +
  theme(legend.box.margin = margin(-15,-15,-15,-15))

# spara figuren
ggsave("Fig_catch.png", plot = fig_catch, dpi = 300, width = 8, height = 8, units = "cm")

