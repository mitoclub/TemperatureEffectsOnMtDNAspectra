library(here)
library(tidyverse)
library(rfishbase)
library(dplyr)
src_dir <- here("code")
data_dir <- here("data")
output_dir <- here("output")
plots_dir <- here(output_dir, "figures")
tables_dir <- here(output_dir, "tables")

mutSpec12 <- read_csv(here(data_dir, "MutSpecVertebrates12fish.csv"))
Spp <- unique(mutSpec12$Species)

Spp <- gsub("_", " ", Spp)
ecosys <- rfishbase::ecosystem(Spp)
body <- rfishbase::popchar(Spp)
temperature <- rfishbase::popqb(Spp)
grow <- rfishbase::popgrowth(Spp)
eco <- rfishbase::ecology(Spp)
matur <- rfishbase::maturity(Spp)
matur <- dplyr::filter(matur, Sex != "male")


eco <- eco %>% dplyr::select(Species, FeedingType, FoodTroph)
body <- body %>% dplyr::select(Species, Wmax, Lmax, tmax)
matur <- matur %>% dplyr::select(Species, tm, Lm)
temperature <- temperature %>% dplyr::select(Species, Temperature)
grow <- grow %>% dplyr::select(Species, Temperature, DeltaT)
ecosys <- ecosys %>% dplyr::select(Species, Climate)
env_temp <- read.table(here(data_dir, "environment_data.txt"), header = TRUE)

ecosys$Climate <- tolower(ecosys$Climate)
ecosys <- ecosys[!is.na(ecosys$Climate),]
str(ecosys$Climate)
ecosys <- ecosys %>% mutate(Climate = fct_inorder(Climate)) %>%
                group_by_all() %>% tally(sort = TRUE) %>%
                slice(1) %>% ungroup() %>%
                dplyr::select(Species, Climate_zone = Climate)

eco <- eco %>% group_by(Species, FeedingType) %>% summarise(FoodTroph = mean(FoodTroph, na.rm = TRUE))
body <- body %>% group_by(Species) %>% summarise_at(c("Wmax", "Lmax", "tmax"), mean, na.rm = TRUE)
matur <- matur %>% group_by(Species) %>% summarise_at(c("tm", "Lm"), mean, na.rm = TRUE)

env_temp$Climate_zone <- tolower(env_temp$Climate_zone)

grow <- grow[!is.na(grow$Temperature) | !is.na(grow$DeltaT),]
grow <- grow %>% group_by(Species) %>% summarise(growMeanTemperature=mean(Temperature), 
                                                  growMedianTemperature = median(Temperature),
                                                  growMinTemperature=min(Temperature),
                                                  growMeanDeltaT=mean(DeltaT), 
                                                  growMaxTemperature=max(Temperature))
grow$growCalcDeltaT <- grow$growMaxTemperature - grow$growMinTemperature

temperature <- temperature %>% group_by(Species) %>% summarise(tempMeanTemperature = mean(Temperature),
                                                               tempMedianTemperature = median(Temperature),
                                                               tempMinTemperature = min(Temperature),
                                                               tempMaxTemperature = max(Temperature))
temperature$tempCalcDeltaT <- temperature$tempMaxTemperature - temperature$tempMinTemperature

eco$Species <- gsub(" ", "_", eco$Species)
body$Species <- gsub(" ", "_", body$Species)
matur$Species <- gsub(" ", "_", matur$Species)
temperature$Species <- gsub(" ", "_", temperature$Species)
grow$Species <- gsub(" ", "_", grow$Species)
ecosys$Species <- gsub(" ", "_", ecosys$Species)

eco_tables <- list(body, eco, matur, env_temp)
ecology <- eco_tables %>% reduce(full_join, by="Species")



write.csv(ecology, file = here(tables_dir, "Full_fish_ecology_table_2025.csv"), row.names = FALSE)

write.csv(ecosys, file = here(tables_dir, "Extra_climate_data_2025.csv"), row.names = FALSE)

write.csv(grow, file = here(tables_dir, "Extra_temperature_grow_data_2025.csv"), row.names = FALSE)

write.csv(temperature, file = here(tables_dir, "Extra_temperature_popQB_data_2025.csv"), row.names = FALSE)
