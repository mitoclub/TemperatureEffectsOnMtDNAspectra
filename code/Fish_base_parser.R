library(rvest)
library(dplyr)
library(purrr)
library(tidyr)
rm(list=ls())

# Read the web page
page1 <- read_html("https://www.fishbase.se/topic/List.php?group=10&start=0")
page2 <- read_html("https://www.fishbase.se/topic/List.php?group=10&start=500")
page3 <- read_html("https://www.fishbase.se/topic/List.php?group=10&start=1000")
page4 <- read_html("https://www.fishbase.se/topic/List.php?group=10&start=1500")
page5 <- read_html("https://www.fishbase.se/topic/List.php?group=10&start=2000")
page6 <- read_html("https://www.fishbase.se/topic/List.php?group=10&start=2500")
page7 <- read_html("https://www.fishbase.se/topic/List.php?group=10&start=3000")
page8 <- read_html("https://www.fishbase.se/topic/List.php?group=29")
page9 <- read_html("https://www.fishbase.se/topic/List.php?group=9&start=0")
page10 <- read_html("https://www.fishbase.se/topic/List.php?group=9&start=500")
page11 <- read_html("https://www.fishbase.se/topic/List.php?group=9&start=1000")
page12 <- read_html("https://www.fishbase.se/topic/List.php?group=9&start=1500")
page13 <- read_html("https://www.fishbase.se/topic/List.php?group=9&start=2000")
page14 <- read_html("https://www.fishbase.se/topic/List.php?group=9&start=2500")
# Find the table containing the list of species
species_table1 <- page1 %>% html_node("table")
species_table2 <- page2 %>% html_node("table")
species_table3 <- page3 %>% html_node("table")
species_table4 <- page4 %>% html_node("table")
species_table5 <- page5 %>% html_node("table")
species_table6 <- page6 %>% html_node("table")
species_table7 <- page7 %>% html_node("table")
species_table8 <- page8 %>% html_node("table")
species_table9 <- page9 %>% html_node("table")
species_table10 <- page10 %>% html_node("table")
species_table11 <- page11 %>% html_node("table")
species_table12 <- page12 %>% html_node("table")
species_table13 <- page13 %>% html_node("table")
species_table14 <- page14 %>% html_node("table")
# Extract the species names from the table
species_list1 <- species_table1 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list2 <- species_table2 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list3 <- species_table3 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list4 <- species_table4 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list5 <- species_table5 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list6 <- species_table6 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list7 <- species_table7 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list8 <- species_table8 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list9 <- species_table9 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list10 <- species_table10 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list11 <- species_table11 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list12 <- species_table12 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list14 <- species_table14 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list13 <- species_table13 %>% html_nodes("tr") %>% 
  .[-1] %>%  # Skip the header row
  html_nodes("td:nth-child(1)") %>% 
  html_text(trim = TRUE)
species_list = c(species_list1, species_list2, species_list3, species_list4, species_list5, species_list6, 
                 species_list7, species_list8, species_list9, species_list10, species_list11, species_list12, species_list13, species_list14)
species_list = gsub(" ", "-", species_list)
species_list = unique(species_list)


# Function to extract environment information for a given species URL
extract_environment_info <- function(species) {
  file_name <- paste0(species, ".html")
  page <- read_html(file_name)
  # Extract the relevant section using correct XPath
  environment_info <- page %>%
    html_nodes(xpath = "//h1[contains(text(), 'Environment: milieu / climate zone / depth range / distribution range')]/following-sibling::div[@class='smallSpace']/span") %>%
    html_text(trim = TRUE)
  return(tibble("species" = species, "environment_info" = environment_info[1]))
}

setwd("../../data/Parse_FB/")
environment_data <- purrr::map_df(species_list, extract_environment_info) 

# Function to extract temperature range and climate zone
extract_info <- function(info) {
  # Extract temperature range
  temp <- regmatches(info, regexpr("\\d+°C - \\d+°C", info))
  if (length(temp) == 0) {
    temp <- regmatches(info, regexpr("\\d+°C", info))
  }
  # Extract climate zone
  climate <- regmatches(info, regexpr("Tropical|Subtropical|Temperate|Polar|Boreal", info))
  return(data.frame(Temperature_range = ifelse(length(temp) > 0, temp, NA),
                    Climate_zone = ifelse(length(climate) > 0, climate, NA)))
}
# Apply the function to the data
extracted_data <- environment_data %>%
  rowwise() %>%
  mutate(extracted = list(extract_info(environment_info))) %>%
  unnest(cols = c(extracted)) %>%
  select(Species = species, Temperature_range, Climate_zone)
# Convert temperature ranges to numeric values and calculate median temperature
extracted_data <- extracted_data %>%
  mutate(Temperature_range = gsub("°C", "", Temperature_range)) %>%
  separate(Temperature_range, into = c("Temp_min", "Temp_max"), sep = " - ", fill = "right") %>%
  mutate(Temp_min = as.numeric(Temp_min),
         Temp_max = as.numeric(Temp_max),
         Temp_avg = ifelse(is.na(Temp_max), Temp_min, (Temp_min + Temp_max) / 2))
environment_data = extracted_data; environment_data$Species = gsub("-", "_", environment_data$Species)

write.table(x = environment_data, file = "../environment_data.txt", col.names = T, row.names = F)
