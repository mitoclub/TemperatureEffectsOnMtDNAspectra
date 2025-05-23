getwd()
library(rvest)
library(dplyr)
library(purrr)
rm(list=ls())

# Read the web page
page1 <- read_html("https://www.fishbase.se/topic/List.php?group=9&start=0")
page2 <- read_html("https://www.fishbase.se/topic/List.php?group=9&start=500")
page3 <- read_html("https://www.fishbase.se/topic/List.php?group=9&start=1000")
page4 <- read_html("https://www.fishbase.se/topic/List.php?group=9&start=1500")
page5 <- read_html("https://www.fishbase.se/topic/List.php?group=9&start=2000")
page6 <- read_html("https://www.fishbase.se/topic/List.php?group=9&start=2500")
# Find the table containing the list of species
species_table1 <- page1 %>% html_node("table")
species_table2 <- page2 %>% html_node("table")
species_table3 <- page3 %>% html_node("table")
species_table4 <- page4 %>% html_node("table")
species_table5 <- page5 %>% html_node("table")
species_table6 <- page6 %>% html_node("table")
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
species_list = c(species_list1, species_list2, species_list3, species_list4, species_list5, species_list6)
species_list = gsub(" ", "-", species_list)

# Define the base URL
base_url <- "https://www.fishbase.se/summary/"

# Function to extract environment information for a given species URL
load_pages <- function(species) {
  # Read the web page
  species_url <- paste0(base_url, species, ".html")
  # Define the local file name
  file_name <- paste0(species, ".html")
  if (!file.exists(file_name)) {
    download.file(species_url, file_name, quiet = TRUE)
  }
}

lapply(species_list, load_pages)
