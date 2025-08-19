rm(list = ls())
library(tidyverse)
library(here)
library(janitor)

#read in lobster data
lobster_abundance <- read_csv(here::here("data", "Lobster_Abundance_All_Years_20210412.csv"), na = c("-99999")) |> #removes -99999 and insert NA
  clean_names() |> #clean names
  uncount(count) #opposite of group_by() (parces data out)

#create a new df of the mean based on site and year
lobster_by_site_year <- lobster_abundance |>
  group_by(site, year) |>
  summarise(mean = mean(size_mm, na.rm = TRUE),
            sample_size = n()) #calculates how many rows there are for unique combinations of group by

ggplot(data = lobster_by_site_year, aes(x = year, y = sample_size))+
  geom_point(colour = "forestgreen")+
  facet_wrap(~site)+
  labs(title = "Site", x = "Individuals", y= "Year")

#filters for lobsters from 2020
lobsters_2020 <- lobster_abundance |>
  filter(year == 2020) |>
  drop_na(size_mm) |>
#lobsters below and above the legal limit
  mutate(legal_limit = case_when(size_mm >= 79.76 ~ "yes",
                                 size_mm <= 79.76 ~ "no")) |>
    count(site, legal_limit, name = "lobster_count")

# graph of absolute counts
ggplot(data = lobsters_2020, aes(x = site, y = lobster_count, fill = legal_limit))+
  geom_col(position = "fill")+
  labs(title = "Legal Limit Counts by Site", x = "Site", y = "Lobster Count")

# filter original data for sites IVEE, CARP, and NAPL
ex_a <- lobster_abundance |>
  filter(site == "IVEE" | site == "CARP"| site == "NAPL")
 #alternate filter(site %in% c("IVEE", "CARP", "NAPL"))

#filter the above for observations in August
ex_b <- lobster_abundance |>
  filter(month == 8)

#filter for site == AQUE or a carapace length greater than 70 mm
ex_c <- lobster_abundance |>
  filter(site == "AQUE" | size_mm >= 70)

#filter to not have site == NAPL included
ex_d <- lobster_abundance |>
  filter(site !="NAPL")

#filter by site to summarise mean and sd
ex_e <- lobster_abundance |>
  group_by(site) |>
  summarise(mean(size_mm, na.rm = TRUE), sd(size_mm, na.rm = TRUE))

#filter by site and month to calculate max size
ex_f <- lobster_abundance |>
  group_by(site, month) |>
  summarise(size_max = max(size_mm, na.rm = TRUE))

#adds a column for size in cm
ex_g <- lobster_abundance |>
  mutate(size_cm = size_mm *10)

#update the site column to all lowercase
ex_h <-
