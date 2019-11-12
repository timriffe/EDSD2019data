
# load packages:
library(readxl)
library(tidyverse)
library(here)
library(ggpointdensity)
library(colorspace)
library(ggridges)

# function preamble:
mono_graduate <- function(Age5, asfr5){
  # assume NAs are 0s, sometimes these are found 
  # in the first or last age groups.
  asfr5[is.na(asfr5)] <- 0
  # anchor the endpoints (added double anchor)
  Age5                <- c(13,14, Age5 + 4, 50,51)
  asfr5               <- c(0,0, asfr5, 0,0)
  # scale up asfr
  asfr5               <- asfr5 * 5
  # accumualte it
  casfr5              <- cumsum(asfr5)
  # start at 14 because we need firs differences
  # to accumulate...
  predict_ages        <- 14:49
  # fit the spline
  xy <- splinefun(x = Age5,
                  y = casfr5,
                  method = "monoH.FC")(predict_ages)
  # xy <- spline(x = Age5,
  #           y = casfr5,
  #           method = "monoH.FC",
  #           xout = predict_ages)
  
  # return tibble
  tibble(Age = 15:49, 
         asfr = diff(xy))
}


read_excel(
  here::here(
    "Data/Age-specific fertility rates, Total fertility and .xls"), 
  na = "..", 
  skip = 4) %>% 
    dplyr::select(1:13) %>% 
 # select the first 13 columns
  dplyr::rename("Country" = "...1", "ISO" = "...2",
                "PeriodVerbose" = "...3","TFR" = "...6",
                "15" = "15-19","20" = "20-24",
                "25" = "25-29","30" = "30-34",
                "35" = "35-39","40" = "40-44",
                "45" = "45-49") %>% 
  select(-c(PeriodVerbose, TFR, Period)) %>% 
  # stack the ages: data will get 7 times longer
  pivot_longer(cols = `15`:`45`, 
               names_to = "Age",
               values_to = "asfr") %>% 
  # coerce Age to integer and divide out 1000 from asfr
  mutate(Age = as.integer(Age),
         asfr = asfr / 1000) %>% 
  # detect missing asfr subsets and remove them
  group_by(Country, ISO, Year) %>% 
  filter(sum(is.na(asfr)) <= 1)  %>% 
  # create new column called data, which is a list column
  # each element of data is a tibble, with Age and asfr
  # as columns, ca 35 rows for single ages
  do(data = mono_graduate(Age5 = .$Age, asfr5 = .$asfr)) %>% 
  # squeeze the data column back into the main data structure
  # we're back to a rectangle!
  unnest(cols = c(data)) %>% 
  # now calculate TFR and MAB
  group_by(Country, Year) %>%
  mutate(TFR = sum(asfr),
         MAB = sum(asfr * (Age + .5))/TFR) %>% 
  # ggplot(mapping = aes(x = MAB, y = TFR)) +
  # geom_pointdensity(size=1.5) + 
  # scale_color_continuous_sequential(palette = "Reds")
  mutate(TFRint = TFR - TFR %% .25) %>% 
  # maybe another filter exercise is due here
  # to remove extreme outliers
  group_by(TFRint) %>% 
  # Or maybe that filtering could happen
  # inside here, instead of min/max we could
  # pick out the nth highest and lowest?
  mutate(extremes = 
           case_when(MAB == max(MAB) ~ "max",
              MAB == min(MAB) ~ "min",
              TRUE ~ as.character(NA)
              )) %>% 
  filter(!is.na(extremes)) %>% 
  ggplot(mapping = aes(x = Age, 
                       y = factor(TFRint, 
                                  levels = sort(unique(TFRint)), 
                                  ordered = TRUE))) + 
  geom_ridgeline(mapping = aes(x = Age, 
                               height = asfr, 
                               fill = extremes),  
                 scale = 4, 
                 alpha = .6) +
  labs(y = "TFR")








read_excel(
  here::here(
    "Data/Age-specific fertility rates, Total fertility and .xls"), 
  na = "..", 
  skip = 4) %>% 
  dplyr::select(1:13) %>% 
  # select the first 13 columns
 dplyr::rename( "Country" = "...1", "ISO" = "...2",
                "PeriodVerbose" = "...3","TFR" = "...6",
                "15" = "15-19","20" = "20-24",
                "25" = "25-29","30" = "30-34",
                "35" = "35-39","40" = "40-44",
                "45" = "45-49") %>% 
  select(-c(PeriodVerbose, TFR, Period)) %>% 
  # stack the ages: data will get 7 times longer
  pivot_longer(cols = `15`:`45`, 
               names_to = "Age",
               values_to = "asfr") %>% 
  # coerce Age to integer and divide out 1000 from asfr
  mutate(Age = as.integer(Age),
         asfr = asfr / 1000) %>% 
  # detect missing asfr subsets and remove them
  group_by(Country, ISO, Year) %>% 
  filter(sum(is.na(asfr)) <= 1)  %>%
  # create new column called data, which is a list column
  # each element of data is a tibble, with Age and asfr
  # as columns, ca 35 rows for single ages
  do(data = mono_graduate(Age5 = .$Age, asfr5 = .$asfr)) %>% 
  # squeeze the data column back into the main data structure
  # we're back to a rectangle!
  unnest(cols = c(data)) %>% 
  # now calculate TFR and MAB
  group_by(Country, Year) %>%
  mutate(TFR = sum(asfr),
         MAB = sum(asfr * (Age + .5))/TFR) %>% 
  # ggplot(mapping = aes(x = MAB, y = TFR)) +
  # geom_pointdensity(size=1.5) + 
  # scale_color_continuous_sequential(palette = "Reds")
  mutate(TFRint = TFR - TFR %% .25) %>% 
  # maybe another filter exercise is due here
  # to remove extreme outliers
  group_by(TFRint) %>% 
  # Or maybe that filtering could happen
  # inside here, instead of min/max we could
  # pick out the nth highest and lowest?
  mutate(extremes = 
           case_when(MAB == max(MAB) ~ "max",
                     MAB == min(MAB) ~ "min",
                     TRUE ~ as.character(NA)
           )) %>% 
  filter(!is.na(extremes)) %>% 
  ggplot(mapping = aes(x = Age, 
                       y = factor(TFRint, 
                                  levels = sort(unique(TFRint)), 
                                  ordered = TRUE))) + 
  geom_ridgeline(mapping = aes(x = Age, 
                               height = asfr, 
                               fill = extremes),  
                 scale = 4, 
                 alpha = .6) +
  labs(y = "TFR")