# Converting Fast Track output to R-friendly format
# Takayuki Nagamine 11 JAN 2022

# Data Preparation --------------------------------------------------------

# Load packages
library(tidyverse)
library(emuR)

# Import dataset, rename a variable and combine all
df.aggr <- read_csv("/Volumes/Samsung_T5/FastTrack_workshop/NWS_formant_estimation/processed_data/aggregated_data.csv")
## "Aggregated_data" contains information about formant frequency values and duration, f0 etc. but lacks in other information.

df.segment.info <- read_csv("/Volumes/Samsung_T5/FastTrack_workshop/NWS_formant_estimation/segmentation_information.csv")
## "Segmentation infomation" supplent the "Aggregated_data.csv" with information about the context of the extracted sounds, vowel durations, stress, comments, etc.

df.segment.info <- df.segment.info %>% rename(file = outputfile) 
## Rename the "outputfile" column to "file" so that it is compatible with the "Aggregated_data.csv".

df <- merge(df.aggr, df.segment.info, by = "file", all = T)
## Merging the two csv files

df <- na.omit(df) # omitting NA

# Data Tidy-up -------------------------------------

# Create long data, add and extract variables
df.long <- df %>%
  pivot_longer(contains(c("f1", "f2", "f3", "f4")), 
               names_to = c("formant", "timepoint"), 
               names_pattern = "(f\\d)(\\d+)",
               values_to = "hz")


df.full <- df.long %>% 
  tidyr::spread(key = formant, value = hz) %>% 
  rename(
    duration = duration.x) %>%
  mutate(
    timepoint = as.numeric(timepoint),
    percent = (timepoint - 1) * 10, 
    speaker =
      str_sub(file, start = 1, end = 3), # This will vary depending on your own naming conventions.
    gender = 
      str_sub(file, start = 5, end = 5), # This will vary depending on your own naming conventions.
    language =
      str_sub(file, start = 7, end = 9), # This will vary  depending on your own naming conventions.
  )

# Selecting only relevant columns for analysis
df.full <- df.full %>% 
  select(file, speaker, gender, language, f0, duration, vowel, previous_sound, next_sound, stress, word, previous_word, next_word, f1, f2, f3, f4, percent) %>% 
  mutate(
    Barkf1 = bark(f1, F),
    Barkf2 = bark(f2, F),
    Barkf3 = bark(f3, F)
  )

# Export csv for back-up
write.csv(df.full, "/Volumes/Samsung_T5/FastTrack_workshop/tidy_data.csv", row.names = TRUE) 
# Exporting csv for back-up