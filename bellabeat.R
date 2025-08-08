install.packages("pkgbuild")

has_rtools <- pkgbuild::has_build_tools(debug = TRUE)
install.packages(c("tidyverse", "lubridate", "janitor", "skimr", "ggplot2"))

library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
daily_activity <- read_csv("dailyActivity_merged.csv")
sleep_data <- read_csv("sleepDay_merged.csv")

# Standardize column names
daily_activity <- clean_names(daily_activity)
sleep_data <- clean_names(sleep_data)

# Convert date formats
daily_activity <- daily_activity %>%
  mutate(date = mdy(activity_date))

sleep_data <- sleep_data %>%
  mutate(date = mdy(date))

# Check for duplicates
sleep_data <- sleep_data %>% distinct()
daily_activity <- daily_activity %>% distinct()

# Preview the data
glimpse(daily_activity)
glimpse(sleep_data)

# Create merge key
daily_activity <- daily_activity %>%
  mutate(merge_key = paste(id, date, sep = "_"))

sleep_data <- sleep_data %>%
  mutate(merge_key = paste(id, date, sep = "_"))

# Merge on merge_key
merged_data <- left_join(daily_activity, sleep_data, by = "merge_key")

# Summary of key features
skim(merged_data)

# Average steps by activity level
merged_data %>%
  group_by(very_active_minutes) %>%
  summarise(avg_steps = mean(total_steps, na.rm = TRUE))

# Add total active minutes
merged_data <- merged_data %>%
  mutate(total_active_minutes = very_active_minutes + fairly_active_minutes + lightly_active_minutes)

# Activity level category
merged_data <- merged_data %>%
  mutate(activity_level = case_when(
    total_steps >= 10000 ~ "Highly Active",
    total_steps >= 5000 ~ "Moderately Active",
    TRUE ~ "Lightly Active"
  ))

ggplot(merged_data, aes(x = total_active_minutes, y = calories)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Calories vs. Active Minutes",
       x = "Total Active Minutes",
       y = "Calories Burned")

merged_data <- merged_data %>%
  mutate(day_of_week = weekdays(date))

merged_data %>%
  group_by(day_of_week) %>%
  summarise(avg_steps = mean(total_steps, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(day_of_week, avg_steps), y = avg_steps)) +
  geom_col(fill = "skyblue") +
  labs(title = "Average Steps by Day of Week",
       x = "Day",
       y = "Average Steps")

library(dplyr)
library(lubridate)

sleep_data <- sleep_data %>%
  mutate(date = mdy(date)) %>%
  mutate(day_of_week = weekdays(date))
library(dplyr)
library(lubridate)

sleep_data <- sleep_data %>%
  mutate(date = mdy(date)) %>%
  mutate(day_of_week = weekdays(date))

library(dplyr)

sleep_summary <- sleep_data %>%
  group_by(day_of_week) %>%
  summarise(avg_minutes_asleep = mean(total_minutes_asleep, na.rm = TRUE)) %>%
  arrange(factor(day_of_week, levels = c(
    "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
  )))


library(ggplot2)

ggplot(sleep_summary, aes(x = day_of_week, y = avg_minutes_asleep)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Sleep Duration per Day of the Week",
    x = "Day of the Week",
    y = "Average Minutes Asleep"
  ) +
  theme_minimal()
library(ggplot2)

ggplot(sleep_summary, aes(x = day_of_week, y = avg_minutes_asleep)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Sleep Duration per Day of the Week",
    x = "Day of the Week",
    y = "Average Minutes Asleep"
  ) +
  theme_minimal()

library(ggplot2)

ggplot(sleep_summary, aes(x = day_of_week, y = avg_minutes_asleep)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Sleep Duration per Day of the Week",
    x = "Day of the Week",
    y = "Average Minutes Asleep"
  ) +
  theme_minimal()

