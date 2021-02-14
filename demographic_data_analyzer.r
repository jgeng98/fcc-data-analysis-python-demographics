library(tidyverse)

df <- read_csv("adult.data.csv")

# How many people of each race are represented in the dataset?
df %>%
    group_by(race) %>%
    count()

# Average age of men and women in the dataset
df %>%
    group_by(sex) %>%
    summarise(avg = mean(age))

# Counts and percentages of people's educational backgrounds
df %>%
    filter(education == "Bachelors") %>%
    summarise(n = n(), percent = n / nrow(df) * 100)

# Salaries of those with advanced education (eg. "Bachelors", "Masters", or "Doctorate")
df_higher_ed <- df %>% filter(education == "Bachelors" | education == "Masters" | education == "Doctorate")

df_higher_ed %>%
    group_by(salary) %>%
    summarise(n = n(), percent = n / nrow(df_higher_ed) * 100)

# Salaries of those without advanced education
df_lower_ed <- df %>% filter(education != "Bachelors" & education != "Masters" & education != "Doctorate")

df_lower_ed %>%
    group_by(salary) %>%
    summarise(n = n(), percent = n / nrow(df_lower_ed) * 100)

# What is the minimum number of hours a person works per week?
min_hours <- min(df$`hours-per-week`)

# Salaries of those who work the minimum number of hours per week
df_min_hours <- df %>% filter(`hours-per-week` == 1)

df_min_hours %>%
    group_by(salary) %>%
    summarise(n = n()) %>%
    mutate(percent = prop.table(n) * 100)

# Countries ordered by the highest percentage of people who earn >50K
df %>%
    group_by(`native-country`, salary) %>%
    summarise(n = n()) %>%
    group_by(`native-country`) %>%
    mutate(percent = n / sum(n) * 100) %>%
    filter(salary == ">50K") %>%
    arrange(desc(prop))

# Occupations of those who earn >50K in India (ordered from most to least popular)
df %>%
    filter(`native-country` == "India" & salary == ">50K") %>%
    group_by(occupation) %>%
    count() %>%
    arrange(desc(n))