install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")




food_data <- Food_Waste_data_and_research_by_country_csv

library(dplyr)
library(ggplot2)

colnames(food_data)

food_data <- food_data %>%
  rename(
    combined = `combined figures (kg/capita/year)`,
    household_kg = `Household estimate (kg/capita/year)`,
    retail_kg = `Retail estimate (kg/capita/year)`,
    service_kg = `Food service estimate (kg/capita/year)`
  )

colSums(is.na(food_data))

food_data <- food_data %>%
  select(Country, Region, combined, household_kg, retail_kg, service_kg)

region_avg <- food_data %>%
  group_by(Region) %>%
  summarise(avg_waste = mean(combined, na.rm = TRUE))

region_avg

top10 <- food_data %>%
  arrange(desc(combined)) %>%
  head(10)

top10

ggplot(region_avg, aes(x = reorder(Region, avg_waste), y = avg_waste)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Average Food Waste by Region",
    x = "Region",
    y = "Kg per capita per year"
  )

ggplot(food_data, aes(x = household_kg, y = retail_kg)) +
  geom_point() +
  labs(
    title = "Household vs Retail Food Waste",
    x = "Household Waste (kg)",
    y = "Retail Waste (kg)"
  )

ggplot(top10, aes(x = reorder(Country, combined), y = combined)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 10 Countries with Highest Food Waste",
    x = "Country",
    y = "Kg per capita"
  )

data_long <- food_data %>%
  select(household_kg, retail_kg, service_kg) %>%
  stack()

ggplot(data_long, aes(x = ind, y = values)) +
  geom_boxplot() +
  labs(
    title = "Comparison of Food Waste Types",
    x = "Waste Type",
    y = "Kg per capita"
  )

