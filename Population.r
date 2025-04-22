# Set the working directory
setwd("C:/Users/Ayush Sarvaiya/OneDrive/Desktop/APL/Project")

# Load required packages
suppressPackageStartupMessages(library(tidyverse))

# Load the datasets
world_pop = read_csv('world_pop_data.csv', show_col_types = FALSE)
country_data = read_csv('metadata_country.csv', show_col_types = FALSE)

# Inspect the first few rows of the datasets
print(head(world_pop))
print(head(country_data))

# Check the structure of the datasets
str(world_pop)
str(country_data)

# Summary of the datasets
summary(world_pop)
summary(country_data)

# Check for missing values
any(is.na(world_pop))
any(is.na(country_data))

# Reshape world_pop data from wide to long format
world_pop_long = world_pop %>%
  pivot_longer(cols = `1960`:`2020`, names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.integer(Year))



# 1(Start)

# Filter data for country (e.g., India)
india_pop = world_pop_long %>% filter(Country == "IND")

# Plot population over time for India
ggplot(india_pop, aes(x = Year, y = Population)) +
  geom_line() +
  labs(title = "Population of India Over Time", x = "Year", y = "Population")

# 1(End)




# 2(Start)

# Merge datasets to include region and income group information
# Joining by 'TableName' since this corresponds to the 'Country' column in 'world_pop'
world_pop_with_info = world_pop_long %>%
  left_join(country_data, by = c("Country" = "S")) %>%
  filter(!is.na(Region) & Region != "null")  # Remove NA and 'null' values in Region

# Population trends by region
regional_pop = world_pop_with_info %>%
  group_by(Region, Year) %>%
  summarize(Total_Population = sum(Population, na.rm = TRUE), .groups = 'drop')

# Plot the population trends by region
ggplot(regional_pop, aes(x = Year, y = Total_Population, color = Region)) +
  geom_line() +
  labs(title = "Population Trends by Region", x = "Year", y = "Total Population")

 # 2(End)






# 3(Start)

# Calculate population trends by income group
income_group_pop = world_pop_with_info %>%
  group_by(IncomeGroup, Year) %>%
  summarize(Total_Population = sum(Population, na.rm = TRUE), .groups = 'drop')

# Filter out NULL and NA values from the IncomeGroup column
income_group_pop_clean = income_group_pop %>%
  filter(!is.na(IncomeGroup) & IncomeGroup != "null")

# Plot Population trends by income group excluding NA and NULL
ggplot(income_group_pop_clean, aes(x = Year, y = Total_Population, color = IncomeGroup)) +
  geom_line() +
  labs(title = "Population Trends by Income Group", x = "Year", y = "Total Population")

# 3(End)







# 4(Start)

# Calculate population changes over time for all countries
pop_changes = world_pop_long %>%
  group_by(Country) %>%
  summarize(
    Initial_Population = first(na.omit(Population)),  # Use na.omit to remove NA values
    Final_Population = last(na.omit(Population)),    # Use na.omit to remove NA values
    .groups = 'drop'
  )

# Identify countries with the highest increase/decrease
pop_changes %>%
  mutate(Change = Final_Population - Initial_Population) %>%
  arrange(desc(Change)) %>%
  head(10)  # Show top 10 countries with highest population change

# 4(End)





# 5 (Start)

# Calculate the annual population growth rate for each country
world_pop_long = world_pop_long %>%
  group_by(Country) %>%
  arrange(Year) %>%
  mutate(Growth_Rate = (Population - lag(Population)) / lag(Population) * 100) %>%
  ungroup()

# Filter data for a specific country (e.g., India)
india_growth = world_pop_long %>% filter(Country == "IND")

# Plot population growth rate over time for India
ggplot(india_growth, aes(x = Year, y = Growth_Rate)) +
  geom_line() +
  labs(title = "Annual Population Growth Rate of India Over Time", x = "Year", y = "Growth Rate (%)")

# 5 (End)






# 6 (Start)

# Filter data for the latest year (2020)
latest_year_pop = world_pop_long %>%
  filter(Year == 2020) %>%
  arrange(desc(Population)) %>%
  head(10)  # Top 10 most populated countries

# Plot top 10 most populated countries
ggplot(latest_year_pop, aes(x = reorder(Country, Population), y = Population)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Top 10 Most Populated Countries in 2020", x = "Country", y = "Population")

# 6 (End)






# 7 (Start)

# Filter data for specific years to compare (e.g., 1960 and 2020), removing 'null' values from IncomeGroup
income_group_comparison = world_pop_with_info %>%
  filter(Year %in% c(1960, 2020) & !is.na(IncomeGroup) & IncomeGroup != "null") %>%
  group_by(IncomeGroup, Year) %>%
  summarize(Total_Population = sum(Population, na.rm = TRUE), .groups = 'drop')

# Plot population comparison by income group
ggplot(income_group_comparison, aes(x = Year, y = Total_Population, fill = IncomeGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Population Comparison by Income Group (1960 vs 2020)", x = "Year", y = "Total Population")

# 7 (End)







# 8 (Start)

# Filter data for the last 10 years (2010 to 2020) and remove NA and null values
last_decade_pop = world_pop_with_info %>%
  filter(Year %in% c(2010, 2020) & !is.na(Region) & Region != "null") %>%
  group_by(Region) %>%
  summarize(
    Pop_2010 = sum(Population[Year == 2010], na.rm = TRUE),
    Pop_2020 = sum(Population[Year == 2020], na.rm = TRUE),
    Growth = Pop_2020 - Pop_2010,
    .groups = 'drop'
  ) %>%
  arrange(desc(Growth))

# Plot population growth for the last decade by region
ggplot(last_decade_pop, aes(x = reorder(Region, Growth), y = Growth)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(title = "Population Growth by Region (2010-2020)", x = "Region", y = "Population Growth")

# 8 (End)





# 9 (Start)

# Calculate average annual growth rate over the last decade
world_pop_growth_projection = world_pop_long %>%
  filter(Year %in% c(2010, 2020)) %>%
  group_by(Country) %>%
  summarize(
    Pop_2010 = first(Population[Year == 2010]),
    Pop_2020 = last(Population[Year == 2020]),
    Growth_Rate = (Pop_2020 - Pop_2010) / 10,
    Projected_Pop_2030 = Pop_2020 + (Growth_Rate * 10),
    .groups = 'drop'
  )

# Display top countries with the highest projected population growth
world_pop_growth_projection %>%
  arrange(desc(Projected_Pop_2030)) %>%
  head(10)

# 9 (End)





# 10 (Start)

# Calculate average population growth rate per decade by income group
income_group_growth_rate = world_pop_with_info %>%
  filter(!is.na(IncomeGroup) & IncomeGroup != "null") %>%  # Remove 'null' and NA from IncomeGroup
  group_by(IncomeGroup, Decade = floor(Year / 10) * 10) %>%
  summarize(Avg_Population_Growth_Rate = (last(Population) - first(Population)) / first(Population) * 100, 
            .groups = 'drop')

# Plot average population growth rate per decade by income group
ggplot(income_group_growth_rate, aes(x = Decade, y = Avg_Population_Growth_Rate, color = IncomeGroup)) +
  geom_line() +
  labs(title = "Average Population Growth Rate by Income Group per Decade", x = "Decade", y = "Growth Rate (%)")

# 10 (End)


