install.packages("tidyverse")
library(tidyverse)

install.packages("gapminder")
library(gapminder)


unicef_indicator_1 <- read_delim("unicef_indicator_1.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
unicef_indicator_2 <- read_delim("unicef_indicator_2.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
unicef_metadata <- read_delim("unicef_metadata.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
data_continent <- read_delim("data_right_3.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

data_join <- unicef_indicator_1 %>%
  left_join(data_continent, by=join_by(country)) %>%
  left_join(unicef_indicator_2, by=join_by(country, time_period, sex)) %>%
  left_join(unicef_metadata, by=join_by(country, time_period))

install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)

# World Map chart

map_world <- map_data("world")

map_data_join <- full_join(data_join, map_world, by=join_by(country == region))


interactive_map <- ggplot(data = map_data_join) +
  aes(x = long, y = lat, group = group, fill = obs_value_1,
      text = paste("Value in %:", obs_value_1)) +
  geom_polygon() +
  scale_fill_gradient(low = "#FFE36A", high = "#AE4A34", na.value = "grey") +
  labs(fill = NULL, 
       title = "Children suffering from at least 4D (%)", 
       x = NULL, 
       y = NULL) +
  theme_classic() +
  theme(legend.position = "left",
        panel.background = element_rect(fill = "#373737")) +
  theme(text = element_text(family = "calibri"))

ggplotly(interactive_map, tooltip = "text") %>%
  config(scrollZoom = TRUE)

# Scatterplot with a linear regression line

scatterplot <- ggplot(data_join, aes(x = obs_value_1, y = life_expectancy)) +
  geom_point(aes(color = continent), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "A tendency between Life Expectancy and 4D", x = "Children suffering at least 4D (%)", y = "Life Expectancy (years)", color = "Continent") +
  scale_color_manual(values = c("Asia" = "#DF4141", "Europe" = "#238024", "Africa" = "#FF9ED5", "Americas" = "#3A5FFF", "Oceania" = "#8D219A")) +
  theme_classic() +
  theme(text = element_text(family = "calibri"))

ggplotly(scatterplot) %>%
  config(scrollZoom = TRUE)

# Bar Chart

data_filtered_gender <- data_join %>% 
  filter(sex %in% c("Male", "Female"))

barplot <- ggplot(data_filtered_gender, aes(x = continent, fill = sex, y = obs_value_1)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Continent", y = "Children suffering at least 4D (%)", fill = "Gender") +
  scale_fill_manual(values = c("Male" = "#74D0F1", "Female" = "#91283B")) +
  theme_classic() +
  theme(text = element_text(family = "calibri"))

interactive_barplot <- ggplotly(barplot)

interactive_barplot <- interactive_barplot %>%
  layout(
    xaxis = list(title = "Continent"),
    yaxis = list(title = "Children suffering at least 4D (%)"),
    hovermode = "closest",
    hoverlabel = list(bgcolor = "white", font = list(family = "Arial", size = 12))
  )

interactive_barplot

# Time-series chart

data_join_1 <- unicef_indicator_1 %>%
  left_join(unicef_metadata, by=join_by(country)) %>%
  left_join(data_continent, by=join_by(country))

timeseries <- data_join_1 %>%
  ggplot() +
  aes(time_period.y, life_expectancy, group = country, color = continent,
      text = paste("Country:", country, "<br>Year:", time_period.y,
                   "<br>Life Expectancy:", life_expectancy)) +
  geom_line() +
  labs(x = "Year", y = "Life Expectancy", color = "Continent") +
  scale_color_manual(values = c("Asia" = "#DF4141", "Europe" = "#238024", "Africa" = "#FF9ED5", "Americas" = "#3A5FFF", "Oceania" = "#8D219A")) +
  theme_classic() +
  theme(text = element_text(family = "calibri"))

ggplotly(timeseries, tooltip = "text")

