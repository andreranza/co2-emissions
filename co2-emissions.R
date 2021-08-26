library(tidyverse)

# Country names -----------------------------------------------------------
eu_countries <- read_lines("data/eu-countries.txt")
codes <- str_subset(eu_countries, pattern = "\\(") %>% 
  str_remove_all("\\(|\\)")
countries <- str_subset(eu_countries, pattern = "\\(", negate = TRUE) 
eu_df <- tibble(country_name = countries, country_code = codes)

# CO2 emissions -----------------------------------------------------------
em_df <- read_csv("data/estat_env_air_gge_en.csv")
em_df <- janitor::clean_names(em_df)
em_df <- filter(em_df, time_period == 2019 & geo %in% codes) %>% 
  select(unit, airpol, unit, src_crf, geo, time_period, obs_value) %>% 
  filter(src_crf == "TOTX4_MEMONIA") %>% 
  mutate(
    obs_value = case_when(
      unit == "THS_T" ~ obs_value*1e3,
      unit == "MIO_T" ~ obs_value*1e6
    )
  )

em_df <- em_df %>% 
  group_by(geo) %>% 
  summarise(total_airpol = sum(obs_value, na.rm = TRUE)) %>% 
  ungroup()

# Population --------------------------------------------------------------
pop_df <- read_csv("data/estat_population.csv")
pop_df <- janitor::clean_names(pop_df)
pop_df <- filter(pop_df, time_period == 2019 & geo %in% codes) %>% 
  select(geo, obs_value)

# Size --------------------------------------------------------------------
size_df <- read_delim("data/country-size.txt", delim = "\t", skip = 1)
size_df <- janitor::clean_names(size_df)

df <- left_join(pop_df, em_df, by = "geo") %>% 
  rename(pop = obs_value)
df <- left_join(df, eu_df, by = c("geo" = "country_code"))
df <- left_join(df, size_df, by = c("country_name" = "category"))


# Scatterplots ------------------------------------------------------------
library(scales)
library(ggrepel)

ggplot(df, aes(x = pop, y = total_airpol, label = country_name)) +
  geom_point(size = 0.5) +
  geom_text_repel(size = rel(4)) + # size = 2, check_overlap = TRUE, hjust = 0, nudge_x = 0.05
  labs(
    x = "Population",
    y = "Greenhouse gas emission (ton)",
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.45)), 
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(colour = "black", size = rel(1.15)),
    axis.title = element_text(colour = "black", size = rel(1.15))
  ) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) 

ggsave("plots/population.png", height = 210, width = 297, units = "mm", dpi = 300)


ggplot(df, aes(x = square_kilometre, y = total_airpol, label = country_name)) +
  geom_point(size = 0.5) +
  geom_text_repel(size = rel(4)) + # size = 2, check_overlap = TRUE, hjust = 0, nudge_x = 0.05
  labs(
    x = expression(paste("Country size (", Km^{2}, ")")),
    y = "Greenhouse gas emission (ton)",
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.45)), 
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(colour = "black", size = rel(1.15)),
    axis.title = element_text(colour = "black", size = rel(1.15))
  ) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))


ggsave("plots/dimension.png", height = 210, width = 297, units = "mm", dpi = 300) 


df %>% 
  mutate(pop_density = pop/square_kilometre) %>% 
  ggplot(aes(x = pop_density, y = total_airpol, label = country_name)) +
  geom_point(size = 0.5) +
  geom_text_repel(size = rel(4)) + # size = 2, check_overlap = TRUE, hjust = 0, nudge_x = 0.05
  labs(
    x = "Population density",
    y = "Greenhouse gas emission (ton)",
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = rel(1.45)), 
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(colour = "black", size = rel(1.15)),
    axis.title = element_text(colour = "black", size = rel(1.15))
  ) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))

ggsave("plots/pop-density.png", height = 210, width = 297, units = "mm", dpi = 300) 
 