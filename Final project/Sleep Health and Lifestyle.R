library(dplyr)         # Work with data frames
library(ggplot2)       # Plot graphs
library(treemapify)    # Plot treemap
library(tidyr)         # Create tidy data

# Create colors
slate_200 <- "#e2e8f0"
slate_700 <- "#334155"
pink_400 <- "#f472b6"
sky_800 <- "#075985"
sky_600 <- "#0284c7"
sky_400 <- "#38bdf8"
sky_100 <- "#e0f2fe"
sky_50 <- "#f0f9ff"

normal_text_size <- 30
title_text_size <- 32
margin_size <- 20
font_family <- 'Franklin Gothic Book' # Arial or serif

# Reads the data
df <- read.csv('sleep-health-and-lifestyle.csv')

df <- df %>%
  mutate(Stress.Level = case_when(
    Stress.Level <= 2 ~ "Very Low",
    Stress.Level <= 4 ~ "Low",
    Stress.Level <= 6 ~ "Moderate",
    Stress.Level <= 8 ~ "High",
    Stress.Level <= 10 ~ "Very High"
  )) %>%
  mutate(Quality.of.Sleep = case_when(
    Quality.of.Sleep <= 2 ~ "Very Poor",
    Quality.of.Sleep <= 4 ~ "Poor",
    Quality.of.Sleep <= 6 ~ "Average",
    Quality.of.Sleep <= 8 ~ "Good",
    Quality.of.Sleep <= 10 ~ "Excellent"
  ))

df$Stress.Level <- factor(
  df$Stress.Level,
  levels = c("Very High", "High", "Moderate", "Low", "Very Low")
)

df$Quality.of.Sleep <- factor(
  df$Quality.of.Sleep,
  levels = c("Excellent", "Good", "Average", "Poor", "Very Poor")
)

df$Sleep.Disorder <- factor(
  df$Sleep.Disorder,
  levels = c("None", "Insomnia", "Sleep Apnea")
)

# Sleep duration across age and gender

ggplot(df,
       aes(x = Age, y = Sleep.Duration, color = Gender)) +
  geom_point(size = 10) +
  geom_smooth(color = slate_700, size = 2) +
  facet_wrap(~ Gender) +
  scale_colour_manual(values = c(
    "Female" = pink_400,
    "Male" = sky_400
  )) +
  labs(title = "Figure 1: Sleep duration across age and gender",
       x = "Age",
       y = "Sleep duration (h)") +
  theme_classic() +
  theme(
    text = element_text(color = slate_700, family = font_family),
    plot.title = element_text(size = title_text_size, face = "bold",
                              margin = margin(b = margin_size)),
    panel.border = element_rect(color = slate_700, fill = NA, linewidth = 2),
    panel.grid.major = element_line(color = slate_200, linewidth = 1),
    strip.background = element_rect(color = slate_700, fill = NA, linewidth = 2),
    strip.text = element_text(size = normal_text_size, color = slate_700),
    legend.key.height = unit(1.25, "cm"),
    legend.text = element_text(size = normal_text_size),
    legend.title = element_text(size = title_text_size),
    axis.text = element_text(size = normal_text_size, color = slate_700),
    axis.title = element_text(size = title_text_size),
    axis.title.x = element_text(margin = margin(t = margin_size)),
    axis.title.y = element_text(margin = margin(r = margin_size))
  )

# Connection between stress and sleep disorders

stress_level_and_sleep_disorder <- df %>%
  count(Stress.Level, Sleep.Disorder) %>%
  complete(Stress.Level, Sleep.Disorder, fill = list(n = 0))

ggplot(stress_level_and_sleep_disorder, 
       aes(x = Stress.Level, y = Sleep.Disorder, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = sky_50, high = sky_400) +
  labs(title = "Connection between stress and sleep disorders",
       x = "Stress level",
       y = "Sleep disorder",
       fill = "Count") +
  theme_classic() +
  theme(
    text = element_text(color = slate_700, family = font_family),
    plot.title = element_text(size = title_text_size,
                              color = slate_700,
                              face = "bold",
                              margin = margin(b = margin_size, l = margin_size)),
    legend.key.height = unit(1.25, "cm"),
    legend.text = element_text(size = normal_text_size),
    legend.title = element_text(size = title_text_size),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = normal_text_size, color = slate_700),
    axis.title = element_text(size = title_text_size),
    axis.title.x = element_text(margin = margin(t = margin_size)),
    axis.title.y = element_text(margin = margin(r = margin_size))
  )

# Stress levels by occupation

ggplot(df,
       aes(x = Occupation, fill = Stress.Level)) +
  geom_bar() +
  scale_fill_manual(
    values = c("Very High" = sky_800,
               "High" = sky_600,
               "Moderate" = sky_400,
               "Low" = sky_100,
               "Very Low" = sky_50)
  ) +
  labs(title = "A count of people and stress levels across occupations",
       x = "Occupation",
       y = "Count",
       fill = "Stress level") +
  theme_classic() +
  theme(
    text = element_text(color = slate_700, family = font_family),
    plot.title = element_text(size = title_text_size,
                              color = slate_700,
                              face = "bold",
                              margin = margin(b = margin_size)),
    legend.key.height = unit(1.25, "cm"),
    legend.text = element_text(size = normal_text_size),
    legend.title = element_text(size = title_text_size),
    axis.line = element_line(linewidth = 1.25),
    axis.ticks = element_line(linewidth = 1),
    axis.text = element_text(size = normal_text_size, color = slate_700),
    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.title = element_text(size = title_text_size),
    axis.title.x = element_text(margin = margin(t = margin_size)),
    axis.title.y = element_text(margin = margin(r = margin_size))
  )

# Phyical activity time & sleep quality

ggplot(df,
       aes(x = Quality.of.Sleep, 
           y = Physical.Activity.Level,
           fill = ifelse(
             Quality.of.Sleep == "Excellent", 
             "Highlighted", 
             "Normal"
           ))) +
  geom_boxplot(size = 1.25) +
  scale_y_continuous(breaks = seq(30, 90, 10)) +
  scale_fill_manual(
    values = c("Highlighted" = "#38bdf8", "Normal" = "#f0f9ff")
  ) +
  labs(title = "The impact of physical activity on sleep",
       x = "Sleep quality",
       y = "Physical activity time (minutes)") +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(color = slate_700, family = font_family),
    plot.title = element_text(size = title_text_size,
                              color = slate_700,
                              face = "bold",
                              margin = margin(b = margin_size)),
    axis.line = element_line(linewidth = 1.25),
    axis.ticks = element_line(linewidth = 1),
    axis.text = element_text(size = normal_text_size, color = slate_700),
    axis.title = element_text(size = title_text_size),
    axis.title.x = element_text(margin = margin(t = margin_size)),
    axis.title.y = element_text(margin = margin(r = margin_size))
  )


