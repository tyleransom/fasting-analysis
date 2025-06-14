library(tidyverse)

# load data
load('../data/cleaned/daily_fitbit.rda')

# Comprehensive diet trial periods from both blog posts (2024-2025)
diet_trials_comprehensive <- tribble(
  ~diet_name, ~start_date, ~end_date, ~color, ~year, ~type, ~label_height,
  # 2024 trials
  "Eating seed oils", "2024-01-01", "2024-03-31", "#A6CEE3", "2024", "diet", -1,
  "Omega-3 Maxxing", "2024-04-01", "2024-09-30", "#E31A1C", "2024", "diet", 0, 
  "Liver Pills", "2024-08-01", "2024-08-31", "#1F78B4", "2024", "diet", -10,
  "Ex-150 + Fruit", "2024-10-01", "2024-12-20", "#33A02C", "2024", "diet", -10,
  "Honey", "2024-12-21", "2024-12-28", "#FB9A99", "2024", "diet", -8,
  # 2025 trials  
  "Omega-3 Maxxing", "2025-01-01", "2025-03-15", "#E31A1C", "2025", "diet", 0,
  "Honey Riff", "2025-03-23", "2025-04-23", "#FF7F00", "2025", "diet", -7,
  "Potato", "2025-05-08", "2025-05-12", "#CAB2D6", "2025", "diet", -10,
  "Carnivore", "2025-05-29", "2025-05-31", "#6A3D9A", "2025", "diet", -8,
  # Vacations
  "Vacation", "2024-08-01", "2024-08-10", "#FFD700", "2024", "vacation", 1,
  "Vacation", "2025-03-17", "2025-03-20", "#FFD700", "2025", "vacation", 1,
  "Vacation", "2025-06-03", "2025-06-11", "#FFD700", "2025", "vacation", 1
) %>%
  mutate(
    start_date = as.Date(start_date),
    end_date = as.Date(end_date),
    duration = as.numeric(end_date - start_date + 1),
    midpoint = start_date + (end_date - start_date)/2
  )
diet_labels <- unique(diet_trials_comprehensive$diet_name[(!(diet_trials_comprehensive$diet_name %in% c("Honey", "Ex-150 + Fruit", "Liver Pills", "Eating seed oils"))) & diet_trials_comprehensive$type == "diet"])
all_labels <- c(diet_labels, "Vacation")

# Chart 1: Comprehensive Timeline (1.5 years)
comprehensive_timeline <- diet_trials_comprehensive %>%
  mutate(
    y_pos = case_when(
      type == "vacation" ~ 10,  # Put all vacations at position 13
      TRUE ~ row_number()       # Keep diet trials at their row numbers
    ),
    diet_label = case_when(
      type == "vacation" ~ paste0(diet_name, "\n(", duration, " days)"),
      duration > 30 ~ paste0(diet_name, "\n(", round(duration/30.4, 1), " months)"),
      TRUE ~ paste0(diet_name, "\n(", duration, " days)")
    ),
    # Different styling for vacations
    line_type = ifelse(type == "vacation", "dashed", "solid"),
    alpha_val = ifelse(type == "vacation", 0.6, 0.8)
  ) %>%
  ggplot(aes(y = y_pos)) +
  # Add year background shading
  annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-12-31"), 
           ymin = 0, ymax = 13, fill = "lightblue", alpha = 0.1) +
  annotate("rect", xmin = as.Date("2025-01-01"), xmax = as.Date("2025-12-31"), 
           ymin = 0, ymax = 13, fill = "lightgreen", alpha = 0.1) +
  # Add year labels
  #annotate("text", x = as.Date("2024-07-01"), y = 12.5, label = "2024", 
  #         size = 5, alpha = 0.7, fontface = "bold") +
  #annotate("text", x = as.Date("2025-07-01"), y = 12.5, label = "2025", 
  #         size = 5, alpha = 0.7, fontface = "bold") +
  # Diet trial segments with different styles for vacations
  geom_segment(
    aes(x = start_date, xend = end_date, yend = y_pos, color = diet_name, alpha = alpha_val),
    size = 6, linetype = "solid"
  ) +
  # Add hatching pattern for vacations using smaller segments
  geom_segment(
    data = . %>% filter(type == "vacation"),
    aes(x = start_date, xend = end_date, yend = y_pos, color = diet_name),
    size = 6, alpha = 0.4, linetype = "dotted"
  ) +
  geom_point(aes(x = start_date, color = diet_name), size = 2.5) +
  geom_point(aes(x = end_date, color = diet_name), size = 2.5) +
  # Labels with smart positioning
  geom_text(
    aes(x = midpoint, label = diet_label),
    hjust = 0.5, vjust = 0.5, size = 2.6, fontface = "bold", angle = 0
  ) +
  scale_color_manual(values = setNames(diet_trials_comprehensive$color, diet_trials_comprehensive$diet_name)) +
  scale_alpha_identity() +
  scale_x_date(
    date_breaks = "2 weeks",
    date_labels = "%b '%y",
    limits = c(as.Date("2025-01-01"), as.Date("2025-07-01"))
  ) +
scale_y_continuous(
  breaks = 6:(5 + length(all_labels)),
  labels = all_labels,
  limits = c(5.5, 10.5)
) +
  labs(
    title = "2025 Diet Experimentation Timeline",
    x = "Date",
    y = "",
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(hjust = 1, size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60"),
    plot.caption = element_text(size = 9, color = "gray50")
  )

ggsave("../exhibits/figures/diet-trials-2025-timeline.png", height = 5, width = 8)


# Chart 2: Weight Over Time with All Trial Overlays (full dataset)
# Filter to show the complete experimental period
weight_data_full <- all %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2025-06-30")) %>%
  filter(!is.na(weight))

comprehensive_weight_plot <- weight_data_full %>%
  ggplot(aes(x = date, y = weight)) +
  # Add colored rectangles for diet trials
  geom_rect(
    data = diet_trials_comprehensive %>% filter(type == "diet"),
    aes(xmin = start_date, xmax = end_date, 
        ymin = -Inf, ymax = Inf, fill = diet_name),
    alpha = 0.25, inherit.aes = FALSE
  ) +
  # Add hatched rectangles for vacations
  geom_rect(
    data = diet_trials_comprehensive %>% filter(type == "vacation"),
    aes(xmin = start_date, xmax = end_date, 
        ymin = -Inf, ymax = Inf),
    fill = "#FFD700", alpha = 0.15, inherit.aes = FALSE
  ) +
  # Add diagonal lines for vacation pattern
  geom_segment(
    data = diet_trials_comprehensive %>% 
      filter(type == "vacation") %>%
      rowwise() %>%
      do({
        dates <- seq(.$start_date, .$end_date, by = "day")
        tibble(
          x = dates,
          xend = dates + 1,
          y = rep(c(-Inf, Inf), length.out = length(dates)),
          yend = rep(c(Inf, -Inf), length.out = length(dates))
        )
      }),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "#FFD700", alpha = 0.3, size = 0.5, inherit.aes = FALSE
  ) +
  # Weight trend line and points
  geom_line(color = "black", size = 0.7, alpha = 0.8) +
  geom_point(color = "black", size = 0.8, alpha = 0.6) +
  # Add trial labels for major periods
  geom_text(
    data = diet_trials_comprehensive %>% filter(type == "diet"),
    aes(x = midpoint, 
        y = max(weight_data_full$weight, na.rm = TRUE) + label_height,
        label = diet_name, color = diet_name),
    angle = 0, hjust = 0.5, vjust = 0, size = 2.5, fontface = "bold",
    inherit.aes = FALSE
  ) +
  # Add vacation labels at bottom
  geom_text(
    data = diet_trials_comprehensive %>% filter(type == "vacation"),
    aes(x = midpoint, y = min(weight_data_full$weight, na.rm = TRUE) + 1),
    label = "Vacation", color = "#B8860B", size = 2.5, fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = setNames(diet_trials_comprehensive$color[diet_trials_comprehensive$type == "diet"], 
                                      diet_trials_comprehensive$diet_name[diet_trials_comprehensive$type == "diet"])) +
  scale_color_manual(values = setNames(diet_trials_comprehensive$color, diet_trials_comprehensive$diet_name)) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b '%y",
    limits = c(as.Date("2024-01-01"), as.Date("2025-06-30"))
  ) +
  labs(
    title = "Weight Tracking with Diet Trials & Vacations, Jan '24-Jun '25",
    #subtitle = "18 months of weight data with all diet trials and travel periods highlighted",
    x = "Date",
    y = "Weight (lbs)",
    caption = "Colored regions = diet trials, Gold hatched = vacations, White regions = normal diet"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60"),
    plot.caption = element_text(size = 9, color = "gray50")
  )

ggsave("../exhibits/figures/diet-trials-24-25.png", height = 5, width = 8)

# Chart 2b: Body fat pct Over Time with All Trial Overlays (full dataset)
# Filter to show the complete experimental period
bf_pct_data_full <- all %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2025-06-30")) %>%
  filter(!is.na(fat_pct))

comprehensive_bf_pct_plot <- bf_pct_data_full %>%
  ggplot(aes(x = date, y = fat_pct)) +
  # Add colored rectangles for diet trials
  geom_rect(
    data = diet_trials_comprehensive %>% filter(type == "diet"),
    aes(xmin = start_date, xmax = end_date, 
        ymin = -Inf, ymax = Inf, fill = diet_name),
    alpha = 0.25, inherit.aes = FALSE
  ) +
  # Add hatched rectangles for vacations
  geom_rect(
    data = diet_trials_comprehensive %>% filter(type == "vacation"),
    aes(xmin = start_date, xmax = end_date, 
        ymin = -Inf, ymax = Inf),
    fill = "#FFD700", alpha = 0.15, inherit.aes = FALSE
  ) +
  # Add diagonal lines for vacation pattern
  geom_segment(
    data = diet_trials_comprehensive %>% 
      filter(type == "vacation") %>%
      rowwise() %>%
      do({
        dates <- seq(.$start_date, .$end_date, by = "day")
        tibble(
          x = dates,
          xend = dates + 1,
          y = rep(c(-Inf, Inf), length.out = length(dates)),
          yend = rep(c(Inf, -Inf), length.out = length(dates))
        )
      }),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "#FFD700", alpha = 0.3, size = 0.5, inherit.aes = FALSE
  ) +
  # BF% trend line and points
  geom_line(color = "black", size = 0.7, alpha = 0.8) +
  geom_point(color = "black", size = 0.8, alpha = 0.6) +
  # Add trial labels for major periods
  geom_text(
    data = diet_trials_comprehensive %>% filter(type == "diet"),
    aes(x = midpoint, 
        y = max(bf_pct_data_full$fat_pct, na.rm = TRUE) + label_height/3,
        label = diet_name, color = diet_name),
    angle = 0, hjust = 0.5, vjust = 0, size = 2.5, fontface = "bold",
    inherit.aes = FALSE
  ) +
  # Add vacation labels at bottom
  geom_text(
    data = diet_trials_comprehensive %>% filter(type == "vacation"),
    aes(x = midpoint, y = min(bf_pct_data_full$fat_pct, na.rm = TRUE) + 1),
    label = "Vacation", color = "#B8860B", size = 2.5, fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = setNames(diet_trials_comprehensive$color[diet_trials_comprehensive$type == "diet"], 
                                      diet_trials_comprehensive$diet_name[diet_trials_comprehensive$type == "diet"])) +
  scale_color_manual(values = setNames(diet_trials_comprehensive$color, diet_trials_comprehensive$diet_name)) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b '%y",
    limits = c(as.Date("2024-01-01"), as.Date("2025-06-30"))
  ) +
  scale_y_continuous(
    limits = c(11, 15)  # Replace with your desired min and max values
  ) +
  labs(
    title = "Body Fat % Tracking with Diet Trials & Vacations, Jan '24-Jun '25",
    #subtitle = "18 months of weight data with all diet trials and travel periods highlighted",
    x = "Date",
    y = "Body Fat %",
    caption = "Colored regions = diet trials, Gold hatched = vacations, White regions = normal diet"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60"),
    plot.caption = element_text(size = 9, color = "gray50")
  )

ggsave("../exhibits/figures/diet-trials-24-25-bf-pct.png", height = 5, width = 8)

# Chart 3: Focused view on 2025 experiments
trials_2025 <- diet_trials_comprehensive %>% filter(year == "2025")

weight_2025_focused <- all %>%
  filter(date >= as.Date("2024-12-31") & date <= as.Date("2025-06-15")) %>%
  filter(!is.na(weight)) %>%
  ggplot(aes(x = date, y = weight)) +
  geom_rect(
    data = trials_2025,
    aes(xmin = start_date, xmax = end_date, 
        ymin = -Inf, ymax = Inf, fill = diet_name),
    alpha = 0.3, inherit.aes = FALSE
  ) +
  geom_line(color = "black", size = 1, alpha = 0.9) +
  geom_point(color = "black", size = 1.2, alpha = 0.8) +
  geom_text(
    data = trials_2025,
    aes(x = midpoint, 
        y = max(all$weight[all$date >= as.Date("2024-12-31") & 
                             all$date <= as.Date("2025-06-15")], na.rm = TRUE) + label_height -1,
        label = diet_name, color = diet_name),
    angle = 0, hjust = 0.5, vjust = -3, size = 3, fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = setNames(trials_2025$color, trials_2025$diet_name)) +
  scale_color_manual(values = setNames(trials_2025$color, trials_2025$diet_name)) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "2025 Diet Trials",
    x = "Date",
    y = "Weight (lbs)",
    caption = "Colored regions = diet trials / vacations, White regions = normal diet"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray60")
  )

ggsave("../exhibits/figures/diet-trials-2025.png", height = 5, width = 8)