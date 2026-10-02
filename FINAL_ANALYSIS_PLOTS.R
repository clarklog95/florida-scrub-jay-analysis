# Florida Scrub-Jay manuscript analysis
# Run from this project folder: Rscript --vanilla FINAL_ANALYSIS_PLOTS.R
# Or open Florida_Scrub_Jay.Rproj and source this script.
# Input CSVs are never modified. Generated results go to outputs/.

packages <- c("dplyr", "tidyr", "tibble", "flextable", "ggplot2", "ggpattern", "patchwork")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install required packages first: ", paste(missing_packages, collapse = ", "),
       ". See README.md for the installation command.")
}
invisible(lapply(packages, library, character.only = TRUE))
if (!file.exists("final_data.csv")) stop("Run this script from the project folder containing final_data.csv.")
final_data <- read.csv("final_data.csv", fileEncoding = "UTF-8-BOM")
required_columns <- c("Jay.ID", "Trial.id", "Age_bin1", "personality", "worm_color", "event", "Trial.time")
stopifnot(all(required_columns %in% names(final_data)))
dir.create("outputs", showWarnings = FALSE)

# NA bird IDs belong to uneaten-worm records and are not counted as birds.
# Preserve supplied age/personality groups and all existing analysis rows.
# Tests and effect sizes retain the manuscript's Yates-corrected convention.

# Total number of individual jays
total_jays <- n_distinct(final_data$Jay.ID, na.rm = TRUE)

# Number of jays with age info (Age_bin1 not NA)
aged_jays <- final_data %>%
  filter(!is.na(Age_bin1)) %>%
  distinct(Jay.ID) %>%
  nrow()

# Number of jays categorized as "old"
old_jays <- final_data %>%
  filter(Age_bin1 == "old") %>%
  distinct(Jay.ID) %>%
  nrow()

# Number of jays categorized as "young"
young_jays <- final_data %>%
  filter(Age_bin1 == "young") %>%
  distinct(Jay.ID) %>%
  nrow()

# Number of jays with personality info
scored_jays <- final_data %>%
  filter(!is.na(personality)) %>%
  distinct(Jay.ID) %>%
  nrow()

# Number of bold jays
bold_jays <- final_data %>%
  filter(personality == "bold") %>%
  distinct(Jay.ID) %>%
  nrow()

# Number of shy jays
shy_jays <- final_data %>%
  filter(personality == "shy") %>%
  distinct(Jay.ID) %>%
  nrow()

# Number of unique trials
unique_trials <- n_distinct(final_data$Trial.id)

# Combine and print results
summary_counts <- list(
  total_jays = total_jays,
  aged_jays = aged_jays,
  old_jays = old_jays,
  young_jays = young_jays,
  personality_scored_jays = scored_jays,
  bold_jays = bold_jays,
  shy_jays = shy_jays,
  unique_trials = unique_trials
)

print(summary_counts)

# ----------------------------
# AGE GROUP CHI-SQUARED TEST
# ----------------------------
age_chi_data <- final_data %>%
  filter(event == 1, !is.na(Age_bin1)) %>%
  count(Age_bin1, worm_color) %>%
  pivot_wider(names_from = worm_color, values_from = n, values_fill = 0)

age_chisq <- chisq.test(age_chi_data[, c("Green", "Orange")], correct = TRUE)

n_age <- sum(age_chi_data$Green + age_chi_data$Orange)
phi_age <- sqrt(age_chisq$statistic / n_age)
cramer_age <- phi_age
contingency_age <- sqrt(age_chisq$statistic / (age_chisq$statistic + n_age))

# ----------------------------
# PERSONALITY CHI-SQUARED TEST
# ----------------------------
personality_chi_data <- final_data %>%
  filter(event == 1, !is.na(personality)) %>%
  count(personality, worm_color) %>%
  pivot_wider(names_from = worm_color, values_from = n, values_fill = 0)

personality_chisq <- chisq.test(personality_chi_data[, c("Green", "Orange")], correct = TRUE)

n_pers <- sum(personality_chi_data$Green + personality_chi_data$Orange)
phi_pers <- sqrt(personality_chisq$statistic / n_pers)
cramer_pers <- phi_pers
contingency_pers <- sqrt(personality_chisq$statistic / (personality_chisq$statistic + n_pers))

# ----------------------------
# COMBINED SUMMARY TABLE
# ----------------------------
combined_summary <- tibble(
  Comparison = c("Age group (Young vs Old)", "Personality (Bold vs Shy)"),
  `Chi-squared` = round(c(age_chisq$statistic, personality_chisq$statistic), 3),
  `df` = c(age_chisq$parameter, personality_chisq$parameter),
  `P-value` = c(format.pval(age_chisq$p.value, digits = 3, eps = 0.001),
                format.pval(personality_chisq$p.value, digits = 3, eps = 0.001)),
  `Phi` = round(c(phi_age, phi_pers), 3),
  `Cramer's V` = round(c(cramer_age, cramer_pers), 3),
  `Contingency Coefficient` = round(c(contingency_age, contingency_pers), 3)
)

# ----------------------------
# FORMAT WITH FLEXTABLE
# ----------------------------
table1 <- flextable(combined_summary) |>
  set_caption("Chi-squared Tests and Effect Sizes: Age and Personality") |>
  autofit()

# Step 1: Age group summary
summary_age <- final_data %>%
  filter(event == 1, !is.na(Age_bin1)) %>%
  group_by(Age_bin1, worm_color) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    Group = recode(Age_bin1,
                   "old" = "Old",
                   "young" = "Young"),
    GroupType = "Age"
  )

# Step 2: Personality summary
summary_personality <- final_data %>%
  filter(event == 1, !is.na(personality)) %>%
  group_by(personality, worm_color) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    Group = recode(personality,
                   "bold" = "Bold",
                   "shy" = "Shy"),
    GroupType = "Personality"
  )

# Step 3: All jays summary
summary_all <- final_data %>%
  filter(event == 1) %>%
  group_by(worm_color) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    Group = "All Jays",
    GroupType = "All"
  )

# Step 4: Combine and set group order
plot_data <- bind_rows(summary_all, summary_age, summary_personality) %>%
  mutate(
    Group = factor(Group, levels = c("All Jays", "Old", "Young", "Bold", "Shy")),
    GroupType = factor(GroupType, levels = c("All", "Age", "Personality")),
    worm_color = factor(worm_color, levels = c("Green", "Orange"))
  )

# Step 5: Plot
figure1 <- ggplot(plot_data, aes(x = Group, y = n)) +
  geom_bar_pattern(
    stat = "identity",
    position = position_dodge(width = 0.9),
    aes(fill = worm_color, pattern = worm_color),
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = 0.25,
    pattern_spacing = 0.04,
    pattern_key_scale_factor = 1.2,
    show.legend = TRUE
  ) +
  facet_wrap(~ GroupType, scales = "free_x") +
  scale_fill_manual(
    values = c("Green" = "darkgreen", "Orange" = "orange"),
    labels = c("Green" = "Green", "Orange" = "Orange"),
    name = "Worm type"
  ) +
  scale_pattern_manual(
    values = c("Green" = "none", "Orange" = "stripe"),
    labels = c("Green" = "Green", "Orange" = "Orange"),
    name = "Worm type"
  ) +
  labs(
    x = NULL,
    y = "Number of waxworms eaten",
    title = "Waxworm Consumption by Age and Personality Groups"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

# Step 1: Filter valid rows
eaten_worms <- final_data %>%
  filter(event == 1, !is.na(Trial.time))

# Step 2: Create a long-form group label
eaten_long <- eaten_worms %>%
  mutate(
    BoldnessGroup = case_when(
      personality == "bold" ~ "Bold Jays",
      personality == "shy" ~ "Shy Jays",
      TRUE ~ NA_character_
    ),
    AgeGroup = case_when(
      Age_bin1 == "young" ~ "Young Jays",
      Age_bin1 == "old" ~ "Old Jays",
      TRUE ~ NA_character_
    )
  )

# Step 3: Stack all relevant groupings (All, Bold/Shy, Old/Young)
eaten_combined <- bind_rows(
  eaten_long %>% mutate(facet_group = "All Jays"),
  eaten_long %>% filter(!is.na(BoldnessGroup)) %>% mutate(facet_group = BoldnessGroup),
  eaten_long %>% filter(!is.na(AgeGroup)) %>% mutate(facet_group = AgeGroup)
)

# Step 4: Cumulative worm counts per group and color
cumulative_counts <- eaten_combined %>%
  group_by(facet_group, worm_color) %>%
  arrange(Trial.time) %>%
  mutate(WormsEaten = row_number()) %>%
  ungroup()

# Step 5: Get group totals
group_totals <- cumulative_counts %>%
  group_by(facet_group) %>%
  summarise(AllEaten = n(), .groups = "drop")

# Relative depletion index: denominator includes both colors within the group.
# Uneaten worms (event = 0) do not contribute to this descriptive index.
plot_data <- cumulative_counts %>%
  left_join(group_totals, by = "facet_group") %>%
  mutate(RelativeRemaining = 100 * (1 - WormsEaten / AllEaten))

# Step 7: Add trial time = 0
start_rows <- plot_data %>%
  group_by(facet_group, worm_color) %>%
  summarise(
    Trial.time = 0,
    WormsEaten = 0,
    AllEaten = unique(AllEaten),
    .groups = "drop"
  ) %>%
  mutate(RelativeRemaining = 100)

# Step 8: Combine
plot_data_final <- bind_rows(start_rows, plot_data)

# Set panel order
plot_data_final$facet_group <- factor(
  plot_data_final$facet_group,
  levels = c("All Jays", "Young Jays", "Old Jays", "Bold Jays", "Shy Jays")
)

# --- Main 5-panel cumulative plot ---
p_main <- ggplot(plot_data_final, aes(x = Trial.time, y = RelativeRemaining,
                                      group = interaction(worm_color, facet_group),
                                      color = worm_color, linetype = worm_color)) +
  geom_step(linewidth = 1.2) +
  facet_wrap(~ facet_group, nrow = 1) +
  scale_color_manual(
    values = c("Green" = "darkgreen", "Orange" = "#E6550D"),  # dark orange
    labels = c("Green" = "Green", "Orange" = "Orange"),
    name = "Worm type"
  ) +
  scale_linetype_manual(
    values = c("Green" = "solid", "Orange" = "dashed"),
    labels = c("Green" = "Green", "Orange" = "Orange"),
    name = "Worm type"
  ) +
  labs(
    x = "Time into trial (seconds)",
    y = "Relative depletion index (%)"
  ) +
  theme_gray(base_size = 14) +
  theme(
    strip.text = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(0, 10, 10, 10)
  )

# Comparison labels above the panels

p_top <- ggplot() +
  annotate("text", x = 2.5, y = 1, label = "Age Comparison", fontface = "italic", size = 5) +
  annotate("text", x = 4.5, y = 1, label = "Personality Comparison", fontface = "italic", size = 5) +
  xlim(0.5, 5.5) +
  ylim(0.5, 1.5) +
  theme_void() +
  theme(
    plot.margin = margin(0, 10, 0, 10)
  )

# --- Combine with patchwork ---
combined_plot <- p_top / p_main + 
  plot_layout(heights = c(0.1, 1)) +
  plot_annotation(
    title = "Cumulative Depletion of Waxworms",
    subtitle = "Relative worm depletion over time by age group and personality"
  )



# Save outputs explicitly so the script also works in a noninteractive session.
write.csv(data.frame(metric = names(summary_counts), value = unlist(summary_counts)),
          "outputs/sample_counts.csv", row.names = FALSE)
write.csv(combined_summary, "outputs/table1.csv", row.names = FALSE)
write.csv(age_chi_data, "outputs/age_color_counts.csv", row.names = FALSE)
write.csv(personality_chi_data, "outputs/personality_color_counts.csv", row.names = FALSE)
write.csv(plot_data_final, "outputs/figure2_data.csv", row.names = FALSE)
htmltools::save_html(flextable::htmltools_value(table1), file = "outputs/table1.html",
                     libdir = "table1_files")
ggsave("outputs/figure1.png", figure1, width = 11, height = 6, dpi = 300, bg = "white")
ggsave("outputs/figure1.pdf", figure1, width = 11, height = 6)
ggsave("outputs/figure2.png", combined_plot, width = 12, height = 6, dpi = 300, bg = "white")
ggsave("outputs/figure2.pdf", combined_plot, width = 12, height = 6)
capture.output(sessionInfo(), file = "outputs/sessionInfo.txt")
print(combined_summary)
if (interactive()) {
  print(table1)
  print(figure1)
  print(combined_plot)
}
message("Analysis complete. Results saved in outputs/.")
