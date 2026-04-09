library(dplyr)
library(ggplot2)
library(gtsummary)
library(flextable)
library(officer)
library(cowplot)
library(scales)

theme_set(theme_minimal(base_size = 11))

script_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
assets_dir <- file.path(script_dir, "ppt_assets")
if (!dir.exists(assets_dir)) {
  dir.create(assets_dir, recursive = TRUE)
}

load(file.path(script_dir, "a1c_final.Rdata"))

customeTheme <- theme(
  title = element_text(size = 11),
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 12),
  axis.title.y = element_text(size = 12)
)

medical_history_vars <- c(
  "cancer",
  "hypertension",
  "cardiorespiratory",
  "coagulopathy",
  "teratogen",
  "anticoagulants",
  "deferral_meds",
  "splenectomy_tcpenia"
)

fmt_p <- function(x) {
  if (is.na(x)) {
    return("p = NA")
  }
  if (x < 0.001) {
    return("p < 0.001")
  }
  paste0("p = ", formatC(x, format = "f", digits = 3))
}

save_plot <- function(plot_obj, filename, width, height) {
  out <- file.path(assets_dir, filename)
  ggsave(out, plot = plot_obj, width = width, height = height, dpi = 300, bg = "white")
  out
}

make_text_block <- function(lines, size = 16, color = "#2f2a24", bold_first = FALSE) {
  blocks <- vector("list", length(lines))
  for (i in seq_along(lines)) {
    prop <- fp_text(
      font.size = size,
      color = color,
      font.family = "Calibri",
      bold = bold_first && i == 1
    )
    blocks[[i]] <- fpar(ftext(lines[[i]], prop = prop))
  }
  do.call(block_list, blocks)
}

add_slide_title <- function(doc, title, subtitle = NULL) {
  doc <- ph_with(
    doc,
    value = fpar(ftext(title, fp_text(font.size = 24, bold = TRUE, color = "#1f1f1f"))),
    location = ph_location(left = 0.4, top = 0.2, width = 12.4, height = 0.5)
  )
  if (!is.null(subtitle)) {
    doc <- ph_with(
      doc,
      value = fpar(ftext(subtitle, fp_text(font.size = 11, color = "#5a5a5a"))),
      location = ph_location(left = 0.4, top = 0.68, width = 12.2, height = 0.35)
    )
  }
  doc
}

styled_ft <- function(df, body_font = 8, header_font = 10) {
  ft <- flextable(df)
  ft <- theme_booktabs(ft)
  ft <- fontsize(ft, part = "all", size = body_font)
  ft <- fontsize(ft, part = "header", size = header_font)
  ft <- bold(ft, part = "header")
  ft <- align(ft, part = "all", align = "left")
  ft <- valign(ft, part = "all", valign = "top")
  ft <- padding(ft, padding = 3, part = "all")
  ft <- autofit(ft)
  ft
}

styled_characteristic_ft <- function(df) {
  ft <- flextable(df %>% select(Characteristic, Summary))
  ft <- theme_booktabs(ft)
  ft <- fontsize(ft, part = "all", size = 8)
  ft <- fontsize(ft, part = "header", size = 10)
  ft <- bold(ft, part = "header")
  ft <- align(ft, part = "all", align = "left")
  ft <- padding(ft, padding = 3, part = "all")
  label_rows <- which(df$row_type == "label" | df$Characteristic == "Medical History")
  level_rows <- which(df$row_type == "level")
  if (length(label_rows) > 0) {
    ft <- bold(ft, i = label_rows, j = 1:2, bold = TRUE)
    ft <- bg(ft, i = label_rows, j = 1:2, bg = "#f2f2f2")
  }
  if (length(level_rows) > 0) {
    ft <- padding(ft, i = level_rows, j = 1, padding.left = 18)
  }
  autofit(ft)
}

# Cohort summary table
a1c_final_distinct <- a1c_final %>%
  distinct(
    id_hashed, ftd, A1C_category, gender, Hb, raceeth, age, age_group, SBP, DBP,
    BP_category, cancer, hypertension, cardiorespiratory, coagulopathy, teratogen,
    anticoagulants, deferral_meds, splenectomy_tcpenia
  )

t0 <- a1c_final_distinct %>%
  tbl_summary(
    include = c(
      gender, raceeth, age, age_group, ftd, A1C_category, Hb, SBP, DBP, BP_category,
      cancer, hypertension, cardiorespiratory, coagulopathy, teratogen, anticoagulants,
      deferral_meds, splenectomy_tcpenia
    ),
    label = list(
      age_group = "age group",
      ftd = "first-time donor",
      raceeth = "race/ethnicity",
      A1C_category = "glycemic status",
      BP_category = "BP range",
      Hb = "hemoglobin%",
      cardiorespiratory = "heart/lung condition",
      teratogen = "on teratogens",
      anticoagulants = "on anticoagulants",
      deferral_meds = "on deferral medications",
      splenectomy_tcpenia = "h/o splenectomy/thrombocytopenia",
      hypertension = "h/o hypertension"
    ),
    missing = "no",
    percent = "column"
  ) %>%
  add_stat_label() %>%
  add_variable_group_header(
    header = "Medical History",
    variables = medical_history_vars
  )

t0_body <- t0$table_body %>%
  transmute(
    Characteristic = label,
    Summary = stat_0,
    row_type = row_type,
    variable = variable
  )

medhist_start <- match("Medical History", t0_body$Characteristic)
if (is.na(medhist_start)) {
  medhist_start <- nrow(t0_body) + 1
}
t0_part1 <- t0_body[seq_len(medhist_start - 1), ]
t0_part2 <- t0_body[medhist_start:nrow(t0_body), ]

ft_t0_part1 <- styled_characteristic_ft(t0_part1)
ft_t0_part2 <- styled_characteristic_ft(t0_part2)

cohort_n <- nrow(a1c_final_distinct)
female_pct <- mean(a1c_final_distinct$gender == "F", na.rm = TRUE) * 100
first_time_pct <- mean(a1c_final_distinct$ftd == 1, na.rm = TRUE) * 100
median_age <- median(a1c_final_distinct$age, na.rm = TRUE)
normal_pct <- mean(a1c_final_distinct$A1C_category == "normal", na.rm = TRUE) * 100
prediabetes_pct <- mean(a1c_final_distinct$A1C_category == "prediabetes", na.rm = TRUE) * 100
diabetes_pct_overall <- mean(a1c_final_distinct$A1C_category == "diabetes", na.rm = TRUE) * 100

# Donor counts by gender/race
df_race <- a1c_final %>%
  filter(!is.na(gender)) %>%
  group_by(age_group, raceeth, gender) %>%
  summarise(counts = n(), .groups = "drop") %>%
  group_by(age_group, gender) %>%
  mutate(percent = round(counts / sum(counts), digits = 3)) %>%
  ungroup()

demo_distribution_tab <- xtabs(
  ~ interaction(age_group, gender) + raceeth,
  data = a1c_final %>% filter(!is.na(gender), !is.na(age_group), !is.na(raceeth))
)
race_gender_age_chisq <- chisq.test(demo_distribution_tab)
race_gender_age_prop <- prop.table(demo_distribution_tab, 1)

donor_counts_plot <- ggplot(df_race, aes(x = reorder(age_group, raceeth), y = counts)) +
  geom_bar(
    aes(fill = raceeth),
    stat = "identity",
    position = position_dodge(0.8),
    width = 0.7
  ) +
  facet_grid(~gender) +
  geom_text(
    aes(label = paste0(counts, " (", 100 * percent, "%)"), group = raceeth),
    position = position_dodge(0.8),
    vjust = 0.5,
    size = 3.2,
    hjust = -0.05
  ) +
  coord_flip() +
  scale_y_continuous(labels = comma, limits = c(0, 135000)) +
  scale_fill_manual(values = c("black", "#645A5A", "darkgray", "lightgray")) +
  labs(
    title = "Donor count (%) by race within each age group stratified by gender",
    subtitle = "March 2026 screening cohort",
    x = "Age group",
    y = "Donor count",
    fill = "Race/ethnicity"
  ) +
  customeTheme +
  theme(
    legend.position = "top",
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

donor_counts_png <- save_plot(donor_counts_plot, "donor_counts_by_gender_race.png", 8.5, 5.2)

# Glycemic status by age and gender
df_a1ccatcount <- a1c_final %>% group_by(age_group, gender, A1C_category) %>% count()
df_count <- a1c_final %>% group_by(age_group, gender) %>% count()

df_diab_p <- df_a1ccatcount %>%
  left_join(df_count, by = c("age_group", "gender")) %>%
  mutate(percentage = round(n.x / n.y, digits = 3)) %>%
  rename(`group count` = n.x, `total donors` = n.y)

df_diab_perc <- df_diab_p %>%
  mutate(percent = paste0(round(100 * percentage, digits = 1), "%")) %>%
  select(age_group, gender, A1C_category, `group count`, `total donors`, percent)

df_diab_plot <- df_diab_p %>% filter(A1C_category != "hypoglycemia")

glycemic_tab <- xtabs(
  ~ interaction(age_group, gender) + A1C_category,
  data = a1c_final %>%
    filter(!is.na(age_group), !is.na(gender), !is.na(A1C_category), A1C_category != "hypoglycemia") %>%
    mutate(A1C_category = droplevels(factor(A1C_category)))
)
glycemic_chisq <- chisq.test(glycemic_tab)
glycemic_prop <- 100 * prop.table(glycemic_tab, 1)

glycemic_plot <- ggplot(df_diab_plot, aes(x = age_group, y = percentage)) +
  geom_bar(
    aes(color = A1C_category, fill = A1C_category),
    stat = "identity",
    position = position_dodge2(0.8),
    width = 0.7
  ) +
  facet_wrap(~gender) +
  geom_text(
    aes(label = paste0(100 * percentage, "%"), group = A1C_category),
    position = position_dodge(0.8),
    vjust = 0,
    hjust = -0.1,
    size = 3.2
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "YlOrRd", direction = 1) +
  scale_color_brewer(palette = "YlOrRd", direction = 1) +
  guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.05)) +
  labs(
    title = "Glycemic status by age group and gender",
    subtitle = "March 2026 screening cohort",
    x = "Age group",
    y = "Percentage of donors"
  ) +
  customeTheme +
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    panel.grid = element_blank()
  )

glycemic_plot_png <- save_plot(glycemic_plot, "glycemic_status_age_gender.png", 8.5, 5.2)
glycemic_table_ft <- styled_ft(df_diab_perc, body_font = 8, header_font = 9)

# Violin plots
df_a1c_age <- a1c_final %>%
  filter(!is.na(a1c), !is.na(age_group)) %>%
  mutate(age_group = factor(as.character(age_group), levels = c("-29", "30-54", "55-")))

age_kw <- kruskal.test(a1c ~ age_group, data = df_a1c_age)

df_a1c_age_plot <- df_a1c_age %>%
  group_by(age_group) %>%
  mutate(
    lower_bound = quantile(a1c, 0.01, na.rm = TRUE),
    upper_bound = quantile(a1c, 0.99, na.rm = TRUE)
  ) %>%
  filter(a1c >= lower_bound, a1c <= upper_bound) %>%
  ungroup() %>%
  select(-lower_bound, -upper_bound)

age_order <- df_a1c_age_plot %>%
  group_by(age_group) %>%
  summarise(n = n(), median_a1c = median(a1c, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    age_group_label = paste0(age_group, " (n=", format(n, big.mark = ","), ")"),
    median_label = sprintf("%.1f", median_a1c)
  )

df_a1c_age_plot <- df_a1c_age_plot %>%
  left_join(age_order, by = "age_group") %>%
  mutate(age_group_label = factor(age_group_label, levels = age_order$age_group_label))

a1c_age_distribution <- ggplot(df_a1c_age_plot, aes(x = age_group_label, y = a1c, fill = age_group_label)) +
  geom_violin(trim = FALSE, alpha = 0.85, color = NA, adjust = 2) +
  geom_boxplot(width = 0.14, outlier.shape = NA, fill = "white", color = "#2f2a24") +
  stat_summary(fun = median, geom = "point", shape = 21, size = 2.2, fill = "#b2182b", color = "white") +
  geom_text(
    data = age_order,
    aes(x = age_group_label, y = median_a1c, label = median_label),
    inherit.aes = FALSE,
    nudge_y = 0.18,
    size = 3.5,
    color = "#2f2a24",
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  scale_fill_brewer(palette = "YlOrRd", guide = "none") +
  labs(title = "Distribution of HbA1c by age group", x = "Age group", y = "HbA1c (%)") +
  customeTheme +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

a1c_age_png <- save_plot(a1c_age_distribution, "a1c_distribution_age.png", 8.5, 5.2)

df_a1c_race <- a1c_final %>%
  filter(!is.na(a1c), !is.na(raceeth), raceeth != "Prefer not to answer")

race_kw <- kruskal.test(a1c ~ raceeth, data = df_a1c_race)

df_a1c_race_plot <- df_a1c_race %>%
  group_by(raceeth) %>%
  mutate(
    lower_bound = quantile(a1c, 0.01, na.rm = TRUE),
    upper_bound = quantile(a1c, 0.99, na.rm = TRUE)
  ) %>%
  filter(a1c >= lower_bound, a1c <= upper_bound) %>%
  ungroup() %>%
  select(-lower_bound, -upper_bound)

race_order <- df_a1c_race_plot %>%
  group_by(raceeth) %>%
  summarise(n = n(), median_a1c = median(a1c, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(median_a1c)) %>%
  mutate(
    race_label = paste0(raceeth, " (n=", format(n, big.mark = ","), ")"),
    median_label = sprintf("%.1f", median_a1c)
  )

df_a1c_race_plot <- df_a1c_race_plot %>%
  left_join(race_order, by = "raceeth") %>%
  mutate(race_label = factor(race_label, levels = race_order$race_label))

a1c_race_distribution <- ggplot(df_a1c_race_plot, aes(x = race_label, y = a1c, fill = race_label)) +
  geom_violin(trim = FALSE, alpha = 0.85, color = NA, adjust = 2) +
  geom_boxplot(width = 0.14, outlier.shape = NA, fill = "white", color = "#2f2a24") +
  stat_summary(fun = median, geom = "point", shape = 21, size = 2.2, fill = "#b2182b", color = "white") +
  geom_text(
    data = race_order,
    aes(x = race_label, y = median_a1c, label = median_label),
    inherit.aes = FALSE,
    nudge_y = 0.18,
    size = 3.5,
    color = "#2f2a24",
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  scale_fill_brewer(palette = "YlOrRd", guide = "none") +
  labs(title = "Distribution of HbA1c by race/ethnicity", x = "Race/ethnicity", y = "HbA1c (%)") +
  customeTheme +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

a1c_race_png <- save_plot(a1c_race_distribution, "a1c_distribution_race.png", 8.5, 5.2)

df_a1c_medhist <- a1c_final %>%
  filter(!is.na(a1c)) %>%
  mutate(
    any_medical_history = ifelse(
      rowSums(across(all_of(medical_history_vars)), na.rm = TRUE) > 0,
      "Any medical history",
      "No medical history"
    ),
    any_medical_history = factor(any_medical_history, levels = c("No medical history", "Any medical history"))
  )

medhist_wilcox <- wilcox.test(a1c ~ any_medical_history, data = df_a1c_medhist, exact = FALSE)

df_a1c_medhist_plot <- df_a1c_medhist %>%
  group_by(any_medical_history) %>%
  mutate(
    lower_bound = quantile(a1c, 0.01, na.rm = TRUE),
    upper_bound = quantile(a1c, 0.99, na.rm = TRUE)
  ) %>%
  filter(a1c >= lower_bound, a1c <= upper_bound) %>%
  ungroup() %>%
  select(-lower_bound, -upper_bound)

medhist_order <- df_a1c_medhist_plot %>%
  group_by(any_medical_history) %>%
  summarise(n = n(), median_a1c = median(a1c, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    medhist_label = paste0(any_medical_history, " (n=", format(n, big.mark = ","), ")"),
    median_label = sprintf("%.1f", median_a1c)
  )

df_a1c_medhist_plot <- df_a1c_medhist_plot %>%
  left_join(medhist_order, by = "any_medical_history") %>%
  mutate(medhist_label = factor(medhist_label, levels = medhist_order$medhist_label))

a1c_medhist_distribution <- ggplot(df_a1c_medhist_plot, aes(x = medhist_label, y = a1c, fill = medhist_label)) +
  geom_violin(trim = FALSE, alpha = 0.85, color = NA, adjust = 2) +
  geom_boxplot(width = 0.14, outlier.shape = NA, fill = "white", color = "#2f2a24") +
  stat_summary(fun = median, geom = "point", shape = 21, size = 2.2, fill = "#b2182b", color = "white") +
  geom_text(
    data = medhist_order,
    aes(x = medhist_label, y = median_a1c, label = median_label),
    inherit.aes = FALSE,
    nudge_y = 0.18,
    size = 3.5,
    color = "#2f2a24",
    fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  scale_fill_brewer(palette = "YlOrRd", guide = "none") +
  labs(title = "Distribution of HbA1c by any medical history", x = NULL, y = "HbA1c (%)") +
  customeTheme +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

a1c_medhist_png <- save_plot(a1c_medhist_distribution, "a1c_distribution_medhist.png", 8.5, 5.2)

# Hotspot map and counts from existing outputs
hotspot_map_png <- file.path(script_dir, "county_diabetes_hotspot_and_raw_map.png")
if (!file.exists(hotspot_map_png)) {
  hotspot_map_png <- file.path(script_dir, "county_diabetes_hotspot_map.png")
}
county_results <- read.csv(file.path(script_dir, "county_diabetes_hotspot_results.csv"))
analysis_counties <- county_results %>% filter(!is.na(hotspot_class))
hotspot_total <- sum(grepl("^Hot spot", analysis_counties$hotspot_class))
coldspot_total <- sum(grepl("^Cold spot", analysis_counties$hotspot_class))

# Build PowerPoint
pptx_path <- file.path(script_dir, "A1c_march_2026_summary.pptx")
doc <- read_pptx()

# Title slide
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- ph_with(
  doc,
  value = external_img(file.path(script_dir, "ARCimage.png"), width = 12.0, height = 2.4),
  location = ph_location(left = 0.6, top = 0.35, width = 12.0, height = 2.4)
)
doc <- ph_with(
  doc,
  value = fpar(ftext("March 2026 A1c Screening Data Analysis", fp_text(font.size = 26, bold = TRUE, color = "#1f1f1f"))),
  location = ph_location(left = 0.7, top = 3.1, width = 12.0, height = 0.5)
)
doc <- ph_with(
  doc,
  value = make_text_block(
    c(
      "Aim: To identify demographic, clinical, and geographic disparities in HbA1c and diabetes burden among U.S. blood donors.",
      "Hypothesis: HbA1c and diabetes burden vary across donor subgroups and counties, with higher levels concentrated in specific demographic and medical-risk groups and in geographic clusters."
    ),
    size = 15
  ),
  location = ph_location(left = 0.9, top = 3.9, width = 11.2, height = 1.2)
)

# Donor characteristics summary
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- add_slide_title(doc, "Donor Characteristics", "Baseline descriptive profile of the March 2026 analytic donor cohort")
doc <- ph_with(doc, ft_t0_part1, location = ph_location(left = 0.4, top = 1.0, width = 7.8, height = 5.5))
doc <- ph_with(
  doc,
  value = make_text_block(
    c(
      paste0("Analytic cohort: ", comma(cohort_n), " unique donors."),
      paste0("Median age was ", round(median_age, 1), " years; ", sprintf("%.1f", female_pct), "% were female and ", sprintf("%.1f", first_time_pct), "% were first-time donors."),
      paste0("Overall glycemic distribution was ", sprintf("%.1f", normal_pct), "% normal, ", sprintf("%.1f", prediabetes_pct), "% prediabetes, and ", sprintf("%.1f", diabetes_pct_overall), "% diabetes."),
      "This slide is descriptive, establishing the cohort context for subsequent subgroup and spatial comparisons rather than testing a specific between-group hypothesis."
    ),
    size = 14
  ),
  location = ph_location(left = 8.45, top = 1.1, width = 4.3, height = 5.5)
)

# Medical history summary
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- add_slide_title(doc, "Medical History Summary", "Selected donor medical history variables carried forward for HbA1c comparisons")
doc <- ph_with(doc, ft_t0_part2, location = ph_location(left = 0.5, top = 1.0, width = 7.7, height = 5.5))
doc <- ph_with(
  doc,
  value = make_text_block(
    c(
      "These variables were combined later into a binary indicator of any medical history versus none.",
      "That grouping was used to test whether HbA1c is systematically shifted upward among donors with a flagged medical history profile.",
      "Separating this section onto its own slide preserves readability while retaining the same table content included in the report."
    ),
    size = 14
  ),
  location = ph_location(left = 8.45, top = 1.25, width = 4.15, height = 4.8)
)

# Donor count composition
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- add_slide_title(doc, "Donor Counts by Gender and Race/Ethnicity")
doc <- ph_with(
  doc,
  value = external_img(donor_counts_png, width = 7.9, height = 5.1),
  location = ph_location(left = 0.35, top = 1.05, width = 7.9, height = 5.1)
)
doc <- ph_with(
  doc,
  value = make_text_block(
    c(
      paste0("Race/ethnicity composition differed significantly across age-by-sex strata (Pearson chi-squared X^2 = ", format(round(unname(race_gender_age_chisq$statistic), 0), big.mark = ","), ", ", fmt_p(race_gender_age_chisq$p.value), ")."),
      paste0("Among men, the proportion of White donors increased from ", sprintf("%.1f", 100 * race_gender_age_prop['-29.M', 'White']), "% in donors younger than 29 years to ", sprintf("%.1f", 100 * race_gender_age_prop['55-.M', 'White']), "% in donors aged 55 years or older."),
      "These denominator differences matter because apparent HbA1c disparities may partly reflect how heavily each demographic stratum is represented in the screened donor pool."
    ),
    size = 13.5
  ),
  location = ph_location(left = 8.45, top = 1.2, width = 4.2, height = 4.9)
)

# Glycemic status figure
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- add_slide_title(doc, "Glycemic Status by Age Group and Gender")
doc <- ph_with(
  doc,
  value = external_img(glycemic_plot_png, width = 7.9, height = 5.1),
  location = ph_location(left = 0.35, top = 1.05, width = 7.9, height = 5.1)
)
doc <- ph_with(
  doc,
  value = make_text_block(
    c(
      paste0("Glycemic status differed significantly across age-by-sex strata (Pearson chi-squared X^2 = ", format(round(unname(glycemic_chisq$statistic), 0), big.mark = ","), ", ", fmt_p(glycemic_chisq$p.value), ")."),
      paste0("Diabetes prevalence increased from ", sprintf("%.1f", glycemic_prop['-29.F', 'diabetes']), "% in women younger than 29 years and ", sprintf("%.1f", glycemic_prop['-29.M', 'diabetes']), "% in men younger than 29 years to ", sprintf("%.1f", glycemic_prop['55-.F', 'diabetes']), "% and ", sprintf("%.1f", glycemic_prop['55-.M', 'diabetes']), "%, respectively, among donors aged 55 years or older."),
      "Older men carried the highest burden of dysglycemia, with both prediabetes and diabetes proportions notably elevated relative to younger donor groups."
    ),
    size = 13.5
  ),
  location = ph_location(left = 8.45, top = 1.15, width = 4.25, height = 5.0)
)

# Glycemic status table
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- add_slide_title(doc, "Exact Counts and Percentages for Glycemic Status")
doc <- ph_with(doc, glycemic_table_ft, location = ph_location(left = 0.4, top = 1.0, width = 8.2, height = 5.2))
doc <- ph_with(
  doc,
  value = make_text_block(
    c(
      paste0("The highest combined prediabetes plus diabetes burden was observed in men aged 55 years or older: ", sprintf("%.1f", glycemic_prop['55-.M', 'prediabetes'] + glycemic_prop['55-.M', 'diabetes']), "%."),
      paste0("For comparison, women younger than 29 years had a combined dysglycemia burden of only ", sprintf("%.1f", glycemic_prop['-29.F', 'prediabetes'] + glycemic_prop['-29.F', 'diabetes']), "%."),
      "The table supports formal reporting by pairing the visual trends with exact subgroup counts and percentages."
    ),
    size = 13.5
  ),
  location = ph_location(left = 8.8, top = 1.2, width = 3.8, height = 4.7)
)

# HbA1c age distribution
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- add_slide_title(doc, "HbA1c Distribution by Age Group")
doc <- ph_with(
  doc,
  value = external_img(a1c_age_png, width = 7.9, height = 5.1),
  location = ph_location(left = 0.35, top = 1.05, width = 7.9, height = 5.1)
)
doc <- ph_with(
  doc,
  value = make_text_block(
    c(
      paste0("HbA1c distributions differed significantly across age groups (Kruskal-Wallis chi-squared = ", format(round(unname(age_kw$statistic), 0), big.mark = ","), ", ", fmt_p(age_kw$p.value), ")."),
      paste0("Median HbA1c increased from ", sprintf("%.1f", age_order$median_a1c[age_order$age_group == '-29']), "% in donors younger than 29 years to ", sprintf("%.1f", age_order$median_a1c[age_order$age_group == '30-54']), "% in donors aged 30-54 years and ", sprintf("%.1f", age_order$median_a1c[age_order$age_group == '55-']), "% in donors aged 55 years or older."),
      "The rightward shift of the full distribution suggests that the age effect is broad-based and not driven only by a small number of highly elevated observations."
    ),
    size = 13.5
  ),
  location = ph_location(left = 8.45, top = 1.2, width = 4.2, height = 4.9)
)

# HbA1c race distribution
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- add_slide_title(doc, "HbA1c Distribution by Race/Ethnicity")
doc <- ph_with(
  doc,
  value = external_img(a1c_race_png, width = 7.9, height = 5.1),
  location = ph_location(left = 0.35, top = 1.05, width = 7.9, height = 5.1)
)
doc <- ph_with(
  doc,
  value = make_text_block(
    c(
      paste0("HbA1c distributions differed significantly across race/ethnicity groups (Kruskal-Wallis chi-squared = ", format(round(unname(race_kw$statistic), 1), big.mark = ","), ", ", fmt_p(race_kw$p.value), ")."),
      paste0("Median HbA1c was highest among Black donors (", sprintf("%.1f", race_order$median_a1c[race_order$raceeth == 'Black']), "%), followed by Asian/other donors (", sprintf("%.1f", race_order$median_a1c[race_order$raceeth == 'Asian/other']), "%), and lower in Hispanic Origin and White donors (both ", sprintf("%.1f", race_order$median_a1c[race_order$raceeth == 'Hispanic Origin']), "%)."),
      "These differences are modest in absolute magnitude but statistically robust because they reflect systematic distributional shifts across a large donor sample."
    ),
    size = 13.2
  ),
  location = ph_location(left = 8.45, top = 1.15, width = 4.2, height = 5.0)
)

# HbA1c medical history
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- add_slide_title(doc, "HbA1c Distribution by Any Medical History")
doc <- ph_with(
  doc,
  value = external_img(a1c_medhist_png, width = 7.9, height = 5.1),
  location = ph_location(left = 0.35, top = 1.05, width = 7.9, height = 5.1)
)
doc <- ph_with(
  doc,
  value = make_text_block(
    c(
      paste0("Donors with any flagged medical history had significantly higher HbA1c values than those without (Wilcoxon rank-sum, ", fmt_p(medhist_wilcox$p.value), ")."),
      paste0("Median HbA1c was ", sprintf("%.1f", medhist_order$median_a1c[medhist_order$any_medical_history == 'Any medical history']), "% among donors with any medical history versus ", sprintf("%.1f", medhist_order$median_a1c[medhist_order$any_medical_history == 'No medical history']), "% among donors without."),
      "The distribution is shifted to the right rather than differing only in the extreme upper tail, supporting a generalized association between medical-history burden and higher glycemic levels."
    ),
    size = 13.2
  ),
  location = ph_location(left = 8.45, top = 1.2, width = 4.2, height = 5.0)
)

# Hotspot slide
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- add_slide_title(doc, "County-Level Hotspot Analysis of Diabetes Prevalence")
doc <- ph_with(
  doc,
  value = external_img(hotspot_map_png, width = 8.0, height = 4.6),
  location = ph_location(left = 0.3, top = 1.1, width = 8.0, height = 4.6)
)
doc <- ph_with(
  doc,
  value = make_text_block(
    c(
      paste0("Among ", comma(nrow(analysis_counties)), " counties with at least 20 donors, ", comma(sum(analysis_counties$hotspot_class != 'Not significant')), " met Gi* significance criteria for a hotspot or coldspot."),
      paste0("This included ", comma(hotspot_total), " hotspot counties and ", comma(coldspot_total), " coldspot counties; ", sum(analysis_counties$hotspot_class == 'Hot spot (99%)'), " hotspots and ", sum(analysis_counties$hotspot_class == 'Cold spot (99%)'), " coldspots met the 99% significance threshold."),
      "Hotspot status reflects local spatial clustering rather than a county's raw percentage alone, which is why isolated high-value counties may not be classified as hotspots.",
      "The interactive county map remains available separately in county_diabetes_hotspot_map_interactive.html."
    ),
    size = 13.2
  ),
  location = ph_location(left = 8.55, top = 1.15, width = 4.05, height = 5.0)
)

# Closing slide
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
doc <- add_slide_title(doc, "Exported Files")
doc <- ph_with(
  doc,
  value = make_text_block(
    c(
      paste0("PowerPoint export: ", basename(pptx_path)),
      paste0("Report source: ", file.path("a1c_march2026", "A1c_march_2026.Rmd")),
      "Interactive map companion: county_diabetes_hotspot_map_interactive.html",
      "All plots used in the PowerPoint were also written to the ppt_assets folder for reuse in manuscripts or posters."
    ),
    size = 16
  ),
  location = ph_location(left = 0.8, top = 1.4, width = 11.5, height = 2.6)
)

print(doc, target = pptx_path)
cat("Saved PowerPoint to:", pptx_path, "\n")
