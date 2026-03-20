library(tidyverse)
library(haven)
library(fixest)
library(nnet)
library(modelsummary)
library(gt)

# ─── Data ─────────────────────────────────────────────────────────────────────

df       <- read_dta("data/fmt/Dataset_Final.dta")
df_death <- df |> filter(!is.na(death_binary))   # sub-sample with death place

cat(sprintf("Full sample        : %d observations\n", nrow(df)))
cat(sprintf("Death-place sample : %d observations\n", nrow(df_death)))

# ─── Descriptive Plots ────────────────────────────────────────────────────────

# Among legislators born in their district: did they also die there?
plot_local <- df |>
  filter(binary == 1, !is.na(death_binary)) |>
  ggplot(aes(x = factor(death_binary,
                        labels = c("Outside District", "Inside District")))) +
  geom_bar(fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Death Place: Legislators Born in Their District",
       x = NULL, y = "Count")

print(plot_local)

# Among legislators NOT born in their district: did they die in the district?
plot_nonlocal <- df |>
  filter(binary == 0, !is.na(death_binary)) |>
  ggplot(aes(x = factor(death_binary,
                        labels = c("Outside District", "Inside District")))) +
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Death Place: Legislators NOT Born in Their District",
       x = NULL, y = "Count")

print(plot_nonlocal)

# Birth-place distance vs. death-place distance
plot_distance <- df |>
  ggplot(aes(x = distance_miles, y = death_dist_miles)) +
  geom_point(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Birth-Place vs. Death-Place Distance to District",
       x = "Birth-place distance (miles)",
       y = "Death-place distance (miles)")

print(plot_distance)

# Histogram of death-place distance by birth-place distance group
df_close <- filter(df, distance_miles <= 100)
df_far   <- filter(df, distance_miles >  100)

hist(df_close$death_dist_miles,
     main = "Born ≤ 100 Miles from District",
     xlab = "Death-place distance to district (miles)", ylab = "Count")

hist(df_far$death_dist_miles,
     main = "Born > 100 Miles from District",
     xlab = "Death-place distance to district (miles)", ylab = "Count")

# ─── Regression Setup ─────────────────────────────────────────────────────────

controls <- "dem + seniority + majority + power + chair + female + inpres"
fe_spec  <- "| districtID + congress"

fml <- function(outcome, rhs) {
  as.formula(paste0(outcome, " ~ ", rhs, " + ", controls, fe_spec))
}

# ─── Model Lists ──────────────────────────────────────────────────────────────
# Four blocks per outcome variable, two specifications each (binary / log dist):
#   (1) Original         – birth place, full sample
#   (2) Original (Restr.)– birth place, death-place sub-sample
#   (3) Death Place      – death place only, death-place sub-sample
#   (4) Pooled           – birth + death + interaction, death-place sub-sample

fit_models <- function(outcome) {
  list(
    # ── (1) Original ─────────────────────────────────────────────────────────
    "Birth (Binary)"                    = feols(fml(outcome, "binary"),
                                                data    = df,
                                                cluster = ~districtID),
    "Birth (Log. Distance)"             = feols(fml(outcome, "logged_distance"),
                                                data    = df,
                                                cluster = ~districtID),

    # ── (2) Original (Restricted to death-place sub-sample) ──────────────────
    "Birth, Restricted (Binary)"        = feols(fml(outcome, "binary"),
                                                data    = df_death,
                                                cluster = ~districtID),
    "Birth, Restricted (Log. Distance)" = feols(fml(outcome, "logged_distance"),
                                                data    = df_death,
                                                cluster = ~districtID),

    # ── (3) Death Place ───────────────────────────────────────────────────────
    "Death Place (Binary)"              = feols(fml(outcome, "death_binary"),
                                                data    = df_death,
                                                cluster = ~districtID),
    "Death Place (Log. Distance)"       = feols(fml(outcome, "death_logged_dist"),
                                                data    = df_death,
                                                cluster = ~districtID),

    # ── (4) Pooled (birth + death + interaction) ──────────────────────────────
    "Pooled (Binary)"                   = feols(fml(outcome,
                                                    "binary * death_binary"),
                                                data    = df_death,
                                                cluster = ~districtID),
    "Pooled (Log. Distance)"            = feols(fml(outcome,
                                                    "logged_distance * death_logged_dist"),
                                                data    = df_death,
                                                cluster = ~districtID)
  )
}

models_staff  <- fit_models("pct_constituencystaff")
models_unity  <- fit_models("party_unity")
models_cospon <- fit_models("inpart_cospon")

cat("\n── Observations per model ──\n")
cat("pct_constituencystaff:\n"); print(sapply(models_staff,  \(m) nobs(m)))
cat("party_unity:\n");           print(sapply(models_unity,  \(m) nobs(m)))
cat("inpart_cospon:\n");         print(sapply(models_cospon, \(m) nobs(m)))

# ─── Table Helper ─────────────────────────────────────────────────────────────

coef_map <- c(
  "binary"                            = "Birth in District (0/1)",
  "logged_distance"                   = "Log(Birth Distance + 1)",
  "death_binary"                      = "Death in District (0/1)",
  "death_logged_dist"                 = "Log(Death Distance + 1)",
  "binary:death_binary"               = "Birth × Death in District",
  "logged_distance:death_logged_dist" = "Log(Birth) × Log(Death)",
  "dem"                               = "Democrat",
  "seniority"                         = "Seniority",
  "majority"                          = "Majority Party",
  "power"                             = "Power Committee Member",
  "chair"                             = "Committee Chair",
  "female"                            = "Female",
  "inpres"                            = "Presidential Vote Share (Co-partisan)"
)

gof_map <- tribble(
  ~raw,        ~clean,         ~fmt,
  "nobs",      "Observations", 0,
  "r.squared", "R²",           3
)

make_table <- function(model_list, title, dep_var_note = "",
                       cm = coef_map, note_prefix = NULL, ...) {

  col_groups <- list(
    "Original"               = c("Birth (Binary)",
                                 "Birth (Log. Distance)"),
    "Original (Restricted)"  = c("Birth, Restricted (Binary)",
                                 "Birth, Restricted (Log. Distance)"),
    "Death Place"            = c("Death Place (Binary)",
                                 "Death Place (Log. Distance)"),
    "Pooled"                 = c("Pooled (Binary)",
                                 "Pooled (Log. Distance)")
  )

  default_prefix <- paste0(
    "OLS estimates with district-clustered standard errors in parentheses. ",
    "District and Congress fixed effects included in all models. ",
    "Columns (1)–(2): full sample. ",
    "Columns (3)–(8): restricted to legislators with geocoded death place. "
  )
  note <- paste0(
    if (!is.null(note_prefix)) note_prefix else default_prefix,
    if (nchar(dep_var_note) > 0) dep_var_note else "",
    " * p < 0.10, ** p < 0.05."
  )

  tbl <- modelsummary(
    model_list,
    output   = "gt",
    coef_map = cm,
    gof_map  = gof_map,
    stars    = c("*" = 0.1, "**" = 0.05),
    title    = title,
    notes    = note,
    ...
  ) |>
    # Add column spanners for the four blocks
    tab_spanner(label = md("**Original**"),
                columns = col_groups[["Original"]]) |>
    tab_spanner(label = md("**Original (Restricted)**"),
                columns = col_groups[["Original (Restricted)"]]) |>
    tab_spanner(label = md("**Death Place**"),
                columns = col_groups[["Death Place"]]) |>
    tab_spanner(label = md("**Pooled**"),
                columns = col_groups[["Pooled"]]) |>
    # Shorten column labels to (Binary) / (Log. Distance) within each spanner
    cols_label(
      `Birth (Binary)`                    = "(Binary)",
      `Birth (Log. Distance)`             = "(Log. Distance)",
      `Birth, Restricted (Binary)`        = "(Binary)",
      `Birth, Restricted (Log. Distance)` = "(Log. Distance)",
      `Death Place (Binary)`              = "(Binary)",
      `Death Place (Log. Distance)`       = "(Log. Distance)",
      `Pooled (Binary)`                   = "(Binary)",
      `Pooled (Log. Distance)`            = "(Log. Distance)"
    ) |>
    # Style: bold coefficient names and spanner labels
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_column_spanners()
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    # General table options
    tab_options(
      table.font.size          = px(12),
      heading.title.font.size  = px(13),
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold",
      row_group.font.weight    = "bold",
      stub.font.weight         = "bold",
      footnotes.font.size      = px(10),
      table.border.top.style   = "solid",
      table.border.top.color   = "black",
      table.border.bottom.style = "solid",
      table.border.bottom.color = "black",
      column_labels.border.top.style  = "solid",
      column_labels.border.top.color  = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.color = "black"
    )

  tbl
}

# ─── Build Tables ─────────────────────────────────────────────────────────────

tbl_staff <- make_table(
  models_staff,
  title        = "Local Roots and Constituency Staff Allocation",
  dep_var_note = "Dependent variable: percent of staff allocated to constituency service."
)

tbl_unity <- make_table(
  models_unity,
  title        = "Local Roots and Party Unity",
  dep_var_note = "Dependent variable: party unity score."
)

tbl_cospon <- make_table(
  models_cospon,
  title        = "Local Roots and Bipartisan Cosponsorship",
  dep_var_note = "Dependent variable: share of cosponsors from the opposing party."
)

# Print tables (renders in RStudio Viewer / Quarto / R Markdown)
tbl_staff
tbl_unity
tbl_cospon

# ─── Export ───────────────────────────────────────────────────────────────────
gtsave(tbl_staff,  "output/table_staff.html")
gtsave(tbl_unity,  "output/table_unity.html")
gtsave(tbl_cospon, "output/table_cospon.html")
# Style tables are saved further below after models_style is defined.
#
gtsave(tbl_staff,  "output/table_staff.tex")
gtsave(tbl_unity,  "output/table_unity.tex")
gtsave(tbl_cospon, "output/table_cospon.tex")

# ─── Legislative Style (Multinomial Logit) ────────────────────────────────────
# Separate treatment: district/congress FE not available in multinomial logit;
# congress included as a factor variable instead.

df_style <- df |>
  drop_na(style3, dem, seniority, majority, power, chair, female, inpres, congress) |>
  mutate(
    congress      = as.factor(congress),
    style_labeled = factor(style3,
                           levels = c(1, 2, 3),
                           labels = c("Party Focused", "District Focused",
                                      "Policy Focused"))
  )

df_style_death <- df_style |> filter(!is.na(death_binary))

models_style <- list(
  "Birth (Binary)"                    = multinom(
    style_labeled ~ binary + dem + seniority + majority +
      power + chair + female + inpres + congress,
    data = df_style, trace = FALSE),

  "Birth (Log. Distance)"             = multinom(
    style_labeled ~ logged_distance + dem + seniority + majority +
      power + chair + female + inpres + congress,
    data = df_style, trace = FALSE),

  "Birth, Restricted (Binary)"        = multinom(
    style_labeled ~ binary + dem + seniority + majority +
      power + chair + female + inpres + congress,
    data = df_style_death, trace = FALSE),

  "Birth, Restricted (Log. Distance)" = multinom(
    style_labeled ~ logged_distance + dem + seniority + majority +
      power + chair + female + inpres + congress,
    data = df_style_death, trace = FALSE),

  "Death Place (Binary)"              = multinom(
    style_labeled ~ death_binary + dem + seniority + majority +
      power + chair + female + inpres + congress,
    data = df_style_death, trace = FALSE),

  "Death Place (Log. Distance)"       = multinom(
    style_labeled ~ death_logged_dist + dem + seniority + majority +
      power + chair + female + inpres + congress,
    data = df_style_death, trace = FALSE),

  "Pooled (Binary)"                   = multinom(
    style_labeled ~ binary * death_binary + dem + seniority + majority +
      power + chair + female + inpres + congress,
    data = df_style_death, trace = FALSE),

  "Pooled (Log. Distance)"            = multinom(
    style_labeled ~ logged_distance * death_logged_dist + dem + seniority +
      majority + power + chair + female + inpres + congress,
    data = df_style_death, trace = FALSE)
)

# ── Split 8-model list into binary and log-distance sublists ──────────────────
# Each sublist has 4 models (one per specification), named for spanner labels.

models_style_bin <- setNames(
  models_style[c("Birth (Binary)", "Birth, Restricted (Binary)",
                 "Death Place (Binary)", "Pooled (Binary)")],
  c("Original", "Orig. (Restr.)", "Death Place", "Pooled")
)

models_style_log <- setNames(
  models_style[c("Birth (Log. Distance)", "Birth, Restricted (Log. Distance)",
                 "Death Place (Log. Distance)", "Pooled (Log. Distance)")],
  c("Original", "Orig. (Restr.)", "Death Place", "Pooled")
)

# coef_map for style tables: only the substantively relevant predictors
coef_map_style <- c(
  "binary"                            = "Birth in District (0/1)",
  "logged_distance"                   = "Log(Birth Distance + 1)",
  "death_binary"                      = "Death in District (0/1)",
  "death_logged_dist"                 = "Log(Death Distance + 1)",
  "binary:death_binary"               = "Birth \u00d7 Death in District",
  "logged_distance:death_logged_dist" = "Log(Birth) \u00d7 Log(Death)",
  "dem"                               = "Democrat",
  "seniority"                         = "Seniority",
  "majority"                          = "Majority Party",
  "power"                             = "Power Committee Member",
  "chair"                             = "Committee Chair",
  "female"                            = "Female",
  "inpres"                            = "Presidential Vote Share (Co-partisan)"
)

# ── Style table builder ────────────────────────────────────────────────────────
# shape = term ~ model + response creates wide format where model names become
# automatic column spanners and response categories ("District Focused",
# "Policy Focused") become the column labels — matching the paper's Table 1.
# modelsummary adds trailing spaces to response labels for uniqueness; we
# normalise them all to "Dist. Focused" / "Policy Focused" via cols_label().

style_col_labels <- as.list(setNames(
  rep(c("Dist. Focused", "Policy Focused"), 4),
  c("District Focused",   "Policy Focused",
    "District Focused ",  "Policy Focused ",
    "District Focused  ", "Policy Focused  ",
    "District Focused   ","Policy Focused   ")
))

style_note <- paste0(
  "Multinomial logit estimates with standard errors in parentheses. ",
  "Congress included as a factor variable (no district FE). ",
  "Coefficients relative to 'Party Focused' baseline. ",
  "'Original': full sample. ",
  "'Orig. (Restr.)' / 'Death Place' / 'Pooled': death-place sub-sample. ",
  "* p < 0.10, ** p < 0.05."
)

gof_map_style <- tribble(
  ~raw,    ~clean,         ~fmt,
  "nobs",  "Observations", 0,
  "AIC",   "AIC",          1,
  "BIC",   "BIC",          1
)

make_table_style <- function(models_4, title) {
  modelsummary(
    models_4,
    shape    = term ~ model + response,
    coef_map = coef_map_style,
    gof_map  = gof_map_style,
    stars    = c("*" = 0.1, "**" = 0.05),
    title    = title,
    notes    = style_note,
    output   = "gt"
  ) |>
    cols_label(.list = style_col_labels) |>
    cols_width(everything() ~ px(62)) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_column_spanners()
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_options(
      table.font.size           = px(9),
      heading.title.font.size   = px(11),
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold",
      row_group.font.weight     = "bold",
      stub.font.weight          = "bold",
      footnotes.font.size       = px(10),
      table.border.top.style    = "solid",
      table.border.top.color    = "black",
      table.border.bottom.style = "solid",
      table.border.bottom.color = "black",
      column_labels.border.top.style  = "solid",
      column_labels.border.top.color  = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.color = "black"
    )
}

tbl_style_bin <- make_table_style(
  models_style_bin,
  title = "Local Roots and Legislative Style: Born in District (0/1)"
)

tbl_style_log <- make_table_style(
  models_style_log,
  title = "Local Roots and Legislative Style: Log(Miles Born from District + 1)"
)

tbl_style_bin
tbl_style_log
gtsave(tbl_style_bin, "output/table_style_binary.html")
gtsave(tbl_style_log, "output/table_style_log.html")
gtsave(tbl_style_bin, "output/table_style_binary.tex")
gtsave(tbl_style_log, "output/table_style_log.tex")

# Post-process .tex files: replace Unicode × (U+00D7) with LaTeX $\times$
# gt escapes $ and \ so we must fix the .tex files after saving.
fix_tex_times <- function(path) {
  txt <- readLines(path, warn = FALSE)
  txt <- gsub("<U+00D7>", "$\\times$", txt, fixed = TRUE)
  writeLines(txt, path)
}
fix_tex_times("output/table_style_binary.tex")
fix_tex_times("output/table_style_log.tex")
