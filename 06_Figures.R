library(tidyverse)
library(haven)
library(fixest)
library(broom)
library(cowplot)
library(scales)

# Load Data ---------------------------------------------------------------

# Full historic data for birth-place replication (congresses 1-116)
df_overtime <- read_dta("Replication_from_dataverse/Dataset_OvertimeComparisons.dta")

# Analysis dataset with death-place variables (congresses 93-116)
df          <- read_dta("data/fmt/Dataset_Final.dta")
df_death    <- df |> filter(!is.na(death_binary))

# Setup -------------------------------------------------------------------

# Ordinal suffix helper (1st, 2nd, ...)
just_nums <- function(n) {
  suff <- case_when(
    n %in% c(11, 12, 13) ~ "th",
    n %% 10 == 1          ~ "st",
    n %% 10 == 2          ~ "nd",
    n %% 10 == 3          ~ "rd",
    TRUE                  ~ "th"
  )
  paste0(n, suff)
}

# Regression helpers
controls <- "dem + seniority + majority + power + chair + female + inpres"
fe_spec  <- "| districtID + congress"

fml <- function(outcome, rhs) {
  as.formula(paste0(outcome, " ~ ", rhs, " + ", controls, fe_spec))
}

# Figure 1: Coefficient Plot ----------------------------------------------
# For each outcome, extract the key local-roots coefficient from three specs:
#   (1) Birth (Original)   - binary / logged_distance on full sample
#   (2) Birth (Restricted) - binary / logged_distance on death-place sub-sample
#   (3) Death Place        - death_binary / death_logged_dist on death-place sub-sample
# Clustered SEs at district level throughout (matches main analysis).

OUTCOMES <- c(
  "pct_constituencystaff" = "Constituency\nStaff (%)",
  "party_unity"           = "Party\nUnity",
  "inpart_cospon"         = "Intra-party\nCosponsorship"
)

# Colour/shape palette for the three specs
SPEC_PAL <- c(
  "Birth (Original)"   = "black",
  "Birth (Restricted)" = "grey50",
  "Death Place"        = "#d73027"
)

SPEC_SHAPE <- c(
  "Birth (Original)"   = 16L,
  "Birth (Restricted)" = 17L,
  "Death Place"        = 15L
)

# Helper: run one model and return tidy row with spec/outcome/facet labels
extract_coef <- function(outcome, rhs, key_term, data, spec_label, facet_label) {
  m <- feols(fml(outcome, rhs), data = data, cluster = ~districtID)
  tidy(m, conf.int = TRUE) |>
    filter(term == key_term) |>
    transmute(
      outcome    = OUTCOMES[[outcome]],
      spec       = spec_label,
      facet      = facet_label,
      estimate,
      conf.low,
      conf.high
    )
}

# Build coefficient data for both binary and log-distance facets
coef_rows <- map(names(OUTCOMES), \(oc) {
  bind_rows(
    # Binary facet
    extract_coef(oc, "binary",           "binary",           df,       "Birth (Original)",   "Binary (0/1)"),
    extract_coef(oc, "binary",           "binary",           df_death, "Birth (Restricted)", "Binary (0/1)"),
    extract_coef(oc, "death_binary",     "death_binary",     df_death, "Death Place",        "Binary (0/1)"),
    # Log-distance facet
    extract_coef(oc, "logged_distance",  "logged_distance",  df,       "Birth (Original)",   "Log Distance"),
    extract_coef(oc, "logged_distance",  "logged_distance",  df_death, "Birth (Restricted)", "Log Distance"),
    extract_coef(oc, "death_logged_dist","death_logged_dist",df_death, "Death Place",        "Log Distance")
  )
}) |>
  list_rbind() |>
  mutate(
    spec    = factor(spec,    levels = names(SPEC_PAL)),
    outcome = factor(outcome, levels = rev(unname(OUTCOMES))),
    facet   = factor(facet,   levels = c("Binary (0/1)", "Log Distance"))
  )

fig_coef <- ggplot(
  coef_rows,
  aes(
    x      = estimate,
    xmin   = conf.low,
    xmax   = conf.high,
    y      = outcome,
    colour = spec,
    shape  = spec
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_pointrange(
    position = position_dodge(width = 0.55),
    linewidth = 0.55,
    size      = 0.45
  ) +
  scale_colour_manual(values = SPEC_PAL, name = NULL) +
  scale_shape_manual(values  = SPEC_SHAPE, name = NULL) +
  facet_wrap(~facet, scales = "free_x") +
  labs(
    title = "Local Roots and Legislative Behaviour: Coefficient Estimates (95% CI)",
    x     = "Coefficient Estimate",
    y     = NULL
  ) +
  theme_classic() +
  theme(
    legend.position    = "bottom",
    strip.text         = element_text(hjust = 0, face = "bold"),
    strip.background   = element_blank(),
    panel.spacing      = unit(1.2, "lines")
  )

ggsave("output/fig_coef_plot.pdf", fig_coef, width = 10, height = 5, dpi = 600)
ggsave("output/fig_coef_plot.png", fig_coef, width = 10, height = 5, dpi = 300)
print(fig_coef)

# Figure 2: Scatter Birth vs. Death Distance by Party --------------------
# One observation per legislator (last congress served in death-place sub-sample).
# Colour distinguishes party affiliation.

PAL_PARTY <- c("Democrat" = "#2166ac", "Republican" = "#d73027")

scatter_df <- df_death |>
  mutate(party = if_else(dem == 1, "Democrat", "Republican")) |>
  slice_max(congress, n = 1, by = bioguide_id, with_ties = FALSE)

fig_scatter <- ggplot(
  scatter_df,
  aes(
    x      = log(distance_miles + 1),
    y      = log(death_dist_miles + 1),
    colour = party
  )
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.5) +
  geom_point(alpha = 0.35, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_colour_manual(values = PAL_PARTY, name = NULL) +
  labs(
    title    = "Birth-Place vs. Death-Place Distance to District",
    subtitle = "One observation per legislator (last congress served)",
    x        = "Log(Birth-Place Distance + 1)",
    y        = "Log(Death-Place Distance + 1)"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave("output/fig_scatter_birth_death.pdf", fig_scatter, width = 7, height = 6, dpi = 600)
ggsave("output/fig_scatter_birth_death.png", fig_scatter, width = 7, height = 6, dpi = 300)
print(fig_scatter)

# Figure 3: Birth vs. Death Distance Over Time (Comparison) ---------------

collapsed_birth_93 <- df_overtime |>
  filter(congress >= 93) |>
  summarise(
    avg_born    = mean(distance_miles, na.rm = TRUE),
    median_born = median(distance_miles, na.rm = TRUE),
    .by         = congress
  )

collapsed_death <- df |>
  filter(!is.na(death_dist_miles)) |>
  summarise(
    avg_death    = mean(death_dist_miles, na.rm = TRUE),
    median_death = median(death_dist_miles, na.rm = TRUE),
    n_obs        = n(),
    pct_in_dist  = mean(death_binary, na.rm = TRUE) * 100,
    .by          = congress
  ) |>
  mutate(
    year  = 2 * congress + 1787,
    label = paste0(just_nums(congress), "\nCongress,\n", year)
  )

combined <- collapsed_birth_93 |>
  left_join(
    collapsed_death |> select(congress, avg_death, median_death),
    by = "congress"
  ) |>
  pivot_longer(
    cols      = c(avg_born, avg_death),
    names_to  = "type",
    values_to = "avg_miles"
  ) |>
  mutate(
    type  = recode(type, "avg_born" = "Birth Place", "avg_death" = "Death Place"),
    year  = 2 * congress + 1787,
    label = paste0(just_nums(congress), " Congress, ", year)
  )

fig_comparison <- ggplot(
  combined,
  aes(x = congress, y = avg_miles, colour = type, linetype = type)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(
    breaks = seq(93, 116, by = 4),
    labels = \(b) paste0(just_nums(b), "\nCongress,\n", 2 * b + 1787)
  ) +
  scale_colour_manual(values = c("Birth Place" = "black", "Death Place" = "grey50")) +
  scale_linetype_manual(values = c("Birth Place" = "solid", "Death Place" = "dashed")) +
  labs(
    title  = "Average Distance from District: Birth Place vs. Death Place",
    y      = "Average Distance from District (Miles)",
    x      = NULL,
    colour = NULL, linetype = NULL
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave("output/fig_birth_death_comparison.pdf", fig_comparison, width = 10, height = 5, dpi = 600)
ggsave("output/fig_birth_death_comparison.png", fig_comparison, width = 10, height = 5, dpi = 300)
print(fig_comparison)
