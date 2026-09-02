#-------------------------------------------------------------------------------------------
# Asymmetric calibration
# Anchors constituency predictions to Bayesian national vote share estimates
# Following Hanretty, Lauderdale and Vivyan (2016) reconciliation approach

# Extract national vote share posteriors from Stan aggregator
aggregator_shares <- summary_table |>
  mutate(party = case_when(
    Party == "Reform%" ~ "Brexit Party/Reform UK",
    Party == "LAB%"    ~ "Labour",
    Party == "CON%"    ~ "Conservative",
    Party == "LIB%"    ~ "Liberal Democrat",
    Party == "Green%"  ~ "Green Party",
    Party == "other"   ~ "Other"
  )) |>
  select(party, aggregator_mean = mean, low_share = lower_95, upper_share = upper_95)

# MRP implied national vote shares
mrp_national <- constituency_vote_shares |>
  group_by(party) |>
  summarise(mrp_mean = mean(vote_share, na.rm = TRUE), .groups = "drop")

# Calibration ratios — anchors demographically driven parties to aggregator
# Liberal Democrat and Other set to ratio = 1 (no calibration) because:
# - LD support reflects place effects beyond demographics — spatial lag
#   captures this geographic signal which calibration would undo
# - Other support reflects candidate specific local effects not captured
#   by national polling — previous vote share is more informative

# Calculate the flat additive difference needed to conform to the aggregator
# Calculate the flat additive difference for ALL parties
calibration <- mrp_national |>
  left_join(aggregator_shares, by = "party") |>
  mutate(
    ratio = aggregator_mean / mrp_mean
  )
    

constituency_vote_shares_calibrated <- constituency_vote_shares |>
  left_join(calibration |> select(party, ratio), by = "party") |>
  mutate(vote_share = vote_share * ratio) |>
  group_by(new_pcon) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()


#-------------------------------------------------------------------------------------------
# Unwinding
# Following YouGov's methodology — corrects MRP tendency to compress
# geographic distributions through partial pooling
#
# Unwinding is applied asymmetrically:
# - When historical_sd > mrp_sd: stretch to match historical norms
# - When historical_sd < mrp_sd: keep MRP predictions — spatial predictors
#   may be capturing genuine current dynamics beyond historical baselines

historical_dist <- bes_elections |>
  filter(Country == "England") |>
  summarise(
    # 2024 only — parties undergoing structural geographic realignment
    sd_lab    = sd(Lab24,   na.rm = TRUE) / 100,
    sd_reform = sd(RUK24,   na.rm = TRUE) / 100,
    sd_con    = sd(Con24,   na.rm = TRUE) / 100,
    sd_ld     = sd(LD24,    na.rm = TRUE) / 100,
    sd_green  = sd(Green24, na.rm = TRUE) / 100,
    sd_other  = sd(Other24, na.rm = TRUE) / 100
  )

party_sd_map <- list(
  "Labour"                 = historical_dist$sd_lab,
  "Conservative"           = historical_dist$sd_con,
  "Liberal Democrat"       = historical_dist$sd_ld,
  "Brexit Party/Reform UK" = historical_dist$sd_reform,
  "Green Party"            = historical_dist$sd_green,
  "Other"                  = historical_dist$sd_other
)

constituency_unwound <- constituency_vote_shares_calibrated |>
  group_by(party) |>
  mutate(
    national_mean = mean(vote_share),
    historical_sd = party_sd_map[[party[1]]],
    current_sd    = sd(vote_share),
    scaling_ratio = historical_sd / current_sd,
    vote_share    = case_when(
      # Symmetric unwinding — parties expected to revert to historical norms relative to the 2024 GE
      # Labour and Lib Dem reverting to 2024 performance, whereas others doing worse / better
      party %in% c("Labour", "Conservative", "Brexit Party/Reform UK") ~
        national_mean + (vote_share - national_mean) * scaling_ratio,
      # Asymmetric unwinding — parties in structural geographic realignment
      # LD, Green and Other have genuine new geographic coalitions
      # only stretch toward historical norm, never compress
      scaling_ratio >= 1 ~
        national_mean + (vote_share - national_mean) * scaling_ratio,
      TRUE ~ vote_share
    ),
    vote_share = pmax(vote_share, 0)
  ) |>
  ungroup() |>
  group_by(new_pcon) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()