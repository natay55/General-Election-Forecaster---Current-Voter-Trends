#-------------------------------------------------------------------------------------------
# Asymmetric calibration
# Anchors constituency predictions to Bayesian national vote share estimates
# Following Hanretty, Lauderdale and Vivyan (2016) reconciliation approach

# Extract national vote share posteriors from Stan aggregator
aggregator_shares_wales <- summary_table_wales |>
  mutate(party = case_when(
    Party == "Reform%" ~ "Brexit Party/Reform UK",
    Party == "LAB%"    ~ "Labour",
    Party == "CON%"    ~ "Conservative",
    Party == "LIB%"    ~ "Liberal Democrat",
    Party == "Green%"  ~ "Green Party",
    Party == "PC%"     ~ "Plaid Cymru",
    Party == "other"   ~ "Other"
  )) |>
  select(party, aggregator_mean = mean)

# MRP implied national vote shares
mrp_national_wales <- constituency_vote_shares_wales |>
  group_by(party) |>
  summarise(mrp_mean = mean(vote_share, na.rm = TRUE), .groups = "drop")

calibration_wales <- mrp_national_wales |>
  left_join(aggregator_shares_wales, by = "party") |>
  mutate(
    ratio = aggregator_mean / mrp_mean,
    ratio = if_else(abs(aggregator_mean - mrp_mean) < 0.03, ratio, 1)
  )

constituency_vote_shares_calibrated_wales <- constituency_vote_shares_wales |>
  left_join(calibration_wales |> select(party, ratio), by = "party") |>
  mutate(vote_share = vote_share * ratio) |>
  group_by(new_pcon) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()
#-------------------------------------------------------------------------------------------
# Unwinding
# Following YouGov's methodology — corrects MRP tendency to compress
# geographic distributions through partial pooling
#
# Unwinding is applied symmetrically:
# - When historical_sd > mrp_sd: stretch to match historical norms
# - When historical_sd < mrp_sd: keep MRP predictions — spatial predictors
#   may be capturing genuine current dynamics beyond historical baselines

# Historical standard deviations from 2024 GE results
historical_dist_wales <- bes_elections |>
  filter(Country == "Wales") |>
  summarise(
    # 2024 only — parties undergoing structural geographic realignment
    sd_lab    =  sd(Lab24,   na.rm = TRUE) / 100,
    sd_reform =  sd(RUK24,   na.rm = TRUE) / 100,
    sd_con    =  sd(Con24,   na.rm = TRUE) / 100,
    sd_ld     =  sd(LD24,    na.rm = TRUE) / 100,
    sd_green  =  sd(Green24, na.rm = TRUE) / 100,
    sd_pc     =  sd(PC24,    na.rm = TRUE) / 100,
    sd_other  =  sd(Other24, na.rm = TRUE) / 100
  )

party_sd_map_wales <- list(
  "Labour"                 = historical_dist_wales$sd_lab,
  "Conservative"           = historical_dist_wales$sd_con,
  "Liberal Democrat"       = historical_dist_wales$sd_ld,
  "Brexit Party/Reform UK" = historical_dist_wales$sd_reform,
  "Green Party"            = historical_dist_wales$sd_green,
  "Plaid Cymru"            = historical_dist_wales$sd_pc,
  "Other"                  = historical_dist_wales$sd_other
)

constituency_unwound_wales <- constituency_vote_shares_calibrated_wales |>
  group_by(party) |>
  mutate(
    national_mean = mean(vote_share),
    historical_sd = party_sd_map_wales[[party[1]]],
    current_sd    = sd(vote_share),
    scaling_ratio = historical_sd / current_sd,
    vote_share    = national_mean + (vote_share - national_mean) * scaling_ratio,
    vote_share    = pmax(vote_share, 0)
  ) |>
  ungroup() |>
  group_by(new_pcon) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()
