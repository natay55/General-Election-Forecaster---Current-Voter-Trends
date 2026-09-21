#-------------------------------------------------------------------------------------------
# Asymmetric calibration (Scotland)
# Anchors constituency predictions to Bayesian national vote share estimates
# Following Hanretty, Lauderdale and Vivyan (2016) reconciliation approach

# Extract national vote share posteriors from Stan aggregator for Scotland
aggregator_shares_scottish <- summary_table_scottish |>
  mutate(party = case_when(
    Party == "Reform%" ~ "Brexit Party/Reform UK",
    Party == "LAB%"    ~ "Labour",
    Party == "CON%"    ~ "Conservative",
    Party == "LIB%"    ~ "Liberal Democrat",
    Party == "Green%"  ~ "Green Party",
    Party == "SNP%"    ~ "Scottish National Party (SNP)",
    Party == "other"   ~ "Other"
  )) |>
  group_by(party) |>
  summarise(aggregator_mean = sum(mean), .groups = "drop")

# MRP implied national vote shares
mrp_national_scottish <- constituency_vote_shares_scotland |>
  group_by(party) |>
  summarise(mrp_mean = mean(vote_share, na.rm = TRUE), .groups = "drop")

# Difference between MRP mean and aggregator mean
diff_table_scottish <- mrp_national_scottish |>
  left_join(aggregator_shares_scottish |> select(party, aggregator_mean), by = "party") |>
  mutate(diff = abs(mrp_mean - aggregator_mean))

target_proportion_scottish <- setNames(as.list(aggregator_shares_scottish$aggregator_mean), aggregator_shares_scottish$party)

logit <- function(p) {
  return(log(p / (1 - p)))
}

logit_shift_scottish <- function(party) {
  target <- target_proportion_scottish[[party]]
  
  # Calculate absolute difference between MRP mean and aggregator mean
  mrp_val <- mrp_national_scottish |> filter(party == !!party) |> pull(mrp_mean)
  diff <- abs(mrp_val - target)
  
  raw_votes_target <- constituency_vote_shares_scotland |> 
    filter(party == !!party) |> 
    pull(vote_share)
  
  f_general <- function(vote_share, delta) {
    return(
      exp(logit(vote_share) + delta) / ((exp(logit(vote_share) + delta)) + (1 - vote_share))
    )
  }
  
  f <- function(delta) {
    return(mean(f_general(raw_votes_target, delta)) - target)
  }
  
  solution <- uniroot(f, interval = c(-10, 10))$root
  
  adjusted_vote_shares <- constituency_vote_shares_scotland |>
    filter(party == !!party) |>
    mutate(
      vote_share = f_general(vote_share, solution)
    )
  
  return(adjusted_vote_shares)
}

# Collect results from the logit shift calibration loop
constituency_vote_shares_calibrated_scotland <- data.frame()

for (parties in parties_of_interest_scotland) {
  calibrated_party <- logit_shift_scottish(party = parties)
  constituency_vote_shares_calibrated_scotland <- bind_rows(
    constituency_vote_shares_calibrated_scotland, 
    calibrated_party
  )
}

# Add any remaining uncalibrated parties back in and normalise across constituencies
uncalibrated_parties_scotland <- constituency_vote_shares_scotland |> 
  filter(!party %in% parties_of_interest_scotland)

constituency_vote_shares_calibrated_scotland <- bind_rows(
  constituency_vote_shares_calibrated_scotland, 
  uncalibrated_parties_scotland
) |> 
  group_by(new_pcon) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()

#-------------------------------------------------------------------------------------------
# Asymmetric Unwinding (Scotland)
# Following YouGov's methodology — corrects MRP tendency to compress
# geographic distributions through partial pooling
#
# Unwinding is applied asymmetrically:
# - When historical_sd > mrp_sd: stretch to match historical norms
# - When historical_sd < mrp_sd: keep MRP predictions — spatial predictors
#   may be capturing genuine current dynamics beyond historical baselines

historical_dist_scot <- bes_elections |>
  filter(Country == "Scotland") |>
  summarise(
    sd_lab    = sd(Lab24,   na.rm = TRUE) / 100,
    sd_snp    = sd(SNP24,   na.rm = TRUE) / 100,
    sd_con    = sd(Con24,   na.rm = TRUE) / 100,
    sd_ld     = sd(LD24,    na.rm = TRUE) / 100,
    sd_green  = sd(Green24, na.rm = TRUE) / 100,
    sd_other  = sd(Other24, na.rm = TRUE) / 100,
    sd_reform = sd(RUK24,   na.rm = TRUE) / 100
  )

party_sd_map_scotland <- list(
  "Labour"                         = historical_dist_scot$sd_lab,
  "Conservative"                   = historical_dist_scot$sd_con,
  "Liberal Democrat"               = historical_dist_scot$sd_ld,
  "Green Party"                    = historical_dist_scot$sd_green,
  "Scottish National Party (SNP)"  = historical_dist_scot$sd_snp,
  "Other"                          = historical_dist_scot$sd_other,
  "Brexit Party/Reform UK"         = historical_dist_scot$sd_reform
)

constituency_unwound_scotland <- constituency_vote_shares_calibrated_scotland |>
  left_join(diff_table_scottish |> select(party, diff), by = "party") |>
  group_by(party) |>
  mutate(
    national_mean = mean(vote_share),
    historical_sd = party_sd_map_scotland[[cur_group()$party]],
    current_sd    = sd(vote_share),
    scaling_ratio = historical_sd / current_sd,
    vote_share    = national_mean + (vote_share - national_mean) * scaling_ratio,
    vote_share    = pmax(vote_share, 0)
  ) |>
  ungroup() |>
  group_by(new_pcon) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()