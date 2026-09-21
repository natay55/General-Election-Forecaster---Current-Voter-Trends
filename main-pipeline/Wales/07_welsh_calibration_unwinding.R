#-------------------------------------------------------------------------------------------
# Asymmetric calibration (Wales)
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
  select(party, aggregator_mean = mean, low_share = lower_95, upper_share = upper_95)

# MRP implied national vote shares
mrp_national_wales <- constituency_vote_shares_wales |>
  group_by(party) |>
  summarise(mrp_mean = mean(vote_share, na.rm = TRUE), .groups = "drop")

# Difference between MRP mean and aggregator mean
diff_table_wales <- mrp_national_wales |>
  left_join(aggregator_shares_wales |> select(party, aggregator_mean), by = "party") |>
  mutate(diff = abs(mrp_mean - aggregator_mean))

target_proportion_wales <- setNames(as.list(aggregator_shares_wales$aggregator_mean), aggregator_shares_wales$party)

logit <- function(p) {
  return(log(p / (1 - p)))
}

logit_shift_wales <- function(party) {
  target <- target_proportion_wales[[party]]
  
  # Calculate absolute difference between MRP mean and aggregator mean
  mrp_val <- mrp_national_wales |> filter(party == !!party) |> pull(mrp_mean)
  diff <- abs(mrp_val - target)
  
  raw_votes_target <- constituency_vote_shares_wales |> 
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
  
  adjusted_vote_shares_wales <- constituency_vote_shares_wales |>
    filter(party == !!party) |>
    mutate(
      vote_share = f_general(vote_share, solution)
    )
  
  return(adjusted_vote_shares_wales)
}

# Create an empty data frame to collect results from the loop
constituency_vote_shares_calibrated_wales <- data.frame()

for (parties in parties_of_interest_wales) { # FIX: was parties_of_interest
  calibrated_party_wales <- logit_shift_wales(party = parties)
  constituency_vote_shares_calibrated_wales <- bind_rows(
    constituency_vote_shares_calibrated_wales, 
    calibrated_party_wales
  )
}

# Add any remaining uncalibrated parties back in and normalise across constituencies
uncalibrated_parties_wales <- constituency_vote_shares_wales |> 
  filter(!party %in% parties_of_interest_wales)

constituency_vote_shares_calibrated_wales <- bind_rows(
  constituency_vote_shares_calibrated_wales, 
  uncalibrated_parties_wales
) |> 
  group_by(new_pcon) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()

#-------------------------------------------------------------------------------------------
# Asymmetric Unwinding (Wales)
# Following YouGov's methodology — corrects MRP tendency to compress
# geographic distributions through partial pooling

historical_dist_wales <- bes_elections |>
  filter(Country == "Wales") |>
  summarise(
    sd_lab    = sd(Lab24,   na.rm = TRUE) / 100,
    sd_reform = sd(RUK24,   na.rm = TRUE) / 100,
    sd_con    = sd(Con24,   na.rm = TRUE) / 100,
    sd_ld     = sd(LD24,    na.rm = TRUE) / 100,
    sd_green  = sd(Green24, na.rm = TRUE) / 100,
    sd_pc     = sd(PC24,    na.rm = TRUE) / 100,
    sd_other  = sd(Other24, na.rm = TRUE) / 100
  )

party_sd_map_wales <- list( # FIX: was party_sd_map
  "Labour"                 = historical_dist_wales$sd_lab,
  "Conservative"           = historical_dist_wales$sd_con,
  "Liberal Democrat"       = historical_dist_wales$sd_ld,
  "Brexit Party/Reform UK" = historical_dist_wales$sd_reform,
  "Green Party"            = historical_dist_wales$sd_green,
  "Plaid Cymru"            = historical_dist_wales$sd_pc,
  "Other"                  = historical_dist_wales$sd_other
)

constituency_unwound_wales <- constituency_vote_shares_calibrated_wales |>
  left_join(diff_table_wales |> select(party, diff), by = "party") |>
  group_by(party) |>
  mutate(
    national_mean = mean(vote_share),
    historical_sd = party_sd_map_wales[[cur_group()$party]],
    current_sd    = sd(vote_share),
    scaling_ratio = historical_sd / current_sd,
    vote_share    = national_mean + (vote_share - national_mean) * scaling_ratio,
    vote_share    = pmax(vote_share, 0)
  ) |>
  ungroup() |>
  group_by(new_pcon) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()