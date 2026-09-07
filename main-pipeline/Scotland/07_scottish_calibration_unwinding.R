#-------------------------------------------------------------------------------------------
# Unwinding
# Following YouGov's methodology — corrects MRP tendency to compress
# geographic distributions through partial pooling
#
# Unlike the English model where unwinding is applied asymmetrically,
# symmetric unwinding is applied for Scotland. This is motivated by the
# thin BES sample (mean 38 respondents per constituency) meaning spatial
# predictors are insufficiently reliable to justify trusting MRP predictions
# over historical variance when they conflict. When MRP exceeds historical
# variance in Scotland it is more likely a model artefact than genuine
# geographic signal — unlike England where strong spatial predictors with
# larger samples justify trusting the model over history.
#
# Historical standard deviations are averaged across 2019 and 2024 elections
# to account for unusual performances in either year — notably Scottish
# Labour's 2024 revival and SNP's 2024 collapse relative to their
# longer term geographic distributions.

# Historical standard deviations from 2024 GE results
historical_dist_scot <- bes_elections |>
  filter(Country == "Scotland") |>
  summarise(
    sd_lab    = sd(Lab24,    na.rm = TRUE) / 100,
    sd_snp    = sd(SNP24,    na.rm = TRUE) / 100,
    sd_con    = sd(Con24,    na.rm = TRUE) / 100,
    sd_ld     = sd(LD24,     na.rm = TRUE) / 100,
    sd_green  = sd(Green24,  na.rm = TRUE)  / 100,
    sd_other  = sd(Other24,  na.rm = TRUE) / 100,
    sd_reform = sd(RUK24,    na.rm = TRUE) / 100
  )

party_sd_map_scotland <- list(
  "Labour"                        = historical_dist_scot$sd_lab,
  "Conservative"                  = historical_dist_scot$sd_con,
  "Liberal Democrat"              = historical_dist_scot$sd_ld,
  "Green Party"                   = historical_dist_scot$sd_green,
  "Scottish National Party (SNP)" = historical_dist_scot$sd_snp,
  "Other"                         = historical_dist_scot$sd_other,
  "Brexit Party/Reform UK"        = historical_dist_scot$sd_reform
)


constituency_unwound_scotland <- constituency_vote_shares_scotland |>
  group_by(party) |>
  mutate(
    national_mean = mean(vote_share),
    historical_sd = party_sd_map_scotland[[party[1]]],
    current_sd    = sd(vote_share),
    scaling_ratio = historical_sd / current_sd,
    vote_share    = national_mean + (vote_share - national_mean) * scaling_ratio,
    vote_share    = pmax(vote_share, 0)
  ) |>
  ungroup() |>
  group_by(new_pcon) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()


#-------------------------------------------------------------------------------------------
#Symmetric Calibration
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
  group_by(party)|>
  summarise(share = sum(mean), .groups="drop")|>
  ungroup()|>
  select(party, aggregator_mean = share)

# MRP implied national vote shares
mrp_national_scottish <- constituency_unwound_scotland |>
  group_by(party) |>
  summarise(mrp_mean = mean(vote_share, na.rm = TRUE), .groups = "drop")

target_proportion <- setNames(as.list(aggregator_shares_scottish$aggregator_mean), aggregator_shares_scottish$party)

logit <- function(p){
  return (log(p / (1-p)))
}

logit_shift <- function(party = party) {
  
  target <- target_proportion[[party]]
  
  raw_votes_target <- constituency_unwound_scotland |> 
    filter(party == !!party) |> 
    pull(vote_share)
  
  f_general <- function(vote_share, delta) {
    return (
      exp(logit(vote_share) + delta) / ((exp(logit(vote_share) + delta)) + (1 - vote_share))
    )
  }
  
  f <- function(delta) {
    return (mean(f_general(raw_votes_target, delta)) - target)
  }
  
  solution <- uniroot(f, interval = c(-10, 10))$root
  
  adjusted_vote_shares_scotland <- constituency_unwound_scotland |>
    filter(party == !!party) |>
    mutate(
      vote_share = f_general(vote_share, solution)
    )
  
  return(adjusted_vote_shares_scotland)
}

# Create an empty data frame to collect results from the loop
constituency_vote_shares_calibrated_scotland <- data.frame()

for(parties in parties_of_interest_scotland) {
  calibrated_party_scotland <- logit_shift(party = parties)
  constituency_vote_shares_calibrated_scotland <- bind_rows(constituency_vote_shares_calibrated_scotland, calibrated_party_scotland)
}

# Add any remaining uncalibrated parties back in and normalise across constituencies
uncalibrated_parties_scotland <- constituency_unwound_scotland |> 
  filter(!party %in% parties_of_interest_scotland)

constituency_vote_shares_calibrated_scotland <- bind_rows(constituency_vote_shares_calibrated_scotland, uncalibrated_parties_scotland) |> 
  group_by(new_pcon) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()