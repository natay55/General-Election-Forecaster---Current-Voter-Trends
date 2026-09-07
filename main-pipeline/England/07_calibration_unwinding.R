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

constituency_unwound <- constituency_vote_shares |>
  group_by(party) |>
  mutate(
    national_mean = mean(vote_share),
    historical_sd = party_sd_map[[party[1]]],
    current_sd    = sd(vote_share),
    scaling_ratio = historical_sd / current_sd,
    vote_share    = if_else(
      scaling_ratio >= 1,
      national_mean + (vote_share - national_mean) * scaling_ratio,
      vote_share
    ),
    vote_share    = pmax(vote_share, 0)
  ) |>
  ungroup() |>
  group_by(new_pcon) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()


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

# Implementation of a logit shift. N is number of constituencies, p is the number of parties of interest 
target_proportion <- setNames(as.list(aggregator_shares$aggregator_mean), aggregator_shares$party)

logit <- function(p){
  return (log(p / (1-p)))
}

logit_shift <- function(party = party) {
  target <- target_proportion[[party]]
  
  raw_votes_target <- constituency_unwound |> 
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
  
  adjusted_vote_shares <- constituency_unwound |>
    filter(party == !!party) |>
    mutate(
      vote_share = f_general(vote_share, solution)
    )
  
  return(adjusted_vote_shares)
}

# Create an empty data frame to collect results from the loop
constituency_vote_shares_calibrated <- data.frame()

for(parties in parties_of_interest) {
  calibrated_party <- logit_shift(party = parties)
  constituency_vote_shares_calibrated <- bind_rows(constituency_vote_shares_calibrated, calibrated_party)
}

# Add any remaining uncalibrated parties back in and normalise across constituencies
uncalibrated_parties <- constituency_unwound |> 
  filter(!party %in% parties_of_interest)

constituency_vote_shares_calibrated <- bind_rows(constituency_vote_shares_calibrated, uncalibrated_parties) |> 
  group_by(new_pcon) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()