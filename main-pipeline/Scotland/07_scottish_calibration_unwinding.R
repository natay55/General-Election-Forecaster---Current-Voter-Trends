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
mrp_national_scottish <- constituency_vote_shares_scotland |>
  group_by(party) |>
  summarise(mrp_mean = mean(vote_share, na.rm = TRUE), .groups = "drop")

#-------------------------------------------------------------------------------------------
# Scottish calibration note
#
# All party calibration ratios are set to aggregator_mean / mrp_mean for Scotland.
# Unlike the English model — where parties with spatial lag predictors and sufficient
# BES data are trusted over the aggregator (ratio = 1) — the Scottish BES sample
# is too thin (mean 38 respondents per constituency) to reliably estimate geographic
# variation beyond national swing. This is evidenced by MRP predictions converging
# with proportional swing for Scotland.
#
# Spatial lag predictors are included for methodological consistency but the
# calibration anchors all predictions to the Scottish polling aggregator.

calibration_scotland <- mrp_national_scottish |>
  left_join(aggregator_shares_scottish, by = "party") |>
  mutate(
    ratio = aggregator_mean / mrp_mean,
    # Where ratio deviates substantially from 1, trust neither model nor aggregator
    ratio = if_else(ratio < 0.8 | ratio > 1.2, 1, ratio)
  )

constituency_vote_shares_calibrated_scotland <- constituency_vote_shares_scotland |>
  left_join(calibration_scotland |> select(party, ratio), by = "party") |>
  mutate(vote_share = vote_share * ratio) |>
  group_by(new_pcon) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()
constituency_vote_shares_calibrated_scotland
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
    sd_lab    = mean(c(sd(Lab24,   na.rm = TRUE), sd(Lab19,    na.rm = TRUE))) / 100,
    sd_snp    = mean(c(sd(SNP24,   na.rm = TRUE), sd(SNP19,    na.rm = TRUE))) / 100,
    sd_con    = mean(c(sd(Con24,   na.rm = TRUE), sd(Con19,    na.rm = TRUE))) / 100,
    sd_ld     = mean(c(sd(LD24,    na.rm = TRUE), sd(LD19,     na.rm = TRUE))) / 100,
    sd_green  = mean(c(sd(Green24, na.rm = TRUE), sd(Green19,  na.rm = TRUE))) / 100,
    sd_other  = mean(c(sd(Other24, na.rm = TRUE), sd(Other19,  na.rm = TRUE))) / 100,
    sd_reform = mean(c(sd(RUK24,   na.rm = TRUE), sd(Brexit19, na.rm = TRUE))) / 100
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


constituency_unwound_scotland <- constituency_vote_shares_calibrated_scotland |>
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
