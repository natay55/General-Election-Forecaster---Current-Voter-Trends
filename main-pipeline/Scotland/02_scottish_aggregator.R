#-------------------------------------------------------------------------------------------
# Polling aggregator following Hanretty, Lauderdale and Vivyan (2015)
# Fetches current Scottish polling data and fits a Bayesian state space model
# to estimate national voting intention with uncertainty

#-------------------------------------------------------------------------------------------
# Fetch and clean polling data from PollCheck

fetch_with_retry_scot <- function(url, max_attempts = 20, wait_seconds = 3) {
  for (attempt in 1:max_attempts) {
    result <- tryCatch(
      fromJSON(url),
      error = function(e) {
        cat("Attempt", attempt, "failed:", conditionMessage(e), "\n")
        Sys.sleep(wait_seconds)
        NULL
      }
    )
    if (!is.null(result)) {
      cat("Successfully fetched on attempt", attempt, "\n")
      return(result)
    }
  }
  stop("Failed to fetch after ", max_attempts, " attempts")
}

raw_scot <- fetch_with_retry_scot("https://britain.votes.now/data/polls/sct_wmc.json")

scot_polls <- raw_scot |>
  as_tibble() |>
  mutate(date = as.Date(date)) |>
  filter(date >= as.Date("2024-07-04")) |>
  filter(!map_lgl(polls, ~ length(.x) == 0)) |>
  mutate(poll_data = map(polls, ~ as_tibble(.x))) |>
  unnest(poll_data) |>
  unnest_wider(values) |> 
  select(
    polling_dates = date,
    pollster,
    `LAB%`    = Lab,
    `CON%`    = C,
    `Reform%` = R,
    `LIB%`    = Lib,
    `Green%`  = G,
    `SNP%`    = SNP,
    other     = Oth
  ) |>
  mutate(
    Pollster    = as.integer(factor(pollster)),
    sample_size = 2000L #Sample size missing for all polls, so assume 2000 were surveyed
  ) |>
  filter(!is.na(polling_dates))

#-------------------------------------------------------------------------------
#Prep data for Bayesian Aggregation

clean_data_scot <- scot_polls |>
  mutate(
    time_id  = as.integer(polling_dates - min(polling_dates)) + 1,
    Pollster = as.integer(factor(pollster))
  ) |>
  filter(!is.na(polling_dates), !is.na(sample_size), !is.na(Pollster), sample_size > 0)

party_cols_scottish <- clean_data_scot |>
  select(ends_with("%"), other) |>
  names()

y_scot <- clean_data_scot |>
  select(all_of(party_cols_scottish), sample_size) |>
  mutate(across(everything(), ~replace_na(round(. * sample_size), 0L))) |>
  select(-sample_size) |>
  as.matrix()

#-------------------------------------------------------------------------------------------
#Prepare Stan data

stan_data_scottish <- list(
  T         = as.integer(max(clean_data_scot$time_id)),
  J         = ncol(y_scot),
  P         = max(clean_data_scot$Pollster),
  N         = nrow(clean_data_scot),
  poll_time = clean_data_scot$time_id,
  pollster  = clean_data_scot$Pollster,
  n         = clean_data_scot$sample_size,
  y         = y_scot
)

#--------------------------------------------------------------------------------------------
#Fit Scottish Stan model

if (file.exists(here("data","Models","Scotland","model_fit_scottish.rds"))) {
  model_fit_scottish <- readRDS(here("data","Models","Scotland","model_fit_scottish.rds"))
  stan_data_scottish <- readRDS(here("data","Models","Scotland","stan_data_scottish.rds")) 
  party_cols_scottish <- readRDS(here("data","Models","Scotland","party_cols_scottish.rds"))
} else {
  model_fit_scottish <- stan(
    file    = here("main-pipeline","All","model.stan"),
    data    = stan_data_scottish,
    chains  = 4,
    warmup  = 1000,
    iter    = 2000,
    cores   = 4,
    refresh = 100
  )
  saveRDS(model_fit_scottish,  here("data","Models","Scotland","model_fit_scottish.rds"))
  saveRDS(stan_data_scottish,  here("data","Models","Scotland","stan_data_scottish.rds"))
  saveRDS(party_cols_scottish, here("data","Models","Scotland","party_cols_scottish.rds"))
}

#-------------------------------------------------------------------------------------------
# Extract posterior vote share estimates

softmax <- function(x) exp(x) / sum(exp(x))
posterior <- extract(model_fit_scottish)

vote_share_draws <- posterior$a[, , stan_data_scottish$T] |>
  apply(1, softmax) |>
  t()
colnames(vote_share_draws) <- party_cols_scottish

# 95% credible intervals per party
summary_table_scottish <- as_tibble(vote_share_draws) |>
  pivot_longer(everything(), names_to = "Party", values_to = "Vote_Share") |>
  group_by(Party) |>
  summarise(
    mean     = mean(Vote_Share),
    lower_95 = quantile(Vote_Share, 0.025),
    upper_95 = quantile(Vote_Share, 0.975),
    .groups  = "drop"
  )

