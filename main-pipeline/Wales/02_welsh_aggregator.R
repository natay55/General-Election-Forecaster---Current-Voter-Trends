#-------------------------------------------------------------------------------------------
# Polling aggregator following Hanretty, Lauderdale and Vivyan (2015)
# Fetches current GB polling data and fits a Bayesian state space model
# to estimate national voting intention with uncertainty

#-------------------------------------------------------------------------------------------
# Fetch and clean polling data from Britain Votes

fetch_with_retry <- function(url, max_attempts = 20, wait_seconds = 3) {
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

raw_wales <- fetch_with_retry("https://britain.votes.now/data/polls/wls_wmc.json")

#Extract relevant polling data past the General Election 2024 date
wales_polls <- raw_wales |>
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
    `PC%`     = PC,
    other     = Oth
  ) |>
  mutate(
    Pollster    = as.integer(factor(pollster)),
    sample_size = 2000L
  ) |>
  filter(!is.na(polling_dates))

#-------------------------------------------------------------------------------
#Prep data for Bayesian Aggregation
clean_data_wales <- wales_polls |>
  mutate(
    time_id  = as.integer(polling_dates - min(polling_dates)) + 1,
    Pollster = as.integer(factor(pollster))
  ) |>
  filter(!is.na(polling_dates), !is.na(sample_size), sample_size > 0)

party_cols_wales <- clean_data_wales |>
  select(ends_with("%"), other) |>
  names()

y_wales <- clean_data_wales |>
  select(all_of(party_cols_wales), sample_size) |>
  mutate(across(everything(), ~round(. * sample_size))) |>
  select(-sample_size) |>
  as.matrix()

#-------------------------------------------------------------------------------
#Parse data into Stan model
stan_data_wales <- list(
  T         = as.integer(max(clean_data_wales$time_id)),
  J         = ncol(y_wales),
  P         = max(clean_data_wales$Pollster),
  N         = nrow(clean_data_wales),
  poll_time = clean_data_wales$time_id,
  pollster  = clean_data_wales$Pollster,
  n         = clean_data_wales$sample_size,
  y         = y_wales
)

#-------------------------------------------------------------------------------------------
# Fit or load Stan model

MODEL_FIT_PATH <- here("data","Models","Wales","model_fit_wales.rds")

if (file.exists(here("data","Models","Wales","model_fit_wales.rds"))) {
  model_fit <- readRDS(here("data","Models","Wales", "model_fit_wales.rds"))
  stan_data <- readRDS(here("data","Models","Wales","stan_data_wales.rds")) 
  party_cols <- readRDS(here("data","Models","Wales","party_cols_wales.rds"))
} else {
  model_fit <- stan(
    file    = here("main-pipeline","All","model.stan"),
    data    = stan_data_wales,
    chains  = 4,
    warmup  = 1000,
    iter    = 2000,
    cores   = 4,
    refresh = 100
  )
  saveRDS(model_fit,  here("data","Models","Wales","model_fit_wales.rds"))
  saveRDS(stan_data,  here("data","Models","Wales","stan_data_wales.rds"))
  saveRDS(party_cols, here("data","Models","Wales","party_cols_wales.rds"))
}

#-------------------------------------------------------------------------------------------
# Extract posterior vote share estimates

softmax <- function(x) exp(x) / sum(exp(x))
posterior <- extract(model_fit)

vote_share_draws <- posterior$a[, , stan_data_wales$T] |>
  apply(1, softmax) |>
  t()
colnames(vote_share_draws) <- party_cols_wales

# 95% credible intervals per party
summary_table_wales <- as_tibble(vote_share_draws) |>
  pivot_longer(everything(), names_to = "Party", values_to = "Vote_Share") |>
  group_by(Party) |>
  summarise(
    mean     = mean(Vote_Share),
    lower_95 = quantile(Vote_Share, 0.025),
    upper_95 = quantile(Vote_Share, 0.975),
    .groups  = "drop"
  )
summary_table_wales

