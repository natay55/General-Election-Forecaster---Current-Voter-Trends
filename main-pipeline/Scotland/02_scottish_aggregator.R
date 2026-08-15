#-------------------------------------------------------------------------------------------
# Polling aggregator following Hanretty, Lauderdale and Vivyan (2015)
# Fetches current Scottish polling data and fits a Bayesian state space model
# to estimate national voting intention with uncertainty

#-------------------------------------------------------------------------------------------
# Fetch and clean polling data from PollCheck

GE2024_DATE <- as.Date("2024-07-04")
URL <- "https://www.electoralcalculus.co.uk/polls_scot.html"

raw_data <- URL |>
  read_html()|>
  html_nodes("table.llcccccccc")|>
  html_table(fill=TRUE)

raw_data <- bind_rows(raw_data) |>
  select(pollster = Pollster, polling_dates = ends_with('dates'), sample_size = ends_with('size'), ends_with('%')) |>
  mutate(
    across(ends_with('%'), (~. / 100)),
    sample_size   = as.integer(str_remove_all(sample_size, ",")),
    polling_dates = as.Date(str_extract(polling_dates, "(?<=-).*"), format = "%d %b %Y"),
    pollster      = str_remove_all(pollster, "/(.*)"),
    pollster      = as.integer(factor(pollster)),
    time_id       = as.integer((polling_dates - GE2024_DATE) + 1)
  ) |>
  filter(!is.na(polling_dates), !is.na(sample_size), sample_size > 0) |>
  # Replace NA party percentages with 0
  mutate(across(ends_with('%'), ~replace_na(., 0))) |>
  # Add other as remainder
  mutate(other = 1 - rowSums(across(ends_with('%')), na.rm = TRUE))

party_cols_scottish <- raw_data |>
  select(ends_with("%"), other) |>
  names()

# Approximate respondent counts per party per poll
y <- raw_data |>
  select(all_of(party_cols_scottish), sample_size) |>
  mutate(across(everything(), ~round(. * sample_size))) |>
  select(-sample_size) |>
  as.matrix()

#-------------------------------------------------------------------------------------------
#Prepare Stan data

stan_data_scottish <- list(
  T         = as.integer(max(raw_data$time_id)),
  J         = ncol(y),
  P         = max(raw_data$pollster),
  N         = nrow(raw_data),
  poll_time = raw_data$time_id,
  pollster  = raw_data$pollster,
  n         = raw_data$sample_size,
  y         = y
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

