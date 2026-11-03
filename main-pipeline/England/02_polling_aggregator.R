#-------------------------------------------------------------------------------------------
# Polling aggregator following Hanretty, Lauderdale and Vivyan (2015)
# Fetches current GB polling data and fits a Bayesian state space model
# to estimate national voting intention with uncertainty

#-------------------------------------------------------------------------------------------
# Fetch and clean polling data from BritPolls

# The connection to the website usually fails a few times before connecting
# this is the solution that 
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

raw_gb_bv <- fetch_with_retry("https://britain.votes.now/data/polls/gb.json")

#Extract relevant polling data past the General Election 2024 date
english_polls <- raw_gb_bv |>
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
    other     = Oth
  ) |>
  mutate(
    Pollster    = as.integer(factor(pollster)),
    sample_size = 2000L #Sample size missing for all polls, so assume 2000 were surveyed
  ) |>
  filter(!is.na(polling_dates))

clean_data <- english_polls |>
  mutate(
    time_id  = as.integer(polling_dates - min(polling_dates)) + 1,
    Pollster = as.integer(factor(pollster))
  ) |>
  filter(!is.na(polling_dates), !is.na(sample_size), sample_size > 0)

party_cols <- clean_data |>
  select(ends_with("%"), other) |>
  names()

y <- clean_data |>
  select(all_of(party_cols), sample_size) |>
  mutate(across(everything(), ~replace_na(round(. * sample_size), 0L))) |>
  select(-sample_size) |>
  as.matrix()

#-------------------------------------------------------------------------------------------
# Prepare Stan data

stan_data <- list(
  T         = as.integer(max(clean_data$time_id)),
  J         = ncol(y),
  P         = max(clean_data$Pollster),
  N         = nrow(clean_data),
  poll_time = clean_data$time_id,
  pollster  = clean_data$Pollster,
  n         = clean_data$sample_size,
  y         = y
)

#-------------------------------------------------------------------------------------------
# Fit or load Stan model

MODEL_FIT_PATH <- here("data", "Models","England","model_fit.rds")

if (file.exists(MODEL_FIT_PATH)) {
  model_fit <- readRDS(here("data", "Models","England","model_fit.rds"))
  stan_data <- readRDS(here("data", "Models","England","stan_data.rds")) 
  party_cols <- readRDS(here("data", "Models","England","party_cols.rds"))
} else {
  model_fit <- stan(
    file    = here("main-pipeline","All","model.stan"),
    data    = stan_data,
    chains  = 4,
    warmup  = 1000,
    iter    = 2000,
    cores   = 4,
    refresh = 100
  )
  saveRDS(model_fit,  here("data", "Models","England","model_fit.rds"))
  saveRDS(stan_data,  here("data", "Models","England","stan_data.rds"))
  saveRDS(party_cols, here("data", "Models","England","party_cols.rds"))
}

#-------------------------------------------------------------------------------------------
# Extract posterior vote share estimates

softmax <- function(x) exp(x) / sum(exp(x))
posterior <- extract(model_fit)

vote_share_draws <- posterior$a[, , stan_data$T] |>
  apply(1, softmax) |>
  t()
colnames(vote_share_draws) <- party_cols

# 95% credible intervals per party
summary_table <- as_tibble(vote_share_draws) |>
  pivot_longer(everything(), names_to = "Party", values_to = "Vote_Share") |>
  group_by(Party) |>
  summarise(
    mean     = mean(Vote_Share),
    lower_95 = quantile(Vote_Share, 0.025),
    upper_95 = quantile(Vote_Share, 0.975),
    .groups  = "drop"
  )
summary_table

#-------------------------------------------------------------------------------------------
# Party display settings

PARTY_ORDER <- c("LAB%", "CON%", "LIB%", "Reform%", "Green%", "other")
PARTY_COLOURS <- c(
  "LAB%"    = "#E4003B",
  "CON%"    = "#0087DC",
  "LIB%"    = "#FAA61A",
  "Reform%" = "#12B6CF",
  "Green%"  = "#02A95B",
  "other"   = "#888888"
)

#-------------------------------------------------------------------------------------------
# Optional plotting functions

plot_time_series <- function() {
  all_days <- map_dfr(1:stan_data$T, function(t) {
    posterior$a[, , t] |>
      apply(1, softmax) |>
      t() |>
      as_tibble() |>
      setNames(party_cols) |>
      pivot_longer(everything(), names_to = "Party", values_to = "Vote_Share") |>
      group_by(Party) |>
      summarise(mean = mean(Vote_Share), .groups = "drop") |>
      mutate(day = t)
  })
  
  all_days |>
    mutate(Party = factor(Party, levels = PARTY_ORDER)) |>
    ggplot(aes(x = day, y = mean, colour = Party)) +
    geom_smooth(se = FALSE, span = 0.1) +
    scale_colour_manual(values = PARTY_COLOURS) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x      = "Days since GE2024",
      y      = "Estimated vote share",
      title  = "National vote share estimates",
      colour = "Party"
    ) +
    coord_cartesian(ylim = c(0.05, 0.45)) +
    theme_minimal()
}

plot_vote_distribution <- function() {
  as.data.frame(vote_share_draws) |>
    pivot_longer(everything(), names_to = "Party", values_to = "Share") |>
    ggplot(aes(x = Share)) +
    geom_histogram(bins = 40) +
    facet_wrap(~Party, scales = "free") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal()
}