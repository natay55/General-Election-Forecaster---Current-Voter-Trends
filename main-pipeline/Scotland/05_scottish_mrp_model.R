#-------------------------------------------------------------------------------------------
# GLM MRP model specification for Scotland
# Following Lauderdale (2018) and Park, Gelman and Bafumi (2004)
# Separate binary logistic model per party with constituency random effects

party_share_map_scottish <- list(
  "Labour"                        = "Lab24",
  "Conservative"                  = "Con24",
  "Liberal Democrat"              = "LD24",
  "Brexit Party/Reform UK"        = "RUK24",
  "Green Party"                   = "Green24",
  "Scottish National Party (SNP)" = "SNP24",
  "Other"                         = "Other24"
)

BASE_VARS_SCOTTISH <- c(
  "ageGroup_scot",
  "gender",
  "p_education_level",
  "housing_tenure_",
  "past_vote_2024",
  "scot_rem",
  "house_rented",
  "Con_pc",
  "party_share_24",
  "dep_index"
)

parties_of_interest_scotland <- c(
  "Labour",
  "Conservative",
  "Liberal Democrat",
  "Green Party",
  "Brexit Party/Reform UK",
  "Scottish National Party (SNP)",
  "Other"
)

#-------------------------------------------------------------------------------------------
# Fit or load models

PARTY_MODELS_SCOTLAND_PATH <- here("data","Models","Scotland","party_models_scotland.rds")

if (file.exists(PARTY_MODELS_SCOTLAND_PATH)) {
  party_models_scotland <- readRDS(PARTY_MODELS_SCOTLAND_PATH)
} else {
  party_models_scotland <- list()
  
  for (party in parties_of_interest_scotland) {
    party_data <- voting_likely_scotland |>
      mutate(
        vote           = if_else(vote_label == party, 1L, 0L),
        party_share_24 = .data[[party_share_map_scottish[[party]]]]
      )
    
    if (party == "Brexit Party/Reform UK") {
      # Reform uses glmer with 2024 vote share as offset
      # Offset acts as informative prior on constituency random effect
      # Prevents demographic extrapolation from English patterns
      # with sparse data (mean 7 Reform voters per constituency)
      party_data <- party_data |>
        mutate(
          ruk24_offset = log(
            pmax(RUK24, 0.001) / (1 - pmax(pmin(RUK24, 0.999), 0.001))
          )
        )
      
      formula_str <- paste(
        "vote ~",
        paste(BASE_VARS_SCOTTISH, collapse = " + "),
        "+ offset(ruk24_offset) + (1 | new_pcon)"
      )
    } else {
      formula_str <- paste(
        "vote ~",
        paste(BASE_VARS_SCOTTISH, collapse = " + "),
        "+ (1 | new_pcon)"
      )
    }
    
    party_models_scotland[[party]] <- glmer(
      as.formula(formula_str),
      data    = party_data,
      control = glmerControl(autoscale = TRUE),
      family  = binomial(link = "logit")
    )
    
    cat("Fitted model for:", party, "\n")
  }
  
  saveRDS(party_models_scotland, PARTY_MODELS_SCOTLAND_PATH)
}