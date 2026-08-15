#-------------------------------------------------------------------------------------------
# Generate predictions for each demographic cell per constituency
# Following the MRP framework — predict for each cell, then poststratify

# Base prediction grid — unique combinations of all predictors across constituencies
prediction_grid_base <- voting_likely_england |>
  distinct(
    new_pcon, ageGroup, p_ethnicity2,
    gender, p_education_level, ethnicity_harmonised,
    housing_tenure_, past_vote_2024, p_eurefvote,
    density, house_rented, muslim_pct, remain,
    con_pct, index, spatial_lag_ld, spatial_lag_green, spatial_lag_con, spatial_lag_lab,
    Lab24, Con24, LD24, RUK24, Green24, Other24,
    by_election_share, current_winner
  )

# Generate predicted vote probability for each party in each demographic cell
prediction_grid <- imap_dfr(party_models, function(model, party) {
  grid <- prediction_grid_base |>
    mutate(
      raw_share = if_else(
        !is.na(by_election_share) & current_winner == party,
        by_election_share,
        .data[[party_share_map[[party]]]]
      ),
      party_share_24 = if_else(
        current_winner == party,
        1 + raw_share,
        raw_share
      )
    )
  
  grid |>
    mutate(
      predicted = predict(
        model,
        newdata          = grid,
        type             = "response",
        allow.new.levels = TRUE
      ),
      party = party
    )
})
#-------------------------------------------------------------------------------------------
# Poststratification weights
# Following Lauderdale (2018) — reweight predictions by true constituency
# demographic composition from census data

# Age x tenure joint distribution from ONS Census 2021
# Reaggregated to 2024 Westminster constituency boundaries
tenure_by_age <- read_xlsx(
  here("data","Excel-Files","custom-filtered-2026-07-15T12_20_21Z.xlsx"),
  sheet     = 1,
  skip      = 1,
  col_names = c("new_pcon_code", "new_pcon", "age_code", "age_group",
                "tenure_code", "tenure", "count")
) |>
  mutate(new_pcon = remove_accents(tolower(new_pcon)))

#-------------------------------------------------------------------------------------------
# Harmonise age x tenure census categories to match BES variable coding

tenure_by_age <- tenure_by_age |>
  filter(
    new_pcon %in% voting_likely_england$new_pcon,
    tenure != "Does not apply"
  ) |>
  select(new_pcon, age_group, tenure, count) |>
  mutate(
    age_num = as.integer(str_extract(age_group, "\\d+")),
    
    # Age groups harmonised to match BES ageGroup categories
    age_group_harmonised = case_when(
      between(age_num, 18, 25) ~ "18-25",
      between(age_num, 26, 35) ~ "26-35",
      between(age_num, 36, 45) ~ "36-45",
      between(age_num, 46, 55) ~ "46-55",
      between(age_num, 56, 65) ~ "56-65",
      age_num > 65             ~ "66+",
      TRUE                     ~ NA_character_
    ),
    
    # Tenure categories harmonised to match BES housing_tenure_ categories
    tenure_type = case_when(
      tenure %in% c(
        "Owned: Owns outright",
        "Owned: Owns with a mortgage or loan or shared ownership"
      ) ~ "house_owned",
      tenure %in% c(
        "Social rented: Rents from council or Local Authority",
        "Social rented: Other social rented",
        "Private rented: Private landlord or letting agency",
        "Private rented: Other private rented or lives rent free"
      ) ~ "house_rented",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(age_group_harmonised), !is.na(tenure_type)) |>
  group_by(new_pcon, tenure_type, age_group_harmonised) |>
  summarise(total_tenure = sum(count, na.rm = TRUE), .groups = "drop")

#-------------------------------------------------------------------------------------------
# Ethnicity x Sex x Education joint distribution
# Source: ONS Census 2021 RM049 table reaggregated to 2024 constituency boundaries

ethn_sex_edu <- read_xlsx(
  here("data","Excel-Files","RM049-2021-1-filtered-2026-07-21T21_00_19Z.xlsx"),
  skip      = 1,
  col_names = c("new_pcon_code", "new_pcon", "highest_qual_code", "qual",
                "ethn_group_code", "ethnic_group", "sex_code", "sex", "count")
) |>
  mutate(new_pcon = remove_accents(tolower(new_pcon)))

#-------------------------------------------------------------------------------------------
# Harmonise ethnicity, sex and education categories to match BES variable coding

ethn_sex_edu <- ethn_sex_edu |>
  filter(
    new_pcon %in% voting_likely_england$new_pcon,
    qual != "Does not apply",
    qual != "Highest level of qualifications (8 categories)",
    ethnic_group != "Does not apply",
    ethnic_group != "Ethnic group (8 categories)",
    sex != "Sex (2 categories)"
  ) |>
  select(new_pcon, sex, qual, ethnic_group, count) |>
  mutate(
    ethnicity = case_when(
      str_detect(ethnic_group, "Asian")       ~ "Any other Asian background",
      str_detect(ethnic_group, "Black")       ~ "African",
      str_detect(ethnic_group, "Mixed")       ~ "Any other Mixed / Multiple ethnic background",
      str_detect(ethnic_group, "White: Engl") ~ "English / Welsh / Scottish / Northern Irish / British",
      str_detect(ethnic_group, "White:")      ~ "Any other white background",
      str_detect(ethnic_group, "Other")       ~ "Any other ethnic group",
      TRUE                                    ~ NA_character_
    ),
    highest_qual = case_when(
      str_detect(qual, "No qual")                      ~ "None",
      str_detect(qual, "Level 1|Level 2|entry|Other")  ~ "School_Leaver",
      str_detect(qual, "Level 3")                      ~ "Further_Education",
      str_detect(qual, "Level 4")                      ~ "Degree",
      TRUE                                             ~ NA_character_
    ),
    gender_coded = case_when(
      sex == "Female" ~ 1,
      sex == "Male"   ~ 2,
      TRUE            ~ NA_real_
    ),
    count = as.numeric(count)
  ) |>
  filter(!is.na(highest_qual), !is.na(ethnicity), !is.na(gender_coded)) |>
  group_by(new_pcon, highest_qual, gender_coded, ethnicity) |>
  summarise(total = sum(count, na.rm = TRUE), .groups = "drop")

#-------------------------------------------------------------------------------------------
# Compute poststratification weights

tenure_weights <- tenure_by_age |>
  group_by(new_pcon) |>
  mutate(prop = total_tenure / sum(total_tenure)) |>
  ungroup()

ethn_sex_edu_weights <- ethn_sex_edu |>
  group_by(new_pcon) |>
  mutate(prop = total / sum(total)) |>
  ungroup()

#-------------------------------------------------------------------------------------------
# Poststratification
# Two step approach under independence assumption

postrat <- prediction_grid |>
  left_join(
    tenure_weights |>
      rename(
        housing_tenure_  = tenure_type,
        ageGroup         = age_group_harmonised,
        tenure_prop      = prop
      ),
    by = c("new_pcon", "housing_tenure_", "ageGroup")
  ) |>
  left_join(
    ethn_sex_edu_weights |>
      rename(
        gender               = gender_coded,
        p_education_level    = highest_qual,
        ethnicity_harmonised = ethnicity,
        ethn_sex_edu_prop    = prop
      ),
    by = c("new_pcon", "p_education_level", "gender", "ethnicity_harmonised")
  ) |>
  mutate(weight = tenure_prop * ethn_sex_edu_prop)

# Poststratify — weighted average of cell predictions per constituency
constituency_vote_shares <- postrat |>
  filter(new_pcon != "") |>
  group_by(new_pcon, party) |>
  summarise(
    vote_share = sum(predicted * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  filter(!is.nan(vote_share)) |>
  group_by(new_pcon) |>
  filter(n() == 6) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()