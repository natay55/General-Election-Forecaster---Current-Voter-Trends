#-------------------------------------------------------------------------------------------
# Generate predictions for each demographic cell per constituency
# Following the MRP framework — predict for each cell, then poststratify

# Prediction grid — unique combinations of all predictors across constituencies
prediction_grid_welsh <- voting_likely_wales |>
  distinct(
    new_pcon, ageGroup, gender, p_education_level, 
    housing_tenure_, past_vote_2024, house_rented,
    con_pct, index_dep_wales, welsh_speaking,
    Lab24, Con24, LD24, RUK24, Green24, PC24, Other24
  )


# Generate predicted vote probability for each party in each demographic cell
predictions_welsh <- imap_dfr(party_models_wales, function(model, party) {
  prediction_grid_welsh |>
    mutate(
      party_share_24 = .data[[party_share_map_wales[[party]]]],
      predicted      = predict(
        model,
        newdata          = prediction_grid_welsh |>
          mutate(party_share_24 = .data[[party_share_map_wales[[party]]]]),
        type             = "response",
        allow.new.levels = TRUE
      ),
      party = party
    )
})

#-------------------------------------------------------------------------------------------
tenure_by_age_wales <- read_xlsx(
  here("data","Excel-Files","custom-filtered-2026-07-15T12_20_21Z.xlsx"),
  sheet     = 1,
  skip      = 1,
  col_names = c("new_pcon_code", "new_pcon", "age_code", "age_group",
                "tenure_code", "tenure", "count")
) |>
  mutate(new_pcon = str_replace(remove_accents(tolower(new_pcon)), "ynys m.*n", "ynys mon"))

#-------------------------------------------------------------------------------------------
tenure_by_age_wales <- tenure_by_age_wales |>
  filter(
    new_pcon %in% voting_likely_wales$new_pcon,
    tenure != "Does not apply"
  ) |>
  select(new_pcon, age_group, tenure, count) |>
  mutate(
    age_num = as.integer(str_extract(age_group, "\\d+")),
    
    age_group_harmonised = case_when(
      between(age_num, 18, 25) ~ "18-25",
      between(age_num, 26, 35) ~ "26-35",
      between(age_num, 36, 45) ~ "36-45",
      between(age_num, 46, 55) ~ "46-55",
      between(age_num, 56, 65) ~ "56-65",
      age_num > 65             ~ "66+",
      TRUE                     ~ NA_character_
    ),
    
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
# Harmonise sex and education categories to match BES variable coding

#-------------------------------------------------------------------------------------------
sex_edu_wales <- read_xlsx(
  here("data","Excel-Files","RM049-2021-1-filtered-2026-07-21T21_00_19Z.xlsx"),
  skip      = 1,
  col_names = c("new_pcon_code", "new_pcon", "highest_qual_code", "qual",
                "ethn_group_code", "ethnic_group", "sex_code", "sex", "count")
) |>
  mutate(new_pcon = str_replace(remove_accents(tolower(new_pcon)), "ynys m.*n", "ynys mon"))

#-------------------------------------------------------------------------------------------
sex_edu_wales <- sex_edu_wales |>
  filter(
    new_pcon %in% voting_likely_wales$new_pcon,
    qual != "Does not apply",
    qual != "Highest level of qualifications (8 categories)",
    sex != "Sex (2 categories)"
  ) |>
  select(new_pcon, sex, qual, count) |>
  mutate(
    highest_qual = case_when(
      str_detect(qual, "No qual")                     ~ "None",
      str_detect(qual, "Level 1|Level 2|entry|Other") ~ "School_Leaver",
      str_detect(qual, "Level 3")                     ~ "Further_Education",
      str_detect(qual, "Level 4")                     ~ "Degree",
      TRUE                                            ~ NA_character_
    ),
    gender_coded = case_when(
      sex == "Female" ~ 1,
      sex == "Male"   ~ 2,
      TRUE            ~ NA_real_
    ),
    count = as.numeric(count)
  ) |>
  filter(!is.na(highest_qual), !is.na(gender_coded)) |>
  group_by(new_pcon, highest_qual, gender_coded) |>
  summarise(total = sum(count, na.rm = TRUE), .groups = "drop") |>
  group_by(new_pcon) |>
  mutate(prop = total / sum(total)) |>
  ungroup()
#-------------------------------------------------------------------------------------------
# Compute poststratification weights

tenure_weights_wales <- tenure_by_age_wales |>
  group_by(new_pcon) |>
  mutate(prop = total_tenure / sum(total_tenure)) |>
  ungroup()

sex_edu_weights_wales <- sex_edu_wales |>
  group_by(new_pcon) |>
  mutate(prop = total / sum(total)) |>
  ungroup()

#-------------------------------------------------------------------------------------------
# Poststratification
# Two step approach under independence assumption

postrat_wales <- predictions_welsh |>
  left_join(
    tenure_weights_wales |>
      rename(
        housing_tenure_ = tenure_type,
        ageGroup        = age_group_harmonised,
        tenure_prop     = prop
      ),
    by = c("new_pcon", "housing_tenure_", "ageGroup")
  ) |>
  left_join(
    sex_edu_weights_wales |>
      rename(
        gender            = gender_coded,
        p_education_level = highest_qual,
        sex_edu_prop      = prop
      ),
    by = c("new_pcon", "p_education_level", "gender") 
  ) |>
  mutate(weight = tenure_prop * sex_edu_prop)

# Poststratify — weighted average of cell predictions per constituency
constituency_vote_shares_wales <- postrat_wales |>
  filter(new_pcon != "") |>
  group_by(new_pcon, party) |>
  summarise(
    vote_share = sum(predicted * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  filter(!is.nan(vote_share)) |>
  group_by(new_pcon) |>
  filter(n() == length(parties_of_interest_wales)) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()

