#-------------------------------------------------------------------------------------------
# Generate predictions for each demographic cell per constituency
# Following the MRP framework — predict for each cell, then poststratify

# Prediction grid — unique combinations of all predictors across constituencies
prediction_grid_scottish <- voting_likely_scotland |>
  distinct(
    new_pcon, ageGroup_scot, 
    gender, p_education_level,
    housing_tenure_, past_vote_2024,
    mortgage_owned, private_rented, Con_pc, scot_rem, 
    dep_index, 
    Lab24, Con24, LD24, SNP24, RUK24, Green24, Other24
  )

prediction_grid_scottish <- imap_dfr(party_models_scotland, function(model, party) {
  
  grid <- prediction_grid_scottish |>
    mutate(party_share_24 = .data[[party_share_map_scottish[[party]]]])
  
  # Add offset for Reform only to anchor Reform voters to past constituency performance 
  # (since data from 7 constituencies was missing and we failed to find a random constituency effect)
  if (party == "Brexit Party/Reform UK") {
    grid <- grid |>
      mutate(
        ruk24_offset = log(
          pmax(RUK24, 0.001) / (1 - pmax(pmin(RUK24, 0.999), 0.001))
        )
      )
  }
  
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

#----------------------------------------------------------------------------------------
#Override age bands for Scottish BES data due to data constraints from census data.
#This is obviously imperfect, however severe data limitations for Scotland forces us to do this for a Tenure x Age poststratification
voting_likely_scotland <- voting_likely_scotland |>
  mutate(
    ageGroup_scot = case_when(
      ageGroup %in% c("18-25", "26-35") ~ "16-34",
      ageGroup %in% c("36-45", "46-55") ~ "35-49",
      ageGroup %in% c("56-65")          ~ "50-64",
      ageGroup == "66+"                  ~ "65+",
      TRUE                               ~ NA_character_
    )
  )

#----------------------------------------------------------------------------------------
#Tenure x Age joint distribution from Scottish census
tenure_by_age_scotland <- read_csv(here("data","Excel-Files","tenure_by_age_scotland.csv"), skip=10) |>
  select(new_pcon = `United Kingdom Parliamentary Constituency 2024`, tenure=`Household Tenure`, age = `Age of Household Reference Person`, total = Count)|>
  mutate(
    new_pcon = tolower(new_pcon),
    scot_tenure = case_when(
      str_detect(tenure, "^Owned") & !tenure %in% "Owned: Total" ~ "house_owned",
      str_detect(tenure, "^Social Rented") ~ "house_rented",
      str_detect(tenure, "^Private rented") & !tenure %in% "Private rented: Total" ~ "house_rented",
      TRUE ~ NA_character_
    ),
    ageGroup_scot = case_when(
      str_detect(age, "16 to 34")    ~ "16-34",
      str_detect(age, "35 to 49")    ~ "35-49",
      str_detect(age, "50 to 64")    ~ "50-64",
      str_detect(age, "65 and over") ~ "65+",
      TRUE                           ~ NA_character_
    )
  )|>
  filter(!is.na(scot_tenure), !is.na(ageGroup_scot))|>
  group_by(new_pcon, scot_tenure, ageGroup_scot)|>
  summarise(total = sum(total), .groups="drop")|>
  group_by(new_pcon)|>
  mutate(prop = total / sum(total))|>
  ungroup()

voting_likely_scotland <- voting_likely_scotland |>
  left_join(
    tenure_by_age_scotland |>
      rename(
        housing_tenure_ = scot_tenure,
        ageGroup_scot   = ageGroup_scot,
        tenure_prop     = prop
      ),
    by = c("new_pcon", "housing_tenure_", "ageGroup_scot")
  )
#----------------------------------------------------------------------------------------
#Qualification x Sex x Age from Scottish census. Marginalizing takes place to produce a Qualification x Sex joint distribution
#This is mainly done due to a lack of data from the Scottish census regarding the new constituencies, but it also has a practical
#modelling useage in the sense that Scotland has less ethnic-diversity which is less concentrated. The identified predictors are sufficient.

qualification_sex_age <- read_csv(here("data","Excel-Files","qualification_by_sex_by_age_scottish.csv"), skip=10)|>
  select(new_pcon = `United Kingdom Parliamentary Constituency 2024`, sex = Sex, age = Age, highest_qual = `Highest Level of Qualification`, total = Count) |>
  mutate(
    new_pcon = tolower(new_pcon),
    p_education_level = case_when(
      str_detect(highest_qual, "No qual")                                  ~ "None",
      str_detect(highest_qual, "Lower school|Upper school|Apprenticeship") ~ "School_Leaver",
      str_detect(highest_qual, "Further Education")                        ~ "Further_Education",
      str_detect(highest_qual, "Degree")                                   ~ "Degree",
      TRUE                                                                 ~ NA_character_
    ),
    gender = case_when(
      str_detect(sex, "Female") ~ 1,
      str_detect(sex, "Male")   ~ 2,
      TRUE                      ~ NA_real_
    )
  ) |>
  filter(!is.na(p_education_level), !is.na(gender)) |>
  group_by(new_pcon, p_education_level, gender) |>
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop") |>
  group_by(new_pcon) |>
  mutate(prop = total / sum(total)) |>
  ungroup()
  
voting_likely_scotland <- voting_likely_scotland |>
  left_join(
    qualification_sex_age |>
      rename(
        qual_sex_prop = prop
      ),
    by = c("new_pcon", "p_education_level", "gender")
  )

#-----------------------------------------------------------------------------------------------------
# Scottish poststratification
# Step 1 — age x tenure (broad age bands due to data availability)
# Step 2 — qualification x sex (replaces ethnicity x sex x education)
# Ethnicity excluded due to Scotland's demographic homogeneity (96% white)

postrat_scotland <- prediction_grid_scottish |>
  left_join(
    tenure_by_age_scotland |>
      rename(
        housing_tenure_ = scot_tenure,
        ageGroup_scot   = ageGroup_scot,
        tenure_prop     = prop
      ),
    by = c("new_pcon", "housing_tenure_", "ageGroup_scot")
  ) |>
  left_join(
    qualification_sex_age |>
      rename(qual_sex_prop = prop),
    by = c("new_pcon", "p_education_level", "gender")
  ) |>
  mutate(weight = tenure_prop * qual_sex_prop)

# Poststratify — weighted average of cell predictions per constituency
constituency_vote_shares_scotland <- postrat_scotland |>
  filter(new_pcon != "") |>
  group_by(new_pcon, party) |>
  summarise(
    vote_share = sum(predicted * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  filter(!is.nan(vote_share)) |>
  group_by(new_pcon) |>
  filter(n() == length(parties_of_interest_scotland)) |>
  mutate(vote_share = vote_share / sum(vote_share)) |>
  ungroup()
