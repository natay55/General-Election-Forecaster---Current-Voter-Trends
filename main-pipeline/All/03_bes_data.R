#-------------------------------------------------------------------------------------------
# Load BES survey and election results data

# Wave 31 has been released!
bes <- read_dta(here("data","BES","BES2024_W31_v31.05.dta"))

# 2024 GE constituency results including Hanretty Brexit estimates
bes_elections <- read_dta(here("data","BES","BES-2024-General-Election-results-file-v1.0.dta"))

# Remove diacritics from constituency names to ensure consistent joining
remove_accents <- function(x) {
  x |>
    stringi::stri_trans_nfc() |>  # normalise to precomposed form first
    stringi::stri_trans_general("Latin-ASCII")
}

bes <- bes |>
  mutate(new_pcon = remove_accents(tolower(as.character(haven::as_factor(new_pcon)))))

bes_elections <- bes_elections |>
  mutate(
    ConstituencyName = remove_accents(tolower(ConstituencyName)),
    new_pcon         = ConstituencyName
  )

bes$wt

#-------------------------------------------------------------------------------------------
# Filter BES to each nation and create likely voter samples

voting <- as_tibble(bes) |>
  filter(
    !is.na(new_pcon),
    new_pcon != ""
  ) |>
  mutate(
    vote_label    = haven::as_factor(generalElectionVote),
    country_label = haven::as_factor(country)
  )

voting_england  <- voting |> filter(country_label == "England")
voting_scotland <- voting |> filter(country_label == "Scotland")
voting_wales    <- voting |> filter(country_label == "Wales")

#-------------------------------------------------------------------------------------------
# Function to clean and filter likely voters for a given nation

make_voting_likely <- function(voting_country) {
  voting_country |>
    filter(
      !is.na(vote_label),
      !vote_label %in% c("I would/did not vote", "Don't know"),
      !p_past_vote_2024 %in% "Don't know",
      p_ethnicity2 != 19,
      !is.na(p_housing),
      p_housing < 8
    ) |>
    mutate(
      # Age groups harmonised to match ONS census categories
      ageGroup = case_when(
        ageGroup == 2 ~ "18-25",
        ageGroup == 3 ~ "26-35",
        ageGroup == 4 ~ "36-45",
        ageGroup == 5 ~ "46-55",
        ageGroup == 6 ~ "56-65",
        ageGroup == 7 ~ "66+",
        TRUE          ~ NA_character_
      ),
      # Housing tenure harmonised to match ONS census categories
      housing_tenure_ = case_when(
        p_housing == 1                   ~ "owned_outright",
        p_housing %in% c(2,3)            ~ "mortgage_owner_loan",
        p_housing %in% c(4,7,8)          ~ "private_rented",
        p_housing %in% c(5,6)            ~ "social_rented",
        TRUE                             ~ NA_character_
      ),
      # Ethnicity harmonised to match ONS census categories
      ethnicity_harmonised = case_when(
        p_ethnicity2 == 1             ~ "English / Welsh / Scottish / Northern Irish / British",
        p_ethnicity2 %in% c(2, 3, 4) ~ "Any other white background",
        p_ethnicity2 %in% c(5:8)     ~ "Any other Mixed / Multiple ethnic background",
        p_ethnicity2 %in% c(9:13)    ~ "Any other Asian background",
        p_ethnicity2 %in% c(14:16)   ~ "African",
        p_ethnicity2 %in% c(17, 18)  ~ "Any other ethnic group",
        TRUE                          ~ NA_character_
      ),
      # 2024 GE vote — strongest individual level predictor
      past_vote_2024 = case_when(
        p_past_vote_2024 == 1          ~ "Conservative",
        p_past_vote_2024 == 2          ~ "Labour",
        p_past_vote_2024 == 3          ~ "Liberal Democrat",
        p_past_vote_2024 == 4          ~ "Scottish National Party (SNP)",
        p_past_vote_2024 == 7          ~ "Green Party",
        p_past_vote_2024 == 12         ~ "Brexit Party/Reform UK",
        p_past_vote_2024 == 5          ~ "Plaid Cymru",
        p_past_vote_2024 %in% c(9, 13) ~ "Other",
        TRUE                           ~ "Did not vote / unknown"
      ),
      # Education harmonised to broad qualification levels
      p_education_level = case_when(
        p_education == 1                        ~ "None",
        p_education %in% c(2, 4, 8, 9, 10)     ~ "School_Leaver",
        p_education %in% c(3, 5, 6, 7, 11, 12) ~ "Further_Education",
        p_education %in% c(13:18)               ~ "Degree",
        TRUE                                    ~ NA_character_
      ),
      # Convert haven labelled variables to base R types for glmer compatibility
      gender      = as.integer(gender),
      p_eurefvote = as.integer(replace_na(p_eurefvote, 0L))
    ) |>
    filter(
      !is.na(ageGroup),
      !is.na(past_vote_2024),
      !is.na(p_education_level),
      !is.na(housing_tenure_)
    )
}

bes$p_housing

# Create likely voter samples for each nation
voting_likely_england  <- make_voting_likely(voting_england)
voting_likely_scotland <- make_voting_likely(voting_scotland)
voting_likely_wales    <- make_voting_likely(voting_wales)

#Create new age brackets for Scotland due to data constraints
voting_likely_scotland <- voting_likely_scotland |>
  mutate(
    ageGroup_scot = case_when(
      ageGroup %in% c("18-25", "26-35") ~ "16-34",
      ageGroup %in% c("36-45", "46-55") ~ "35-49",
      ageGroup == "56-65"               ~ "50-64",
      ageGroup == "66+"                 ~ "65+",
      TRUE                              ~ NA_character_
    )
  )

#-------------------------------------------------------------------------------------------
# Add Brexit leave/remain share per constituency
# Source: Hanretty estimates integrated into BES elections file

brexit_share <- bes_elections |>
  filter(Country %in% c("England", "Scotland", "Wales")) |>
  select(
    new_pcon = ConstituencyName,
    leave    = HanrettyLeave,
    remain   = HanrettyRemain
  ) |>
  mutate(
    new_pcon = tolower(new_pcon), 
    across(where(is.numeric), ~. / 100)
  )

# Join to each nation
voting_likely_england  <- voting_likely_england  |> left_join(brexit_share, by = "new_pcon")
voting_likely_scotland <- voting_likely_scotland |> left_join(brexit_share, by = "new_pcon")
voting_likely_wales    <- voting_likely_wales    |> left_join(brexit_share, by = "new_pcon")

#---------------------------------------------------------------------------------------------
#Add Hanretty estimate for Scottish independence vote
scot_ind <- bes_elections |>
  filter(Country == "Scotland")|>
  select(
    new_pcon = ConstituencyName,
    scot_ind = ScotRefYes,
    scot_rem = ScotRefNo
  )|>
  mutate(
    new_pcon = tolower(new_pcon),
    across(where(is.numeric), ~./100)
  )

voting_likely_scotland <- voting_likely_scotland |> left_join(scot_ind, by="new_pcon")

#---------------------------------------------------------------------------------------------
#Manually change accent for Welsh constituency
voting_likely_wales <- voting_likely_wales |>
  mutate(new_pcon = str_replace(new_pcon, "ynys m.*n", "ynys mon"))

#---------------------------------------------------------------------------------------------
#Add Welsh language predictor to voting likely wales
welsh_speak <- bes_elections |>
  filter(tolower(ConstituencyName) %in% voting_likely_wales$new_pcon) |>
  select(new_pcon = ConstituencyName, welsh_speaking = c21AnyWelsh) |>
  mutate(
    new_pcon = tolower(new_pcon),
    welsh_speaking = as.numeric(welsh_speaking) / 100
  )

voting_likely_wales <- voting_likely_wales |>
  left_join(welsh_speak, by = "new_pcon")

#---------------------------------------------------------------------------------------------
#Incumbency Indicator
#Data found here: https://www.parliament.uk/about/how/elections-and-voting/by-elections/by-elections-since-the-2024-general-election/
#Data found here for defections: https://en.wikipedia.org/wiki/List_of_British_politicians_who_have_changed_party_affiliation

incumbency_override <- list(
  # By-elections
  "runcorn and helsby"          = "Brexit Party/Reform UK",  # Labour -> Reform
  "gorton and denton"           = "Green Party",             # Labour -> Green
  "makerfield"                  = "Labour",                  # Labour -> Labour (different MP)
  "clacton"                     = "Brexit Party/Reform UK",  # Reform -> Reform (Farage held)
  # Defections
  "east wiltshire"              = "Brexit Party/Reform UK",  # Danny Kruger: Conservative -> Reform
  "coventry south"              = "Other",                   # Zarah Sultana: Labour -> Independent -> Your Party
  "newark"                      = "Brexit Party/Reform UK",  # Robert Jenrick: Conservative -> Reform
  "romford"                     = "Brexit Party/Reform UK",  # Andrew Rosindell: Conservative -> Reform
  "fareham and waterlooville"   = "Brexit Party/Reform UK",  # Suella Braverman: Conservative -> Reform
  "great yarmouth"              = "Other",                   # Rupert Lowe: Reform -> Independent -> Restore
  "islington north"             = "Other"                    # Jeremy Corbyn: Independent -> Your Party
)

by_election_results <- list(
  "runcorn and helsby" = list(party = "Brexit Party/Reform UK", share = 0.387),
  "gorton and denton"  = list(party = "Green Party",            share = 0.406),
  "makerfield"         = list(party = "Labour",                 share = 0.548),
  "clacton"            = list(party = "Brexit Party/Reform UK", share = 0.7)
)

#-------------------------------------------------------------------------------
# Build constituency_data with overrides
constituency_data <- bes_elections |>
  filter(Country == "England") |>
  mutate(
    new_pcon = remove_accents(tolower(ConstituencyName)),
    Winner24 = case_when(
      Winner24 == "Con"   ~ "Conservative",
      Winner24 == "Lab"   ~ "Labour",
      Winner24 == "LD"    ~ "Liberal Democrat",
      Winner24 == "RUK"   ~ "Brexit Party/Reform UK",
      Winner24 == "Green" ~ "Green Party",
      Winner24 == "Ind"   ~ "Other",
      TRUE                ~ Winner24
    )
  ) |>
  left_join(
    tibble(
      new_pcon      = names(incumbency_override),
      current_party = unlist(incumbency_override)
    ),
    by = "new_pcon"
  ) |>
  left_join(
    tibble(
      new_pcon          = names(by_election_results),
      by_election_share = map_dbl(by_election_results, ~ .x$share)
    ),
    by = "new_pcon"
  ) |>
  mutate(
    current_winner = coalesce(current_party, Winner24)
  ) |>
  select(new_pcon, current_winner, by_election_share)

#-------------------------------------------------------------------------------
voting_likely_england <- voting_likely_england |>
  left_join(
    constituency_data |>
      select(new_pcon, current_winner, by_election_share),
    by = "new_pcon"
  )

#-------------------------------------------------------------------------------
#Incumbency for Scotland
incumbency_override_scotland <- list(
  "aberdeen south"               = "Conservative",
  "arbroath and broughty ferry"  = "Scottish National Party (SNP)"
)

by_election_results_scotland <- list(
  "aberdeen south" = list(party="Conservative", share = 0.495),
  "arbroath and broughty ferry" = list(party = "Scottish National Party (SNP)", share = 0.411)
)

constituency_data_scotland <- bes_elections |>
  filter(Country == "Scotland") |>
  mutate(
    new_pcon = remove_accents(tolower(ConstituencyName)),
    Winner24 = case_when(
      Winner24 == "Con"   ~ "Conservative",
      Winner24 == "Lab"   ~ "Labour",
      Winner24 == "LD"    ~ "Liberal Democrat",
      Winner24 == "RUK"   ~ "Brexit Party/Reform UK",
      Winner24 == "Green" ~ "Green Party",
      Winner24 == "SNP"   ~ "Scottish National Party (SNP)",
      Winner24 == "Ind"   ~ "Other",
      TRUE                ~ Winner24
    )
  ) |>
  left_join(
    tibble(
      new_pcon      = names(incumbency_override_scotland),
      current_party = unlist(incumbency_override_scotland)
    ),
    by = "new_pcon"
  ) |>
  left_join(
    tibble(
      new_pcon          = names(by_election_results_scotland),
      by_election_share = map_dbl(by_election_results_scotland, ~ .x$share)
    ),
    by = "new_pcon"
  ) |>
  mutate(
    current_winner = coalesce(current_party, Winner24)
  ) |>
  select(new_pcon, current_winner, by_election_share)

#-------------------------------------------------------------------------------
#Add the incumbency to voting likely scotland
voting_likely_scotland <- voting_likely_scotland |>
  left_join(
    constituency_data_scotland |>
      select(new_pcon, current_winner, by_election_share),
    by = "new_pcon"
  )

#-------------------------------------------------------------------------------
#Filter out Plaid Cymru from English voting grid (new data had English person intending to vote for Plaid Cymru)
voting_likely_england <- voting_likely_england |>
  mutate(
    past_vote_2024 = if_else(
      past_vote_2024 == "Plaid Cymru",
      "Other",
      past_vote_2024
    )
  )

#-------------------------------------------------------------------------------
#Same as above but for the SNP in Wales
voting_likely_wales <- voting_likely_wales |>
  mutate(
    past_vote_2024 = case_when(
      past_vote_2024 == "Scottish National Party (SNP)" ~ "Other",
      TRUE                                              ~ past_vote_2024
    )
  )