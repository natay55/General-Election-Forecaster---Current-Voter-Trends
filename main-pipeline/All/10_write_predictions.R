#-------------------------------------------------------------------------------
#Write Constituency seat estimates to an excel file to use for the frontend

# England
england_constituency <- constituency_unwound |>
  group_by(new_pcon) |>
  slice_max(vote_share, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(new_pcon, party, vote_share) |>
  mutate(nation = "England")

# Scotland
scotland_constituency <- constituency_unwound_scotland |>
  group_by(new_pcon) |>
  slice_max(vote_share, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(new_pcon, party, vote_share) |>
  mutate(nation = "Scotland")

# Wales
wales_constituency <- constituency_unwound_wales |>
  group_by(new_pcon) |>
  slice_max(vote_share, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(new_pcon, party, vote_share) |>
  mutate(nation = "Wales")

# Northern Ireland
ni_constituency <- ni_constituency |>
  mutate(nation = "Northern Ireland", vote_share = NA_real_)


# Combine
all_constituencies <- bind_rows(
  england_constituency,
  scotland_constituency,
  wales_constituency,
  ni_constituency
) |>
  arrange(nation, new_pcon)

# Add the constituency code to join to the constituency name
pcon_codes <- constituencies_sf |>
  st_drop_geometry() |>
  mutate(new_pcon = remove_accents(tolower(PCON24NM))) |>
  select(new_pcon, PCON24CD)

all_constituencies <- all_constituencies |>
  left_join(pcon_codes, by = "new_pcon")

#Rewrite the missing constituencies to match shapefile names for NI
ni_constituency <- ni_constituency |>
  mutate(new_pcon = case_when(
    new_pcon == "antrim east"       ~ "east antrim",
    new_pcon == "antrim north"      ~ "north antrim",
    new_pcon == "antrim south"      ~ "south antrim",
    new_pcon == "down north"        ~ "north down",
    new_pcon == "down south"        ~ "south down",
    new_pcon == "londonderry east"  ~ "east londonderry",
    new_pcon == "tyrone west"       ~ "west tyrone",
    new_pcon == "ulster mid"        ~ "mid ulster",
    TRUE                            ~ new_pcon
  ))

# Write to Excel
write_xlsx(
  list(
    predictions     = all_constituencies,
    uk_summary      = uk_seat_predictions
  ),
  here("frontend", "data", "uk_election_predictions.xlsx")
)

cat("Written", nrow(all_constituencies), "constituencies to Excel\n")