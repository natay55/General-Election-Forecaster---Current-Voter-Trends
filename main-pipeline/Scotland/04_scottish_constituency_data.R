#-------------------------------------------------------------------------------------------
# Load constituency level data sources for Scotland
#-------------------------------------------------------------------------------------------

#Get population counts for each constituency
scottish_population <- read_xlsx(
  path = here("data","Excel-Files","CBP-10529.xlsx"),
  sheet = 4
) |>
  filter(str_starts(con_code, "S")) |>
  group_by(con_name) |>
  summarise(scot_pop_count = sum(con_number, na.rm = TRUE), .groups = "drop") |>
  select(new_pcon = con_name, scot_pop_count) |>
  mutate(new_pcon = tolower(new_pcon))

#-------------------------------------------------------------------------------------------

#Load data for tenure by constituency and clean data
scottish_tenure <- read_csv(here("data","Excel-Files","scottish_tenure.csv"), skip=10) |>
  select(tenure = `Household Tenure`, new_pcon = `United Kingdom Parliamentary Constituency 2024`, Count)|>
  filter(tolower(new_pcon) %in% tolower(voting_likely_scotland$new_pcon))|>
  mutate(
    new_pcon = tolower(new_pcon),
    tenure_grouped = case_when(
      str_detect(tenure, "^Owned") & !tenure %in% "Owned: Total" ~ "house_owned",
      str_detect(tenure, "^Social Rented") ~ "house_rented",
      str_detect(tenure, "^Private rented") & !tenure %in% "Private rented: Total" ~ "house_rented",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(tenure_grouped)) |>
  group_by(new_pcon, tenure_grouped) |>
  summarise(total = sum(Count, na.rm = TRUE), .groups = "drop") |>
  group_by(new_pcon) |>
  mutate(prop = total / sum(total)) |>
  ungroup() |>
  select(new_pcon, tenure_grouped, prop) |>
  pivot_wider(names_from = tenure_grouped, values_from = prop)

#-------------------------------------------------------------------------------------------
#Get qualifications by constituency
scottish_quals <- read_xlsx(path=here("data","Excel-Files","Qualifications_census.xlsx"), sheet=5)|>
  mutate(new_pcon = tolower(`ConstituencyName`))|>
  filter(new_pcon %in% voting_likely_scotland$new_pcon)|>
  filter(`Groups` == "Degree level qualifications or above")|>
  select(new_pcon, Con_pc)

#-------------------------------------------------------------------------------------------
#Get deprivation index for Scottish constituencies
scottish_deprivation <- read_xlsx(here("data","Excel-Files","uk_index.xlsx"), sheet=14)|>
  filter(tolower(`constituency-name`) %in% tolower(voting_likely_scotland$new_pcon))|>
  select(new_pcon = `constituency-name`, dep_index = `parl25-deprivation-score`)|>
  mutate(new_pcon=tolower(new_pcon))

#-------------------------------------------------------------------------------------------
#Construct the population density (since no data freely available for Scotland)

scottish_constituency_sf <- constituencies_sf |>
  filter(str_starts(PCON24CD, "S")) |>
  mutate(
    new_pcon = tolower(PCON24NM),
    area_km2 = as.numeric(st_area(geometry)) / 1e6
  )|>
  st_drop_geometry()|>
  select(new_pcon, area_km2)

scot_density <- scottish_population |>
  left_join(scottish_constituency_sf, by = "new_pcon") |>
  mutate(density = scot_pop_count / area_km2) |>
  select(new_pcon, density)


#--------------------------------------------------------------------------------------------
#Join tenure onto voting likely grid for Scotland

voting_likely_scotland <- voting_likely_scotland |>
  left_join(scottish_tenure, by="new_pcon")

#--------------------------------------------------------------------------------------------
#Join qualification to voting likely grid for Scotland
voting_likely_scotland <- voting_likely_scotland |>
  left_join(scottish_quals, by="new_pcon")

#--------------------------------------------------------------------------------------------
#Join party share vote in the 2024 election
voting_likely_scotland <- voting_likely_scotland |>
  left_join(
    bes_elections |>
      filter(tolower(ConstituencyName) %in% tolower(voting_likely_scotland$new_pcon)) |>
      select(new_pcon = ConstituencyName, Lab24, Con24, LD24, Green24, RUK24, SNP24, Other24) |>
      mutate(
        across(where(is.numeric), ~. / 100),
        new_pcon = tolower(new_pcon)
      ),
    by = "new_pcon"
  )
#---------------------------------------------------------------------------------------------
#Join deprivation index to voting likely grid for Scotland

voting_likely_scotland <- voting_likely_scotland |>
  left_join(scottish_deprivation, by="new_pcon")
  
