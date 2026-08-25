#-------------------------------------------------------------------------------------------
# Load constituency level data sources for England and Wales

# House of Commons Library census data for English constituencies
census_data_2024 <- read_excel(
  here("data", "Excel-Files","census.xlsx"),
  sheet     = 4,
  skip      = 3,
  col_names = c("Topic", "Constituency", "Variable", "Constituency_Value", "Total")
) |>
  mutate(Constituency = tolower(Constituency))

# ONS population density estimates by constituency 2021-2024
pop_density <- read_xlsx(
  here("data", "Excel-Files","foi20263363westminsterparliamentaryconstituenciesmid2021tomid2024.xlsx"),
  sheet     = 5,
  skip      = 3,
  col_names = c("new_pcon_code", "new_pcon", "area",
                "pop_2024", "density_2024",
                "pop_2023", "density_2023",
                "pop_2022", "density_2022",
                "pop_2021", "density_2021")
) |>
  mutate(new_pcon = tolower(new_pcon))

# Index of Multiple Deprivation by constituency
deprivation_index <- read_xlsx(
  here("data", "Excel-Files","CBP10526.xlsx"),
  sheet = 4
) |>
  mutate(ConstituencyName = tolower(ConstituencyName))

# Qualification levels by constituency
con_quals <- read_xlsx(
  here("data", "Excel-Files","Qualifications_census.xlsx"),
  sheet     = 3,
  col_names = c("new_pcon_code", "new_pcon", "region_id", "region_name",
                "natcomp", "variables", "group", "var_order", "group_ord",
                "con_count", "con_pct", "rn_pct", "nat_pct", "l", "m")
) |>
  mutate(new_pcon = tolower(new_pcon))

# 2024 Westminster constituency boundary shapefiles
# Used to compute spatial lag predictors following Moran's I analysis
constituencies_sf <- st_read(
  here("data",
       "Westminster_Parliamentary_Constituencies_July_2024_Boundaries_UK_BUC_2210534386407638194",
       "PCON_JULY_2024_UK_BUC.shp")
)

#Welsh deprivation index file
welsh_dep <- read_xlsx(
  path = here("data", "Excel-Files","welsh_dep.xlsx")
)

#-------------------------------------------------------------------------------------------
# Join constituency level predictors to voting_likely for England and Wales

# Population density
voting_likely_england <- voting_likely_england |>
  left_join(
    pop_density |>
      filter(new_pcon %in% voting_likely_england$new_pcon) |>
      select(new_pcon, density = density_2024) |>
      mutate(density = as.numeric(density)),
    by = "new_pcon"
  )

voting_likely_wales <- voting_likely_wales |>
  left_join(
    pop_density |>
      filter(new_pcon %in% voting_likely_wales$new_pcon) |>
      select(new_pcon, density=density_2024)|>
      mutate(density = as.numeric(density)),
    by = "new_pcon"
  )
#-------------------------------------------------------------------------------

# Housing tenure composition
# Captures the renter/owner divide which is politically significant
voting_likely_england <- voting_likely_england |>
  left_join(
    census_data_2024 |>
      filter(
        Constituency %in% voting_likely_england$new_pcon,
        Topic == "Housing tenure"
      ) |>
      select(new_pcon = Constituency, tenure_type = Variable, pct = Constituency_Value) |>
      mutate(tenure_type = case_when(
        tenure_type %in% c("Owned outright")                                  ~ "house_owned",
        tenure_type %in% c("Owned with a mortgage or loan")                   ~ "mortgage_owned",
        tenure_type %in% c("Private rented")                                  ~ "private_rented",
        tenure_type %in% c('Social rented')                                   ~ "social_rented",
        TRUE                                                                   ~ "house_other"
      )) |>
      group_by(new_pcon, tenure_type) |>
      summarise(total_pct = sum(pct, na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = tenure_type, values_from = total_pct),
    by = "new_pcon"
  )

voting_likely_wales <- voting_likely_wales |>
  left_join(
    census_data_2024 |>
      filter(
        Constituency %in% voting_likely_wales$new_pcon,
        Topic == "Housing tenure"
      ) |>
      select(new_pcon = Constituency, tenure_type = Variable, pct = Constituency_Value) |>
      mutate(tenure_type = case_when(
        tenure_type %in% c("Owned outright")                                  ~ "house_owned",
        tenure_type %in% c("Owned with a mortgage or loan")                   ~ "mortgage_owned",
        tenure_type %in% c("Private rented")                                  ~ "private_rented",
        tenure_type %in% c('Social rented')                                   ~ "social_rented",
        TRUE                                                                   ~ "house_other"
      )) |>
      group_by(new_pcon, tenure_type) |>
      summarise(total_pct = sum(pct, na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = tenure_type, values_from = total_pct),
    by = "new_pcon"
  )
#-------------------------------------------------------------------------------

# Index of Multiple Deprivation
# Captures economic disadvantage — key predictor of Reform support
voting_likely_england <- voting_likely_england |>
  left_join(
    deprivation_index |>
      filter(ConstituencyName %in% voting_likely_england$new_pcon) |>
      select(new_pcon = ConstituencyName, index = `IMD rank 2025`),
    by = "new_pcon"
  )

#-------------------------------------------------------------------------------
#Separate deprivation index for Wales
#For this, we take the percentage of the 10% most deprived LSOA in each constituency

total_areas <- welsh_dep |>
  filter(
    `Deprivation group` == "Total LSOAs",
    Domain == "WIMD"
  ) |>
  select(new_pcon = `Area name`, total_area = `Data values`) |>
  mutate(
    new_pcon = tolower(new_pcon),
    new_pcon = str_replace(new_pcon, "ynys m.*n", "ynys mon") 
  ) |>
  filter(new_pcon %in% voting_likely_wales$new_pcon)

deprived_areas <- welsh_dep |>
  filter(
    `Deprivation group` == "Most deprived 10% LSOAs in Wales (ranks 1-191)",
    `Domain`            == "WIMD"
  )|>
  select(new_pcon = `Area name`, deprived = `Data values`)|>
  mutate(
    new_pcon = tolower(new_pcon),
    new_pcon = str_replace(new_pcon, "ynys m.*n", "ynys mon")
  )

wimd_pct <- total_areas |>
  left_join(deprived_areas, by = "new_pcon")|>
  mutate(index_dep_wales = deprived / total_area)|>
  select(new_pcon, index_dep_wales)

voting_likely_wales <- voting_likely_wales |>
  left_join(wimd_pct, by = "new_pcon")

#-------------------------------------------------------------------------------

# 2024 GE constituency vote shares
# Used as party specific constituency level predictors
voting_likely_england <- voting_likely_england |>
  left_join(
    bes_elections |>
      filter(ConstituencyName %in% voting_likely_england$new_pcon) |>
      select(new_pcon = ConstituencyName,
             Green24, Con24, Lab24, LD24, RUK24, Other24) |>
      mutate(across(where(is.numeric), ~. / 100)),
    by = "new_pcon"
  )

voting_likely_wales <- voting_likely_wales |>
  left_join(
    bes_elections |>
      filter(ConstituencyName %in% voting_likely_wales$new_pcon) |>
      select(new_pcon = ConstituencyName,
             Green24, Con24, Lab24, LD24, RUK24, PC24, Other24) |>
      mutate(across(where(is.numeric), ~. / 100)),
    by = "new_pcon"
  )

#-------------------------------------------------------------------------------

# Percentage of degree holders per constituency
# Higher education correlates with Labour and Lib Dem support
voting_likely_england <- voting_likely_england |>
  left_join(
    con_quals |>
      filter(
        new_pcon %in% voting_likely_england$new_pcon,
        group == "Higher education qualifications"
      ) |>
      select(new_pcon, con_pct) |>
      mutate(con_pct = as.numeric(con_pct)),
    by = "new_pcon"
  )

voting_likely_wales <- voting_likely_wales |>
  left_join(
    con_quals |>
      filter(group == "Higher education qualifications") |>
      select(new_pcon, con_pct) |>
      mutate(
        con_pct  = as.numeric(con_pct),
        new_pcon = str_replace(tolower(new_pcon), "ynys m.*n", "ynys mon")
      ),
    by = "new_pcon"
  )

#-------------------------------------------------------------------------------------------
# Spatial lag predictors
# Justified by Moran's I spatial autocorrelation analysis showing significant
# geographic clustering for all major parties (I > 0.33, p < 0.001)

# Filter to English constituencies only
constituencies_england <- constituencies_sf |>
  filter(str_starts(PCON24CD, "E")) |>
  mutate(new_pcon = tolower(PCON24NM)) |>
  arrange(new_pcon)

# Build constituency adjacency matrix
# adj_matrix[i,j] = 1 if constituency i borders constituency j
touches <- st_touches(constituencies_england)
n <- nrow(constituencies_england)

adj_matrix <- matrix(0, n, n)
for (i in 1:n) {
  if (length(touches[[i]]) > 0) {
    adj_matrix[i, touches[[i]]] <- 1
  }
}

rownames(adj_matrix) <- constituencies_england$new_pcon
colnames(adj_matrix) <- constituencies_england$new_pcon

# Normalise by number of neighbours
row_sums <- rowSums(adj_matrix)
row_sums[row_sums == 0] <- 1

# Compute spatial lag for each party
# Spatial lag = average 2024 vote share in neighbouring constituencies
parties_spatial <- list(
  lab    = "Lab24",
  con    = "Con24",
  ld     = "LD24",
  reform = "RUK24",
  green  = "Green24"
)

bes_england_ordered <- bes_elections |>
  filter(Country == "England") |>
  arrange(new_pcon)

spatial_lags <- tibble(new_pcon = constituencies_england$new_pcon)

for (p in names(parties_spatial)) {
  share_ordered <- tibble(new_pcon = constituencies_england$new_pcon) |>
    left_join(
      bes_england_ordered |>
        select(new_pcon, share = !!parties_spatial[[p]]) |>
        mutate(share = share / 100),
      by = "new_pcon"
    ) |>
    pull(share) |>
    replace_na(0)
  
  spatial_lags[[paste0("spatial_lag_", p)]] <- as.vector(
    adj_matrix %*% share_ordered / row_sums
  )
}

#-------------------------------------------------------------------------------
# Join spatial lags and Muslim population percentage to voting_likely

voting_likely_england <- voting_likely_england |>
  left_join(
    spatial_lags |>
      select(new_pcon, starts_with("spatial_lag_")),
    by = "new_pcon"
  ) |>
  mutate(new_pcon = remove_accents(new_pcon)) |>
  left_join(
    census_data_2024 |>
      filter(Topic == "Religion", str_detect(Variable, "Muslim")) |>
      select(new_pcon = Constituency, muslim_pct = Constituency_Value),
    by = "new_pcon"
  )
