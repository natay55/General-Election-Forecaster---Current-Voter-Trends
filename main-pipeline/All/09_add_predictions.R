#-------------------------------------------------------------------------------
#Add seat numbers for each party
# Combine all nation seat predictions

england <- seat_predictions
scotland <- seat_predictions_scotland
wales <- seat_predictions_wales

#Northern Ireland essentially has no data to use MRP on. So, I'm using the national polling intention
#from LucidTalk from their survey panels and applying that to Electoral Calculus' custom user polls
#This is far from ideal, but there isn't much of an alternative.
#(for reference, we have SF on 24%, DUP on 18%, UUP on 13%, SDLP on 11%, Alliance on 11%, TUV on 11% and MIN on 12%)


ni_constituency <- tibble(
  new_pcon = c(
    "fermanagh and south tyrone", "belfast west", "west tyrone",
    "newry and armagh", "mid ulster", "south down",
    "foyle", "belfast north", "east londonderry",
    "north down", "south antrim", "north antrim",
    "belfast south and mid down", "upper bann", "east antrim",
    "lagan valley", "strangford", "belfast east"
  ),
  party = c(
    "sinn fein", "sinn fein", "sinn fein",
    "sinn fein", "sinn fein", "sinn fein",
    "sdlp", "sinn fein", "sinn fein",
    "other", "uup", "tuv",
    "sdlp", "dup", "uup",
    "alliance", "dup", "dup"
  ),
  nation     = "Northern Ireland",
  vote_share = NA_real_
)

northern_ireland <- ni_constituency |>
  count(party, name = "predicted_seats")


cat("Total NI seats:", sum(northern_ireland$predicted_seats), "\n")

uk_seat_predictions <- bind_rows(
  england, scotland, wales, northern_ireland
)|>
  group_by(party)|>
  summarise(predicted_seats = sum(predicted_seats), .groups = "drop")|>
  ungroup()
uk_seat_predictions
