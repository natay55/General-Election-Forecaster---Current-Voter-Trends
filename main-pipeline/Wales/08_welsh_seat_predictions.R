#-------------------------------------------------------------------------------------------
# FPTP seat allocation
# Whoever has the highest predicted vote share in each constituency wins the seat
seat_predictions_wales <- constituency_unwound_wales |>
  mutate(vote_share = round(vote_share, 6)) |>
  group_by(new_pcon) |>
  slice_max(vote_share, n = 1, with_ties = FALSE) |>
  ungroup() |>
  count(party, name = "predicted_seats") |>
  arrange(desc(predicted_seats))

cat("Total Welsh seats predicted:", sum(seat_predictions_wales$predicted_seats), "\n")
seat_predictions_wales
