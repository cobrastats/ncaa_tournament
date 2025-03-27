# filtering by attendance 
# weird thing is it basically just looks to be Final 4s? 
big_venues_schedule <- function(years = 2011:2024, tournament_id = 22) {
  purrr::map_dfr(years, function(year) {
    hoopR::load_mbb_schedule(year) |>
      dplyr::filter(tournament_id == !!tournament_id) |>
      dplyr::mutate(year = year)
  })
}

# pulling only the columns that look relevant 
stadium_data <- big_venues_schedule() |> 
  dplyr::select(
    game_id,
    game_date,
    season,
    notes_headline,
    team_a = home_location,
    team_b = away_location,
    team_a_pts = home_score,
    team_b_pts = away_score,
    venue_full_name,
    attendance,
    venue_address_city,
    venue_address_state
  )

stadium_data = stadium_data %>% 
  filter((grepl("SWEET 16|ELITE 8|FINAL FOUR|NATIONAL CHAMPIONSHIP|Sweet 16|Elite 8|Final Four|National Championship",
               notes_headline)|season==2021))


stadium_data = stadium_data %>% 
  mutate(stadium = if_else(season==2024 & venue_full_name == "State Farm Stadium",1,0),
         stadium = if_else(season==2023 & venue_full_name == "NRG Stadium",1,stadium),
         stadium = if_else(season==2022 & venue_full_name == "Caesars Superdome",1,stadium),
         stadium = if_else(season==2021 & venue_full_name == "Lucas Oil Stadium",1,stadium),
         stadium = if_else(season==2019 & venue_full_name == "U.S. Bank Stadium",1,stadium),
         stadium = if_else(season==2018 & venue_full_name == "Alamodome",1,stadium),
         stadium = if_else(season==2017 & venue_full_name == "State Farm Stadium",1,stadium),
         stadium = if_else(season==2016 & venue_full_name == "NRG Stadium",1,stadium),
         stadium = if_else(season==2015 & venue_full_name %in% c("Lucas Oil Stadium","JMA Wireless Dome","NRG Stadium"),1,stadium),
         stadium = if_else(season==2014 & venue_full_name %in% c("Lucas Oil Stadium","AT&T Stadium"),1,stadium),
         stadium = if_else(season==2013 & (venue_full_name=="Georgia Dome"|grepl("INDIANAPOLIS|ARLINGTON",notes_headline)),1,stadium),
         stadium = if_else(season==2012 & venue_full_name %in% c("Caesars Superdome","Georgia Dome","The Dome at America's Center"),1,stadium),
         stadium = if_else(season==2011 & grepl("HOUSTON|SAN ANTONIO",notes_headline),1,stadium)) %>% 
  filter(stadium==1)
