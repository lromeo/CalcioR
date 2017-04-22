results <- R6::R6Class(
    "results",
    public = list(

        data = NULL,

        initialize = function(data) {
            self$data <- data
        },

        extract_season = function(txt) {
            stringr::str_extract(txt, "[0-9]{4}") %>%
                as.numeric() - 2012
        },
        calc_points = function(p_score, o_score) {
            ifelse(p_score > o_score, 3,
                   ifelse(p_score == o_score, 1,
                          0))},
        remove_extra_matches = function(results) {
            results %>%
                filter(!(season == 3 & match_day > 35 & is.na(score1)))
        },
        seperate_primary_teams = function(results) {
            home_as_primary <- results %>%
                 rename(p_team = team1_key, o_team = team2_key, p_score = score1,
                           o_score = score2) %>%
                    mutate(p_home = TRUE)

             away_as_primary <- results %>%
                 rename(p_team = team2_key, o_team = team1_key, p_score = score2,
                           o_score = score1) %>%
                    mutate(p_home = FALSE)

             bind_rows(home_as_primary, away_as_primary) %>%
                 select(p_team, o_team, p_score, o_score, p_home, match_day,
                        season)

        },

        update_all_seasons = function() {
            self$data <- self$data %>%
                mutate(season = self$extract_season(season)) %>%
                self$remove_extra_matches() %>%
                self$seperate_primary_teams() %>%
                mutate(points = self$calc_points(p_score, o_score)) %>%
                tidyr::nest(-season, -match_day, .key = "matches") %>%
                tidyr::nest(-season, .key = "results") %>%
                arrange(season)
        }
    )
)

