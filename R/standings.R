
standings <- R6::R6Class(
    "standings",
    public = list(

        data = NA,
        season = NA,

        initialize = function() {
            self$data <- serie_a
        },

        initialize_standings = function() {
            by_match_day = function(match_day, data) {
                 data <- data %>%
                     select(p_team) %>%
                     mutate(position = NA, points = NA, matches_played = NA,
                            goals_for = NA, goals_against = NA, goal_diff = NA)
                 if(match_day == 0) {
                     data <- data  %>%
                         mutate(points = 0, matches_played = 0,
                                goals_for = 0, goals_against = 0, goal_diff = 0)
                 }
                 return(data)
            }
            by_season <- function(ratings) {
                ratings %>%
                    mutate(
                       data = purrr::map2(match_day, data, .f = by_match_day))
            }

            self$data <- self$data %>%
                mutate(standings = purrr::map(ratings, by_season))
        },

        get_match_day_results = function(season, match_day) {
            self$data %>%
                filter(season == get("season")) %>%
                tidyr::unnest(results) %>%
                filter(match_day == get("match_day")) %>%
                tidyr::unnest(data)
        },

        get_match_day_standings = function(season, match_day) {
            self$data %>%
                filter(season == get("season")) %>%
                select(standings) %>%
                tidyr::unnest() %>%
                filter(match_day == get("match_day")) %>%
                select(data) %>%
                tidyr::unnest()
        },

        update_match_day = function(match_day) {


            match_day_results <- self$get_match_day_results(self$season, match_day)
            previous_standings <- self$get_match_day_standings(self$season,
                                                        match_day - 1)

            match_day_results <- match_day_results %>%
                mutate(
                    match_played = !is.na(points),
                    p_score = ifelse(!match_played, 0, p_score),
                    o_score = ifelse(!match_played, 0, o_score),
                    points = ifelse(!match_played, 0, points))


            updated_standings <- previous_standings %>%
                left_join(match_day_results %>%
                              select(p_team, result_points = points,
                                     match_played, p_score, o_score),
                          by = "p_team") %>%
                mutate(
                    points = points + result_points,
                    goals_for = goals_for + p_score,
                    goals_against = goals_against + o_score,
                    goal_diff = goals_for - goals_against,
                    matches_played = matches_played + match_played
                    ) %>%
                arrange(desc(points), goal_diff) %>%
                mutate(position = seq(nrow(.))) %>%
                select(-result_points, -match_played, -p_score, -o_score)

            self$data$standings[[self$season]] <- self$data$standings[[self$season]] %>%
                mutate(data = purrr::map2(
                    match_day, data,
                    ~ if(.x == match_day) updated_standings else .y))
        },

        update_season = function(season) {
            self$season <- season
            matches <- seq(self$data$match_days_complete[self$season])
            purrr::walk(matches, self$update_match_day)
        },

        update_all_seasons = function() {
            self$initialize_standings()
            purrr::walk(seq(nrow(self$data)), self$update_season)
            serie_a <- self$data
            save(serie_a, file = "data/serie_a.rData")
            save(serie_a, file = "R/app/serie_a.rData")
        }

    )
)

