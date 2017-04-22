seasons <- R6::R6Class(
    "seasons",
    public = list(

        n_teams = 20,
        max_missing = 2,
        data = NULL,

        initialize = function(data) {
            self$data <- data
        },

        sum_matches_complete = function(matches) {
            sum(!is.na(matches$p_score))
        },
        season_max_match_day = function(results) {
            results %>%
                mutate(match_complete = purrr::map_dbl(data,
                                                       self$sum_matches_complete)) %>%
                filter(match_complete >= self$n_teams - self$max_missing) %>%
                .$match_day %>%
                max()
        },
        season_distinct_teams = function(results) {
            results$data %>%
                purrr::flatten_df() %>%
                distinct(p_team)
        },
        add_match_days_complete = function() {
            self$data <-  self$data %>%
                mutate(match_days_complete =
                           purrr::map_dbl(results, self$season_max_match_day))
        },
        add_teams = function() {
            self$data <-  self$data %>%
                mutate(teams = purrr::map(results,
                                          self$season_distinct_teams))

        },
        summarize = function() {
            self$add_match_days_complete()
            self$add_teams()
        }
    )
)
