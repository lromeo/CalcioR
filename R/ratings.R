ratings <- R6::R6Class(
    "ratings",
    public =  list(

        data = NA,
        season = NA,
        match_day = NA,
        after_season_revert = 0.25,

        initialize = function(data) {
            self$data <- data
        },

        calc_w_e = function(p_r0, o_r0, p_home, home_r_adv = 100) {
            home_advantage <- ifelse(is.na(p_home), 0,
                                     ifelse(p_home, home_r_adv,
                                            -home_r_adv))

            r_diff <- (p_r0 + home_advantage) - (o_r0)

             w_e <- 1/(10^(-r_diff/400) + 1)
            return(w_e)
        },
        r_update = function(p_r0, o_r0, p_score, o_score, p_home, k = 20) {
            w_e <- self$calc_w_e(p_r0, o_r0, p_home)

            w <- ifelse(p_score > o_score, 1,
                        ifelse(p_score == o_score, 0.5,
                               0))

            score_diff <- abs(p_score - o_score)

            g <- ifelse(score_diff < 2, 1,
                ifelse(score_diff == 2, 1.5,
                       (11 + score_diff)/8))

            p_r1 <- p_r0 + k*g*(w - w_e)
            return(p_r1)
        },

        teams_start_1 = function() {
            mutate(self$data$teams[[self$season]], r = 1500)
        },
        teams_start = function() {

            last_season_end <- self$data$ratings[[self$season - 1]] %>%
                filter(match_day == max(match_day)) %>%
                tidyr::unnest(data) %>%
                select(p_team, r) %>%
                mutate(r = r + (1500 - r) * self$after_season_revert)

            this_season <- self$data$teams[[self$season]]

            new_teams <- this_season %>%
                anti_join(last_season_end, by = "p_team")
            dropped_teams <- last_season_end %>%
                anti_join(this_season, by = "p_team")

            new_teams_start <- new_teams %>%
                mutate(r = mean(dropped_teams$r))
            return_teams_start <- last_season_end %>%
                semi_join(this_season, by = "p_team")

            bind_rows(new_teams_start,return_teams_start)
        },

        find_previous_r = function(l) {
            self$data$ratings[[self$season]] %>%
                filter(match_day == self$match_day - 1) %>%
                tidyr::unnest(data) %>%
                filter(p_team == l$p_team) %>%
                .$r
        },
        find_previous_r_o = function(l) {
            l$p_team <- self$data$results[[self$season]] %>%
                filter(match_day == self$match_day) %>%
                tidyr::unnest(data) %>%
                filter(p_team == l$p_team) %>%
                .$o_team
            self$find_previous_r(l)
        },
        find_team_results = function(l) {
            self$data$results[[self$season]] %>%
                filter(match_day == self$match_day) %>%
                tidyr::unnest(data) %>%
                filter(p_team == l$p_team)
        },

        initialize_na_ratings = function() {

            by_season <- function(teams) {
                n_matches = nrow(teams) * 2 - 2
                teams <- teams %>% mutate(r = NA_real_)

                data_frame(
                    match_day = seq(n_matches),
                    data = purrr::rerun(n_matches, teams))
            }

            self$data <- self$data %>%
                mutate(ratings = purrr::map(teams, by_season))
        },
        initialize_season_ratings = function() {
            if(self$season == 1) {
                teams_start <- self$teams_start_1()
            } else {
                teams_start <- self$teams_start()
            }
            self$data$ratings[[self$season]] <-
                bind_rows(
                    self$data$ratings[[self$season]],
                    tibble(match_day = 0, data = list(teams_start)))
        },

        update_team = function(team_data) {
            p_r0 <- self$find_previous_r(team_data)
            o_r0 <- self$find_previous_r_o(team_data)
            team_results <- self$find_team_results(team_data)

            if(is.na(team_results$p_score)) {
                r_updated <- p_r0
            } else {
                r_updated = self$r_update(p_r0, o_r0, team_results$p_score,
                                     team_results$o_score, team_results$p_home)
            }
            team_data %>% mutate(r = r_updated)
        },
        update_match_day = function(match_day) {

            self$match_day <- match_day

            match_day_ratings_update <- self$data$teams[[self$season]] %>%
                group_by(p_team) %>%
                do(self$update_team(.)) %>%
                ungroup()

            self$data$ratings[[self$season]]$data[[self$match_day]] <- match_day_ratings_update
        },
        update_season = function(season) {
            self$season <- season
            self$initialize_season_ratings()
            matches <- seq(self$data$match_days_complete[self$season])
            purrr::walk(matches, self$update_match_day)
        },
        update_all_seasons = function() {
            self$initialize_na_ratings()
            purrr::walk(seq(nrow(self$data)), self$update_season)
        }
    )
)


