library(R6)

update_round_ratings <- R6::R6Class(
    "update_match_ratings",
    public =  list(
        initialize = function(match_results) {
            self$data <- match_results
            self$initialize_ratings()
        },

        data = NA,
        season = NA,
        match_day = NA,
        after_season_revert = .25,

        calc_w_e = function(p_r0, o_r0, p_home) {
            home_advantage <- ifelse(is.na(p_home), 0,
                                     ifelse(p_home, 100, -100))
            r_diff <- (p_r0 + home_advantage) - (o_r0)
            w_e <- 1/(10^(-r_diff/400) + 1)
            return(w_e)
        },

        r_update = function(p_r0, o_r0, p_score, o_score, p_home, k = 20) {
            w_e <- self$calc_w_e(p_r0, o_r0, p_home)
            w <- ifelse(p_score > o_score, 1,
                        ifelse(p_score == o_score, 0.5, 0))
            score_diff <- abs(p_score - o_score)
            g <- ifelse(score_diff < 2, 1,
                        ifelse(score_diff == 2, 1.5,
                               (11 + score_diff)/8))
            p_r1 <- p_r0 + k*g*(w - w_e)
            return(p_r1)
        },

        initialize_ratings = function() {
            self$data <- self$data %>%
                mutate(
                    ratings = purrr::map(results,
                                         ~ distinct(.x, p_team, match_day) %>%
                                             mutate(r = NA_real_) %>%
                                             tidyr::nest(-match_day,
                                                         .key = "match_day")))
        },

        teams_start_1 = function() {
            self$data$teams[[self$season]] %>%
                mutate(r = 1500)
        },
        teams_start = function() {

            last_season_end <- self$data$ratings[[self$season - 1]]$match_day[[38]] %>%
                select(p_team, r) %>%
                mutate(r = r + (1500 - r) * self$after_season_revert)

            this_season <- self$data$teams[[self$season]]

            new_teams <- anti_join(this_season, last_season_end, by = "p_team")
            dropped_teams <- anti_join(last_season_end, this_season, by = "p_team")
            dropped_teams_ave <- mean(dropped_teams$r)

            new_teams_start <- new_teams %>%
                mutate(r = dropped_teams_ave)

            return_teams_start <- last_season_end %>%
                semi_join(this_season, by = "p_team")

            bind_rows(
                new_teams_start,
                return_teams_start)
        },

        find_previous_r = function(l) {
            previous_match_day <- ifelse(self$match_day == 1, 39,
                                         self$match_day - 1)

            self$data$ratings[[self$season]]$match_day[[previous_match_day]] %>%
                filter(p_team == l$p_team) %>%
                .$r
        },

        find_previous_r_o = function(l) {
            l$p_team <- self$data$results[[self$season]] %>%
                filter(match_day == self$match_day, p_team == l$p_team) %>%
                .$o_team
            self$find_previous_r(l)
        },

        find_team_results = function(l) {
            self$data$results[[self$season]] %>%
                filter(p_team == l$p_team, match_day == self$match_day)
        },

        initialize_season_ratings = function() {
            if(self$season == 1) {
                teams_start <- self$teams_start_1()
            } else {
                teams_start <- self$teams_start()
            }
            self$data$ratings[[self$season]] <-
                tibble::tibble(match_day =
                                   c(self$data$ratings[[self$season]]$match_day,
                                     list(teams_start)))
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
            if(match_day == 39) return()

            self$match_day <- match_day
            match_day_ratings <- self$data$ratings[[self$season]]$match_day[[match_day]]

            match_day_ratings_update <- match_day_ratings %>%
                group_by(p_team) %>%
                do(self$update_team(.))

            self$data$ratings[[self$season]]$match_day[[match_day]] <-
                match_day_ratings_update
        },

        find_max_match_day = function() {
            self$data$results[[self$season]] %>%
                group_by(match_day) %>%
                summarise(n = sum(!is.na(p_score))) %>%
                filter(n >= nrow(self$data$teams[[self$season]]) - 2) %>%
                .$match_day %>%
                max()
        },

        update_season = function(season) {
            self$season <- season
            self$initialize_season_ratings()
            max_match_day <- self$find_max_match_day()
            purrr::walk(seq(max_match_day), self$update_match_day)
        },

        update_all_seasons = function() {
            purrr::walk(seq(nrow(foo$data)), self$update_season)
        }

    )
)


# library(ggplot2)
# tmp <- foo$data$ratings %>%
#     purrr::map_df( ~ .x %>%
#                        rename(data = match_day) %>%
#                        mutate(match_day = 1:nrow(.)) %>%
#                        filter(match_day != 39) %>%
#                        tidyr::unnest(),
#                    .id = "season") %>%
#     filter(!is.na(r))
#
# ggplot(tmp, aes(x = match_day, y = r)) +
#     geom_line(aes(color = p_team)) +
#     facet_grid(. ~ season)
