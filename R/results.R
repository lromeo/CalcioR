
results <- R6::R6Class(
    "results",
    public = list(
        con = dbConnect(RSQLite::SQLite(), dbname="sport.db"),

        load_table = function(table_name) {
            "SELECT * FROM %s" %>%
                sprintf(table_name) %>%
                DBI::dbGetQuery(self$con, .) %>%
                tbl_df()
        },

        get_season_results = function() {
            required_tables <- c("teams", "events", "games", "rounds")
            tables <- purrr::map(required_tables, self$load_table) %>%
                purrr::set_names(required_tables)

            season_results <- tables$games %>%
                select(team1_id, team2_id, round_id, score1, score2, winner,
                       home) %>%
                left_join(tables$rounds %>%
                              select(id, event_id, title),
                          by = c("round_id" = "id")) %>%
                left_join(tables$events %>%
                              select(season = key, id),
                          by = c("event_id" = "id")) %>%
                left_join(tables$teams %>%
                              select(id, team1_key = key),
                          by = c("team1_id" = "id")) %>%
                left_join(tables$teams %>%
                              select(id, team2_key = key),
                          by = c("team2_id" = "id"))
            return(season_results)
        },

        extract_match_day = function(title) {
            ifelse(title == "Genoa       1-0 Fiorentina  (3.Giornata)        15.12.",
                   "15^ Giornata", title) %>%
                stringr::str_extract("^[0-9]+") %>%
                as.numeric()
        },
        extract_season = function(season) {
            stringr::str_extract(season, "[0-9]{4}") %>%
                as.numeric() - 2012
        },
        remove_extra_matches = function(season_results) {
            season_results %>%
                filter(!(season == 3 & match_day > 35 & is.na(score1)))
        },
        gather_teams = function(season_results) {
            bind_rows(
                season_results %>%
                    rename(p_team = team1_key, o_team = team2_key, p_score = score1,
                           o_score = score2) %>%
                    mutate(p_home = TRUE),
                season_results %>%
                    rename(p_team = team2_key, o_team = team1_key, p_score = score2,
                           o_score = score1) %>%
                    mutate(p_home = FALSE))
        },
        distinct_season_teams = function(season_results) {
            season_results %>%
                mutate(teams = purrr::map(results, ~ .x$data %>%
                                              purrr::flatten_df() %>%
                                              distinct(p_team)))
        },
        find_max_match_day = function(results, n_teams = 20, max_missing = 2)
            {
            results %>%
                mutate(match_complete =
                           purrr::map_dbl(data, ~ sum(!is.na(.x$p_score)))) %>%
                filter(match_complete >= n_teams - max_missing) %>%
                .$match_day %>%
                max()
        },

        format_season_results = function(season_results) {
            season_results %>%
                mutate(
                    match_day = self$extract_match_day(title),
                    season =  self$extract_season(season)) %>%
                self$remove_extra_matches() %>%
                select(team1_key, team2_key, score1, score2, match_day, season) %>%
                self$gather_teams() %>%
                mutate(points = ifelse(p_score > o_score, 3,
                                       ifelse(p_score == o_score, 1, 0))) %>%
                tidyr::nest(-season, -match_day, .key = "data") %>%
                tidyr::nest(-season, .key = "results") %>%
                self$distinct_season_teams() %>%
                mutate(match_days_complete =
                           purrr::map_dbl(results, self$find_max_match_day)) %>%
                arrange(season)
        },

        save_season_results = function() {
            season_results <- self$get_season_results() %>%
                self$format_season_results()
            save(season_results, file = "data/season_results.rData")
        }

    )
)
