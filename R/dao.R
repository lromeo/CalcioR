dao <- R6::R6Class(
    "dao",
    public = list(
        con = dbConnect(RSQLite::SQLite(), dbname="sport.db"),
        required_tables = c("teams", "events", "games", "rounds"),
        tables = list(),
        data = NULL,

        query = function(q) {
            DBI::dbGetQuery(self$con, q) %>%
                tbl_df()
        },
        get_table = function(table_name) {
            q <- sprintf("SELECT * FROM %s", table_name)
            self$tables[[table_name]] <- self$query(q)

        },
        get_all_tables = function() {
            purrr::walk(self$required_tables, self$get_table)
        },
        join_tables = function() {
            self$data <- self$tables$games %>%
                select(team1_id, team2_id, round_id, score1, score2, winner,
                       home) %>%
                left_join(self$tables$rounds %>%
                              select(id, event_id, title, match_day = pos),
                          by = c("round_id" = "id")) %>%
                left_join(self$tables$events %>%
                              select(season = key, id),
                          by = c("event_id" = "id")) %>%
                left_join(self$tables$teams %>%
                              select(id, team1_key = key),
                          by = c("team1_id" = "id")) %>%
                left_join(self$tables$teams %>%
                              select(id, team2_key = key),
                          by = c("team2_id" = "id")) %>%
                select(-team1_id, -team2_id)
        },
        get_data = function() {
            self$get_all_tables()
            self$join_tables()
        }
    )
)
