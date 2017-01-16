library(dplyr)
library(stringr)
library(purrr)
library(RSQLite)

load_table <- function(table_name, con) {
    "SELECT * FROM %s" %>%
        sprintf(table_name) %>%
        dbGetQuery(con, .) %>%
        tbl_df()
}

get_results <- function() {
    required_tables <- c("teams", "events", "games", "rounds")
    tables <- purrr::map(required_tables, load_table,
                         con = dbConnect(RSQLite::SQLite(), dbname="sport.db")) %>%
        purrr::set_names(required_tables)

    results <- tables$games %>%
        select(team1_id, team2_id, round_id, score1, score2, winner, home) %>%
        left_join(tables$rounds %>% select(id, event_id, title),
                  by = c("round_id" = "id")) %>%
        left_join(tables$events %>% select(season = key, id),
                  by = c("event_id" = "id")) %>%
        left_join(tables$teams %>% select(id, team1_key = key),
                  by = c("team1_id" = "id")) %>%
        left_join(tables$teams %>% select(id, team2_key = key),
                  by = c("team2_id" = "id"))
    return(results)
}

format_results <- function(results) {

    extract_match_day <- function(title) {
        ifelse(title == "Genoa       1-0 Fiorentina  (3.Giornata)        15.12.",
               "15^ Giornata", title) %>%
            str_extract("^[0-9]+") %>%
            as.numeric()
    }
    extract_season <- function(season) {
        str_extract(season, "[0-9]{4}") %>%
            as.numeric() - 2012
    }
    remove_extra_matches <- function(results) {
        results %>%
            filter(!(season == 3 & match_day > 35 & is.na(score1)))
    }

    results_clean <- results %>%
        mutate(
            match_day = extract_match_day(title),
            season =  extract_season(season)) %>%
        remove_extra_matches() %>%
        select(team1_key, team2_key, score1, score2, match_day, season) %>%
        arrange(season, match_day)

    results_long <- bind_rows(
        results_clean %>%
            rename(p_team = team1_key, o_team = team2_key, p_score = score1,
                   o_score = score2) %>%
            mutate(p_home = TRUE),
        results_clean %>%
            rename(p_team = team2_key, o_team = team1_key, p_score = score2,
                   o_score = score1) %>%
            mutate(p_home = FALSE))
    results_long %>%
        tidyr::nest(-season, .key = "results") %>%
        mutate(teams = purrr::map(results, ~ distinct(.x, p_team)))
}

save_results <- function() {
    results <- get_results() %>%
        format_results()
    save(results, file = "data/results.rData")
}
