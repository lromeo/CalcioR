
update_serie_a <- function() {
    d <- dao$new()
    d$get_data()

    res <- results$new(foo$data)
    res$update_all_seasons()

    sea <- seasons$new(results$data)
    sea$update_all_seasons()

    rat <- ratings$new(seasons$data)
    rat$update_all_seasons()

    stand <- standings$new(rat$data)
    stand$update_all_seasons()

    serie_a <- stand$data
    save(serie_a, file = "data/serie_a.rData")
    save(serie_a, file = "R/app/serie_a.rData")
}

# serie_a %>% select(season, results) %>% tidyr::unnest(results) %>% tidyr::unnest(data)
# serie_a %>% select(season, ratings) %>% tidyr::unnest(ratings) %>% tidyr::unnest(data)
# serie_a %>% select(season, standings) %>% tidyr::unnest(standings) %>% tidyr::unnest(data)
#
