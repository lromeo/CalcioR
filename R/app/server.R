library(ggvis)
library(dplyr)
library(forcats)

load('serie_a.rData')

ratings <- serie_a %>%
    select(ratings, season) %>%
    tidyr::unnest() %>%
    tidyr::unnest()

standings <- serie_a %>%
    select(standings, season) %>%
    tidyr::unnest() %>%
    tidyr::unnest()

all <- ratings %>%
    left_join(standings, by = c("season", "match_day", "p_team")) %>%
    left_join(serie_a %>% select(season, match_days_complete)) %>%
    mutate(p_team = factor(p_team)) %>%
    mutate(match_day_total = 38*(season - 1) + match_day)


function(input, output, session) {
    tool_tip_position <- function(df) {
        paste(paste("<b>", df$p_team, "</b>"),
              paste("Points:", df$points),
              paste("Rating:", round(as.numeric(df$r))),
              sep = "<br>")
    }

    position_vis <- reactive({
        all %>%
            filter(season == input$season, match_day > 0,
                   match_day <= match_days_complete)  %>%
            ggvis(~match_day, ~ points, stroke = ~ p_team) %>%
            layer_lines() %>%
            layer_points(size = ~r, fill = ~p_team, fillOpacity := 0.75,
                         fillOpacity.hover := 1, strokeOpacity := 0) %>%
            add_tooltip(tool_tip_position, "hover") %>%
            add_axis("x", title = "Match Day") %>%
            add_axis("y", title = "Points") %>%
            add_legend(c("fill", "stroke"), title = "Team") %>%
            add_legend("size", title = "Rating",
                       properties = legend_props(legend = list(x = 20))) %>%
            set_options(duration=0, height = 480, width = 800)
    })

    position_vis %>% bind_shiny("position_vis")

    tool_tip_rating <- function(df) {
        if(is.null(df$p_team) || is.null(df$r)) return("Season Break")
        df <- all %>%
            filter(match_day > 0) %>%
            filter(p_team == df$p_team, match_day_total == df$match_day_total)

        if(df$match_day_total == 1) {
            return(paste0("<b>", df$p_team, "</b>"))
        }

        paste(
            paste0("<b>", df$p_team, "</b>"),
            paste("Rating:", round(as.numeric(df$r))),
            paste("Season:", as.character(df$season)),
            paste("Match Day:", df$match_day),
            sep = "<br>")
    }

    rating_vis <- reactive({

        all %>%
            filter(match_day > 0,
                   match_day <= match_days_complete) %>%
            bind_rows(
                data_frame(
                    y_range = c(rep(1300, nrow(.) -1), 1800),
                    season_1_end = 38 + .5,
                    season_2_end = 38*2 + .5,
                    season_3_end = 38*3 + .5
                )) %>%
            ggvis(~ match_day_total, ~ r, stroke = ~ p_team) %>%
            # layer_points(fill = ~ p_team, size := 120, fillOpacity := 0.0,
            #              fillOpacity.hover := 1, strokeOpacity := 0) %>%
            layer_lines(strokeWidth := 4,
                        strokeOpacity.hover :=1,
                        strokeOpacity := 0.3) %>%
            add_tooltip(tool_tip_rating, "hover") %>%
            layer_lines(x = ~ season_1_end, y = ~ y_range, stroke := "grey",
                        strokeWidth := 3, strokeOpacity := .75) %>%
            layer_lines(x = ~ season_2_end, y = ~ y_range, stroke := "grey",
                        strokeWidth := 3, strokeOpacity := .75) %>%
            layer_lines(x = ~ season_3_end, y = ~ y_range, stroke := "grey",
                        strokeWidth := 3, strokeOpacity := .75) %>%
            add_axis("x", title = "Match Day") %>%
            add_axis("y", title = "Rating") %>%
            hide_legend(c("stroke", "fill")) %>%
            set_options(duration=0, height = 600, width = 1000)

    })

    rating_vis %>% bind_shiny("rating_vis")


}
