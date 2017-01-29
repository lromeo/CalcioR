library(ggvis)


navbarPage("Serie A",

           tabPanel("Position",
                    fluidRow(
                        wellPanel(
                            radioButtons("season", "Season:",
                                         c("2013/14" = 1,
                                           "2014/15" = 2,
                                           "2015/16" = 3,
                                           "2016/17" = 4))
                        ),
                        fluidRow(
                            column(12,
                                   ggvisOutput("position_vis")))
                    )
           ),
           tabPanel("Rating",
                       fluidRow(
                           fluidRow(
                               column(12,
                                      ggvisOutput("rating_vis")))
                       )
                       )
)
