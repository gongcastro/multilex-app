library(shiny)
library(bslib)
library(thematic)
library(shinycssloaders)
library(multilex)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(stringr)
library(scales)
library(gt)
library(here)

ml_connect() # connect to multilex database

ml_data <- get_multilex()
ml_data$logs <- arrange(ml_data$logs, time_stamp)

# for log scale in slider input
js_code_1 <-
    "$(function() {
setTimeout(function(){
var vals = [0];
var powStart = -2;
var powStop = 2;
for (i = powStart; i <= powStop; i++) {
var val = Math.pow(10, i);
val = parseFloat(val.toFixed(8));
vals.push(val);
}
$('#norms_prior_alpha').data('ionRangeSlider').update({'values':vals})
}, 10)})"

js_code_2 <-
    "$(function() {
setTimeout(function(){
var vals = [0];
var powStart = -2;
var powStop = 2;
for (i = powStart; i <= powStop; i++) {
var val = Math.pow(10, i);
val = parseFloat(val.toFixed(8));
vals.push(val);
}
$('#norms_prior_beta').data('ionRangeSlider').update({'values':vals})
}, 10)})"

# # set custom ggplot2 theme
# theme_set(
#     theme_minimal() +
#         theme(
#             axis.text = element_text(colour = "black"), 
#             panel.grid.major.x = element_blank(), 
#             panel.grid.minor.x = element_blank(), 
#             panel.border = element_rect(
#                 fill = NA,
#                 colour = "grey", 
#                 size = 0.75
#             )
#         )
# )

# define user interface 
ui <- fluidPage(
    # theme = bs_theme(
    #     bg = "white",
    #     fg = "#c8102e",
    #     primary = "#c8102e",
    #     secondary = "#1b3187", 
    # ),
    
    
    navbarPage(
        "multilex",
        
        tabPanel(
            "Participants",
            titlePanel("Participants"),
            br(),
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    selectInput("participants_id", label = "Participant (ID)", selected = unique(ml_data$logs$id)[1:20], choices = unique(ml_data$logs$id), multiple = TRUE, selectize = TRUE),
                    selectInput("participants_is_longitudinal", label = "Longitudinal?", selected = "all", choices = c("all", "no", "first", "last")),
                    sliderInput("participants_age_range", label = "Age (months)", value = c(0, 54), min = 0, max = 54, dragRange = TRUE),
                    selectInput("participants_lp", label = "Language profile", choices = c("Monolingual", "Bilingual", "Other"), selected = c("Monolingual", "Bilingual", "Other"), multiple = TRUE, selectize = TRUE),
                    dateRangeInput("participants_time_stamp", label = "Time stamp", start = as.Date("2019-10-22"), end = today(), min = as.Date("2019-10-22"), max = today()),
                    dateRangeInput("participants_date_birth", label = "Time stamp", start = as.Date("2015-09-30"), end = today(), min = as.Date("2015-09-30"), max = today()),
                    actionButton("participants_filter_button", label = "Apply", class = "btn-primary")
                ),
                mainPanel(
                    width = 10,
                    withSpinner(tableOutput("participants_id_table"))
                )
            )
        ),
        
        tabPanel(
            "Responses",
            titlePanel("Responses"),
            br(),
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    selectInput("responses_is_longitudinal", label = "Longitudinal?", selected = "all", choices = c("all", "no", "first", "last")),
                    sliderInput("responses_age_range", label = "Age (months)", value = c(0, 54), min = 0, max = 54, dragRange = TRUE),
                    selectInput("responses_lp", label = "Language profile", choices = c("Monolingual", "Bilingual", "Other"), selected = c("Monolingual", "Bilingual", "Other"), multiple = TRUE, selectize = TRUE),
                    dateRangeInput("responses_time_stamp", label = "Time stamp", start = as.Date("2019-10-22"), end = today(), min = as.Date("2019-10-22"), max = today()),
                    dateRangeInput("responses_date_birth", label = "Time stamp", start = as.Date("2015-09-30"), end = today(), min = as.Date("2015-09-30"), max = today()),
                    actionButton("responses_filter_button", label = "Apply", class = "btn-primary")
                ),
                mainPanel(
                    width = 10,
                    tabsetPanel(
                        id = "responses_panel", selected = "Summary", type = "pills",
                        tabPanel("Summary", plotOutput("responses_n_plot")),
                        tabPanel(
                            "Age", 
                            column(width = 2, withSpinner(tableOutput("responses_n_age_table"))),
                            column(width = 10, withSpinner(plotOutput("responses_n_age_plot")))
                        )
                    )
                )
            )
        ),
        
        tabPanel(
            "Vocabulary",
            titlePanel("Vocabulary"),
            br(),
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    selectInput("vocabulary_is_longitudinal", label = "Longitudinal?", selected = "all", choices = c("all", "no", "first", "last")),
                    sliderInput("vocabulary_age_range", label = "Age (months)", value = c(0, 54), min = 0, max = 54, dragRange = TRUE),
                    selectInput("vocabulary_lp", label = "Language profile", choices = c("Monolingual", "Bilingual", "Other"), selected = c("Monolingual", "Bilingual", "Other"), multiple = TRUE, selectize = TRUE),
                    sliderInput("vocabulary_time_stamp", label = "Time stamp", value = c(as.Date("2019-10-22"), today()), min = as.Date("2019-10-22"), max = today(), dragRange = TRUE),
                    sliderInput("vocabulary_date_birth", label = "Time stamp", value = c(as.Date("2015-09-30"), today()), min = as.Date("2015-09-30"), max = today(), dragRange = TRUE),
                    selectInput("vocabulary_type", label = "Type", selected = "all", choices = c("Comprehension" = "understands", "Production" = "produces"), multiple = FALSE),
                    actionButton("vocabulary_filter_button", label = "Apply", class = "btn-primary")
                ),
                mainPanel(
                    width = 10,
                    tabsetPanel(
                        id = "vocabulary_panel", selected = "Total", type = "pills",
                        tabPanel(
                            "Total",
                            column(width = 4, withSpinner(plotOutput("vocabulary_total_plot", hover = "vocabulary_total_plot_hover"))),
                            column(width = 2, withSpinner(tableOutput("vocabulary_total_data")))
                        ),
                        tabPanel(
                            "By language",
                            column(width = 6, withSpinner(plotOutput("vocabulary_dominance_plot", hover = "vocabulary_dominance_plot_hover"))),
                            column(width = 2, withSpinner(tableOutput("vocabulary_dominance_data")))
                        ),
                        tabPanel(
                            "Conceptual",
                            column(width = 4, withSpinner(plotOutput("vocabulary_conceptual_plot", hover = "vocabulary_conceptual_plot_hover"))),
                            column(width = 2, withSpinner(tableOutput("vocabulary_conceptual_data")))
                        ),                        
                        tabPanel(
                            "Translation equivalents",
                            column(width = 4, withSpinner(plotOutput("vocabulary_te_plot", hover = "vocabulary_te_plot_hover"))),
                            column(width = 2, withSpinner(tableOutput("vocabulary_te_data")))
                        )                      
                    )
                )
            )
        ),
        tabPanel(
            "Norms",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    selectInput("norms_label", label = "Item", selected = "casa", choices = unique(ml_data$norms$label)),
                    sliderInput("norms_age_range", label = "Age (months)", value = c(8, 40), min = 8, max = 40, dragRange = TRUE),
                    selectInput("norms_lp", label = "Language profile", choices = c("Monolingual", "Bilingual", "Other"), selected = c("Monolingual", "Bilingual", "Other"), multiple = TRUE, selectize = TRUE),
                    selectInput("norms_type", label = "Type", selected = "Comprehension", choices = c("Comprehension" = "understands", "Production" = "produces"), multiple = FALSE),
                    selectInput("norms_item_dominance", label = "Dominance", selected = "L1", choices = c("L1", "L2"), multiple = TRUE),
                    wellPanel(
                        tags$head(tags$script(HTML(js_code_1))),
                        "Prior: Beta(α, β)",
                        sliderInput("norms_prior_alpha", label = "α", value = 5, min = 0, max = 100, step = 0.1, ticks = TRUE),
                        sliderInput("norms_prior_beta", label = "β", value = 5, min = 0, max = 1000, step = 0.1, ticks = TRUE),
                    ),
                    actionButton("norms_filter_button", label = "Apply", class = "btn-primary")
                ),
                mainPanel(
                    width = 9,
                    withSpinner(plotOutput("norms_plot"))
                )
            )
        ),
        navbarMenu(
            "Other",
            tabPanel("Authors"),
            tabPanel("GitHub"),
            tabPanel("multilex R package")
        )
        
    )
)
