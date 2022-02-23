# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # set ggplot theme
    thematic_shiny()
    
    # main multilex data
    ml_data <- reactive({ get_multilex() })
    
    # reactive logs (trigger upon button click)
    logs <- eventReactive(
        input$responses_filter_button, ignoreNULL = FALSE, {
            ml_data()$logs %>% 
                get_longitudinal(input$responses_is_longitudinal) %>% 
                # get only complete responses from participants in the selected 
                # age range, language profile, and date range
                filter(
                    completed,
                    between(age, input$responses_age_range[1], input$responses_age_range[2]),
                    lp %in% input$responses_lp,
                    between(time_stamp, input$responses_time_stamp[1], input$responses_time_stamp[2]),
                    between(date_birth, input$responses_date_birth[1], input$responses_date_birth[2])
                )  
        })
    
    vocabulary <- eventReactive(
        input$vocabulary_filter_button, ignoreNULL = FALSE, {
            
            logs <- ml_data()$logs %>% 
                get_longitudinal(input$vocabulary_is_longitudinal) %>% 
                # get only complete responses from participants in the selected 
                # age range, language profile, and date range
                filter(
                    completed,
                    between(age, input$vocabulary_age_range[1], input$vocabulary_age_range[2]),
                    lp %in% input$vocabulary_lp,
                    between(time_stamp, input$vocabulary_time_stamp[1], input$vocabulary_time_stamp[2]),
                    between(date_birth, input$vocabulary_date_birth[1], input$vocabulary_date_birth[2])
                )  
            
            left_join(logs, ml_data()$vocabulary) %>% 
                filter(type %in% input$vocabulary_type)
            
        })
    
    # participant searcher
    participants <- eventReactive(
        input$participants_filter_button, ignoreNULL = FALSE, {
            ml_data()$logs %>% 
                get_longitudinal(input$participants_is_longitudinal) %>% 
                # get only complete responses from participants in the selected 
                # age range, language profile, and date range
                filter(
                    completed,
                    id %in% input$participants_id,
                    between(age, input$participants_age_range[1], input$participants_age_range[2]),
                    lp %in% input$participants_lp,
                    between(time_stamp, input$participants_time_stamp[1], input$participants_time_stamp[2]),
                    between(date_birth, input$participants_date_birth[1], input$participants_date_birth[2])
                )  
        })
    
    
    # table with N by age, LP, and dominance
    output$participants_id_table <- renderTable({
        participants() %>%
            select(
                id, id_db, code, time, version, time_stamp, date_birth, age,
                lp, doe_catalan, doe_spanish, doe_others
            ) %>% 
            arrange(desc(time_stamp)) %>% 
            mutate_at(vars(starts_with("doe")), percent) %>% 
            mutate(age = round(age, 2)) %>% 
            head(20)
    }, width = "100%", na = "-")
    
    # table with N by age, LP, and dominance
    output$responses_n_age_table <- renderTable({
        logs() %>%
            mutate(age_group = cut_width(round(age, 0), 4)) %>% 
            count(age_group, lp) %>% 
            pivot_wider(names_from = lp, values_from = n, values_fill = 0) %>% 
            mutate(Total = rowSums(across(where(is.numeric)))) %>% 
            rename(Age = age_group)
    }, width = "100%", na = "-", digits = 0)
    
    # plot with N by LP, and dominance
    output$responses_n_plot <- renderPlot({
        
        logs() %>% 
            # compute sample sizes
            count(lp, dominance) %>%
            # make plot
            ggplot() + 
            aes(dominance, n, fill = dominance, label = n) +
            facet_wrap(vars(lp)) + 
            geom_col(position = position_dodge(width = 1)) +
            geom_text(colour = "white", position = position_stack(vjust = 0.5)) +
            labs(
                x = "Language profile", 
                y = "Number of responses", 
                colour = "Dominance", 
                fill = "Dominance"
            ) +
            theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.x = element_blank(),
                legend.position = "none"
            )
        
    }, res = 96)
    
    # table with N by age, LP, and dominance
    output$responses_n_age_table <- renderTable({
        logs() %>%
            mutate(age_group = cut_width(round(age, 0), 4)) %>% 
            count(age_group, lp) %>% 
            pivot_wider(names_from = lp, values_from = n, values_fill = 0) %>% 
            mutate(Total = rowSums(across(where(is.numeric)))) %>% 
            rename(Age = age_group)
    }, width = "100%", na = "-", digits = 0)
    
    # plot with N by age, LP, and dominance
    output$responses_n_age_plot <- renderPlot({
        logs() %>% 
            mutate(age_group = round(age, 0)) %>% 
            count(age_group, lp, dominance) %>% 
            ggplot() + 
            aes(age_group, n, fill = dominance) +
            facet_wrap(vars(lp)) + 
            geom_col() +
            labs(
                x = "Age (months)", 
                y = "Number of responses", 
                colour = "Dominance", 
                fill = "Dominance"
            ) +
            theme(
                legend.position = "top"
            )
        
    }, res = 96)
    
    # plot vocabulary size by age
    output$vocabulary_total_plot <- renderPlot({
        vocabulary() %>% 
            ggplot() + 
            aes(age, vocab_prop_total, colour = lp) +
            geom_point(size = 1, alpha = 0.5) + 
            labs(
                x = "Age (months)", 
                y = "Vocabulary size", 
                colour = "Language profile", 
                fill = "Language profile"
            ) +
            scale_y_continuous(limits = c(0, 1), labels = percent) + 
            theme(
                legend.position = "top",
                legend.title = element_blank()
            )
        
    }, res = 96)
    
    output$vocabulary_total_data <- renderTable({
        nearPoints(
            vocabulary() %>% 
                select(id, age, vocab_prop_total),
            input$vocabulary_total_plot_hover,
            xvar = "age",
            yvar = "vocab_prop_total"
        )
    })
    
    
    output$vocabulary_dominance_plot <- renderPlot({
        vocabulary() %>% 
            pivot_longer(
                c(vocab_prop_dominance_l1, vocab_prop_dominance_l2),
                names_transform = list(name = ~str_to_sentence(str_remove(., "vocab_prop_dominance_")))
            ) %>% 
            ggplot() + 
            facet_wrap(vars(name)) +
            aes(age, vocab_prop_total, colour = lp) +
            geom_point(size = 1, alpha = 0.5) + 
            labs(
                x = "Age (months)", 
                y = "Vocabulary size", 
                colour = "Language profile", 
                fill = "Language profile"
            ) +
            scale_y_continuous(limits = c(0, 1), labels = percent) + 
            theme(
                legend.position = "top",
                legend.title = element_blank()
            )
        
    }, res = 96)
    
    output$vocabulary_dominance_data <- renderTable({
        nearPoints(
            vocabulary() %>% 
                pivot_longer(
                    c(vocab_prop_dominance_l1, vocab_prop_dominance_l2),
                    names_transform = list(name = ~str_to_sentence(str_remove(., "vocab_prop_dominance_")))
                ) %>% 
                select(id, age, name, value),
            input$vocabulary_dominance_plot_hover, 
            xvar = "age", 
            yvar = "value"
        )
    })
    
    # plot vocabulary size by age
    output$vocabulary_conceptual_plot <- renderPlot({
        vocabulary() %>% 
            ggplot() + 
            aes(age, vocab_prop_conceptual, colour = lp) +
            geom_point(size = 1, alpha = 0.5) + 
            labs(
                x = "Age (months)", 
                y = "Vocabulary size", 
                colour = "Language profile", 
                fill = "Language profile"
            ) +
            scale_y_continuous(limits = c(0, 1), labels = percent) + 
            theme(
                legend.position = "top",
                legend.title = element_blank()
            )
        
    }, res = 96)
    
    output$vocabulary_conceptual_data <- renderTable({
        nearPoints(
            vocabulary()  %>% 
                select(id, age, vocab_prop_conceptual), 
            input$vocabulary_conceptual_plot_hover,
            xvar = "age", 
            yvar = "vocab_prop_conceptual"
        )
    })
    
    output$vocabulary_te_plot <- renderPlot({
        vocabulary() %>% 
            ggplot() + 
            aes(age, vocab_prop_conceptual, colour = lp) +
            geom_point(size = 1, alpha = 0.5) + 
            labs(
                x = "Age (months)", 
                y = "Vocabulary size", 
                colour = "Language profile", 
                fill = "Language profile"
            ) +
            scale_y_continuous(limits = c(0, 1), labels = percent) + 
            theme(
                legend.position = "top",
                legend.title = element_blank()
            )
        
    }, res = 96)
    
    output$vocabulary_te_data <- renderTable({
        nearPoints(
            vocabulary() %>% 
                select(id, age, vocab_prop_te),
            input$vocabulary_te_plot_hover, 
            xvar = "age", 
            yvar = "vocab_prop_te"
        )
    })
    
    
    # norms (word prevalence) ----
    norms <- eventReactive(
        input$norms_filter_button, ignoreNULL = FALSE, {
            ml_data()$norms %>% 
                filter(
                    label %in% input$norms_label,
                    between(age_bin, input$norms_age_range[1], input$norms_age_range[2]),
                    lp %in% input$norms_lp,
                    type %in% input$norms_type,
                    item_dominance %in% input$norms_item_dominance
                )  %>% 
                group_by(te, label) %>% 
                summarise(
                    yes = sum(yes, na.rm = TRUE),
                    n = sum(n, na.rm = TRUE),
                    .groups = "drop"
                )
        })
    
    
    output$norms_plot <- renderPlot({
            plot_bayes_grid(
                yes = norms()$yes,
                n = norms()$n, 
                prior_alpha = input$norms_prior_alpha, 
                prior_beta = input$norms_prior_beta
            )
    })
    
    
    
    
    
}