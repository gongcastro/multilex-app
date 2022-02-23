# get multilex data, update if necessary
get_multilex <- function(update = FALSE){
    
    if (update){
        participants <- multilex::ml_participants()
        responses <- multilex::ml_responses(participants)
        logs <- multilex::ml_logs(participants, responses)
        vocabulary <- multilex::ml_vocabulary(participants, responses, scale = c("prop", "counts"))
        norms <- multilex::ml_norms(participants, responses)
        
        ml_data <- tibble::ls(participants, responses, logs, vocabulary, norms)
        
    } else {
        
        ml_data <- lapply(list.files(here::here("data"), full.names = TRUE), readRDS) 
        names(ml_data) <- gsub(".rds", "", list.files(here::here("data")))
        
    }
    
    return(ml_data)
}


# grid approximation for binomial counts
plot_bayes_grid <- function(yes, n, prior_alpha = 5, prior_beta = 5){
    
    grid_data <- data.frame(
        pi_grid = seq(
            from = 0.1, 
            to = 1, 
            length = 1000
        )
    ) %>% 
        mutate(
            prior = dbeta(pi_grid, prior_alpha, prior_beta),
            prior_normalised = dbeta(pi_grid, prior_alpha, prior_beta) / sum(prior, na.rm = TRUE),
            likelihood = dbinom(.env$yes, .env$n, pi_grid),
            posterior = likelihood * prior,
            posterior_normalised = posterior / sum(posterior, na.rm = TRUE)
        ) %>% 
        pivot_longer(
            c(prior_normalised, posterior_normalised), 
            names_to = "distribution",
            values_to = "value",
            names_transform = list(distribution = ~str_to_sentence(gsub("_normalised", "", .)))
        )
    
    grid_data %>% 
        ggplot() +
        aes(x = pi_grid, y = value, colour = distribution) + 
        geom_line(size = 1) + 
        # annotate(
        #     geom = "text",
        #     x = 0, 
        #     y = max(grid_data$posterior_normalised, na.rm = TRUE),
        #     hjust = 0, 
        #     vjust = 1,
        #     label = sprintf("Success = %i\nN = %i\n", yes, n)
        # ) +
        labs(
            x = "Word prevalence",
            y = "Probability",
            colour = "Distribution",
            linetype = "Distribution"
        ) +
        scale_linetype_manual(values = c("solid", "dotted")) +
        scale_y_continuous(labels = scales::percent) +
        scale_x_continuous(labels = scales::percent) +
        theme(
            legend.position = "right",
            legend.title = element_blank()
        )
    
}