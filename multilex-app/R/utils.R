run_app <- function(){
    tic("run app")
    runApp(
        "multilex-app", 
        launch.browser = TRUE
    )
    toc()
}

update_multilex <- function(...){
    
    participants <- multilex::ml_participants()
    responses <- multilex::ml_responses(participants, ...)
    logs <- multilex::ml_logs(participants, responses)
    vocabulary <- multilex::ml_vocabulary(participants, responses, scale = c("prop", "counts"))
    norms <- multilex::ml_norms(participants, responses)
    
    ml_data <- tibble::lst(
        participants,
        responses, 
        logs, 
        vocabulary, 
        norms
    )
    
    invisible(
        purrr::map2(
            .x = ml_data,
            .y = names(ml_data),
            .f = ~arrow::write_ipc_stream(.x, here("data", paste0(.y, ".parquet")))
        )
    )
}

# get multilex data, update if necessary
get_multilex <- function(){
    
    ml_data <- purrr::map(
        list.files(
            here::here("data"), 
            full.names = TRUE,
            pattern = ".parquet"
        ), 
        arrow::read_ipc_stream,
        as_data_frame = FALSE
    ) 
    
    names(ml_data) <- stringr::str_remove(
        list.files(
            here::here("data"),
            pattern = ".parquet"
        ),
        ".feather"
    )
    
    ml_data$predictions <- ungroup(ml_data$predictions)
    
    return(ml_data)
}

# custom ggplot theme
theme_custom <- function(){
    theme(
        panel.background = element_rect(fill = "white", colour = NA),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 10),
        strip.text = element_text(face = "bold", size = 10),
        title = element_text(face = "bold", size = 12),
        axis.text = element_text(colour = "black", size = 10),
        axis.line = element_line(colour = "black"),
        legend.box.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA)
    )
}

# grid approximation for binomial counts
# plot_bayes_grid <- function(
#     yes, 
#     n,
#     prior_alpha = 5, 
#     prior_beta = 5
# ){
#     
#     grid_data <- data.frame(
#         pi_grid = seq(
#             from = 0.1, 
#             to = 1, 
#             length = 1000
#         )
#     ) %>% 
#         mutate(
#             prior = dbeta(pi_grid, prior_alpha, prior_beta),
#             prior_normalised = dbeta(pi_grid, prior_alpha, prior_beta) / sum(prior, na.rm = TRUE),
#             likelihood = dbinom(.env$yes, .env$n, pi_grid),
#             posterior = likelihood * prior,
#             posterior_normalised = posterior / sum(posterior, na.rm = TRUE)
#         ) %>% 
#         pivot_longer(
#             c(prior_normalised, posterior_normalised), 
#             names_to = "distribution",
#             values_to = "value",
#             names_transform = list(distribution = ~str_to_sentence(gsub("_normalised", "", .)))
#         )
#     
#     grid_data %>% 
#         ggplot() +
#         aes(x = pi_grid, y = value, colour = distribution) + 
#         geom_line(size = 1) + 
#         # annotate(
#         #     geom = "text",
#         #     x = 0, 
#         #     y = max(grid_data$posterior_normalised, na.rm = TRUE),
#         #     hjust = 0, 
#         #     vjust = 1,
#         #     label = sprintf("Success = %i\nN = %i\n", yes, n)
#         # ) +
#         labs(
#             x = "Word prevalence",
#             y = "Probability",
#             colour = "Distribution",
#             linetype = "Distribution"
#         ) +
#         scale_linetype_manual(values = c("solid", "dotted")) +
#         scale_y_continuous(labels = scales::percent) +
#         scale_x_continuous(labels = scales::percent) +
#         theme(
#             legend.position = "right",
#             legend.title = element_blank()
#         )
#     
# }


# model posterior predictions
plot_predictions <- function(
    .predictions,
    .aoas,
    show_uncertainty = TRUE
) {
    
    title_label <- unique(.predictions$label)
    
    plot_curves <- .predictions %>%  
        ggplot() +
        aes(
            x = age,
            y = .epred,
            colour = interaction(dominance, .category, sep = " / "),
            fill = interaction(dominance, .category, sep = " / "),
            shape = interaction(dominance, .category, sep = " / ")
        ) +
        # linea de referencia (50% suele ser considerado el punto de adquisición)
        geom_hline(
            yintercept = 0.5,
            colour = "grey"
        ) 
    
    # posterior predictions for each individual posterior draw
    if (show_uncertainty){
        plot_curves <- plot_curves +
            geom_line(
                aes(group = interaction(.draw, dominance, .category , sep = " / ")),
                size = 0.75,
                alpha = 0.15,
                linetype = "solid"
            ) 
    } 
        # posterior predictions for each individual posterior draw
    plot_curves <- plot_curves +
        stat_summary(
            fun = "mean",
            geom = "line",
            size = 1
        ) +
        labs(
            x = "Age (months)",
            y = "P(acquisition | age, dominance)",
            colour = "Response",
            fill = "Response",
            shape = "Response",
            linetype = "Response",
            title = "Acquisition curves",
            subtitle = "Each line corresponds to a posterior prediction"
        ) +
        scale_color_d3() +
        scale_fill_d3() +
        scale_y_continuous(
            limits = c(0, 1),
            labels = scales::percent
        ) +
        scale_x_continuous(
            limits = c(0, 45),
            breaks = seq(0, 45, 5)
        ) +
        theme(
            legend.position = "none",
            legend.title = element_blank(),
            panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
            axis.text.x.top = element_text(),
            axis.title.x.top = element_text(colour = "top"),
            axis.ticks.x.top = element_line(),
            axis.title.x.bottom = element_blank(),
            axis.text.x.bottom = element_blank(),
            axis.ticks.x.bottom = element_blank(),
            axis.line.x = element_blank()
        ) 
    
    plot_aoas <- .aoas %>% 
        ggplot() +
        aes(
            x = aoa,
            y = 0,
            colour = interaction(dominance, .category, sep = "/")
        ) +
        stat_pointinterval(
            position = position_dodge(width = 0.5)
        ) +    labs(
            x = "Age (months)",
            y = "Response",
            colour = "Response",
            title = "Age of acquisition"
        ) +
        scale_x_continuous(
            limits = c(0, 45),
            breaks = seq(0, 45, 5)
        ) +
        scale_color_d3() +
        scale_fill_d3() +
        theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
        )
    
    preds_plot <- plot_curves + 
        plot_aoas +
        plot_layout(
            ncol = 1,
            heights = c(0.8, 0.2)
        ) +
        plot_annotation(
            title = title_label,
            subtitle = "Marginal item posterior predictions",
            tag_levels = "A"
        )
    
    
    return(preds_plot)
}


