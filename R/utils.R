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
        list.files(here("data"), full.names = TRUE) %>% 
            purr:::map(readRDS) %>% 
            purrr::set_names(str_remove(list.files(here("data")), ".rds"))
    }
}