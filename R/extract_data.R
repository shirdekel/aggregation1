#' Extract data
#'
#' For aggregation experiment
#'
#' @param data_prep data
#' @param names_to pivot names
#' @param names_pattern pivot pattern
#' @param values_to pivot value
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{extract_data(data_prep, names_to, names_pattern)}
extract_data <- function(data_prep, names_to, names_pattern, values_to = "value") {
  demographics <- data_prep %>%
    filter(trial_type == "survey-html-form") %>%
    pull_JSON_tibble(responses)

  checks <- data_prep %>%
    filter(str_detect(responses, "250") |
             str_detect(responses, "check") |
             str_detect(responses, "rep_aggregated") |
             str_detect(responses, "samuelson"))  %>%
    pull_JSON_tibble(responses) %>%
    pivot_longer(cols = everything(),
                 names_to = names_to,
                 names_pattern = names_pattern,
                 values_to = values_to)

  joint_order <- data_prep %>%
    filter(str_detect(responses, "joint")) %>%
    pull(question_order) %>%
    fromJSON() + 1 # convert JS numbering (0:9) to intuitive numbering (1:10)

  data_combined <- c("separate", "joint") %>%
    map2(list(1:10, joint_order),
         ~ data_prep %>%
           filter(str_detect(responses, .x), str_detect(responses, "240")) %>%
           pull_JSON_tibble(responses) %>%
         pivot_longer(cols = everything(),
                               names_to = names_to,
                               names_pattern = names_pattern,
                               values_to = values_to) %>%
           mutate(order = .y)) %>%
    bind_rows() %>%
    full_join(checks, by = c("condition", "project", "pos", "dif", "prob", "value")) %>%
    bind_cols(demographics)

  return(data_combined)
}
