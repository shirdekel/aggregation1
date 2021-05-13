##' @title Aggregation 1 screenshot file names

##' @return
##' @author Shir Dekel
##' @export
get_file_name_materials_experiment1 <- function() {
  project_choice <-
    list(
      presentation = c("separate", "joint"),
      projects = c(10, 1)
    ) %>%
    pmap(
      ~ str_c(
        "project_choice",
        "oil-well",
        .x,
        "similarity",
        "low",
        1:.y,
        sep = "_"
      )
    ) %>%
    unlist()


  other_gambles <-
    c(
      "samuelson_project",
      "samuelson_original",
      "redelmeier"
    ) %>%
    map(
      ~ str_c(
        .x,
        c("single", "multiple", "aggregated"),
        sep = "_"
      )
    ) %>%
    unlist()

  c(
    "instructions",
    str_c(
      "awareness",
      c("naive", "aware"),
      sep = "_"
    ),
    project_choice,
    "project_choice_aggregated",
    str_c(
      "neg_cond", seq_len(2),
      sep = "_"
    ),
    other_gambles
  )
}
