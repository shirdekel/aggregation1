##' @title Pivot aggregation 1 data
##'
##' The goal is to keep all DVs in the same dataframe so that we can
##' subsequently just `nest_by()` to get what we want. Maybe it would have been
##' better to use separate dataframes.

##' @return
##' @author Shir Dekel
##' @export
##' @param data
pivot_aggregation_1 <- function(data) {
  data %>%
    pivot_wider(
      names_from = c(condition, project, pos, dif, prob, order),
      values_from = value
    ) %>%
    pivot_longer(
      starts_with(c("separate", "joint")),
      names_to = c(
        "presentation", "project", "pos", "dif", "prob",
        "project_order"
      ),
      names_sep = "_",
      values_to = "choice"
    ) %>%
    pivot_longer(
      c(starts_with("follow_up")),
      names_to = c(
        "condition_follow_up", "project_follow_up", "pos_follow_up",
        "dif_follow_up", "prob_follow_up", NA
      ),
      names_sep = "_",
      names_prefix = "follow_up_",
      values_to = "follow_up_choice"
    ) %>%
    pivot_wider(
      names_from = condition_follow_up,
      values_from = c(
        follow_up_choice, project_follow_up, pos_follow_up,
        dif_follow_up, prob_follow_up
      )
    )
}
