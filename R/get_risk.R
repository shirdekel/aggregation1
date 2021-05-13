##' @title Get risk proportion
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_risk <- function(data) {
  data_sum <- data %>%
    pull(choice) %>%
    sum()

  total <- data %>%
    pull(choice) %>%
    length()

  data_sum / total
}
