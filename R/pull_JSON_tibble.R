##' @title Pull column, extract JSON, convert to tibble

##' @param data Data
##'
##' @param col Column to pull
##'
##' @return
##' @author Shir Dekel
##' @export
##'
pull_JSON_tibble <- function(data, col) {

  col <- enquo(col)

  data %>%
    pull(!!col) %>%
    map(fromJSON) %>%
    flatten() %>%
    as_tibble(.name_repair = make.unique) # so that it duplicate fixes correspond to the way we rename them later on

}
