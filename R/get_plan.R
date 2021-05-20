#' @title `{drake}` plan

#' @return
#' @author Shir Dekel
#' @export
get_plan <- function() {
  drake_plan(
    experiment = target(
      file.path("inst", "experiment"),
      target = "file"
    ),
    # Same as above, but order not randomised (in `main.js`)
    experiment_testing = target(
      file.path("inst", "experiment_testing"),
      target = "file"
    ),
    materials = get_screenshots(experiment_testing)
  )
}
