#' @title `{drake}` plan

#' @return
#' @author Shir Dekel
#' @export
get_plan <- function() {
  drake_plan(
    experiment_directory = target(
      file.path("inst", "experiment"),
      target = "file"
    ),
    materials = get_screenshots(experiment_directory)
  )
}
