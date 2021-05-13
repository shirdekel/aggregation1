##' @title Get screenshots

##' @return
##' @author Shir Dekel
##' @export
get_screenshots <- function(experiment_directory) {
  file_name_materials_experiment1 <-
    get_file_name_materials_experiment1()

  materials_directory_experiment1 <-
    file.path("inst", "materials")

  if (!dir.exists(materials_directory_experiment1)) {
    dir.create(materials_directory_experiment1)
  }

  file_path_materials <-
    shirthesis::get_file_path_materials(
      materials_directory_experiment1,
      file_name_materials_experiment1
    )

  casper_calls <-
    c(
      "this.click('.jspsych-btn');" %>%
        rep(27)
    ) %>%
    slider::slide(~., .before = Inf) %>%
    append(.[2], after = 2) %>% # awareness
    ## For some reason, 14 gives weird code instead of a project, and 15 is the
    ## interstitial
    .[-c(14:15)] %>%
    map_chr(
      ~ str_c(
        .,
        collapse = "\n"
      )
    )

  awareness <-
    c(
      "naive" %>%
        rep(2),
      "aware" %>%
        rep(24)
    )

  webshot_eval <-
    list(
      casper_calls,
      awareness
    ) %>%
    pmap_chr(
      function(casper_calls, awareness) {
        str_c(
          "casper.thenOpen(this.getCurrentUrl() + '?alignment=lowA",
          "&awareness=",
          awareness,
          str_c(
            "', function() {", casper_calls, "});",
            sep = "\n"
          )
        )
      }
    )

  file.path(experiment_directory, "index.html") %>%
    webshot::webshot(
      file = file_path_materials,
      eval = webshot_eval
    )
}
