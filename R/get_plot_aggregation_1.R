##' @title Get plot for Experiment 1
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot <- function(data = aggregation1::data) {
  proportion_omnibus <-
    data %>%
    get_omnibus()

  dv_label <- "Mean proportions of project acceptance"
  dodge_width <- 0.5

  awareness <-
    proportion_omnibus %>%
    afex::afex_plot(
      x = "awareness",
      mapping = c("shape", "color"),
      error_arg = list(width = 0.05),
      data_geom = ggbeeswarm::geom_quasirandom,
      point_arg = list(size = 3),
      factor_levels = list(
        awareness = c(
          naive = "Naive",
          aware = "Aware"
        )
      )
    ) +
    ggplot2::labs(
      x = "Awareness",
      y = dv_label
    ) +
    papaja::theme_apa() +
    ggplot2::theme(legend.position = "none")

  presentation <-
    proportion_omnibus %>%
    afex::afex_plot(
      x = "presentation",
      mapping = c("shape", "color"),
      error = "within",
      error_arg = list(width = 0.05),
      data_geom = ggbeeswarm::geom_quasirandom,
      point_arg = list(size = 3),
      factor_levels = list(
        presentation = c(
          separate = "Separate",
          joint = "Joint"
        )
      )
    ) +
    ggplot2::labs(
      x = "Presentation",
      y = dv_label
    ) +
    papaja::theme_apa() +
    ggplot2::theme(legend.position = "none")


  similarity_presentation <-
    proportion_omnibus %>%
    afex::afex_plot(
      x = "similarity",
      trace = "presentation",
      mapping = c("shape", "color"),
      error = "none",
      data_geom = ggbeeswarm::geom_quasirandom,
      data_arg = list(
        dodge.width = dodge_width,
        color = "darkgrey"
      ),
      point_arg = list(size = 3),
      dodge = dodge_width,
      factor_levels = list(
        presentation = c(
          separate = "Separate",
          joint = "Joint"
        ),
        similarity = c(
          low = "Low",
          high = "High"
        )
      ),
      legend_title = "Presentation"
    ) +
    ggplot2::labs(
      x = "Similarity",
      y = dv_label
    ) +
    papaja::theme_apa()

  data_trials <-
    data %>%
    dplyr::mutate(
      dplyr::across(
        c(awareness, similarity, presentation),
        stringr::str_to_sentence
      )
    )

  trials <-
    data_trials %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = project_order,
        y = choice,
        color = awareness,
        linetype = awareness
      )
    ) +
    ggplot2::geom_smooth(method = "loess") +
    ggplot2::facet_wrap(ggplot2::vars(similarity, presentation), labeller = "label_both") +
    ggplot2::scale_x_continuous("Trial", breaks = 1:10) +
    papaja::theme_apa() +
    ggplot2::labs(
      y = "Proportion of project acceptance",
      color = "Awareness",
      linetype = "Awareness"
    )

  trials_separate_awareness <-
    data_trials %>%
    dplyr::filter(presentation == "Separate") %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = project_order,
        y = choice,
        color = awareness,
        linetype = awareness
      )
    ) +
    ggplot2::geom_smooth(method = "loess") +
    ggplot2::scale_x_continuous("Trial", breaks = 1:10) +
    papaja::theme_apa() +
    ggplot2::labs(
      y = "Proportion of project acceptance",
      color = "Awareness",
      linetype = "Awareness"
    )

  tibble::lst(
    awareness,
    presentation,
    similarity_presentation,
    trials,
    trials_separate_awareness
  )
}
