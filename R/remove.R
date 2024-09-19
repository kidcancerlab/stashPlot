#' Remove a Plot
#'
#' This function removes a plot with the specified name from your stash.
#'
#' @inheritParams stash_plot
#'
#' @return None. The function is called for its side effect of removing the plot.
#'
#' @examples
#' \dontrun{
#'   # Remove a plot named "my_plot" from the current environment
#'   remove_plot("my_plot")
#'
#'   # Remove a plot named "my_plot" from a specified environment
#'   remove_plot("my_plot", figures_dir = "custom_dir/figures")
#' }
#'
#' @export
remove_plot <- function(plot_name,
                        figures_dir = "output/figure_objs",
                        verbose = TRUE) {
    metadata <- get_plot_metadata(figures_dir)

    plot_file <- metadata[[plot_name]]$file_path

    message(paste0("Removing plot file ", plot_file, " for ", plot_name))
    file.remove(plot_file)

    metadata[[plot_name]] <- NULL
    qs::qsave(metadata, file.path(figures_dir, "/plot_metadata.qs"))
}