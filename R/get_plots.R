#' Retrieve a Plot by Name
#'
#' This function retrieves a plot object based on the provided plot name.
#'
#' @inheritParams save_plot
#'
#' @return The plot object corresponding to the given plot name.
#' @examples
#' \dontrun{
#'   plot <- get_plot("my_plot")
#'   print(plot)
#' }
#' @export
get_plot <- function(plot_name,
                     figures_dir = "output/figure_objs",
                     verbose = TRUE) {
    metadata <- get_plot_metadata(figures_dir)

    plot_file <- metadata[[plot_name]]$file_path

    if (length(plot_file) == 0) {
        stop(plot_name, " plot not found in metadata.")
    }

    if (verbose) {
        message(
            "Retrieving plot from ", plot_file,
            ". File created by ", metadata[[plot_name]]$user,
            " on ", metadata[[plot_name]]$timestamp
            )
    }

    return(qs::qread(plot_file))
}

#' List Plot Files
#'
#' This function lists all plot files in the specified directory.
#'
#' @param figures_dir A character string specifying the directory where plot
#'   files are stored. Default is "output/figure_objs".
#'
#' @return A character vector of file names in the specified directory.
#'
#' @examples
#' \dontrun{
#'   list_plots()
#'   list_plots("custom/figure_objs")
#' }
#'
#' @export
list_plots <- function(figures_dir = "output/figure_objs") {
    metadata <- get_plot_metadata(figures_dir)

    return(metadata_as_df(metadata))
}
