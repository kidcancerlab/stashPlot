#' Retrieve a Plot by Name
#'
#' This function retrieves a plot object based on the provided plot name.
#'
#' @inheritParams stash_plot
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

    plot_data <-
        tryCatch(
            qs::qread(plot_file),
            error = function(e) {
                message(
                    "Error reading plot file. ",
                    "The file may be corrupted or missing.",
                    " Error message: ", e
                )
                return(NULL)
            },
            warning = function(w) {
                if (grep("PROMSXP detected, replacing with NULL", w$message)) {
                    message(
                        "Error reading in file due to a known issue with qs. ",
                        "To fix this issue, run the following command:\n\n",
                        "qs::set_trust_promises(TRUE)\n\n",
                        "See: https://github.com/qsbase/qs/issues/93."
                    )
                    return(NULL)
                } else {
                    message(
                        "Warning reading plot file. ",
                        "The file may be corrupted or missing.",
                        " Warning message: ", w
                    )
                return(NULL)
                }
            }
        )

    return(plot_data)
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
