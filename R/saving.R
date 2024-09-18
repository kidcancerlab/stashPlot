#' Save Plot to File and Metadata
#'
#' This function saves a given plot to a specified file path and adds metadata.
#'
#' @param plot The plot object to be saved.
#' @param plot_name A character string specifying the name of the plot.
#' @param figures_dir A character string specifying the directory to save the
#'   plot to. The default is "output/figure_objs".
#' @param compression A character string specifying the compression level to
#'   use when saving the plot. The default is "high".
#' @param cleanup A logical value indicating whether to remove old plot files
#'   when writing out a new plot with the same plot_name. The default is TRUE.
#' @param verbose A logical value indicating whether to print messages. The
#'   default is TRUE.
#'
#' @return None. The function is called for its side effect of saving the plot
#'   to a file and adding the info to the metadata.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'   p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#'   save_plot(plot = p, plot_name = "mtcars_wt_mpg_scatter")
#' }
save_plot <- function(plot,
                      plot_name,
                      figures_dir = "output/figure_objs",
                      compression = "high",
                      cleanup = TRUE,
                      verbose = TRUE) {
    # Ensure the output directory exists
    if (!dir.exists(figures_dir)) {
        dir.create(figures_dir, recursive = TRUE)
    }

    # Create a timestamp
    timestamp <- format(lubridate::now(), "%Y_%m_%d_%H.%M.%S")

    # Create the file name
    file_name <- paste0(plot_name, "_", timestamp, ".qs")
    file_path <- file.path(figures_dir, file_name)

    remove_old_plot(plot_name, cleanup)

    # Save the plot object with compression
    qs::qsave(plot, file_path, preset = compression)

    # Save metadata to a qs file
    metadata_file <- file.path(figures_dir, "/plot_metadata.qs")
    if (file.exists(metadata_file)) {
        metadata <- get_plot_metadata(figures_dir)
    } else {
        metadata <- list()
    }
    metadata[[plot_name]] <-
        list(
            plot_name = plot_name,
            file_path = file_path,
            timestamp = timestamp,
            user = Sys.info()[["user"]]
        )

    qs::qsave(metadata, metadata_file)
}


#' Import Plot from QS File
#'
#' This function imports a plot from a specified QS file.
#'
#' @inheritParams save_plot
#' @param qs_file A string representing the path to the QS file containing the
#'   plot data.
#' @param user A string representing the user who imported the plot. The default
#'   is "imported from file".
#'
#' @return The imported plot object.
#'
#' @examples
#' # Example usage:
#' plot <- import_plot("path/to/your/plot.qs")
#'
#' @export
import_plot <- function(qs_file,
                        figures_dir = "output/figure_objs",
                        user = "imported from file",
                        cleanup = TRUE,
                        verbose = TRUE) {
    if (!grepl(
        x = basename(qs_file),
        pattern = ".+_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}\\.qs",
        )) {
            message(
                "The qs file should look like this: ",
                "plot_name_YYYY_MM_DD_HH.MM.SS.qs and should have been created",
                "by the save_plot function."
            )
            stop("This does not look like a file from the save_plot function.")
    }

    file.copy(qs_file, "output/figure_objs")

    plot_name <-
        stringr::str_remove(
            string = basename(qs_file),
            pattern = "_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}\\.qs"
        )

    remove_old_plot(plot_name, cleanup = cleanup)

    metadata_file <- file.path(figures_dir, "/plot_metadata.qs")
    if (file.exists(metadata_file)) {
        metadata <- get_plot_metadata(figures_dir)
    } else {
        metadata <- list()
    }
    metadata[[plot_name]] <-
        list(
            plot_name = plot_name,
            file_path = paste0(figures_dir, "/", basename(qs_file)),
            timestamp = timestamp,
            user = user
        )

    qs::qsave(metadata, metadata_file)
}


remove_old_plot <- function(plot_name, cleanup = TRUE) {
    # We don't need to keep old plots
    if (plot_file_name_exists(plot_name) && cleanup == TRUE) {
        message(paste0("File for ", plot_name, " already exists. Overwriting."))
        message(get_plot_name_files(plot_name))
        invisible(file.remove(get_plot_name_files(plot_name)))
    }
}
