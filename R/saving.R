#' Save Plot to File and Metadata
#'
#' This function saves a given plot to a specified file path and adds metadata.
#'
#' @param plot The plot object to be saved.
#' @param plot_name A character string specifying the name of the plot.
#' @param user A character string specifying the user who created the plot.
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
#'   stash_plot(plot = p, plot_name = "mtcars_wt_mpg_scatter")
#' }
stash_plot <- function(plot,
                       plot_name,
                       user = Sys.info()[["user"]],
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

    remove_old_plot(
        plot_name,
        figures_dir = figures_dir,
        cleanup = cleanup
    )

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
            user = user
        )

    qs::qsave(metadata, metadata_file)
}


#' Import Plot from qs File
#'
#' This function imports a plot from a specified qs file.
#'
#' @inheritParams stash_plot
#' @param qs_file A string representing the path to the qs file containing the
#'   plot data.
#' @param user A string representing the user who imported the plot. The default
#'   is "imported from file".
#' @param overwrite_newer A logical value indicating whether to overwrite a plot
#'   file even if the new one is older. The default is FALSE.
#'
#' @return The imported plot object.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   plot <- import_plot("path/to/your/plot.qs")
#' }
#' @export
import_plot <- function(qs_file,
                        figures_dir = "output/figure_objs",
                        user = "imported from file",
                        cleanup = TRUE,
                        verbose = TRUE,
                        overwrite_newer = FALSE) {
    if (!grepl(
        x = basename(qs_file),
        pattern = ".+_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}\\.qs",
        )) {
            message(
                "The qs file should look like this: ",
                "plot_name_YYYY_MM_DD_HH.MM.SS.qs and should have been created",
                "by the stash_plot function."
            )
            stop("This does not look like a file from the stash_plot function.")
    }

    plot_name <- get_plot_name_from_qs_file(qs_file)

    # Check if a file with this plot_name already exists and warn if the new one
    # is older
    old_plot_filename <-
        get_plot_name_files(
            plot_name = get_plot_name_from_qs_file(qs_file),
            figures_dir = figures_dir
        )

    if (length(old_plot_filename) > 0) {
        new_time <- get_plot_file_time(qs_file)
        old_time <-
            get_plot_file_time(old_plot_filename) |>
            sort(decreasing = TRUE) |>
            head(n = 1)
        if (new_time < old_time && !overwrite_newer) {
            message(
                "Not overwriting ",
                paste(old_plot_filename, collapse = " and "),
                " with ",
                qs_file
            )
            message("Use overwrite_newer = TRUE to overwrite anyway.")
            invisible()
        }
    }

    remove_old_plot(
        plot_name,
        figures_dir = figures_dir,
        cleanup = cleanup
    )

    file.copy(qs_file, "output/figure_objs")

    # Read in the metadata file if it exists
    metadata_file <- file.path(figures_dir, "/plot_metadata.qs")
    if (file.exists(metadata_file)) {
        metadata <- get_plot_metadata(figures_dir)
    } else {
        metadata <- list()
    }

    # Put the new plot's info into the metadata
    metadata[[plot_name]] <-
        list(
            plot_name = plot_name,
            file_path = paste0(figures_dir, "/", basename(qs_file)),
            timestamp = get_plot_file_time(qs_file, as_string = TRUE),
            user = user
        )

    qs::qsave(metadata, metadata_file)
}

#' Import All QS Plots
#'
#' This function imports all QS plots from a specified directory.
#'
#' @inheritParams import_plot
#' @param from_figures_dir A character string specifying the directory
#'   containing the qs plot files to be imported. These must have been created
#'   by the stash_plot function.
#' @param to_figures_dir A character string specifying the directory to save the
#'   imported plots to. The default is "output/figure_objs".
#'
#' @return A list of imported QS plots.
#'
#' @examples
#' figures_dir <- "/path/to/figures"
#' plots <- import_all_qs_plots(figures_dir)
#'
#' @export
import_all_qs_plots <- function(from_figures_dir,
                                to_figures_dir = "output/figure_objs",
                                user = "imported from file",
                                cleanup = TRUE,
                                overwrite_newer = FALSE) {
    qs_files <-
        list.files(
            from_figures_dir,
            pattern = ".+_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}\\.qs",
            full.names = TRUE
        )

    for (qs_file in qs_files) {
        import_plot(
            qs_file,
            user = user,
            figures_dir = to_figures_dir,
            cleanup = cleanup,
            overwrite_newer = overwrite_newer
        )
    }
}

#' Remove Old Plot
#'
#' This function removes an old plot file if it exists.
#'
#' @inheritParams stash_plot
#'
#' @return None. This function is called for its side effects.
#'
#' @keywords internal
remove_old_plot <- function(plot_name,
                            figures_dir = "output/figure_objs",
                            cleanup = TRUE) {
    # We don't need to keep old plots
    if (plot_file_name_exists(
            plot_name,
            figures_dir = figures_dir
        ) && cleanup == TRUE) {
        message(
            paste0(
                "File for ", plot_name, " already exists. ",
                "Overwriting ", get_plot_name_files(plot_name)))

        invisible(
            file.remove(
                get_plot_name_files(
                    plot_name,
                    figures_dir = figures_dir
                )
            )
        )
    } else if (plot_file_name_exists(
            plot_name,
            figures_dir = figures_dir
        ) && cleanup == FALSE) {
        message(
            paste0(
                "File for ", plot_name, " already exists. ",
                "Not overwriting."))
    }
}
