#' Get Plot Metadata
#'
#' This function reads a plot metadata file and returns its contents as a data
#' frame.
#'
#' @inheritParams stash_plot
#'
#' @return A data frame containing the plot metadata if the file exists.
#'
#' @details The function checks if the specified plot metadata file exists. If
#'    it does, it reads the file using `read.table` with tab-separated values
#'    and returns the resulting data frame. If the file does not exist, the
#'    function stops execution and returns an error message.
#'
#' @keywords internal
get_plot_metadata <- function(figures_dir = "output/figure_objs") {
    plot_metadata_file <- file.path(figures_dir, "/plot_metadata.qs")
    if (file.exists(plot_metadata_file)) {
        return(
            qs::qread(plot_metadata_file)
        )
    } else {
        stop("No plot metadata file found.")
    }
}

#' Get Plot Name Files
#'
#' This function retrieves the file names associated with a given plot name.
#'
#' @inheritParams stash_plot
#'
#' @return A character vector containing the file names associated with the
#'   specified plot name.
#'
#' @keywords internal
get_plot_name_files <- function(plot_name,
                                figures_dir = "output/figure_objs") {
    grep(
        x = list.files(figures_dir, full.names = TRUE),
        pattern = paste0(
            "/",
            plot_name,
            "_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}\\.qs"
        ),
        perl = TRUE,
        value = TRUE
    )
}

#' Check if a Plot File Name Exists
#'
#' This function checks whether a plot file with the given name already exists.
#'
#' @inheritParams stash_plot
#'
#' @return A logical value indicating whether the plot file name exists (TRUE)
#'   or not (FALSE).
#'
#' @keywords internal
plot_file_name_exists <- function(plot_name,
                                  figures_dir = "output/figure_objs") {
    length(get_plot_name_files(plot_name, figures_dir)) > 0
}

#' Convert Metadata List to Data Frame
#'
#' This function takes metadata and converts it into a data frame.
#'
#' @param metadata A list of metadata from get_plot_metadata()
#'
#' @return A data frame containing the metadata.
#'
#' @keywords internal
metadata_as_df <- function(metadata) {
    metadata_df <-
        lapply(metadata, function(x) {
            data.frame(
                plot_name = x$plot_name,
                file_path = x$file_path,
                timestamp = x$timestamp,
                user = x$user,
                stringsAsFactors = FALSE
            )
        }) |>
        dplyr::bind_rows()

    return(metadata_df)
}

#' Get Plot File Time
#'
#' This function retrieves the timestamp of a given plot file.
#'
#' @param plot_qs_file A string representing the path to the plot file.
#' @param as_string A logical value indicating whether to return the timestamp
#'   as a string (TRUE) or as a POSIXct object (FALSE). Default is FALSE.
#'
#' @return A POSIXct object representing the timestamp of the plot file.
#'
#' @keywords internal
get_plot_file_time <- function(plot_qs_file,
                               as_string = FALSE) {
    timestamp <-
        stringr::str_extract(
            string = basename(plot_qs_file),
            pattern = "[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}"
        )

    if(as_string) {
        return(timestamp)
    } else {
        return(lubridate::ymd_hms(timestamp))
    }
}

#' Extract Plot Name from QS File
#'
#' This function retrieves the plot name from a given QS file.
#'
#' @param plot_qs_file A string representing the path to the QS file.
#'
#' @return A string containing the name of the plot extracted from the QS file.
#'
#' @keywords internal
get_plot_name_from_qs_file <- function(plot_qs_file) {
    plot_name <-
        stringr::str_remove(
            string = basename(plot_qs_file),
            pattern = "_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}\\.qs"
        )
    return(plot_name)
}
