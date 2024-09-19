test_that("saving a plot works", {
    if (file.exists("output/figure_objs/plot_metadata.qs")) {
        file.remove("output/figure_objs/plot_metadata.qs")
    }
    stash_plot(
        plot = ggplot2::ggplot(
            mtcars,
            ggplot2::aes(x = wt, y = mpg)
        ) +
            ggplot2::geom_point(),
        plot_name = "test_plot"
    )
    expect_true(file.exists("output/figure_objs/plot_metadata.qs"))

    retrieved_plot <- get_plot("test_plot")
})

test_that("getting the metadata works", {
    expect_error(get_plot_metadata("this_file_does_not_exist"))

})

test_that("import_plot imports plots with or without overwriting",{
    plot_name <-
        ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point()

    # this one is older
    stash_plot(
        plot = plot_name,
        plot_name = "testing_import",
        figures_dir = "output/other_folder"
    )
    old_file <-
        stashPlot:::get_plot_name_files(
            "testing_import",
            "output/other_folder"
        )

    Sys.sleep(10)
    # this one is newer
    stash_plot(plot = plot_name, plot_name = "testing_import")
    new_file <- stashPlot:::get_plot_name_files("testing_import")

    # Try to import the old file into the folder with a newer one
    suppressMessages(import_plot(old_file))

    current_file <- stashPlot:::get_plot_name_files("testing_import")

    # The newer file should still be there
    expect_identical(new_file, current_file)

    # Now try to force the import to overwrite the newer file
    suppressMessages(import_plot(old_file, overwrite_newer = TRUE))

    current_file <- stashPlot:::get_plot_name_files("testing_import")

    # The older file should now be there
    expect_identical(basename(old_file), basename(current_file))
})

