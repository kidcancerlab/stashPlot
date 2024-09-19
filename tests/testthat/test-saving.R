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
