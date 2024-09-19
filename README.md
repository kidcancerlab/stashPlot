# stashPlot

## Overview

The purpose of this package is to provide a way to save off plot objects and metadata about the plot object so that they can be easily reloaded and modified or assembled into a larger figure. This way plots that are created in one part of the code or in a separate qmd/Rmd file can be accessed in a distant part of the code by referencing their name with a simple command.

This package also provides a way to import plots from someone else's folder, while avoiding overwriting plots with older versions.

## Installation

```{r}
devtools::install_github("kidcancerlab/stashPlot")
```

## Usage

Stash a plot for later use
```{r}
plot_stuff <-
    ggplot(mtcars, aes(x = mpg, y = wt)) +
    geom_point()

stash_plot(
    plot_stuff,
    plot_name = "mpg_wt"
)
```

Look at what plots you have stashed
```{r}
list_plots()
```

Then pull it pack in later by referencing the name. You can modify the plot object as needed.
```{r}
get_plot("mpg_wt") +
    labs(title = "Miles per gallon vs weight") +
    theme(
        legend.position = "none",
        plot.title = element_text(size = 20)
    )
```

See vignette for more examples.
