% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saving.R
\name{stash_plot}
\alias{stash_plot}
\title{Save Plot to File and Metadata}
\usage{
stash_plot(
  plot,
  plot_name,
  user = Sys.info()[["user"]],
  figures_dir = "output/figure_objs",
  compression = "high",
  cleanup = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{plot}{The plot object to be saved.}

\item{plot_name}{A character string specifying the name of the plot.}

\item{user}{A character string specifying the user who created the plot.}

\item{figures_dir}{A character string specifying the directory to save the
plot to. The default is "output/figure_objs".}

\item{compression}{A character string specifying the compression level to
use when saving the plot. The default is "high".}

\item{cleanup}{A logical value indicating whether to remove old plot files
when writing out a new plot with the same plot_name. The default is TRUE.}

\item{verbose}{A logical value indicating whether to print messages. The
default is TRUE.}
}
\value{
None. The function is called for its side effect of saving the plot
to a file and adding the info to the metadata.
}
\description{
This function saves a given plot to a specified file path and adds metadata.
}
\examples{
\dontrun{
  library(ggplot2)
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  stash_plot(plot = p, plot_name = "mtcars_wt_mpg_scatter")
}
}
