\name{visustat-package}
\alias{VisuStatR}
\title{

VisuStatR: Visualizing Motility Parameters on Images in R}

\description{
VisustatR allows to visualize time-resolved motility parameters or any other summary statistic onto images in R. Originally this package was intended to be used on live-cell microscopy image and tracking data, but it can be used with any kind of data supplying a dataframe with track Ids, time, X and Y coordinates, as well as numeric or discrete mapping parameters and corresponding images.
}

\details{
It allows to map (time-resolved) motility parameters or any other calcuclated summary statistic based on coordinates back on corresponding image data. The package acts as a wrapper for annotating provided images with several, modifiable ggplot2-layers.
    With \code{\link{visustat_frame}}, continous and discrete parameters can be mapped individually on color, shape and size for one timepoint. visustat_summary() allows to create a corresponding plot of all given statistics for the whole range of the provided data.
    A time-resolved image-series can be created with \code{\link{visustat_all}} which allows to combine mapped frames and summary plots created by \code{\link{visustat_frame}} and \code{\link{visustat_summary}}. The ouput of all functions are ggplot2-objects which can further be manipulated in a common manner.
    All functionalties can also be accessed via a Shiny app by calling \code{\link{visustat_shiny}}

}

\usage{
visustat_frame(df, ...)
visustat_summary(df, ...)
visustat_all(df, images, ...)
visustat_shiny()
}

