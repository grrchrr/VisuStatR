\name{visumotr-package}
\alias{visumotR}
\title{VisumotR: Visualizing Motility Parameters on Images in R}

\description{
VisumotR allows to visualize time-resolved motility parameters or any other summary statistic onto images in R. Originally this package was intended to be used on live-cell microscopy image and tracking data, but it can be used with any kind of data supplying a dataframe with track Ids, time, X and Y coordinates, as well as numeric or discrete mapping parameters and corresponding images.
}

\details{
It allows to map (time-resolved) motility parameters or any other calcuclated summary statistic based on coordinates back on corresponding image data. The package acts as a wrapper for annotating provided images with several, modifiable ggplot2-layers.
    With \code{\link{visumot_frame}}, continous and discrete parameters can be mapped individually on color, shape and size for one timepoint. visumot_summary() allows to create a corresponding plot of all given statistics for the whole range of the provided data.
    A time-resolved image-series can be created with \code{\link{visumot_all}} which allows to combine mapped frames and summary plots created by \code{\link{visumot_frame}} and \code{\link{visumot_summary}}. The ouput of all functions are ggplot2-objects which can further be manipulated in a common manner.

}

\usage{
visumot_frame(df, ...)
visumot_summary(df, ...)
visumot_all(df, images, ...)
}


\examples{
hello()
}
