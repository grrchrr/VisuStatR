% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clonogenic_assay_data.R
\docType{data}
\name{clonogenic_assay}
\alias{clonogenic_assay}
\title{Clonogenic assay colony growth data}
\usage{
data(clonogenic_assay)
}
\description{
This dataframe contains tracking data based on images \code{\link{clonogenic_assay_images}}. For every colony, position, time point, size (area [px]) and the momentary growth rate are recorded. This growth rate was calculated using a sliding window of size 3 , where the momentary growth rate λ(t) was calculated as the size difference within the sliding window relative to the colony size at time t.
For timepoints where the averaging window exceeded the length of the track, no values were calculated. Together with VisuStatR’s behavior of giving a distinct color to NA values, this can be used to visualize highlight soon-ending tracks in the image data.
}
\examples{
data(clonogenic_assay)
clonogenic_assay
}
\references{
Koch RA, Harmel C, Alber M, Bahn E. A framework for automated time-resolved analysis of cell colony growth after irradiation. Phys Med Biol. 2021 Jan 29;66(3):035017. doi: 10.1088/1361-6560/abd00d. PMID: 33264763.
}
\keyword{datasets}
