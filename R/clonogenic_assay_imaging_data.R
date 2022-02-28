#' @title Clonogenic assay colony growth imaging data
#' @description Loads example images of colony growth in a clonogenic assay.
#'  Images represent 15 frames of H3122 human non-small cell lung adenocarcinoma cells growing in a single well on a 96-well plate.
#'   Data courtesy of Dr. Ivana Dokic. The images were registered and cells and colonies were segmented, tracked and quantified to extract colony growth data as described in Koch et al. (2021).
#'   Corresponding data table can be found here: \code{\link{clonogenic_assay}}
#' @references Koch RA, Harmel C, Alber M, Bahn E. A framework for automated time-resolved analysis of cell colony growth after irradiation. Phys Med Biol. 2021 Jan 29;66(3):035017. doi: 10.1088/1361-6560/abd00d. PMID: 33264763.
#' @examples
#' clonogenic_assay_images()
#' @export
clonogenic_assay_images <- function(){
  files <- system.file("extdata/clonogenic_assay", c("frame_01.tif",
                                                     "frame_02.tif",
                                                     "frame_03.tif",
                                                     "frame_04.tif",
                                                     "frame_05.tif",
                                                     "frame_06.tif",
                                                     "frame_07.tif",
                                                     "frame_08.tif",
                                                     "frame_09.tif",
                                                     "frame_10.tif",
                                                     "frame_11.tif",
                                                     "frame_12.tif",
                                                     "frame_13.tif",
                                                     "frame_14.tif",
                                                     "frame_15.tif"), package = "VisuStatR")
  if (length(files)==1) {
    stop("Could not find example images. Try re-installing VisuStatR.", call. = FALSE)
  }
  files
}
