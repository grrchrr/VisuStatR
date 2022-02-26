#' @title clonogenic_assay_images
#' @description Loads example images of colony growth in a clonogenic assay.
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
