#' @title Subcellular particle dynamics of labeled genomic loci in engineered cell line
#' @description Loads example images of subcellular particle dynamics of labeled genomic loci in engineered cell line.
#' @examples
#' subcellular_particle_dynamics_images()
#' @export
subcellular_particle_dynamics_images <- function(){
  files <- system.file("extdata/subcellular_particle_dynamics", c("frame_001.tif",
                                                                  "frame_002.tif",
                                                                  "frame_003.tif",
                                                                  "frame_004.tif",
                                                                  "frame_005.tif",
                                                                  "frame_006.tif",
                                                                  "frame_007.tif",
                                                                  "frame_008.tif",
                                                                  "frame_009.tif",
                                                                  "frame_010.tif",
                                                                  "frame_011.tif",
                                                                  "frame_012.tif",
                                                                  "frame_013.tif",
                                                                  "frame_014.tif",
                                                                  "frame_015.tif"), package = "VisuStatR")
  if (length(files)==1) {
    stop("Could not find example images. Try re-installing VisuStatR.", call. = FALSE)
  }
  files
}
