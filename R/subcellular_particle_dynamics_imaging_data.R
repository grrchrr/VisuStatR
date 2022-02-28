#' @title Subcellular particle dynamics of labeled genomic loci in engineered cell line
#' @description Loads example images of subcellular particle dynamics of labeled genomic loci in engineered cell line.
#' Images represent 15 frames of Feeder-independent Chic1 TetO/TetR-eGFP PGK12.1 mouse female embryonic stem cells which were acquired with
#' 35 ms exposure time and 100×/1.49 objective with 0.3 μm intervals for z-stacks and 30 s intervals between timepoints on a modified Eclipse
#' Ti-E brightfield/fluorescence microscope (Nikon) for HILO microscopy. Provided images are maximum intensity projections.
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
