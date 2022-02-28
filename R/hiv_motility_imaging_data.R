#' @title HIV Momages
#' @description Loads example images of CD4+ T cells in a complex collagen environment.
#' @examples
#' hiv_motility_images()
#' @export
hiv_motility_images <- function(){
  files <- system.file("extdata/hiv_motility", c('loose_000.tif',
                                    'loose_001.tif',
                                    'loose_002.tif',
                                    'loose_003.tif',
                                    'loose_004.tif',
                                    'loose_005.tif',
                                    'loose_006.tif',
                                    'loose_007.tif',
                                    'loose_008.tif',
                                    'loose_009.tif',
                                    'loose_010.tif',
                                    'loose_011.tif',
                                    'loose_012.tif',
                                    'loose_013.tif',
                                    'loose_014.tif'), package = "VisuStatR")
  if (length(files)==1) {
    stop("Could not find example images. Try re-installing VisuStatR.", call. = FALSE)
  }
  files
}
