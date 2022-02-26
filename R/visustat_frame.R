#' @title visustat_frame
#' @description With \code{visustat_frame}, continuous and discrete parameters can be mapped individually on color, shape and size for one timepoint.
#' @examples
#' # import hiv motility tracking data
#' data('hiv_motility')
#' # get image files
#' images <- hiv_motility_images()
#' # run visustat_frame with default settings
#' visustat_frame(hiv_motility, image=images[15], frame=15, image.normalize=1)
#' # run visustat_frame with specified settings
#' visustat_frame(hiv_motility,
#'    image = images[15],
#'    frame = 15,
#'    tracks = c(48, 66, 102, 108),
#'    sub.img = TRUE,
#'    sub.col = 2,
#'    sub.window= 300,
#'    par.map ='speed',
#'    par.shape ='type',
#'    points.size=2,
#'    image.normalize=1
#'  )
#' # import clonogenic assay colony growth data
#' data('clonogenic_assay')
#' # get image files
#' images <- clonogenic_assay_images()
#' # run visustat_frame with default settings
#' visustat_frame(clonogenic_assay, image=images[15], frame=15, tracks.length=0, axis.display=0)
#'
#' # import subcellular particle data
#' data('subcellular_particle_dynamics')
#' # get image files
#' images <- subcellular_particle_dynamics_images()
#' visustat_frame(subcellular_particle_dynamics,
#'  image=images[2], frame=2,
#'  tracks.length=0,
#'  axis.display=0,
#'  par.map='MEAN_INTENSITY') + scale_color_viridis_c(option='plasma')

#' @export
visustat_frame <- function(df, ...) {
  #' @import tidyverse
  #' @import grid
  #' @import gridExtra
  #' @import ggecho
  #' @import rlang
  #' @import magick
  #' @import facetscales
  #' @import cowplot
  #' @import parallel
  #' @import foreach
  #' @import doSNOW
  #' @import plotly
  # set default parameters
  pars.list.default <- list(image = NULL, stack=FALSE, image.depth = 8, image.normalize = FALSE , frame = NULL, tracks = NULL, all.list = FALSE,
                            par.map = NULL, par.shape = NULL, par.display = TRUE, par.max = NaN, par.min=NaN, par.unit = NULL,
                            crop = FALSE, sub.img = FALSE , sub.window = 200, sub.col = 3,
                            tracks.size = 1, tracks.alpha = 0.5, tracks.length = NULL,
                            points.size = 1, points.alpha = 0.8, points.stat = 'identity', points.shape = 16,
                            axis.tick = 100, axis.display = TRUE, axis.labs = TRUE, calibrate=FALSE,
                            unit = 'px', scaling = 1, dimensions = 2, projection = NULL, manual.z = NULL,
                            track.label = TRUE, tracks.label.x = 10, tracks.label.y = 10,
                            scale.bar = FALSE, scale.width = 40, scale.height = 10, scale.x = 10,
                            scale.y = 10, scale.color = 'grey70', interactive=FALSE)

  #' @param df dataframe of the form: \code{df(track, time, X, Y, (Z,) mapping_parameters, ...)}
  #' @param image \code{character}: filename of image
  #' @param stack \code{logical}: default: \code{FALSE}, single image file provided if time-resolved imagestack is used, set: \code{TRUE}
  #' @param image.depth \code{numeric}: set image bit-depth; just important if Z-projections are calculated
  #' @param image.normalize \code{logical}: normalize image
  #' @param frame \code{integer}: frame to be mapped
  #' @param tracks \code{vector}: defining tracks to be displayed
  #' @param par.map \code{character}: specifying parameter in \code{df} to be visualized by color
  #' @param par.shape \code{character}: specifying parameter in \code{df} to be mapped on shape
  #' @param par.display display option for mapping; default: \code{TRUE}, mapping is disable with: \code{FALSE}
  #' @param par.max \code{numeric}: defining upper range of color mapping
  #' @param par.min \code{numeric}: defining lower range of color mapping
  #' @param par.unit \code{character}: unit of the numeric mapped parameter
  #' @param crop \code{logical}: option for cropping images; default: \code{FALSE}
  #' @param sub.img \code{logical}: option for creating sub-images from specified \code{tracks} or pre-filtered \code{df}; default: \code{FALSE}
  #' @param sub.window \code{numeric}: size of the sub-images in pixels
  #' @param sub.col \code{numeric}: number of columns in which sub-images are arranged
  #' @param tracks.size \code{numeric}: size of tracks
  #' @param tracks.alpha \code{numeric}: transparency of tracks
  #' @param tracks.length \code{numeric}: length of tracks (in frames)
  #' @param tracks.label \code{logical}: when sub.img is used, display or hide track label
  #' @param tracks.label.x \code{numeric}: when sub.img is used, set x-position of label
  #' @param tracks.label.y \code{numeric}: when sub.img is used, set y-position of label
  #' @param points.size \code{numeric}: size of points
  #' @param points.alpha \code{numeric}: transparency of points
  #' @param points.stat \code{character}: display statistic; default: \code{'echo'}, for blurring; without blurring \code{'identity'}
  #' @param points.shape \code{numeric}: set shape from ggplot2 shape palette
  #' @param axis.tick \code{numeric}: axis ticks in px
  #' @param axis.display \code{logical}: display axis
  #' @param axis.labs \code{logical}: display labs
  #' @param unit \code{character}: setting name of unit; default: \code{'px'}
  #' @param scaling \code{numeric}: scaling factor for unit; default: \code{1}
  #' @param dimensions \code{numeric}: specify whether the images are 2D or 3D.
  #' If 3D is selected the data is assumed to be in the form: \code{df(track, time, X, Y, Z, mapping paramters, ...)}
  #' @param manual.z \code{numerice}: specify Z-plane to be visualized if no projection or sub windows are used
  #' @param scale.bar \code{logical}: show scalebar; default: \code{FALSE}
  #' @param scale.width \code{numeric}: width of scalebar; default: \code{40}
  #' @param scale.height \code{numeric}: height of scalebar; default: \code{10}
  #' @param scale.x \code{numeric}: distance from left border of the image towards scalebar
  #' @param scale.y \code{numeric}: distance from bottom border of the image towards scalebar
  #' @param scale.color \code{character}: specify color from R-color palette or hexcode
  #' @param interactive \code{logical}: return the plot as an interactive plotly object. Not supported when using
  #' sub.img or crop modes.
  #' @return returns a ggplot2 plot-object which can be further modified

  # get user input
  pars.list.user <- list(...)

  # check if all arguments were passed in a list or not
  if (length(pars.list.user) == 0) {
    pars.list <- pars.list.default
  } else {
    if (!is.null(pars.list.user$all.list)) {
      if(pars.list.user$all.list){
        pars.list.user <- pars.list.user[-which(names(pars.list.user) == "all.list")]
        pars.list.user <- pars.list.user[[1]]
      }
    }
    # match user and default values
    pars.list <- transfer_pars(pars.list.user,pars.list.default)
  }
  # check image path
  if (is.null(pars.list$image)) {
    stop('Image file not specified.')
  }
  # check frame
  if (is.null(pars.list$frame)) {
    stop('Frame not specified.')
  }
  # check if mapping parameter is specified
  if (is.null(pars.list$par.map)) {
    pars.list$par.map <- colnames(df)[5]
    if (is.character(pars.list$par.map)) {
      message('par.map not specified\n',
              paste('defaulted to:', pars.list$par.map,'\n'),
              'assuming: df(track, time, X, Y, mapping_parameters, ...)')
      if (is.numeric(df[pars.list$par.map] %>% pull())) {
        pars.list$par.max <- df %>% select(c(pars.list$par.map)) %>% pull() %>% max(na.rm = TRUE)
        pars.list$par.min <- df %>% select(c(pars.list$par.map)) %>% pull() %>% min(na.rm = TRUE)
      }
    } else {
      pars.list$par.map <- NULL
      message('par.map not specified:\n','no mapping parameter found\n',
              'color mapping disabled', call. = FALSE)
    }
  } else {
    # find min value for given parameter
    if(is.nan(pars.list$par.min)){
      pars.list$min <- df %>% select(c(pars.list$par.map)) %>% pull() %>% min(na.rm = TRUE)
    }
    # find max value for given parameter
    if(is.nan(pars.list$par.max)){
      pars.list$max <- df %>% select(c(pars.list$par.map)) %>% pull() %>% max(na.rm = TRUE)
    }
  }
  # add frames to df
  df <- df %>% mutate(frame=match(time, sort(unique(time))))

  # read in image
  image <- image_read(pars.list$image)

  # correct coordinate system for ggplot based on image properties
  image_height <- image_info(image) %>%
    select(height) %>%
    pull() %>%
    unique()

  # set up ggplot coordinates and image coordinates
  df <- df %>%
    mutate(Y_img = Y,
           X_img = X,
           Y= image_height - Y,
           X = X-1)

  # select image from stack
  if(pars.list$stack==TRUE & pars.list$dimension == 2){
  image <- image[pars.list$frame]
  }
  # normalize image
  if (pars.list$image.normalize) {
    image <- image %>% image_normalize()
  }
  # get pars for single imagefile
  if (pars.list$dimensions == 2) {
    pars.list$width <- image_info(image) %>% select(width) %>% pull() %>% unique()
    pars.list$height <- image_info(image) %>% select(height) %>% pull() %>% unique()
    if (length(pars.list$width) != 1 | length(pars.list$height) != 1){
      stop('VisuStatR detected different image sizes among the dataset and stopped. Please check your image files.')
    }
  }
  # get pars for image stack
  if (pars.list$dimensions == 3) {
    pars.list$width <- image_info(image) %>% select(width) %>% pull() %>% unique()
    pars.list$height <- image_info(image) %>% select(height) %>% pull() %>% unique()
    if (length(pars.list$width) != 1 | length(pars.list$height) != 1){
      stop('VisuStatR detected different images sizes among the dataset and stopped. Please check your image files.')
    }
    # calculate z-projection
    if (!is.null(pars.list$projection)) {
      image <- project_z(image, pars.list$width, pars.list$height, pars.list$projection, pars.list$image.depth)
    }
  }
  # calibrate images, just for debugging
  if (pars.list$calibrate) {
    image <- calibrate_img(df, pars.list$width, pars.list$height, pars.list)
  }
  # get cropping pars
  pars.list$crop_pars <- get_crop_pars(df, pars.list)

  # image processing
  image <- process_img(df,image, pars.list)

  # plot according to parameters
  suppressWarnings(
    if (pars.list$sub.img) {
      return(plot_frame_sub(df, image, pars.list))
    } else {
      if(pars.list$interactive==FALSE | pars.list$crop==TRUE){
        if(pars.list$crop== 1 & pars.list$interactive==TRUE){
          message('Set crop to false in order to use the interactive viewer. Defaulting to normal output.')
        }
        return(plot_frame(df, image, pars.list))
      } else {
        return(plot_frame(df, image, pars.list) %>%
                 ggplotly() %>%
                 layout(
                   images = list(
                     source = plotly::raster2uri(as.raster(image)),
                     x = 0, y = 0,
                     sizex = image_info(image) %>% select(width) %>% pull() %>% unique(),
                     sizey = image_info(image) %>% select(height) %>% pull() %>% unique(),
                     xref = "x", yref = "y",
                     xanchor = "left", yanchor = "bottom",
                     sizing = "fill",
                     layer='below'
                   )))
      }
    }
  )
}
