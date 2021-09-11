#' @title visustat_check
#' @description Gives a summary that allows to check the linking between a passed dataframe
#'  and images in a frame-wise manner.
#' @examples
#' data(hiv_motility)
#' images <- hiv_motility_images()
#' visustat_check(hiv_motility, images)

#' @export
visustat_check <- function(df, images, stack_time=FALSE, dimensions=2){
  #' @param df dataframe of the form: \code{df(track, time, X, Y, (Z,) mapping_parameters, ...)}
  #' @param images \code{vector}: filenames of images ordered by frames/time
  #' @param stack \code{logical}: default: \code{FALSE}, single image file provided if time-resolved imagestack is used, set: \code{TRUE}
  #' @param dimensions \code{numeric}: specify whether the images are 2D or 3D.
  #' If 3D is selected data is assumed to be in the form: \code{df(track, time, X, Y, Z, mapping paramters, ...)}

  # add frames to dataframe
  df <- df %>% mutate(frame=match(time, sort(unique(time)))) %>% distinct(frame, time)

  if(dimensions == 3 & stack_time == TRUE){
    stop("When using 3D data, supply individual time/frame resolved Z-stacks and set stack_time=FALSE")
  }

  if (length(images)==nrow(df)){
    if(stack_time==TRUE){
      message('Image stack has same number of slices as timepoints in the dataset.')
      return(df)
    } else {
      message('Same number of images as timepoints in the dataset.')
      return(df %>% mutate(image=images))
    }
  } else {
    message('VisuStatR detected differing number of timepoints in the dataset and images. Please check your dataset and image files or be careful.')
  }
}
