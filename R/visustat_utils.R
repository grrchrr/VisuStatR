# Utility functions ####
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




summary_mot <- function(df, measure_vars, group_vars){
  #' @import Rmisc
  out_df <- tibble()
  for (i in measure_vars) {
    df_i <- Rmisc::summarySE(df, measurevar = i, groupvars = group_vars, na.rm = TRUE ) %>% mutate(measure = i)
    colnames(df_i) <- gsub(i,'mean',colnames(df_i))
    out_df <- bind_rows(out_df,df_i)
  }
  return(out_df)
}

crop_string <- function(pars){
  x_min <- pars[['x_min_img']]
  x_max <- pars[['x_max_img']]
  y_min <- pars[['y_min_img']]
  y_max <- pars[['y_max_img']]
  window_x <- x_max - x_min
  window_y <- y_max - y_min
  center_x <- x_min + window_x/2
  center_y <- y_min + window_y/2
  offset_x <- center_x - (window_x / 2)
  if (offset_x >= 0) {
    offset_x <- paste("+", offset_x, sep = "")
  }
  offset_y <- center_y - (window_y / 2)
  if (offset_y >= 0) {
    offset_y <- paste("+", offset_y, sep = "")
  }
  crop_string <- paste(
    paste(window_x),
    "x",
    paste(window_y),
    paste(offset_x),
    paste(offset_y),
    sep = "")

  return(crop_string)
}

crop_string_df <- function(x_min, x_max, y_min, y_max){
  window_x <- x_max - x_min
  window_y <- y_max - y_min
  center_x <- x_min + window_x/2
  center_y <- y_min + window_y/2
  offset_x <- center_x - (window_x / 2)
  offset_x <- paste("+", offset_x, sep = "")
  offset_y <- center_y - (window_y / 2)
  offset_y <- paste("+", offset_y, sep = "")
  crop_string <- paste(
    paste(window_x),
    "x",
    paste(window_y),
    paste(offset_x),
    paste(offset_y),
    sep = "")
  return(crop_string)
}

transfer_pars <- function(user,default){
  names_user <- names(user)
  names_default <- names(default)
  user_pars <- intersect(names_user,names_default)
  default_pars <- setdiff(names_default, names_user)
  pars.list <- c(user[user_pars], default[default_pars])
}

get_crop_pars <- function(df, pars.list){
  crop_pars <- NULL
  if (!is.null(pars.list$tracks)) {
    df <- df %>% filter(track %in% pars.list$tracks)
  }
  if (pars.list$sub.img) {
    if (pars.list$crop) {# with subimages that display each track completly
      crop_pars <- df %>% group_by(track) %>%
        summarise(x_min = min(X),
                  x_max = max(X),
                  y_min = min(Y),
                  y_max = max(Y),
                  x_min_img = min(X_img)-1,
                  x_max_img = max(X_img)-1,
                  y_min_img = min(Y_img),
                  y_max_img = max(Y_img)) %>%
        group_by(track) %>%
        mutate(string = crop_string_df(x_min_img, x_max_img, y_min_img, y_max_img)) %>% ungroup()
    } else {# each window has the same size
      window_half <- pars.list$sub.window/2
      crop_pars <- df %>%
        group_by(track) %>%
        filter(frame <= pars.list$frame) %>%
        filter(frame == max(frame)) %>%
        mutate(X_img = ifelse(X_img < window_half, window_half, # if windows exceeds image boundaries...
                          ifelse(pars.list$width - X_img < window_half, pars.list$width - window_half, X_img-1)),
               Y_img = ifelse(Y_img < window_half, window_half,
                          ifelse(pars.list$height - Y_img < window_half, pars.list$height - window_half ,Y_img)),
               x_min_img = X_img - window_half,
               x_max_img = X_img + window_half,
               y_min_img = Y_img - window_half,
               y_max_img = Y_img + window_half,

               X = ifelse(X < window_half, window_half, # if windows exceeds image boundaries...
                              ifelse(pars.list$width - X < window_half, pars.list$width - window_half, X)),
               Y = ifelse(Y < window_half, window_half,
                              ifelse(pars.list$height - Y < window_half, pars.list$height - window_half ,Y)),
               x_min = X - window_half,
               x_max = X + window_half,
               y_min = Y - window_half,
               y_max = Y + window_half) %>%
        mutate(string = crop_string_df(x_min_img, x_max_img, y_min_img, y_max_img)) %>%
        ungroup()
    }
  }
  # crop whole image
  if (pars.list$crop & !pars.list$sub.img) {
    crop_pars <- df %>% summarise(x_min_img = min(X_img)-1,
                                  x_max_img = max(X_img)-1,
                                  y_min_img = min(Y_img),
                                  y_max_img = max(Y_img),
                                  x_min = min(X),
                                  y_min=min(Y),
                                  x_max=max(X),
                                  y_max=max(Y)) %>% as.list()
  }
  return(crop_pars)
}

process_img <- function(df,image, pars.list){
  if (pars.list$sub.img) {
    if (!is.null(pars.list$tracks)) {
      tracks <- pars.list$tracks
    } else {
      tracks <- df %>% distinct(track) %>% pull()
    }
    if (is.null(pars.list$projection) & pars.list$dimensions == 3) {
      for (i in tracks) {# create cropped images and store as stack
        crop_string_track <- pars.list$crop_pars %>% filter(track == i) %>% select(string) %>% pull()
        z <- df %>% filter(track == i,
                           frame == pars.list$frame) %>% select(Z) %>% pull()
        z_low <- as.integer(z)
        z_up <- z_low + 1
        weights <- c(1 - (z - z_low), 1 - (z_up - z_low))
        image_process <- image[c(z_low,z_up)] %>% project_z(pars.list$width, pars.list$height, 'mean',pars.list$image_depth, weights)
        if (length(crop_string_track) == 1) {
          image_cropped <- image_process[1] %>%
            image_crop(crop_string_track) #%>%
            #image_flip()
          image_process <- c(image_process,image_cropped)
        } else {# if track not present yet, add blank image
          image_process <- c(image_process,image_blank(pars.list$sub.window, pars.list$sub.window))
        }
      }
      image <- image_process
    }
    for (i in tracks) {# create cropped images and store as stack
      crop_string_track <- pars.list$crop_pars %>% filter(track == i) %>% select(string) %>% pull()
      if (length(crop_string_track) == 1) {
        image_cropped <- image[1] %>%
          image_crop(crop_string_track) #%>%
          #image_flip()
        image <- c(image,image_cropped)
      } else {# if track not present yet, add blank image
        # get crop string for first occurence of track
        window_half <- pars.list$sub.window / 2
        crop_string_track <- df %>% filter(track == i, frame == min(frame[track == i])) %>%
        mutate(X = ifelse(X < window_half, window_half, # if windows exceeds image boundaries...
                          ifelse(pars.list$width - X < window_half, pars.list$width - window_half, X)),
               Y = ifelse(Y < window_half, window_half,
                          ifelse(pars.list$height - Y < window_half, pars.list$height - window_half ,Y)),
               x_min = X - window_half,
               x_max = X + window_half,
               y_min = Y - window_half,
               y_max = Y + window_half) %>%
          mutate(string = crop_string_df(x_min, x_max, y_min, y_max)) %>% select(string) %>% pull()
        image_cropped <- image[1] %>%
          image_crop(crop_string_track) #%>%
          #image_flip()
        image <- c(image,image_cropped)
      }
    }
  } else {
    if (pars.list$crop) {
      image <- image %>% image_crop(crop_string(pars.list$crop_pars)) #%>% image_flip()
    } else {
      image <- image #%>% image_flip()
    }
  }
  return(image)
}


plot_frame <- function(df, image, pars.list){
  # set coodinates to center of pixels
  df <- df %>% mutate(X = X + 0.5, Y = Y + 0.5)

  # set labeling string
  pars.list$label.col <- ifelse(is.null(pars.list$par.unit) == TRUE, str_to_sentence(pars.list$par.map),
                                str_c(str_to_sentence(pars.list$par.map),' [',pars.list$par.unit, ']'))

   # define background image
  bg <- rasterGrob(image, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)

   # filter for tracks
  if (!is.null(pars.list$tracks)) {
    df <- df %>% filter(track %in% pars.list$tracks)
  }
  # filter for track length
  if (is.numeric(pars.list$tracks.length) & !identical(pars.list$tracks.length,0)) {
      df <-  df %>% filter(frame >= pars.list$frame - pars.list$tracks.length)
  }
  # set aesthetics
  if (pars.list$par.display) {
    if (is.null(pars.list$par.shape)) {
      p <- ggplot(df %>% filter(frame <= pars.list$frame),
                  aes_(x = ~X,
                       y = ~Y,
                       group = ~track,
                       col = as.name(pars.list$par.map))) +
        labs(x = paste0('X [', pars.list$unit,']'),
             y = paste0('Y [', pars.list$unit,']'),
             color = pars.list$label.col)
    } else {
      p <- ggplot(df %>% filter(frame <= pars.list$frame),
                  aes_(x = ~X,
                       y = ~Y,
                       group = ~track,
                       col = as.name(pars.list$par.map),
                       shape = as.name(pars.list$par.shape))) +
        labs(x = paste0('X [', pars.list$unit,']'),
             y = paste0('Y [', pars.list$unit,']'),
             color = pars.list$label.col,
             shape = str_to_sentence(pars.list$par.shape))
    }
  } else {
    p <- ggplot(df %>% filter(frame <= pars.list$frame),
                aes_(x = ~X,
                     y = ~Y,
                     group = ~track,
                     col = 'red')) +
      labs(x = paste0('X [', pars.list$unit,']'),
           y = paste0('Y [', pars.list$unit,']')) +
      theme(legend.position = 'none')
  }

  # set theme and fixed coords
  p <- p +
    theme(plot.margin = unit(c(2,2,2,2),'mm'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    coord_fixed()

  # create either cropped or full background image with scales
  if (pars.list$crop) {
    crop_pars <- pars.list$crop_pars
    axis_ticks_x <- seq(crop_pars[['x_min']], crop_pars[['x_max']], pars.list$axis.tick)
    axis_ticks_y <- seq(crop_pars[['y_min']], crop_pars[['y_max']], pars.list$axis.tick)
    p <- p + annotation_custom(bg,
                               xmin = crop_pars[['x_min']],
                               xmax = crop_pars[['x_max']],
                               ymin = crop_pars[['y_min']],
                               ymax = crop_pars[['y_max']]) +
      scale_x_continuous(limits = c(crop_pars[['x_min']], crop_pars[['x_max']]),
                         breaks = axis_ticks_x,
                         labels = axis_ticks_x*pars.list$scaling,
                         expand = c(0, 0)) +
      scale_y_continuous(limits = c(crop_pars[['y_min']], crop_pars[['y_max']]),
                         breaks = axis_ticks_y,
                         labels = axis_ticks_y*pars.list$scaling,
                         expand = c(0, 0))
  } else {
    # set axis ticks
    axis_ticks_x <- seq(pars.list$axis.tick, pars.list$width, pars.list$axis.tick)
    axis_ticks_y <- seq(pars.list$axis.tick, pars.list$height, pars.list$axis.tick)
    p <- p + annotation_custom(grob = bg,
                               xmin = 0,
                               xmax = pars.list$width,
                               ymin = 0,
                               ymax = pars.list$height) +
      scale_x_continuous(limits = c(0, pars.list$width),
                         expand = c(0, 0),
                         breaks = c(1, axis_ticks_x),
                         labels = c(0, axis_ticks_x)*pars.list$scaling) +
      scale_y_continuous(limits = c(0, pars.list$height),
                         expand = c(0, 0),
                         breaks = c(1, axis_ticks_y),
                         labels = c(0, axis_ticks_y)*pars.list$scaling)
  }
  # add tracks
  if (!identical(pars.list$tracks.length,0)) {
      p <- p +
        geom_path(alpha = pars.list$tracks.alpha,
                  size = pars.list$tracks.size) +
        geom_point(data = df %>% filter(frame == pars.list$frame),
                   alpha = pars.list$points.alpha,
                   stat = pars.list$points.stat,
                   position = 'identity',
                   size = pars.list$points.size)
  } else {
    p <- p +
      geom_point(data = df %>% filter(frame == pars.list$frame),
                 alpha = pars.list$points.alpha,
                 stat = pars.list$points.stat,
                 position = 'identity',
                 size = pars.list$points.size)
  }
  # add continuous color scale
  if (pars.list$par.display) {
    if (is.numeric(df[pars.list$par.map] %>% pull())) {
      if (str_count(pars.list$label.col, "\\S+") > 1) {
        p <- p + scale_color_viridis_c(limits = c(pars.list$par.min,pars.list$par.max),
                                       na.value = 'red',
                                       guide = guide_colorbar(title.position = 'left',
                                                              title.theme = element_text(angle = 90),
                                                              label.position = 'right',
                                                              title.hjust = 0.5))
      } else {
        p <- p + scale_colour_viridis_c(limits = c(pars.list$par.min,pars.list$par.max),
                                        na.value = 'red')
      }
    }
  }
  # add discrete color scale
  if (pars.list$par.display) {
    if (is.factor(df[pars.list$par.map] %>% pull()) | is.character(df[pars.list$par.map] %>% pull()) ) {
      p <- p + scale_colour_viridis_d(na.value = 'red')
    }
  }
  # add scale bar
  if (pars.list$scale.bar == TRUE) {
    p <- p + geom_rect(xmin = pars.list$width - pars.list$scale.width - pars.list$scale.x,
                       xmax = pars.list$width - pars.list$scale.x,
                       ymin = pars.list$scale.y,
                       ymax = pars.list$scale.height + pars.list$scale.y,
                       fill = pars.list$scale.color,
                       col = pars.list$scale.color)
  }
  if(pars.list$axis.display == FALSE){
    p <- p + theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank())
  }
  return(p)
}

plot_frame_sub <- function(df, image, pars.list){
  # set coodinates to center of pixels
  df <- df %>% mutate(X = X +  0.5, Y = Y + 0.5)

  # set labeling string
  pars.list$label.col <- ifelse(is.null(pars.list$par.unit) == TRUE, str_to_sentence(pars.list$par.map),
                                str_c(str_to_sentence(pars.list$par.map),' [',pars.list$par.unit, ']'))

  # update tracks if not specified
  if (is.null(pars.list$tracks)) {
    pars.list$tracks <- df %>% distinct(track) %>% pull()
  }
  # filter for track length
  if (is.numeric(pars.list$tracks.length) & !identical(pars.list$tracks.length,0)) {
    df <- df %>% filter(frame >= pars.list$frame - pars.list$tracks.length)
  }
  # set up list for subplots
  plots <- vector("list",length = length(pars.list$tracks))

  # set aesthetics
  if (pars.list$par.display) {
    if (is.null(pars.list$par.shape)) {
      p <- ggplot(df %>% filter(frame <= pars.list$frame),
                  aes_(x = ~X,
                       y = ~Y,
                       group = ~track,
                       col = as.name(pars.list$par.map))) +
        labs(x = paste0('X [', pars.list$unit,']'),
             y = paste0('Y [', pars.list$unit,']'),
             color = pars.list$label.col)
    } else {
      p <- ggplot(df %>% filter(frame <= pars.list$frame),
                  aes_(x = ~X,
                       y = ~Y,
                       group = ~track,
                       col = as.name(pars.list$par.map),
                       shape = as.name(pars.list$par.shape))) +
        labs(x = paste0('X [', pars.list$unit,']'),
             y = paste0('Y [', pars.list$unit,']'),
             color = pars.list$label.col,
             shape = str_to_sentence(pars.list$par.shape))
    }
  } else {
    p <- ggplot(df %>% filter(frame <= pars.list$frame),
                aes_(x = ~X,
                     y = ~Y,
                     group = ~track,
                     col = 'red')) +
      labs(x = paste0('X [', pars.list$unit,']'),
           y = paste0('Y [', pars.list$unit,']')) +
      theme(legend.position = 'none')
  }

  # add continuous color scale
  if (pars.list$par.display) {
    if (is.numeric(df[pars.list$par.map] %>% pull())) {
      if (str_count(pars.list$label.col, "\\S+") > 1) {
        p <- p + scale_color_viridis_c(limits = c(pars.list$par.min, pars.list$par.max),
                                       na.value = 'red',
                                       guide = guide_colorbar(title.position = 'left',
                                                              title.theme = element_text(angle = 90),
                                                              label.position = 'right',
                                                              title.hjust = 0.5))
      } else {
        p <- p + scale_colour_viridis_c(limits = c(pars.list$par.min, pars.list$par.max),
                                        na.value = 'red')
      }
    }
    # add discrete color scale
    if (is.factor(df[pars.list$par.map] %>% pull()) | is.character(df[pars.list$par.map] %>% pull()) ) {
      p <- p + scale_colour_viridis_d(na.value = 'red')
    }

    # get legend
    legend <- get_legend(p + geom_point())
  }

  # set theme
  p <- p + theme(plot.margin = unit(c(2,2,2,2),'mm'),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank()) +
    coord_fixed()



  # subplots
  for (i in c(1:length(pars.list$tracks))) {
    pars_plot <- pars.list$crop_pars %>% filter(track == pars.list$tracks[i]) %>% select(-track) %>% as.list()
    bg <- rasterGrob(image[i + 1], # index skipped original uncropped image
                     width = unit(1, "npc"),
                     height = unit(1, "npc"),
                     interpolate = TRUE)
     if (length(pars_plot[['string']]) == 0) { # if track not present, empty window
       plots[[i]] <- p +
         annotation_custom(bg,
                           xmin = -Inf,
                           xmax = Inf,
                           ymin = -Inf,
                           ymax = Inf) +
         theme(legend.position = 'none',
               axis.title = element_blank(),
               axis.text = element_blank(),
               axis.ticks = element_blank()) +
         annotate("text",
                  x = pars.list$tracks.label.x,
                  y = pars.list$tracks.label.y,
                  label = ifelse(pars.list$tracks.label == 1, pars.list$tracks[i], NULL),
                  col = pars.list$scale.color) +
         coord_fixed() +
         scale_x_continuous(limits = c(1, pars.list$sub.window),
                            expand = c(0, 0)) +
         scale_y_continuous(limits = c(1, pars.list$sub.window),
                            expand = c(0, 0))
     } else {
       # add tracks
        if (identical(pars.list$tracks.length,0)) {
           p1 <- p +
             annotation_custom(bg,
                               xmin = pars_plot[['x_min']],
                               xmax = pars_plot[['x_max']],
                               ymin = pars_plot[['y_min']],
                               ymax = pars_plot[['y_max']]) +
             annotate("text",
                      x = pars_plot[['x_min']] + pars.list$tracks.label.x,
                      y = pars_plot[['y_min']] + pars.list$tracks.label.y,
                      label = ifelse(pars.list$tracks.label == 1, pars.list$tracks[i], NULL),
                      col = pars.list$scale.color) +
             geom_point(data = df %>% filter(frame == pars.list$frame),
                        alpha = pars.list$points.alpha,
                        stat = pars.list$points.stat,
                        size = pars.list$points.size) +
             scale_x_continuous(limits = c(pars_plot[['x_min']], pars_plot[['x_max']]),
                                expand = c(0, 0)) +
             scale_y_continuous(limits = c(pars_plot[['y_min']],pars_plot[['y_max']]),
                                expand = c(0, 0)) +
             theme(legend.position = 'none',
                   axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank())
       } else {
         p1 <- p +
           annotation_custom(bg,
                             xmin = pars_plot[['x_min']],
                             xmax = pars_plot[['x_max']],
                             ymin = pars_plot[['y_min']],
                             ymax = pars_plot[['y_max']]) +
           annotate("text",
                    x = pars_plot[['x_min']] + pars.list$tracks.label.x,
                    y = pars_plot[['y_min']] + pars.list$tracks.label.y,
                    label = ifelse(pars.list$tracks.label == 1, pars.list$tracks[i], NULL),
                    col = pars.list$scale.color) +
           geom_path(alpha = pars.list$tracks.alpha,
                     size = pars.list$tracks.size) +
           geom_point(data = df %>% filter(frame == pars.list$frame),
                      alpha = pars.list$points.alpha,
                      stat = pars.list$points.stat,
                      size = pars.list$points.size) +
           scale_x_continuous(limits = c(pars_plot[['x_min']], pars_plot[['x_max']]),
                              expand = c(0, 0)) +
           scale_y_continuous(limits = c(pars_plot[['y_min']],pars_plot[['y_max']]),
                              expand = c(0, 0)) +
           theme(legend.position = 'none',
                 axis.title = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank())
           }
      # add scale bars
      if (pars.list$scale.bar == TRUE) {
        p1 <- p1 + geom_rect(xmin = pars_plot[['x_max']] - pars.list$scale.width - pars.list$scale.x,
                             xmax = pars_plot[['x_max']] - pars.list$scale.x,
                             ymin = pars_plot[['y_min']] + pars.list$scale.y,
                             ymax = pars_plot[['y_min']] + pars.list$scale.height + pars.list$scale.y,
                             fill = pars.list$scale.color,
                             col = pars.list$scale.color)
      }
      plots[[i]] <- p1
    }
  }
  # putting all plots together
  collage <- arrangeGrob(grobs = plots, ncol = pars.list$sub.col)
  if (pars.list$par.display) {
    p2 <- plot_grid(collage, NULL , legend, NULL, rel_widths = c(10, 0.5, 1, 0.5), ncol = 4)
    return(p2)
  } else {
    return(plot_grid(collage))
  }

}

# Z-projection for 3D-image stacks
project_z <- function(image, width, height, method, depth, weights=NULL) {

  methods <- list(mean = mean,
                  sd = sd,
                  min = min,
                  max = max,
                  median = median)
  n <- length(image)

  stack <- array(NA, dim = c(height,width,n))
  for (i in c(1:n)) {
    stack[,,i] <- image[[i]] %>% as.integer()
  }

  if (is.null(weights)) {
    projection <- array(apply(stack, c(1,2), methods[[method]]) %>% as.vector(), dim = c(height,width, 1))
  } else {
    projection <- array(apply(stack, c(1,2), weighted.mean, weights) %>% as.vector(), dim = c(height,width, 1))
  }

  if (depth == 8) {
    return(image_read(projection/255))
  }

  if (depth == 16) {
    return(image_read(projection/65536))
  }
}


# calibration tiffs
calibrate_img <- function(df,width,height,pars.list){
  pos <- df %>% filter(time == pars.list$frame - 1) %>%
    mutate(X = round(X), Y = round(Y)) %>% select(X, Y) %>% as.list()
  array <- array(NA, dim = c(height, width, 1))
  for (i in c(1:length(pos$Y))) {
    array[pos$Y[i], pos$X[i], 1] <- 1
  }
  return(image_read(array))
}




# _________________________ ####
