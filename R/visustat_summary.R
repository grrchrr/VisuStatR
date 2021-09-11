#' @title visustat_summary
#' @description
#'  With \code{visustat_summary}, time-resolved summary statistics are calculated. In correspondence to \code{visustat_frame} continuous and discrete parameters can be mapped individually on color and shape.
#'  In addition, the population mean and either standard deviations, confidence intervals or standard errors for respective statistics can be displayed as facetted ribbon plots.
#' @examples
#' data(hiv_motility)
#' visustat_summary(hiv_motility %>% na.omit(), par.map="speed", tracks=c(48, 66, 102, 108))

#' @export
visustat_summary <- function(df, ...) {
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
  # set default parameters
  pars.list.default <- list(par.map = NULL, par.numeric = NULL, group.vars = NULL, tracks = NULL,
                            ribbon = TRUE, ribbon.stat = 'sd', ribbon.alpha = 0.75, frame = NULL, time.unit = NULL,
                            geom = c('line','point'), legend = TRUE, all.list = FALSE,
                            line.size = 1, line.alpha = 1,
                            points.size = 1, points.alpha = 1, points.shape = 16)

  #' @param df dataframe of the form: \code{df(track, time, X, Y, mapping_parameters, ...)}
  #' @param frame \code{integer}: frame to be mapped
  #' @param time.unit \code{character}: time unit
  #' @param tracks \code{vector}: defining tracks to be displayed
  #' @param par.map \code{character}: specifying parameter in \code{df} to be visualized by color
  #' @param par.shape \code{character}: specifying parameter in \code{df} to be mapped on shape
  #' @param ribbon \code{logical}: display ribbon
  #' @param ribbon.stat \code{character}: choose ribbon-statistic: \code{'sd'}: standard deviation,
  #'  \code{'se'}: standard error,
  #'  \code{'ci'}: confidence interval
  #' @param line.size \code{numeric}: size of lines
  #' @param line.alpha \code{numeric}: transparency of lines
  #' @param line.length \code{numeric}: length of lines (in frames)
  #' @param points.size \code{numeric}: size of points
  #' @param points.alpha \code{numeric}: transparency of points
  #' @param points.shape \code{numeric}: set shape from ggplot2 shape palette
  #' @param unit \code{character}: setting name of unit; default: \code{'px'}

  # get user input
  pars.list.user <- list(...)

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

  # extracting values for defaults
  if (is.null(pars.list$frame)) {
    timepoints <- df %>% select(time) %>% pull()
    pars.list$frame <- max(match(timepoints, sort(unique(timepoints))))
    message(paste('frame not specified, defaulting to latest frame in dataset:', pars.list$frame))
  }

  # set grouping variables
  if (is.null(pars.list$group.vars)) {
    pars.list$group.vars <- colnames(df)[2]
  }

  # set mapping parameter
  if (is.null(pars.list$par.map)) {
    pars.list$par.map <- colnames(df)[5]
    warning(paste('par.map not specified, defaulting to:', pars.list$par.map), call. = FALSE)
  }
  # set x_max
  x_max <- df[,pars.list$group.vars] %>% max(na.rm = 1)
  x_min <- df[,pars.list$group.vars] %>% min(na.rm = 1)

  # check numeric parameters and get max-values
  if (is.null(pars.list$par.numeric)) {
    pars.list$par.numeric <- colnames(df[,5:ncol(df)])[grepl('numeric', sapply(df[,5:ncol(df)], class))]
    y_max <-  df %>% select(pars.list$par.numeric) %>% na.omit() %>%
      summarise_all(max) %>% as.list()
    y_min <-  df %>% select(pars.list$par.numeric) %>% na.omit() %>%
      summarise_all(min) %>% as.list()
    warning('pars.numeric not specified, ',
            paste('assuming the parameters to be: ', str_c(names(y_max), collapse = ', '),sep = ''))
  } else {
    y_max <-  df %>% select(pars.list$par.numeric) %>% na.omit() %>%
      summarise_all(max) %>% as.list()
    y_min <-  df %>% select(pars.list$par.numeric) %>% na.omit() %>%
      summarise_all(min) %>% as.list()
  }

  # setting y_scales
  scales_y <- mapply(function(x,y){scale_y_continuous(limits = c(x,y))}, y_min, y_max)

  # create summary df
  df_stat <- summary_mot(df, measure_vars = pars.list$par.numeric, group_vars = pars.list$group.vars) %>%
    select(c('time','mean','measure',pars.list$ribbon.stat)) %>% rename_(sd = as.name(pars.list$ribbon.stat))

  # create plot_df
  df_plot <- df  %>% mutate_(visu_col = as.name(pars.list$par.map)) %>%
    select(time, track, visu_col, c(pars.list$par.numeric)) %>%
    gather('measure','value',-track, -time, -visu_col) %>% mutate(frame=match(time, sort(unique(time))))

  frame_time <- df_plot  %>% distinct(frame, time) %>% filter(frame==pars.list$frame) %>% pull(time)

  # plotting
  plot <- ggplot(df_stat, aes(x = time, y = mean)) +
    geom_path() +
    facet_grid_sc(rows = vars(measure), scales = list(y = scales_y)) +
    scale_colour_viridis_c(limits = c(y_min[[pars.list$par.map]], y_max[[pars.list$par.map]]), na.value = 'red') +
    labs(y = 'Mean',
         color = str_to_sentence(pars.list$par.map)) +
    scale_x_continuous(limits = c(x_min,x_max), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_vline(xintercept = frame_time,
               alpha = 0.5,
               col = 'red')
  # add legend
  if (!pars.list$legend) {
    plot <- plot + theme(legend.position = "none",
                         plot.margin = unit(c(5,5,5,5),'mm'))
  } else {
    plot <- plot + theme(plot.margin = unit(c(5,5,5,5),'mm'))
  }
  # add ribbon
  if (pars.list$ribbon) {
    plot <- plot + geom_ribbon(aes(ymin = mean - sd,
                                    ymax = mean + sd),
                                    col = NA,
                                    fill = 'grey70',
                                    alpha = pars.list$ribbon.alpha)
  }
  # add xlab
  if (!is.null(pars.list$time.unit)) {
    plot <- plot + xlab(paste0(str_to_sentence(pars.list$group.vars), ' [', pars.list$time.unit, ']'))
  } else {
    plot <- plot + xlab(str_to_sentence(pars.list$group.vars))
  }
  # check df for NAs and remove them for certain timepoints (beginning of plotting area)
  if (df_plot %>% filter(frame == pars.list$frame) %>% na.omit() %>% nrow() > 0) {
    df_plot <- df_plot %>% na.omit()
  }
  # filter for tracks
  if (!is.null(pars.list$tracks)) {
    df_plot <- df_plot %>% filter(track %in% pars.list$tracks)
  }
  # add jitter
  if (any(grepl('jitter',pars.list$geom))) {
    p <- plot +
      geom_jitter(data = df_plot %>% filter(frame == pars.list$frame),
                  aes(x = time, y = value, col = visu_col),
                  alpha = pars.list$points.alpha, shape = pars.list$points.shape,
                  size = pars.list$points.size, width = 2, height = 0.1)
  } else {# add path and point
    p <- plot + geom_path(data = df_plot %>% filter(frame <= pars.list$frame),
                          aes(x = time, y = value, col = visu_col, group = track))
    if (any(grepl('point',pars.list$geom))) {
      p <- p + geom_point(data = df_plot %>% filter(frame == pars.list$frame),
                          aes(x = time, y = value, col = visu_col),
                          alpha = pars.list$points.alpha, shape = pars.list$points.shape,
                          size = pars.list$points.size)
    }
  }
  return(p)
}
