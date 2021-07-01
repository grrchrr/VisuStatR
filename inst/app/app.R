#### Shiny VisuStatR ####
#______________________________________ ####

# 1: Load Libraries ####
library(shiny)
library(rlang)
library(magrittr)
library(VisuStatR)
library(DT)
library(shinythemes)
library(ggecho)
library(facetscales)
library(shinydashboard)
library(shinyFiles)
library(shinyWidgets)
library(colourpicker)
library(shinyBS)
#______________________________________ ####

# 2: UI elements ####

# 2.1: Dashboard elements ####

# 2.1.1: Header ####
header <- dashboardHeader(title = 'VisuStatR')

# 2.1.2: Sidebar ####
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem('Read Data', tabName = 'read', icon = icon('folder-open'),startExpanded = TRUE,
                 menuSubItem('Image Files', tabName = 'read_images', icon = icon('images')),
                 menuSubItem('Tracking Data', tabName = 'read_data', icon = icon('line-chart'))),
        menuItem('Run VisuStatR', tabName = 'visustatR', icon = icon('laptop-code'),
                 menuSubItem('Frame', tabName = 'visustat_frame', icon = icon('image')),
                 menuSubItem('Summary', tabName = 'visustat_summary', icon = icon('bar-chart-o')),
                 menuSubItem('Animation', tabName = 'visustat_all', icon = icon('film'))),
        menuItem('How to', tabName = 'how_to', icon = icon('mortar-board')),
        menuItem('About', tabName = 'about', icon = icon('question')))
    )

# 2.1.3: Tab Items ####
# 2.1.3.1: Read Data ####
read_data <- tabItem(tabName = 'read_data',
           box(width = 12, title = 'Tracking Data',
               fluidRow(column(width = 2,
                               shinyFilesButton('file', ' Browse Files', 'Please select file(s)',
                                                multiple = TRUE, viewtype = 'detail',
                                                icon = icon('folder'),
                                                            style = "margin-top: +25px;")),
                        column(width = 3,uiOutput('select_data')),
                        column(width = 2,actionButton('load_df', label = 'Read in', icon = icon('download'),
                                                    style = "margin-top: +25px;")),
                        column(width = 4,actionButton('update_df', 'Update dataframe', icon = icon('refresh'),
                                                    style = "margin-top: +25px;"))
                        ),
               fluidRow(
                   column(width = 2,
                          numericInput('scale_dim', 'Dimension scaling', 1),
                          radioGroupButtons('dims_df', 'Dimensions', choices = list('2D' = 2, '3D' = 3))),
                   uiOutput('prepare_df')),
               uiOutput('df_table'))
            )

# 2.1.3.2: Read Images ####
read_images  <- tabItem(tabName = 'read_images', fluidRow(
    column(width = 3,
           box(width = NULL, title = 'Image Specifications',
               shinyFilesButton('images', 'Load Images', 'Please select file(s)', multiple = TRUE, viewtype = 'detail', icon = icon('folder')),
               checkboxInput('example_images', 'Use example images'),
               br(), hr(),
               radioGroupButtons('color_space', 'Color Space', choices = list('Grayscale' = 'gray', 'RGB' = 'rgb')),
               radioGroupButtons('bit_depth','Bit Depth', choices = list('8-bit' = 8,'16-bit' = 16,'32-bit' = 32)),
               radioGroupButtons('dims_img', 'Dimensions', choices = list('2D' = 2,'3D' = 3)),
               radioGroupButtons('stack','Timeseries', choices = list('Multiple Files' = FALSE, 'Stack' = TRUE)),
               switchInput(label = 'Normalize',
                           inputId = 'normalize',
                           value = FALSE)),hr()),
    column(width = 9,
           box(width = NULL, height = '100%' ,title = 'Image Browser',
               uiOutput('select_image_ui'),
               hr(),
               fillPage(imageOutput('img', height = '400px')),
               hr())
           ))
)


# 2.1.3.3: Frame ####
visustat_frame_tab <- tabItem(tabName = 'visustat_frame', fluidRow(
    column(width = 3,
           box(width = NULL, title = 'Options',
               switchInput(label = 'Mapping',
                           inputId = 'mapping',
                           value = TRUE
               ),
               fluidRow(
                   column(width = 9,
                          checkboxGroupButtons(inputId = 'map.select',choiceValues = c('color','shape'),
                                               choiceNames = c('Color','Shape'),
                                               status = 'primary',
                                               selected = 'color',
                                               justified = TRUE)),
                   column(width = 1, actionButton('map_settings', '',  icon = icon('gear')))),
               uiOutput('par.map.options'),
               fluidRow(column(width = 9, switchInput(label = 'Subwindow',
                                                    inputId = 'sub_window',
                                                    value = FALSE)),
                        column(width = 1, actionButton('subwindow_settings','', icon = icon('gear')))),
               bsModal('popup_subwindow_settings','Subwindow Options','subwindow_settings',
                       sliderInput('sub_n_col','Columns', value = 3, min = 1, max = 15, step = 1),
                       sliderInput('sub_window_size', 'Windowsize', value = 50, min = 5, max = 250, step = 5)
                       ),
               fluidRow(column(width = 9, switchInput(label = 'Tracks',
                                                    inputId = 'tracks',
                                                    value = TRUE)),
                        column(width = 1, actionButton('tracks_settings','', icon = icon('gear')))),
               bsModal('popup_tracks_settings','Tracks Options','tracks_settings',
                       uiOutput('tracks_opt')),
               fluidRow(column(width = 9, switchInput(label = 'Points',
                                                    inputId = 'points',
                                                    value = TRUE)),
                        column(width = 1, actionButton('points_settings','', icon = icon('gear')))),
               bsModal('popup_points_settings','Point Options','points_settings',
                       sliderInput('points.size','Size', value = 1, min = 1, max = 20, step = 1),
                       sliderInput('points.alpha','Alpha', value = 0.75, min = 0, max = 1, step = 0.05),
                       radioGroupButtons('points.stat','Blur',choices = list('On' = 'echo', 'Off' = 'identity'))),
               fluidRow(column(width = 9, switchInput(label = 'Scalebar',
                                                   inputId = 'scalebar',
                                                   value = FALSE)),
                        column(width = 1, actionButton('scale_settings','', icon = icon('gear')))),
               bsModal('popup_scale_settings','Scalebar Options','scale_settings',
                       numericInput('scale.width','Width', min = 1, max = NA, value = 40),
                       numericInput('scale.height', 'Height', min = 1, max = NA, value = 10),
                       numericInput('scale.x', 'X offset', min = 1, max = NA, value = 10),
                       numericInput('scale.y', 'Y offset', min = 1, max = NA, value = 10),
                       colourInput('scale.color','Color',value = 'grey70')
                       ),
               fluidRow(column(width = 9, switchInput(label = 'Axis',
                                                   inputId = 'axis',
                                                   value = FALSE)),
                        column(width = 1, actionButton('axis_settings','', icon = icon('gear')))),
               bsModal('popup_axis_settings','Axis Options','axis_settings',
                       switchInput('axis.display','Display Axis'),
                       switchInput('axis.labs','Display Labels'),
                       textInput('unit','Unit:', value = 'px'),
                       numericInput('scaling', 'Scale by:', min = NA, max = NA, value = 1),
                       numericInput('axis.tick','Ticks:', min = 1, value = NULL, max = 3000))
               ),
    box(width = NULL, title = NULL,
                     fluidRow(column(width = 9,
                                downloadButton('save_frame','Download', icon = icon('download'), color = 'primary')),
                         column(width = 3,
                                actionButton('download_settings','',icon = icon('gear')))),
        bsModal('popup_download_settings', 'Download Settings', 'download_settings',
                selectInput('file_format','Format:',choices = c('png','pdf','jpeg')),
                selectInput('unit_download','Unit:', choices = c('cm','in')),
                numericInput('resolution','DPI:', min = 10, max = 600, value = 300),
                numericInput('width','Width:', min = 1, max = 100, value = 20),
                numericInput('height','Height:', min = 1, max = 100, value = 20))),
    ), column(width = 9,
              box(width = NULL, height = '90vh', title = 'Viewer',
                  uiOutput('frame_select'),
                  div(plotOutput('visustat_frame', height = '65vh'), align = 'center'),
                  )
              )
    )
)

# 2.1.3.4: Summary ####
visustat_sum_tab <- tabItem('visustat_summary', fluidRow(
    column(width = 3,
           box(width = NULL, title = 'Options',
               uiOutput('summary_ui'))),
    column(width = 9,
           box(width = NULL, title = 'Summary',
           plotOutput('visustat_summary')))

    )
)
# 2.1.3.5: Animation ####
visustat_all_tab <- tabItem('visustat_all', fluidRow(
    column(width = 2,
           box(width = NULL, title = 'Options',
               selectInput('display', 'Layout', choices = c('Frame','Summary','Both')),
               uiOutput('animation_range'),
               numericInput('width_all', 'Width', min = 1, max = NA, value = 2000, step = 1),
               numericInput('height_all', 'Height', min = 1, max = NA, value = 1200, step = 1),
               numericInput('rel_width', 'Ratio', value = 0.65, step = 0.05, min = 0.05, max = 0.95),
               textInput('file_name', label = 'Filename', placeholder = 'Enter filename'))),
    column(width = 10,
           box(width = NULL, title = 'Image Series Viewer',
               'When clicking Start rendering the image series with the options from the Frame and Summary panels will be rendered.
               You can preview the output of single frames in order to be sure everything is of your liking. Resulting animations will be saved to your current working directory.',
               hr(),
               actionButton('preview_frame', 'Update Preview', icon = icon('refresh')),
               actionGroupButtons(inputIds = c('next_preview', 'prev_preview'),
                                  labels = list(tags$span(icon('angle-left'),''), tags$span(icon('angle-right'),'')),
                                  status = 'primary'),
               actionButton('run_all','Start rendering', icon = icon('refresh')),
               hr(),
               br(),
               imageOutput('preview_all'),
               br(),
               hr()
               ))

)
)
# 2.1.3.6: How to ####

# 2.1.3.7: About ####
about <- tabItem(tabName = 'about'
         )

# 2.1.4: Body ####
body <- dashboardBody(
    tabItems(read_data, read_images, visustat_frame_tab, visustat_sum_tab, visustat_all_tab, about))



# 2.2: Set up UI ####
ui <- dashboardPage(header, sidebar, body)

#______________________________________ ####

# 3: Server ####
server <- function(input, output, session) {

    # 3.1: Read In Data ####
    # 3.1.1: Filesystem Data ####

    volumes <- c(Home = fs::path_home(), 'R Installation' = R.home(), getVolumes()())

    ## select tracking data
    shinyFileChoose(input, 'file', roots = volumes, session = session)


    ## show rendered overview over selected files and their paths
    output$filepaths <- renderPrint({
        if (is.integer(input$file)) {
            cat('No files have been selected...')
        } else {
            parseFilePaths(volumes, input$file)
        }
    })

    ## create select data ui
    output$select_data <- renderUI({
        list(
            selectInput(
                'tracking_data', label = 'Dataset',
                choices = c(parseFilePaths(volumes, input$file) %>% distinct(name) %>% pull(),'hiv_motility_example')
            )
        )}
    )

    ## read in dataframe
    df_raw <- eventReactive(input$load_df,{
        if (input$tracking_data == 'hiv_motility_example') {
            hiv_motility
        } else {
            read.csv(parseFilePaths(volumes, input$file) %>% filter(name == input$tracking_data) %>% pull(datapath)) %>% as_tibble()
        }
    })

    ## modify column names
    df <- eventReactive(input$update_df,{
        vars <- c(track = input$track, X = input$X, Y = input$Y)
        if (input$dims_df == 3) {
            vars <- c(vars, Z = input$Z)
        }
        df_raw() %>% rename(vars)
    })


    ## UI for selecting X, Y, Z, time, and track columns

    output$prepare_df <- renderUI({
        if (is.null(input$tracking_data)) {
            return(list())
        } else {
            columns <- df_raw() %>% colnames()
            column_names <- list(
                column(width = 2, selectInput('track','Track', choices = columns, selected = columns[1]),
                       selectInput('time', 'Time', choices = columns, selected = columns[2])),
                column(width = 2, selectInput('X', 'X position', choices = columns, selected = columns[3]),
                       selectInput('Y', 'Y position', choices = columns, selected = columns[4]))
            )
        }
        if (input$dims_df == 3) {
            column_names <- append(column_names,
                                   list(column(width = 2,
                                               selectInput('Z', 'Z position',
                                                           choices = columns, selected = columns[5]))))
        }
        return(column_names)
    })


    ## 3.1.1.1: Dataframe Browser ####
    output$df_raw <- renderDataTable({df_raw()}, options = list(scrollX = TRUE, pageLength = 5,
                                                                lengthMenu = c(5, 10, 25)))

    ## create ui
    observeEvent(input$load_df,{
        output$df_table <- renderUI({
            if (input$tracking_data == '') {
                list()
            } else {
                list(dataTableOutput('df_raw'))
            }

        })
    })

    output$df_tracking <- renderDataTable({df()}, options = list(scrollX = TRUE, pageLength = 5,
                                                                 lengthMenu = c(5, 10, 25)))

    ## create ui
    observeEvent(input$update_df,{
    output$df_table <- renderUI({
        if (input$tracking_data == '') {
            list()
        } else {
            list(dataTableOutput('df_tracking'))
        }

    })
    })

    # 3.1.2: Filesystem Images ####

    shinyFileChoose(input, 'images', roots = volumes, session = session)

    images <- reactive({
        if (input$example_images) {
            tibble(hiv_motility_images()) %>%
                rename(datapath = `hiv_motility_images()`) %>%
                mutate(name = str_extract(datapath,'loose.+'))
        } else {
            parseFilePaths(volumes, input$images)
        }
    })

    ## create select data ui
    output$select_image_ui <- renderUI(
        if (input$stack) {
            list(fluidRow(
                column(4,selectInput(
                    'select_image', 'Image',
                    choices = images() %>% distinct(name) %>% pull())),
                column(1,actionGroupButtons(inputIds = c('prev_img','next_img'),
                                            labels = list(tags$span(icon('angle-up'),''), tags$span(icon('angle-down'),'')),
                                            status = 'primary',
                                            direction = 'vertical')),
                column(2, numericInput('slice','Slice', min = 1, max = NA, step = 1, value = 1)))
            )
        } else {
            list(fluidRow(
                column(4,selectInput(
                    'select_image', 'Image',
                    choices = images() %>% distinct(name) %>% pull())),
                column(1,actionGroupButtons(inputIds = c('prev_img','next_img'),
                                            labels = list(tags$span(icon('angle-up'),''), tags$span(icon('angle-down'),'')),
                                            status = 'primary',
                                            direction = 'vertical'))))
        }


    )

    # update frames with action buttons
    observeEvent(input$prev_img, {
        if (input$stack) {
            slice_update <- input$slice - 1
            if (slice_update < 1) {
                slice_update <- 1
            }
            updateNumericInput(session, inputId = 'slice', value = slice_update )
        } else {
            image_list <- images() %>% distinct(name) %>% pull()

            current_image <- which(image_list == input$select_image)

            updateSelectInput(session,inputId = 'select_image', selected = image_list[current_image - 1])
        }

    })

    observeEvent(input$next_img, {
        if (input$stack) {
            slice_update <- input$slice + 1
            updateNumericInput(session, inputId = 'slice', value = slice_update )
        } else {
            image_list <- images() %>% distinct(name) %>% pull()
            current_image <- which(image_list == input$select_image)
            updateSelectInput(session, inputId = 'select_image', selected = image_list[current_image + 1])
        }
    })


    # 3.1.2.1: Image Viewer ####
    # A plot of fixed size
    output$img <- renderImage({
        if (is.null(input$select_image)) {
            tmpfile <- image_blank(400, 400, color = 'white')
        } else if (input$select_image == "") {
            tmpfile <- image_blank(400, 400, color = 'white')
        } else {
            tmpfile <- image_read(images() %>%
                                      filter(name == input$select_image) %>%
                                      pull(datapath))
            if (input$stack) {
                tmpfile <- tmpfile[input$slice]
            }
            if (input$normalize) {
                tmpfile <- tmpfile %>% image_normalize()
            }
        }

        tmpfile <- tmpfile %>% image_write(tempfile(fileext = 'jpg'), format = 'jpg')
        # Return a list
        list(src = tmpfile, contentType = 'image/jpg', height = '100%')
    }, deleteFile = TRUE)


    # 3.2: View Data ####

    # 3.3: Run VisuStatR ####
    # 3.3.1: Frame ####
    # 3.3.1.1: Conditional Option UI ####

    output$frame_select <- renderUI({
        fluidRow(
            column(width = 2, align = "center", div(actionGroupButtons(
                inputIds = c('frame_b', 'frame_f'),
                labels = list(tags$span(icon('angle-left'),''), tags$span(icon('angle-right'),'')),
                status = 'primary',
                direction = 'horizontal')),
                style = "margin-top: +15px;"),
            column(width = 7, align = "center",
                   sliderInput('frame', NULL, min = 1, max = images() %>% nrow(), value = 1, step = 1)),
            column(width = 1, align = "center",
                   actionButton('update_frame','',
                                icon = icon('sync'), color = 'primary', style = "margin-top: +15px;"))
        )
    })

    output$select_range <- renderUI({
        if (!is.null(input$par.map)) {
            if (df() %>% pull(input$par.map) %>% is.numeric()) {
                min_value <- df() %>% select_(input$par.map) %>% pull() %>% min(na.rm = TRUE) %>% round(digits = 1)
                max_value <- df() %>% select_(input$par.map) %>% pull() %>% max(na.rm = TRUE) %>% round(digits = 1)
                sliderInput('par.map.cont.range','Define range',value = c(min_value,max_value), min = min_value, max = max_value)
            }
        } else {
            list()
        }

    })

    # conditional options for mapping
    output$par.map.options <- renderUI({
        list(bsModal('popup_maps_settings','Parameter Mapping Options','map_settings',
                     selectInput('par.map','Color parameter',
                             choices = df() %>% colnames()),
                                uiOutput('select_range'),
                     selectInput('par.shape','Shape parameter', choices = df() %>% select_if(is.character) %>% colnames())
        ))
    })

    # conditional options for tracks
    output$tracks_opt <- renderUI({
        max_frame <- df() %>% distinct(time) %>% nrow()
            list(
                sliderInput('tracks.length', 'Length', value = max_frame, min = 0, max = max_frame, step = 1),
                sliderInput('tracks.size', 'Size', value = 1, min = 1, max = 20, step = 1),
                sliderInput('tracks.alpha', 'Alpha', value = 0.5, min = 0, max = 1, step = 0.05),
                selectizeInput('tracks_select','Filter tracks', choices = df() %>% distinct(track) %>% pull(),
                               selected = NULL, multiple = TRUE)
            )
    })

    # update frames with action buttons
    observeEvent(input$frame_b, {
        current_frame <- input$frame
        updateSliderInput(session,inputId = 'frame', value = current_frame - 1)
    })
    observeEvent(input$frame_f, {
        current_frame <- input$frame
        updateSliderInput(session,inputId = 'frame', value = current_frame + 1)
    })


    # 3.3.1.2: Run visustat_frame() ####
    frame_options <- eventReactive(list(input$update_frame,input$frame_b,input$frame_f),{
        opt.list <- list(frame = input$frame,
                         image = images() %>% slice(input$frame) %>% pull(datapath),
                         points.size = input$points.size,
                         points.alpha = input$points.alpha,
                         points.stat = input$points.stat,
                         tracks = input$tracks_select,
                         par.display = input$mapping,
                         tracks.size = input$tracks.size,
                         tracks.length = input$tracks.length,
                         tracks.alpha = input$tracks.alpha,
                         sub.img = input$sub_window,
                         sub.window = input$sub_window_size,
                         sub.col = input$sub_n_col,
                         par.min = input$par.map.cont.range[1],
                         par.max = input$par.map.cont.range[2],
                         par.map = input$par.map,
                         par.shape = input$par.shape,
                         image.normalize = input$normalize,
                         scale.bar = input$scalebar,
                         scale.width = input$scale.width,
                         scale.height = input$scale.height,
                         scale.x = input$scale.x,
                         scale.y = input$scale.y,
                         scale.color = input$scale.color,
                         axis.display = input$axis,
                         stack = input$stack)

        if (is.null(opt.list$par.min)) {
            opt.list['par.min'] <- NaN
        }
        if (is.null(opt.list$par.max)) {
            opt.list['par.max'] <- NaN
        }
        if (is.null(opt.list$tracks.size)) {
            opt.list['tracks.size'] <- 1
        }
        if (is.null(opt.list$tracks.alpha)) {
            opt.list['tracks.alpha'] <- 0.5
        }
        if (is.null(input$map.select)) {
            opt.list['par.display'] <- FALSE
        }
        if (input$tracks == FALSE) {
            opt.list['tracks.alpha'] <- 0
        }
        if (input$points == FALSE) {
            opt.list['points.alpha'] <- 0
        }
        if (!('shape' %in% input$map.select)) {
            opt.list['par.shape'] <- NULL
        }
        return(opt.list)
        })

    frame_gg <- eventReactive(list(input$update_frame,input$frame_b,input$frame_f),{
        df_visu_frame <- df()
        if (any(input$map.select == 'shape')) {
            if (df() %>% pull(input$par.shape) %>% is.numeric()) {
                df_visu_frame <- df() %>% mutate_at(input$par.shape,as.factor)
            }
        }
        visustat_frame(df_visu_frame,
                       frame_options(),
                       all.list = TRUE)
    }
    )
    observeEvent(list(input$update_frame,input$frame_b,input$frame_f),{
        output$visustat_frame <- renderPlot(frame_gg())
    })



    # 3.3.2: Summary ####
    output$summary_ui <- renderUI({
        columns_sum <- df() %>% colnames()
        list(
            selectizeInput('par.numeric','Summary statistics', choices = columns_sum, multiple = TRUE),
            selectInput('group.vars', 'Time', choices = columns_sum, selected = columns_sum[2]),
            selectizeInput('tracks_select_sum','Filter tracks',
                           choices = df() %>% distinct(track) %>% pull(),
                           selected = NULL, multiple = TRUE),
            selectInput('par.map.sum', 'Color', choices = columns_sum),
            selectInput('ribbon', 'Show Ribbon', choices = list('Show' = TRUE,
                                                                'Hide' = FALSE)),
            selectInput('ribbon.stat', 'Ribbon statistic', choices = list('Standard deviation' = 'sd',
                                                                          'Standard error' = 'se',
                                                                          'Confidence interval' = 'ci')),
            actionButton('refresh_sum','', icon = icon('refresh'))
        )
    })


    summary_gg <- eventReactive(input$refresh_sum,{
        visustat_summary(df(),
                        par.numeric = input$par.numeric,
                        group.vars = input$group.vars,
                        par.map = input$par.map.sum,
                        tracks = input$tracks_select_sum,
                        ribbon = input$ribbon,
                        ribbon.stat = input$ribbon.stat)
    })

    output$visustat_summary <- renderPlot(summary_gg())


    # 3.3.3: Animation ####
    output$animation_range <- renderUI({
        list(sliderInput('frame.range','Frame range',
                         min = 1, max = df() %>% distinct(time) %>% nrow(),
                         value = c(1,df() %>% distinct(time) %>% nrow()),
                         step = 1))
    })

    observeEvent(input$run_all,{
        withProgress(message = 'Running visustat_all()...', value = 1, expr = {
        df_visu_frame <- df()
        if (any(input$map.select == 'shape')) {
            if (df() %>% pull(input$par.shape) %>% is.numeric()) {
                df_visu_frame <- df() %>% mutate_at(input$par.shape,as.factor)
            }
        }

        summary_options = list(par.numeric = input$par.numeric,
                               group.vars = input$group.vars,
                               par.map = input$par.map.sum)
        frame_options <- frame_options()
        frame_options[c('frame','image')] <- NULL

        visustat_all(df_visu_frame,
                    images = images() %>% pull(datapath),
                    frame_range = c(input$frame.range[1],input$frame.range[2]),
                    visustat_frame.list = frame_options,
                    visustat_summary.list = summary_options,
                    width = input$width_all,
                    height = input$height_all,
                    rel_width = input$rel_width,
                    display_summary = ifelse(input$display %in% c('Summary','Both'), TRUE, FALSE),
                    display_frame = ifelse(input$display %in% c('Frame','Both'), TRUE, FALSE)
                    )
        })
    })

    observeEvent(input$preview_frame,{
        withProgress(message = 'Running visustat_all()...', value = 1, expr = {
        output$preview_all <- renderImage({
            df_visu_frame <- df()
            if (any(input$map.select == 'shape')) {
                if (df() %>% pull(input$par.shape) %>% is.numeric()) {
                    df_visu_frame <- df() %>% mutate_at(input$par.shape,as.factor)
                }
            }

            summary_options = list(par.numeric = input$par.numeric,
                                   group.vars = input$group.vars,
                                   par.map = input$par.map.sum)
            frame_options <- frame_options()
            frame_options[c('frame','image')] <- NULL
            tmpfile <- visustat_all(df_visu_frame,
                                   images = images() %>% pull(datapath),
                                   frame_range = c(input$frame,input$frame),
                                   visustat_frame.list = frame_options,
                                   visustat_summary.list = summary_options,
                                   width = input$width_all,
                                   height = input$height_all,
                                   rel_width = input$rel_width,
                                   display_summary = ifelse(input$display %in% c('Summary','Both'), TRUE, FALSE),
                                   display_frame = ifelse(input$display %in% c('Frame','Both'), TRUE, FALSE),
                                   save = FALSE,
                                   return = TRUE)

            tmpfile <- tmpfile %>% image_write(tempfile(fileext = 'jpg'), format = 'jpg')
            # Return a list
            list(src = tmpfile, contentType = 'image/jpg', height = '100%')
        })
        })
    })


    # 3.4: Download ####
    output$save_frame <- downloadHandler(
        filename = function(){paste("visustat_frame_",Sys.Date(),'.', input$file_format, sep = '')},
        content = function(file){
                ggsave(file, plot = frame_gg(),
                       device = input$file_format,
                       unit = input$unit_download,
                       width = input$width,
                       height = input$height,
                       dpi = input$resolution)

            }
    )


}
#______________________________________ ####

shinyApp(ui, server)
