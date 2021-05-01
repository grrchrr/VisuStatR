#### Shiny VisumotR ####
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
#______________________________________ ####

# 2: UI elements ####

# 2.1: Dashboard elements ####

# 2.1.1: Header ####
header <- dashboardHeader(title='VisuStatR')

# 2.1.2: Sidebar ####
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem('Read Data', tabName = 'read', icon = icon('folder-open'),startExpanded = TRUE,
                 menuSubItem('Tracking Data', tabName = 'read_data', icon = icon('line-chart')),
                 menuSubItem('Image Files', tabName = 'read_images', icon = icon('images'))),
        # menuItem('View Data', tabName = 'view', icon = icon('eye'),
        #          menuSubItem('Tracking Data', tabName = 'browse_data', icon = icon('line-chart')),
        #          menuSubItem('Image Files', tabName = 'browse_images', icon = icon('images'))),
        menuItem('Run VisumotR', tabName = 'visumotR', icon = icon('laptop-code'),
                 menuSubItem('Frame', tabName = 'visumot_frame', icon = icon('image')),
                 menuSubItem('Summary', tabName = 'visumot_summary', icon = icon('bar-chart-o')),
                 menuSubItem('Animation', tabName = 'visumot_all', icon = icon('film'))),
        menuItem('How to', tabName = 'how_to', icon = icon('mortar-board')),
        menuItem('About', tabName = 'about', icon = icon('question')))
    )

# 2.1.3: Tab Items ####
# 2.1.3.1: Read Data ####
read_data <- tabItem(tabName = 'read_data', fluidRow(
    column(width=12,
           tabBox(width=NULL, title = 'Tracking Data',
                  tabPanel('Read In',
                           'Select .csv files which contain your tracking data. Click on Read in Dataset to import the dataset and continue with the other tabs to prepare the dataset for using it with VisumotR.',
                           hr(),
                           shinyFilesButton('file', ' Browse Files', 'Please select file(s)', multiple = TRUE, viewtype = 'detail', icon = icon('folder')),
                           br(),
                           br(),
                           div(verbatimTextOutput('filepaths'), style= 'width:50%'),
                           hr(),
                           uiOutput('select_data'),
                           actionButton('load_df', label = 'Read in', icon = icon('download')),
                           hr(),
                           uiOutput('df_table')
                           ),
                  tabPanel('Prepare Dataset',
                           'Please set the scaling factors to refer to pixel and frames as well as
                           indicate which columns in your dataset correspond to track, time and X-, Y- and Z- positions. Then click Update dataframe to continue with the Run VisumotR tab.',
                           hr(),
                           fluidRow(
                               column(6,
                                      numericInput('scale_time','Time scaling', 1),
                                      numericInput('scale_dim', 'Dimension scaling', 1),
                                      radioGroupButtons('dims_df', 'Dimensions', choices = list('2D' = 2, '3D' = 3))),
                               column(6, uiOutput('prepare_df'))),
                           actionButton('update_df', 'Update dataframe', icon = icon('refresh')),
                           hr(),
                           uiOutput('df_table_prepared')
                           )
           )
        )
    )
)


# 2.1.3.2: Read Images ####
read_images  <- tabItem(tabName = 'read_images', fluidRow(
    column(width=3,
           box(width=NULL, title = 'Image Specifications',
               radioGroupButtons('color_space', 'Color Space', choices = list('Grayscale'='gray', 'RGB'='rgb')),
               radioGroupButtons('bit_depth','Bit Depth', choices = list('8-bit'=8,'16-bit'=16,'32-bit'=32)),
               radioGroupButtons('dims_img', 'Dimensions', choices = list('2D'=2,'3D'=3)),
               radioGroupButtons('stack','Timeseries', choices = list('Multiple Files'=FALSE, 'Stack'=TRUE)),
               switchInput(label = 'Normalize',
                           inputId = 'normalize',
                           value = FALSE))
    ), column(width=9,
              tabBox(width=NULL, height = '100%' ,title='Image Browser',
                     tabPanel('Import',
                              shinyFilesButton('images', 'Browse Images', 'Please select file(s)', multiple = TRUE, viewtype = 'detail', icon = icon('folder')),
                              hr(),
                              div(verbatimTextOutput('filepaths_img'), style= 'width:100%')),
                     tabPanel('Viewer',
                              uiOutput('select_image_ui'),
                              hr(),
                              imageOutput('img'),
                              hr())
                  ))
)
)

# 2.1.3.3: Frame ####
visumot_frame_tab <- tabItem(tabName = 'visumot_frame', fluidRow(
    column(width=3,
           box(width=NULL, title = 'Options',
               switchInput(label = 'Mapping',
                           inputId = 'mapping',
                           value = TRUE
               ),
               uiOutput('par.map'),
               uiOutput('par.map.options'),
               hr(),
               switchInput(label = 'Subwindow',
                   inputId = 'sub_window',
                   value = FALSE
               ),
               uiOutput('sub_image_opt'),
               hr(),
               switchInput(label = 'Tracks',
                           inputId = 'tracks',
                           value = TRUE
               ),
               uiOutput('tracks_opt'),
               hr(),
               switchInput(label = 'Points',
                           inputId = 'points',
                           value = TRUE
               ),
               uiOutput('points_opt'),
               hr(),
               switchInput(label = 'Scalebar',
                           inputId = 'scalebar',
                           value = FALSE
               ),
               uiOutput('scale_opt'),
               hr(),
               switchInput(label = 'Axis',
                           inputId = 'axis',
                           value = FALSE
               ),
               switchInput(label = '3D',
                           inputId = 'dims_3d',
                           value = FALSE
               )

               )
    ), column(width=9,
              box(width=NULL, title='Viewer',
                  uiOutput('frame_select'),
                  hr(),
                  fluidRow(column(width = 1, actionGroupButtons(
                      inputIds = c('frame_b', 'frame_f'),
                      labels = list(tags$span(icon('angle-left'),''), tags$span(icon('angle-right'),'')),
                      status = 'primary',
                      direction = 'vertical')),
                      column(width=10,plotOutput('visumot_frame', height='800px')),
                      column(width=1,actionButton('update_frame','',icon = icon('sync'), color = 'primary'),
                             actionButton('save_frame','', icon = icon('download'), color='primary')),
                  hr()
                  )
              ))
)
)

# 2.1.3.4: Summary ####
visumot_sum_tab <- tabItem('visumot_summary', fluidRow(
    column(width = 3,
           box(width = NULL, title = 'Options',
               uiOutput('summary_ui'))),
    column(width=9,
           box(width=NULL, title = 'Summary',
           plotOutput('visumot_summary', height = 800)))

    )
)
# 2.1.3.5: Animation ####
visumot_all_tab <- tabItem('visumot_all', fluidRow(
    column(width = 2,
           box(width = NULL, title = 'Options',
               selectInput('display', 'Layout', choices = c('Frame','Summary','Both')),
               sliderInput('frame.range','Frame range', min=1, max = 120, value = c(1,120)),
               numericInput('width','Width',min=1, max=NA, value = 2000, step = 1),
               numericInput('height','Height',min=1, max=NA, value = 1200, step = 1),
               numericInput('rel_width', 'Ratio', value = 0.65, step = 0.05, min = 0.05, max = 0.95))),
    column(width=10,
           box(width=NULL, title = 'Image Series Viewer',
               'When clicking Start rendering the image series with the options from the Frame and Summary panels will be rendered.
               You can preview the output of single frames in order to be sure everything is of your liking.',
               hr(),
               actionButton('preview_frame', 'Update Preview', icon = icon('refresh')),
               actionGroupButtons(inputIds = c('next_preview', 'prev_preview'),
                                  labels = list(tags$span(icon('angle-left'),''), tags$span(icon('angle-right'),'')),
                                  status = 'primary'),
               hr(),
               br(),
               imageOutput('preview_all'),
               br(),
               hr(),
               actionButton('run_all','Start rendering', icon = icon('refresh'))))

)
)
# 2.1.3.6: How to ####

# 2.1.3.7: About ####
about <- tabItem(tabName = 'about'
         )

# 2.1.4: Body ####
body <- dashboardBody(
    tabItems(read_data, read_images, visumot_frame_tab, visumot_sum_tab, visumot_all_tab, about))



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
                'tracking_data', 'Select dataset:',
                choices = parseFilePaths(volumes, input$file) %>% distinct(name) %>% pull()
            )
        )}
    )

    ## read in dataframe
    df_raw <- eventReactive(input$load_df,{
        read.csv(parseFilePaths(volumes, input$file) %>% filter(name == input$tracking_data) %>% pull(datapath)) %>% as_tibble()
    })

    ## modify column names

    df <- eventReactive(input$update_df,{
        vars <- c(track=input$track, time=input$time, X=input$X, Y=input$Y)
        if (input$dims_df==3) {
            vars <- c(vars,Z=input$Z)
        }
        df_raw() %>% rename(vars)
    })



    ## UI for selecting X, Y, Z, time, and track columns

    output$prepare_df <- renderUI({
        if (input$tracking_data=='') {
            return(list())
        } else {
            columns <- df_raw() %>% colnames()
            column_names <- list(
                selectInput('track','Track', choices = columns, selected = columns[1]),
                selectInput('time', 'Time', choices = columns, selected = columns[2]),
                selectInput('X', 'X position', choices = columns, selected = columns[3]),
                selectInput('Y', 'Y position', choices = columns, selected = columns[4])
            )
        }
        if (input$dims_df==3) {
             column_names <- append(column_names,list(selectInput('Z', 'Z position', choices = columns, selected = columns[5])))
        }
        return(column_names)
    })


    ## 3.1.1.1: Dataframe Browser ####
    output$df_raw <- renderDataTable({df_raw()}, options = list(scrollX = TRUE))

    ## create ui
    observeEvent(input$load_df,{
        output$df_table <- renderUI({
            if(input$tracking_data==''){
                list()
            } else {
                list(dataTableOutput('df_raw'))
            }

        })
    })

    output$df_tracking <- renderDataTable({df()}, options = list(scrollX = TRUE))

    ## create ui
    observeEvent(input$update_df,{
    output$df_table_prepared <- renderUI({
        if(input$tracking_data==''){
            list()
        } else {
            list(dataTableOutput('df_tracking'))
        }

    })
    })

    # 3.1.2: Filesystem Images ####
    shinyFileChoose(input, 'images', roots = volumes, session = session)

    output$filepaths_img <- renderPrint({
        if (is.integer(input$images)) {
            cat('No images have been selected...')
        } else {
            parseFilePaths(volumes, input$images)
        }
    })

    ## create select data ui
    output$select_image_ui <- renderUI(
        if(input$stack){
            list(fluidRow(
                column(4,selectInput(
                    'select_image', 'Image',
                    choices = parseFilePaths(volumes, input$images) %>% distinct(name) %>% pull())),
                column(1,actionGroupButtons(inputIds=c('prev_img','next_img'),
                                            labels = list(tags$span(icon('angle-up'),''), tags$span(icon('angle-down'),'')),
                                            status = 'primary',
                                            direction = 'vertical')),
                column(2, numericInput('slice','Slice',min=1,max=NA,step=1, value=1)))
            )
        } else {
            list(fluidRow(
                column(4,selectInput(
                    'select_image', 'Image',
                    choices = parseFilePaths(volumes, input$images) %>% distinct(name) %>% pull())),
                column(1,actionGroupButtons(inputIds=c('prev_img','next_img'),
                                            labels = list(tags$span(icon('angle-up'),''), tags$span(icon('angle-down'),'')),
                                            status = 'primary',
                                            direction = 'vertical'))))
        }


    )

    # update frames with action buttons
    observeEvent(input$prev_img, {
        if(input$stack){
            slice_update <- input$slice - 1
            if(slice_update<1){
                slice_update <- 1
            }
            updateNumericInput(session, inputId = 'slice', value = slice_update )
        } else {
            image_list <- parseFilePaths(volumes, input$images) %>% distinct(name) %>% pull()

            current_image <- which(image_list==input$select_image)

            updateSelectInput(session,inputId='select_image', selected = image_list[current_image-1])
        }

    })

    observeEvent(input$next_img, {
        if(input$stack){
            slice_update <- input$slice + 1
            updateNumericInput(session, inputId = 'slice', value = slice_update )
        } else {
            image_list <- parseFilePaths(volumes, input$images) %>% distinct(name) %>% pull()
            current_image <- which(image_list==input$select_image)
            updateSelectInput(session,inputId='select_image', selected = image_list[current_image+1])
        }
    })


    # 3.1.2.1: Image Viewer ####
    # A plot of fixed size
    output$img <- renderImage({
        if(input$select_image==''){
            tmpfile <- image_blank(400,400,color='white')
        } else {
            tmpfile <- image_read(parseFilePaths(volumes, input$images) %>%
                                      filter(name == input$select_image) %>%
                                      pull(datapath))
        }
        if(input$stack){
            tmpfile <- tmpfile[input$slice]
        }
        if(input$normalize){
            tmpfile <- tmpfile %>% image_normalize()
        }
        tmpfile <- tmpfile %>% image_write(tempfile(fileext='jpg'), format = 'jpg')
        # Return a list
        list(src = tmpfile, contentType = 'image/jpg', height='100%')
    })


    # 3.2: View Data ####

    # 3.3: Run VisumotR ####
    # 3.3.1: Frame ####
    # 3.3.1.1: Conditional Option UI ####
    # conditional sliders
    output$par.map <- renderUI({
        if(input$mapping){
            list(
                checkboxGroupButtons(
                    inputId = 'map.select',
                    choiceValues = c('color',
                                'shape'),
                    choiceNames = c('Color','Shape'),
                    status = 'primary',
                    selected = 'color'
                )
            )
        } else {
            list()
        }
    })

    output$frame_select <- renderUI({
        sliderInput('frame', 'Frame:', min=1, max=df()%>%distinct(time)%>%nrow(), value=1)
    })

    output$select_range <- renderUI({
        if(any(input$map.select=='color' & !is.null(input$par.map))) {
            if(df() %>% pull(input$par.map) %>% is.numeric()){
                min_value <- df() %>% select_(input$par.map) %>% pull() %>% min(na.rm = TRUE) %>% round(digits=1)
                max_value <- df() %>% select_(input$par.map) %>% pull() %>% max(na.rm = TRUE) %>% round(digits=1)
                sliderInput('par.map.cont.range','Define range',value = c(min_value,max_value), min = min_value, max = max_value)
            }

        } else {
            list()
        }

    })

    # conditional options for mapping
    output$par.map.options <- renderUI({
        ui_color <- list()
        ui_shape <- list()
        if(input$mapping){
            if(any(input$map.select=='color')) {
                ui_color <- list(selectInput('par.map',
                                             'Color parameter',
                                             choices = df() %>% colnames()),
                                uiOutput('select_range'))
            }
            if(any(input$map.select=='shape')){
                ui_shape <- list(selectInput('par.shape','Shape parameter', choices = df() %>% colnames()))
            }
        }
        append(ui_color, ui_shape)
    })

    # conditional options for subwindow
    output$sub_image_opt <- renderUI({
        if (input$sub_window) {
            list(sliderInput('sub_n_col','Columns', value = 3, min=1, max=15, step = 1),
                sliderInput('sub_window_size', 'Windowsize', value = 50, min=5, max = 250, step = 5)
                )
        } else {
            list()
        }
    })
    # conditional options for points
    output$points_opt <- renderUI({
        if(input$points){
            list(
                sliderInput('points.size','Size', value = 1, min = 1, max=20, step = 1),
                sliderInput('points.alpha','Alpha', value = 0.75, min=0, max = 1, step = 0.05),
                radioGroupButtons('points.stat','Blur',choices = list('On'='echo', 'Off'='identity'))
            )
        } else {
            list()
        }
    })



    # conditional options for tracks
    output$tracks_opt <- renderUI({
        max_frame <- df()%>%distinct(time)%>%nrow()
        if(input$tracks){
            list(
                sliderInput('tracks.length', 'Length', value=max_frame, min = 0, max=max_frame, step = 1),
                sliderInput('tracks.size', 'Size', value = 1, min = 1, max = 20, step = 1),
                sliderInput('tracks.alpha', 'Alpha', value = 0.5, min=0, max = 1, step = 0.05),
                selectizeInput('tracks_select','Filter tracks',choices=df()%>%distinct(track)%>%pull(), selected=NULL, multiple=TRUE)
            )
        } else {
            list()
        }
    })

    # conditional options for scalebar

    output$scale_opt <- renderUI({
        if (input$scalebar) {
            list(
                numericInput('scale.width','Width',min = 1, max= NA, value = 40),
                numericInput('scale.height', 'Height', min=1, max = NA, value=10),
                numericInput('scale.x', 'X offset', min=1, max=NA, value = 10),
                numericInput('scale.y', 'Y offset', min=1, max=NA, value = 10),
                colourInput('scale.color','Color',value = 'grey70')
            )
        } else {
            list()
        }
    })


    # update frames with action buttons
    observeEvent(input$frame_b, {
        current_frame <- input$frame
        updateSliderInput(session,inputId='frame', value = current_frame-1)
    })
    observeEvent(input$frame_f, {
        current_frame <- input$frame
        updateSliderInput(session,inputId='frame', value = current_frame+1)
    })



    # 3.3.1.2: Run vismot_frame() ####
    frame_gg <- eventReactive(list(input$update_frame,input$frame_b,input$frame_f),{
        df_visu_frame <- df()
        if(any(input$map.select=='shape')){
            if(df()%>%pull(input$par.shape)%>%is.numeric()){
                df_visu_frame <- df()%>%mutate_at(input$par.shape,as.factor)
            }
        }

        visumot_frame(df_visu_frame,
                      frame = input$frame,
                      image = parseFilePaths(volumes, input$images) %>% slice(input$frame) %>% pull(datapath),
                      points.size = input$points.size,
                      points.alpha = input$points.alpha,
                      points.stat = input$points.stat,
                      tracks = input$tracks_select,
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
    }
    )

    output$visumot_frame <- renderPlot(frame_gg())


    # 3.3.2: Summary ####
    output$summary_ui <- renderUI({
        columns_sum <- df() %>% colnames()
        list(
            selectizeInput('par.numeric','Summary statistics',choices = columns_sum, multiple=TRUE),
            selectInput('group.vars', 'Time ', choices = columns_sum, selected = columns_sum[2]),
            selectInput('par.map.sum', 'Color', choices = columns_sum),
            selectInput('par.shape.sum', 'Shape', choices = columns_sum),
            selectInput('ribbon', 'Show Ribbon', choices = list('Show'=TRUE,
                                                                'Hide'=FALSE)),
            selectInput('ribbon.stat', 'Ribbon statistic', choices = list('Standard deviation'='sd',
                                                                          'Standard error'='se',
                                                                          'Confidence interval'='ci')),
            actionButton('refresh_sum','',icon = icon('refresh'))
        )
    })


    summary_gg <- eventReactive(input$refresh_sum,{
        visumot_summary(df(),
                        par.numeric=input$par.numeric,
                        group.vars=input$group.vars,
                        par.map=input$par.map.sum)
    })

    output$visumot_summary <- renderPlot(summary_gg())


    # 3.3.3: Animation ####

    observeEvent(input$run_all,{
        df_visu_frame <- df()
        if (any(input$map.select=='shape')) {
            if( df() %>% pull(input$par.shape) %>% is.numeric()) {
                df_visu_frame <- df() %>% mutate_at(input$par.shape,as.factor)
            }
        }

        frame_options = list(points.size = input$points.size,
                             points.alpha = input$points.alpha,
                             points.stat = input$points.stat,
                             tracks = input$tracks_select,
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

        summary_options = list(par.numeric=input$par.numeric,
                               group.vars=input$group.vars,
                               par.map=input$par.map.sum)

        visumot_all(df_visu_frame,
                    images = parseFilePaths(volumes, input$images) %>% pull(datapath),
                    frame_range = c(input$frame.range[1],input$frame.range[2]),
                    visumot_frame.list = frame_options,
                    visumot_summary.list = summary_options,
                    width=input$width,
                    height = input$height,
                    rel_width = input$rel_width,
                    display_summary = ifelse(input$display %in% c('Summary','Both'), TRUE, FALSE),
                    display_frame = ifelse(input$display %in% c('Frame','Both'), TRUE, FALSE)
                    )
    })



    observeEvent(input$preview_frame,{
        output$preview_all <- renderImage({
            df_visu_frame <- df()
            if (any(input$map.select=='shape')) {
                if( df() %>% pull(input$par.shape) %>% is.numeric()) {
                    df_visu_frame <- df() %>% mutate_at(input$par.shape,as.factor)
                }
            }

            frame_options = list(points.size = input$points.size,
                                 points.alpha = input$points.alpha,
                                 points.stat = input$points.stat,
                                 tracks = input$tracks_select,
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

            summary_options = list(par.numeric=input$par.numeric,
                                   group.vars=input$group.vars,
                                   par.map=input$par.map.sum)

            tmpfile <- visumot_all(df_visu_frame,
                                   images = parseFilePaths(volumes, input$images) %>% pull(datapath),
                                   frame_range = c(input$frame,input$frame),
                                   visumot_frame.list = frame_options,
                                   visumot_summary.list = summary_options,
                                   width=input$width,
                                   height = input$height,
                                   rel_width = input$rel_width,
                                   display_summary = ifelse(input$display %in% c('Summary','Both'), TRUE, FALSE),
                                   display_frame = ifelse(input$display %in% c('Frame','Both'), TRUE, FALSE),
                                   save=FALSE,
                                   return = TRUE)

            tmpfile <- tmpfile %>% image_write(tempfile(fileext='jpg'), format = 'jpg')
            # Return a list
            list(src = tmpfile, contentType = 'image/jpg', height='100%')
        })
    })





}
#______________________________________ ####

shinyApp(ui, server)
