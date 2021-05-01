
# VisuStatR - Visualizing Motility and Morphology Statistics on Images in R
<!-- badges: start -->
![](https://img.shields.io/badge/lifecycle-alpha-orange.svg)
<!-- badges: end -->
 
Live-cell microscopy has become an essential tool for analyzing dynamic processes in various biological applications. To critically assess the influence of individual cells on the calculated summary statistics, and to detect heterogeneous cell populations or possible confounding factors, such as misclassified or -tracked objects, a direct mapping of gained statistical information onto the actual image data is necessary. VisuStatR allows to visualize time-resolved motility parameters or any other summary statistic onto images in R. Originally, this package was intended to be used with live-cell microscopy images and cell-tracking data. But in general VisuStatR can be used with any kind of data supplying a dataframe with trackIds, time and spatial coordinates (2D/3D), as well as numeric or discrete mapping parameters and corresponding images.

![Concepts of VisuStatR](images/visumotr_concept.png)

The package acts as a wrapper for annotating provided images with several, modifiable `ggplot2`-layers. With `visustat_frame()`, continous and discrete parameters can be mapped individually on color, shape and size for one timepoint. `visustat_summary()` allows to create a corresponding plot of all given statistics for the whole range of the provided data. A time-resolved image-series can be created with `visustat_all()` which allows to combine mapped frames and summary plots created by `visustat_frame()` and `visustat_summary()`. The output of all functions are `ggplot2`-objects which can further be manipulated in a common manner.

## Display modes

### Mapping color and shape
VisuStatR allows for several display modes. The most common is shown above where the complete image is captured and one continuous parameter is mapped on the track color. In addition, it is also possible to map discrete variables on shape and/or color or continuous variables on size and/or alpha. In this example, contact-state and infection-status of tracked cells were mapped to color and shape.

![Mapping contact state and infection state](images/shape_color.jpg)

### Color-mapping combined with summary statistics
Each output-type of `visustat_frame()` can be combined with the output from `visustat_summary()`.

![Output from visustat_frame() combined with visustat_summary()](images/frame_summary.jpg)

### Follow single tracks within sub-windows
The output of `visustat_frame()` does not need to be the whole image. It can be cropped manually or automatically or create sub-windows that allow to follow individual tracks, that might be interesting due to prior performed statistical analysis that highlighted for example outliers.

![Sub-window output](images/visu_sub.png)

### 3D images and Z-projections
VisuStatR supports .tiff-stacks of 3D image data as input. Since `ggplot2` just works in 2D, the user can choose from different Z-projections to view the data in 2D. All output modes work with 3D-image data. In addition, it is possible to follow several individual tracks on their respective Z-axis in parallel.

### GUI: Shiny App
In addition to the functionality as an R-package, VisuStatR can be completely used within a Shiny-app by calling `visustat_shiny()`. The Shiny-app acts as an graphical-user interface and allows to import and prepare tracking and image data as well as process these with all VisuStatR functionalities. In contrast to using VisuStatR in an IDE or terminal, the Shiny-app adds a much more interactive way of data-analysis and allows users that are not proficient with the R language to process their data with VisuStatR.

![Shiny screenshot](images/app_visustat_frame.png)

## Install VisuStatR
```{r}
# install dependencies which are not found on CRAN
devtools::install_github("zeehio/facetscales")
remotes::install_github("coolbutuseless/ggecho")

# install VisuStatR from git repo
devtools::install_git("https://github.com/grrchrr/VisuStatR")
```
