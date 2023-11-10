---
title: "Taking the Scenic Route: Interactive and Performant Tour Animations"
abstract: >
  The tour provides a useful vehicle for exploring high dimensional datasets. It works by combining a sequence of projections---the tour path---in to an animation---the display method. Current display implementations in R are limited in their interactivity and portability, and give poor performance and jerky animations even for small datasets.

  We take a detour into web technologies, such as Three.js and WebGL, to support smooth and performant tour visualisations. The R package \pkg{detourr} implements a set of display tools that allow for rich interactions (including orbit controls, scrubbing, and brushing) and smooth animations for large datasets. It provides a declarative R interface which is accessible to new users, and it supports linked views using \pkg{crosstalk} and \pkg{shiny}. The resulting animations are portable across a wide range of browsers and devices. We also extend the radial transformation of the Sage Tour (@laa2021burning) to 3 or more dimensions with an implementation in 3D, and provide a simplified implementation of the Slice Tour (@laa2020slice).
  
draft: true
author:  
  # see ?rjournal_article for more information
  - name: Casper Hart
    affiliation: University of Auckland
    address: Department of Statistics
    email: casperhart93@gmail.com
  - name: Earo Wang
    affiliation: The University of Auckland
    address: Department of Statistics
    email: earo.wang@gmail.com
type: package
output: 
  rjtools::rjournal_web_article:
    self_contained: yes
    toc: no
    number_sections: no
bibliography: detourr.bib
header-includes:
  - \usepackage{tabu}
  - \usepackage{colortbl}
---




# Introduction







An important first step in any data analysis task is to plot the data so that we can get an intuitive understanding of its structure, for example identifying the presence of clusters or outliers. When the data consists of one or two variables this is quite straightforward, but as the dimensionality of the data increases it becomes more difficult to visualise. 

Several methods exist for high dimensional data visualisation. Given a data matrix $\mathbf X$ we can simply plot each variable $X_1 \dots X_p$ against the others in a pairwise fashion with the result being a scatterplot matrix (e.g. [@becker1987brushing]). We can also view projections of our data by calculating $\mathbf Y = \mathbf X \mathbf A$ where $\mathbf A$ is a $p \times d$ projection matrix with $d$ usually being 1 or 2. We can choose $\mathbf A$ in several different ways, some examples being Principal Component Analysis (PCA) which chooses the directions which explain the maximum variance, and Linear Descriminant Analysis (LDA) which maximises the ratio of between-group and within-group sums of squares. The scatterplot matrix can also be thought of as a projection method where the projections are parallel to each pair of coordinate axes. These are all examples of _linear dimension reduction_ techniques, but non-linear techniques are also available such as t-SNE [@van2008tsne] and UMAP [@mcinnes2018umap] that aim to preserve both local and global structure of the data.

Rather than generating a single static visual, the _tour_ [@asimov1985grand]; [@buja2005computational] works by combining a smooth sequence of projections in to an animation, which can then be viewed using a variety of different _display methods_ [@wickham2011tourr]. This allows the viewer to explore the data from a number of different perspectives while being able to visually connect what would otherwise be disjointed views. However, existing display implementations for tours in R are limited in their interactivity, performance, and portability, and generally result in jerky animations even for small datasets with only tens or hundreds of observations.

In this paper we introduce a new R package called \CRANpkg{detourr}, which provides portable and performant display methods for tours. In the first section we give a background of tours and review a few existing software implementations, and in the section following we describe how the software is used. We will then highlight some of the implementation decisions related to performance, and later provide a case study using embeddings created from the MNIST [@lecun1998mnist] dataset. In the final section we will discuss how this work might be extended in future.

# Background and related works







At its core, the tour is a sequence of projections of a data set that are combined together to form an animation. If we denote an $n \times p$ data matrix $\mathbf X$ and a $p \times d$ projection matrix $\mathbf A$, then we can denote our $n \times d$ projected data set $\mathbf Y$ as $\mathbf Y = \mathbf X \mathbf A$. 

Each projection matrix $\mathbf A$ is often referred to as a _plane_, _frame_, or _basis_. (Note that in this paper, the term _frame_ is avoided in this context to avoid ambiguity with _animation frames_). These bases are constrained to be orthonormal, so each column of $\mathbf A$ is a unit vector, and is orthogonal to each other column. In order to produce a smooth animation, a set of target bases are selected and interpolated between. Geodesic interpolation is generally used as described in [@buja2005computational]. 

## Types of tours

Different tour paths arise from using different methods for selecting the target bases. For example, the _grand tour_ was introduced by [@asimov1985grand] and chooses a set of target projections at random. This can be thought of as a random walk around projections of the data.

The projection pursuit guided tour chooses bases to find a more interesting projection than the current one, where the interestingness is defined by some index function. Index functions such as _central mass_, _holes_, and _lda_ are described in [@cook2007interactive].

Other types of tours include the _little tour_, which ensures bases parallel to the axes are visited; the _frozen tour_ will fix some of the values in the projection matrix $\mathbf A$ between bases; the local tour chooses bases that are within some angular distance of the starting basis.

In all of these, the tour path is calculated in two steps; the target basis is calculated and then interpolation is done so that the transition to the next basis is smooth. Some methods generate the tour path in a single step, for example the \CRANpkg{langevitour} package in R [@harrison2022langevitour] produces tours as more of a physics simulation where points have position, velocity, momentum, and damping, and the position of points in subsequent animation frames is allowed to evolve while taking in to account user interactions.

## Display methods

The typical display methods for tours include histograms or density plots for 1D projections, and x-y scatter plots for 2D ones. 3D data can be viewed by a 3D scatterplot with a virtual perspective camera to enable displaying on a monitor. This can be enhanced with fog to make closer points more prominent, and interactive rotation controls to give a more immersive 3D experience. Projections with 3 or more dimensions can be displayed using parallel coordinates plots. Other displays exist such as Andrew's plot [@andrews1972plots], where each point is represented by a Fourier curve plotted between $-\pi$ and $\pi$.

These display methods can be enhanced to display additional information, for example the _slice tour_ described in [@laa2020slice] highlight points whose orthogonal distance to the projection plane is smaller than some threshold, and fade out points that are further away. This is good for finding hollowness in data, with an example shown in the case study.

Furthermore, the data may be transformed after being projected. One consequence of the curse of dimensionality is that when projecting from high to low dimensions, the points tend to crowd towards the centre. [@laa2021burning] describes the _sage tour_, and provides a radial transformation that ensures the relative volume at a radius $r$ in the data space is preserved in the projected space. The effect of this is that the crowding is reduced, and uniformly distributed data in the original space will continue to be uniform in the projected space.

## Software implementations

The \CRANpkg{tourr} package [@wickham2011tourr] is the most prevalent and comprehensive software in R [@R] for visualising tours. It implements many of the tour paths described previously including grand, little, guided, frozen, etc. and display methods including scatter plots (with variations for the sage and slice tour), parallel coordinates plots, depth displays, Andrew's plot. The package also allows exporting tours as GIF images via the \CRANpkg{gifski} package [@gifski], or exporting to GGobi [@swayne2003ggobi] to allow for interaction and linked brushing, etc. However, the \pkg{tourr} package uses the R graphics device as the primary display, which is quite limited in performance and interactivity. 

The \CRANpkg{spinifex} package [@spinifex] provides manual tours built on \pkg{tourr} and using R \CRANpkg{shiny} [@shiny], and allows the user to manipulate the contribution of each variable one at a time. The \CRANpkg{liminal} R package [@lee2021liminal] provides an interactive gadget for displaying tour visuals. Linked selection and brushing is implemented on both visuals, and play / pause / restart controls are provided.

The \pkg{langevitour} R package instead uses the \CRANpkg{htmlwidgets} package [@htmlwidgets] to display the tour. The main calculations are performed in JavaScript and the points are displayed as a scatter plot using HTML5 Canvas. The displays have good performance so large numbers of data points can be plotted with the animation remaining smooth, and includes interactive features such as drag-and-drop of additional plot elements, and modifying parameters of the tour and having the changes reflected in real time. Once the tour visualisation is generated it then no longer relies on the R runtime so it can be easily exported and embedded on a website for example. But this package is developed with a particular focus of visualising physics dynamics, rather than the more classical tour methods like in \pkg{tourr}.

[@kipp2019connecting] uses D3.js [@bostock2011d3] combined with the R \CRANpkg{shiny} [@shiny] package to display dynamic tour visualisations. However, this setup had limited performance; the client-server nature of \pkg{shiny} led to inconsistent frame rates, and the number of points that could be drawn was limited to <2000 because of the limitations of SVG when drawing many individual elements.

# Usage and interactivity







When designing the user API for \pkg{detourr}, a data-oriented approach is taken to make it approachable, and the visuals are built in JavaScript to enable rich user interactions. \pkg{detourr} also supports the full suite of tour path generating functions from the \pkg{tourr} package. The result is an experience that is feature-rich, immersive, and accessible to newcomers.

This chapter is structured as follows. The first section describes the user API in R and supported features, the second describes how the user can interact with the resulting visual. Throughout the chapter, we use the `pdfSense` dataset (@wang2018mapping; @lee2021liminal) to provide a running example. This data set consists of 2808 observations and 56 input variables from  CT14HERA parton distribution function fits. The first 6 principal components are used to create the tour, accounting for ~55% of the variance in the data. 



## User interface

\pkg{detourr} has a data-oriented user interface heavily influenced by the Tidy Data (@tidydata) workflow, Grammar of Graphics (@wilkinson2012grammar; @wickham2010layered), and \CRANpkg{ggplot2} (@wickham2016ggplot2). The visualisation is built in a sequence of steps which follow the logical flow of data in the tour building process, which makes the API intuitive and accessible.

### Instantiating the tour

To begin, we instantiate a tour using the `detour()` function:


```r
p <- detour(pdf_df, tour_aes(
    projection = starts_with("PC"),
    colour = Type,
    label = ID
))
```

The first argument to `detour()` is a data frame in tidy format containing the tour data and aesthetics. Enforcing the use of data frames encourages data-centric statistical thinking. The second argument defines the aesthetic mapping of data variables through the `tour_aes()` function, similar to \pkg{ggplot2}. The currently supported aesthetics are:

- `projection`: (required) the numeric columns to be projected
- `colour`: point colour
- `label`: label text to be shown when the mouse is hovered over a point.

These mappings support tidy evaluation and \CRANpkg{tidyselect} syntax (@tidyselect) such as `starts_with()`, `where(is.numeric())`, column ranges using `col_1:col_n`, negation `-col_n`, and others, for easier column selection.

### Generating the tour path

Once the tour is initialised with the data and aesthetics, the tour path is defined by piping the output from `detour()` to the `tour_path()` function. Note that `|>` is the pipe operator introduced in R 4.1:

\pagebreak


```r
p <- p |> tour_path(grand_tour(3))
p
```

```
#> # A tibble: 474 x 2
#>   is_new_basis projection_matrix
#>   <lgl>        <list>           
#> 1 TRUE         <dbl [6 x 3]>    
#> 2 FALSE        <dbl [6 x 3]>    
#> 3 FALSE        <dbl [6 x 3]>    
#> 4 FALSE        <dbl [6 x 3]>    
#> 5 FALSE        <dbl [6 x 3]>    
#> # i 469 more rows
```

The `tour_path()` function defines parameters for the tour such as:

- `tour_path`: the tour path generator, e.g. `grand_tour()`, `guided_tour()`, or any other path generator compatible with the \pkg{tourr} package
- `start`: the starting basis or projection matrix
- `fps`: frames per second with which to display the animation. Defaults to 30 but can be increased for a smoother animation or decreased for very large data.
- `max_bases`:  the number of basis frames to generate. A higher number will give a longer tour animation.

The resulting `detour` object is stored in a standard data frame for easy consumption and inspection. It contains the full details of the tour path, where the $i^{th}$ row corresponds to the $i^{th}$ animation frame of the tour, with the following columns:

1. `is_new_basis`: whether the projection matrix corresponds to a new basis (`TRUE`) or is interpolated (`FALSE`)
2. `projection_matrix`: the projection matrix.

This form gives the user full visibility of the tour path, and allows the projection matrices to be traced and extracted for further analyses.

### Creating the animation

To display the tour animation, we simply pipe the output of `tour_path()` to any of the functions prefixed with `show_` provided by the \pkg{detourr} package. The available display functions are:

- `show_scatter()`: the core 2D or 3D scatter plot display
- `show_slice()`: a slice tour display based on @laa2020slice
- `show_sage()`: a sage tour implementation based on @laa2021burning


```r
p |> show_scatter(axes = FALSE)
```
 
\begin{figure}
\includegraphics[width=\textwidth]{figures/implementation/pdfsense_scatter_3d} \caption{Initial frame of the scatterplot display generated by the `show\_scatter` display function. Controls are on the left, and an interactive timeline is on the bottom of the plot.}(\#fig:scatter-display-static)
\end{figure}



The output of `tour_path()` becomes the input of `show_*()`, forming a fluent pipeline. For the three display methods described above, the common parameters are:

- `palette`: the colour palette to use for the tour.
- `center`: whether the data should be centred before displaying.
- `axes`: whether to show axis / what the axis titles should be
- `edges`: a two-column numeric matrix defining indices of points where line segments should be drawn between
- `paused`: whether the animation should be initialised in a paused state.
- `scale_factor`: used to scale the points in or out so that they appear on a sensible range, similar to a zoom function. Defaults to the reciprocal of maximum distance from a point to the origin, so that the points fit inside a unit ball. 

There are also parameters specific to each display method, such as `slice_relative_volume` for `show_slice()`, and `gamma` and `R` for `show_sage()`. These details will be described further in the Display Methods section.

\pagebreak

Putting all of this together, we have:


```r
detour(pdf_df, tour_aes(
    projection = starts_with("PC"),
    colour = Type,
    label = ID
)) |>
    tour_path(grand_tour(3)) |>
    show_scatter()
```

This chaining process allows us to construct the tour visualisation incrementally in a way that is intuitive and easy to follow. The user is able to inspect the result at each step in the chain, and it aligns well with the _grammar of graphics_ and _tidy data_ workflows. This makes \pkg{detourr} accessible to newcomers who may not have worked with tours previously.

## Interactivity

 Presently, several well-developed R packages allow the use of web technologies in R; \pkg{htmlwidgets} allows binding R code with HTML and JavaScript to create standalone widgets; \pkg{shiny} provides features for combining various elements in to interactive web applications powered by R; \CRANpkg{crosstalk} (@crosstalk) enables linked selection and brushing between different HTML Widgets; and \CRANpkg{rmarkdown} (@rmarkdown) allows creating HTML documents with HTML Widgets embedded within. The use of web technologies such as JavaScript enable the resulting visuals to be portable and accessible, and enable the implementation of rich interactive features. In this section we will describe these interactive features and how they can be configured.

### Label aesthetics

In the above example, labels are defined within the call to `tour_aes()`, which contains all of the aesthetic mappings for the tour. The `label` aesthetic produces a tooltip which is shown whenever the mouse is hovered over the data point. By default, the text in the tooltip will have the format `column_name: value`, with each specified column on a new line. If users want more control over what appears in the tooltip, one can use the `I()` function so that the values in the aesthetic column appear as-is. For example in \@ref(fig:hover-tooltip), the left plot specifies the label aesthetic as `label = c(InFit, Type, ID, pt, x, mu)` and the right is specifies the label as-is by using `label = I(ID)` in the call to `tour_aes()`. When using the `I()` function for the label aesthetic, only one column can be specified at a time.

\begin{figure}
\includegraphics[width=0.5\linewidth]{figures/implementation/hover_tooltip} \includegraphics[width=0.5\linewidth]{figures/implementation/hover_tooltip_asis} \caption{(Left) Tooltip showing data from the 6 columns specified in the `label` aesthetic. Note that both the column names and values are present in the tooltip. (Right) The `ID` column is provided as-is to the label aesthetic via the `I()` function.}(\#fig:hover-tooltip)
\end{figure}


### Controls

Table \@ref(tab:pdf-controls) shows a breakdown of the controls found on the left side of the visual. Note that the icon for the currently selected control will be highlighted blue; otherwise it will be black. When the icons are hovered over in the `show_scatter()` widget, alternative text will be shown.





\begin{table}

\caption{(\#tab:pdf-controls)An overview of the interactive controls available in the \pkg{detourr} displays}
\centering
\begin{tabular}[t]{>{\raggedright\arraybackslash}p{0.2\textwidth}>{\raggedright\arraybackslash}p{0.2\textwidth}>{\raggedright\arraybackslash}p{0.6\textwidth}}
\toprule
Control & Icon & Description\\
\midrule
Orbit & \raisebox{-\totalheight}{\includegraphics[width=0.2\textwidth]{figures/implementation/orbit_control_button.png}} & When the `show\_scatter()` widget is generated, orbit controls will be enabled by default. This allows click and drag to rotate the visual, and scrolling/pinching to zoom. Note that orbit controls for the 2D variant work best if dragging from left to right, not up and down. Also note that the icon for the currently selected control will be highlighted blue; otherwise it will be black.\\
\midrule
Pan & \raisebox{-\totalheight}{\includegraphics[width=0.2\textwidth]{figures/implementation/pan_button.png}} & The pan control also allows scrolling to zoom, and click and drag to pan.\\
\midrule
Box Selection & \raisebox{-\totalheight}{\includegraphics[width=0.2\textwidth]{figures/implementation/select_button.png}} & The selection control allows for transitory box selection by brushing. Holding the `shift` key will allow for persistent selection, and points outside of the selection will be indicated by increased transparency. There is currently a limitation where only visible points can be selected. If a point is completely obscured by other points, it will not be selected.\\
\midrule
Brush & \raisebox{-\totalheight}{\includegraphics[width=0.2\textwidth]{figures/implementation/brush.png}} & The brush button will apply the current colour to the selected points.\\
\midrule
Colour Selector & \raisebox{-\totalheight}{\includegraphics[width=0.2\textwidth]{figures/implementation/colour_selector.png}} & The colour selector will look slightly different depending on the browser being used. When the colour selection is changed, the selected points will be updated immediately.\\
\bottomrule
\end{tabular}
\end{table}

\begin{figure}
\includegraphics[width=0.49\linewidth]{figures/implementation/pdfsense-brushing-1} \includegraphics[width=0.49\linewidth]{figures/implementation/pdfsense-brushing-2} \includegraphics[width=0.49\linewidth]{figures/implementation/pdfsense-brushing-3} \includegraphics[width=0.49\linewidth]{figures/implementation/pdfsense-brushing-4} \caption{An illustration of the box selection and brush tool being used together.}(\#fig:brushing)
\end{figure}

### Timeline controls

The timeline at the bottom of the widget controls play and pause, and allows for scrubbing to a specific point in the tour. The timeline can also be used to jump to a specific basis by clicking on any of the white basis markers, and hovering the mouse over the basis markers will display the index of that basis.

### Linked selection and filtering

\pkg{detourr} supports linked selection and filtering by integrating with \pkg{crosstalk}. When a \pkg{crosstalk} `SharedData` object is provided to `detour()` in place of a data frame, selections made using the _box selection_ tool will be reflected in all linked visuals. Likewise, any selection or filtering applied to a linked visual will be reflected by \pkg{detourr}. Compatible widgets include \CRANpkg{plotly} (@plotly), \CRANpkg{leaflet} (@leaflet), and \CRANpkg{DT} (@dt). An example of this is shown in the case study.

# Web technologies for performance







One of the goals of this work is to improve upon the animation performance of existing tour displays. \pkg{detourr} uses several different web technologies to maximise performance so that smooth animations can be played with large data sets consisting of upwards of 100k data points. This performance also enables the animations to work with less powerful devices, making \pkg{detourr} accessible to a wider range of users.

The primary technology that allows for high-performance data visualisation is JavaScript itself. JavaScript engines in browsers such as Chrome and Firefox are highly optimised, leveraging methods such as Just-In-Time (JIT) compilation for improved runtime speed. However JavaScript is single-threaded, dynamically typed, and garbage collected, so despite these optimisations we can still run in to performance bottlenecks in some situations.

Figure \@ref(fig:dataflow) shows a simplified overview of the data flow in \pkg{detourr} when creating and viewing a widget. On the left are the operations that are performed by R, which only occur once when the visual is first created and have a minimal performance impact. On the right are the main operations performed by JavaScript when the widget is displayed in a browser or IDE. Linear algebra and rendering operations need to run 30 times per second, so the technology decisions surrounding them have a big impact on performance. These technology decisions and are discussed in this chapter.

\begin{figure}
\includegraphics[width=\textwidth]{detourr_files/figure-latex/dataflow-1} \caption{An overview of the data flow when creating a detourr visualisation. The full tour path is generated in R and then passed to JavaScript when the widget is created. The operations that occur in the animation loop in JavaScript are the most important to optimise.}(\#fig:dataflow)
\end{figure}


## Linear algebra operations

The single-threaded nature of JavaScript makes matrix multiplication a performance bottleneck. At each animation frame, we must calculate the product $\mathbf{XA}$ where $\mathbf{X}$ is our data matrix and $\mathbf{A}$ is our projection matrix. The slice tour generated by the `show_slice()` display function requires an additional step of calculating the distance from each point to the projection plane which involves several more matrix operations.

To address this, \pkg{detourr} uses TensorFlow.js (@abadi2016tensorflow) as the main library for storing data and projection matrices and performing matrix operations. TensorFlow.js requires the user chose between one of three available backends:

__CPU__ is a single-threaded JavaScript implementation which carries with it the limitations of JavaScript dynamic typing and garbage collection causing non-deterministic slow-downs at runtime.

__WebAssembly__ (WASM) is a binary format that is used as a compilation target allowing code written in other languages like C, C++, and Rust to be run in the browser. This circumvents the dynamic typing and garbage collection limitations of JavaScript and allows near-native execution speed. The TensorFlow WASM backend uses the XNNPACK library from Google to accelerate matrix operations, which can run operations in parallel using threads and SIMD (Single Instruction Multiple Data).

__WebGL__: uses WebGL shaders to perform matrix operations on the GPU. According to the documentation, the performance benefit is primarily seen with large and complex deep learning models, so is unlikely to provide much benefit over the WebAssembly backend for our use case, and so is not investigated further in this section. 

## Performance comparison

To compare these backend options a simple performance profile was run in Microsoft Edge (Chromium) on a Macbook Pro 2019 (i7, 32Gb RAM). The implementations that were compared were:

- _Hand Coded_: a manual JavaScript implementation coded using `for` loops, operating on nested arrays representing data and projection matrices.
- _TensorFlow CPU_: the vanilla single-threaded CPU backend for TensorFlow.js.
-  _TensorFlow WASM_: the TensorFlow.js WASM backend.

These backends were compared across 3 datasets of different sizes and complexity, using a 2D Grand Tour:

- _pdfsense_: The same data set used throughout this chapter; 2808 observations across 56 variables, taking the first 6 principal components for the tour.
- _mnist\_embeddings\_8d_: 8-dimensional embeddings of the MNIST dataset, with a total of 10k observations.
- _mnist\_embeddings\_32d_: 32-dimensional embeddings of the MNIST dataset, again with 10k observations.

\begin{figure}

{\centering \includegraphics[width=0.75\linewidth]{detourr_files/figure-latex/backend-comparison-1} 

}

\caption{Performance comparison across different data sets and backends. TensorFlow provides better performance than a hand-coded implementation across the board. For smaller datasets like pdfsense, there is little difference between CPU and WASM backends for TensorFlow.js, but for larger dataset WASM performs much better.}(\#fig:backend-comparison)
\end{figure}

Figure \@ref(fig:backend-comparison) shows the performance of the three backends across the example datasets. TensorFlow provides better performance across the board when compared to the hand-coded implementation, but the difference between the CPU and WASM backends only becomes apparent with the larger MNIST embeddings datasets. Note that the metric `% Scripting Time` is the time spent across _all_ JavaScript scripting for the visual, and not just the time spend on linear algebra operations. This is why we see such diminishing returns with the smaller _pdfsense_ dataset.

Another important comparison is the performance of the `show_slice()` display function between these datasets. The slice tour uses additional matrix operations to calculate the distance from each point to the projection plane, so the benefit of WASM backend is even more apparent. This is shown in Figure \@ref(fig:backend-comparison-slice)

\begin{figure}

{\centering \includegraphics[width=0.75\linewidth]{detourr_files/figure-latex/backend-comparison-slice-1} 

}

\caption{The additional matrix operations required by the slice tour display function make the performance benefit of the WASM backend much more apparent.}(\#fig:backend-comparison-slice)
\end{figure}

## Rendering

When displaying data visuals using JavaScript in a browser, there are three main technologies that can be used:

__SVG__ is commonly used for web-based visuals, including in software such as D3.js (@bostock2011d3) with good support for interaction and animation. @kipp2019connecting uses D3.js with SVG for rendering tours, but describes performance issues when the number of points gets close to 2,000. This is because while SVG is suitable for drawing large and complex shapes, performance can degrade when rendering many individual shapes.

__HTML5 Canvas (2D)__ uses a canvas element with a 2D rendering context and provides good performance, allowing many thousand data points to be used with smooth animation. This is the rendering method used by the \pkg{langevitour} package, and provides much better performance over SVG for this use case.

__HTML5 Canvas (WebGL)__ uses the WebGL rendering context with GPU acceleration to achieve high performance, and is used by a range of browser-based 3D animations and games. This typically provides higher performance than using the 2D canvas rendering context.

\pkg{detourr} implements __HTML5 Canvas__ with the __WebGL__ rendering context using the Three.js (@threejs) library. This is the same library that powers the TensorFlow Embedding Projector (@smilkov2016embedding), and allows for flexible and performant 2D and 3D data visuals.

One downside of using HTML5 Canvas elements is that custom logic is needed to determine where the mouse pointer is relative to visual elements when interactions occur. This issue is resolved is by rendering the image twice; the first pass renders to the screen and the second renders to an invisible "picking" scene. The colours of the points in this picking scene correspond to the ID of the point that was rendered. When a mouse is hovered over a pixel or a set of pixels are selected, we simply check their colour in the picking scene to determine which point IDs relate to the event. Rendering the scene twice at each frame makes performance all the more important. 

Despite this extra step, a naive performance benchmark of the rendering performance of \pkg{detourr} using the `mnist_embeddings_8d` data set at 30 FPS shows only 3% of the time is devoted to rendering and painting points, which for our use case is negligible. 



# Display methods







There are three display functions implemented in the \pkg{detourr} package: `show_scatter()`, `show_sage()`, and `show_slice()`. All three support 2D and 3D tour paths, and are based on the core `show_scatter()` function. In this section, we will delve in to some of the implementation details of these functions and how the original and _sage_ display has been extended to three dimensions.

## Scatter display

The scatter display forms the core of the three display methods, and contains all of the features and interactions described in previous sections. It is implemented in TypeScript using the Three.js library (@threejs) for rendering and TensorFlow.js (@abadi2016tensorflow) for linear algebra operations.

## Slice display

The slice display is implemented in the `show_slice()` function, and is based on the _slice tour_ described in @laa2020slice. At each animation frame, the distance from each point to the projection plane is computed. Those points closer than some threshold $h$ to the projection plane are highlighted, and those further away are greyed out. Slices offset from the origin are also supported.

Despite the slice tour itself being equivalent to that in \pkg{tourr}, the implementation has been modified for a simpler implementation. @laa2020slice calculates the distance as:

\begin{equation}
\tilde \nu_i^2 = ||\mathbf{x}_i^\prime||^2
(\#eq:nu)
\end{equation}

where

\begin{equation}
\mathbf{x}_i^\prime = \mathbf{x}_i - 
  (\mathbf{x}_i \cdot \mathbf{a}_1)\mathbf{a}_1 - 
  (\mathbf{x}_i \cdot \mathbf{a}_2)\mathbf{a}_2
(\#eq:xprime)
\end{equation}

and similar for the 3-dimensional case but with an additional term. With some rearranging, we can instead express this with the equivalent:

\begin{equation}
\tilde \nu^2 = (\mathbf{X} - \mathbf{XAA}^T)^2 \mathbf{1}_p
(\#eq:nu2)
\end{equation}

This requires fewer terms than the original, and is in a form that is more elegant to implement using TensorFlow.js. The implementation is also the same for both the 2D and 3D variants which keeps the code simple.

### Offsetting the slice

@laa2020slice provides a generalisation of equations \@ref(eq:nu) and \@ref(eq:xprime) for a projection plane passing through an arbitrary anchor point $\mathbf{c}$ as follows:

1. Calculate $\mathbf{x_i}^\prime$ as per equation \@ref(eq:xprime)
2. Calculate the component $\mathbf{c}^\prime$ of $\mathbf{c}$ orthogonal to the projection plane as:
$$
\mathbf{c}^\prime = \mathbf{c} - (\mathbf{c} \cdot \mathbf{a}_1)\mathbf{a}_1 - (\mathbf{c} \cdot \mathbf{a}_2)\mathbf{a}_2
$$
3. Calculate $\nu_i^2 = || \mathbf{x}_i^\prime - \mathbf{c}^\prime ||^2 = \mathbf{x}_i^{\prime 2} + \mathbf{c}^{\prime 2} - 2\mathbf{x}_i^\prime \cdot \mathbf{c}^\prime$ where the cross-term is expressed as:
$$
\mathbf{x}_i^\prime \cdot \mathbf{c}^\prime = \mathbf{x_i} \cdot \mathbf{c} - 
  (\mathbf{c} \cdot \mathbf{a}_1)(\mathbf{x}_i \cdot \mathbf{a}_1) -
  (\mathbf{c} \cdot \mathbf{a}_2)(\mathbf{x}_i \cdot \mathbf{a}_2)
$$

With this method there are many terms to calculate, and it was found it was difficult to implement and test. To circumvent this issue we instead take a different approach. Rather than offsetting the __projection plane__ to pass through the point $\mathbf{c}$ and then calculating the distances for each point, we instead offset the __data points__ by $\mathbf{c}$ in the opposite direction. This gives a distance calculation between points and projection plane that is equivalent to the original implementation but is much simpler to calculate. First we calculate the offset points $\mathbf{X}^\prime$:

\begin{equation}
\mathbf{X}^\prime = \begin{bmatrix}
    \mathbf{x}_1 - \mathbf{c} \\
    \mathbf{x}_2 - \mathbf{c} \\
    \vdots \\
    \mathbf{x}_n - \mathbf{c}
    \end{bmatrix}
(\#eq:xprime2)
\end{equation}

And then calculate the distances to the projection plane similar to equation \@ref(eq:nu2):

\begin{equation}
\tilde \nu^2 = (\mathbf{X}^\prime - \mathbf{X}^\prime \mathbf{AA}^T)^2 \mathbf{1}_p
(\#eq:nu3)
\end{equation}

Figure \@ref(fig:slice-sphere) shows a slice tour implemented using equations \@ref(eq:xprime2) and \@ref(eq:nu3) with an anchor of $(1, 0, 0)$. Initially $v_1$ is almost parallel to the projection plane, and so the slice runs close to the origin and only the points near the outside of the hollow sphere are highlighted. As the tour progresses, $v_1$ becomes nearly orthogonal to the projection plane, and so the slice runs close to the edge of the sphere and only a small number of points near the centre of the visual are highlighted.




\begin{figure}

{\centering \includegraphics[width=0.32\linewidth]{figures/display_methods/slice_sphere_1} \includegraphics[width=0.32\linewidth]{figures/display_methods/slice_sphere_2} \includegraphics[width=0.32\linewidth]{figures/display_methods/slice_sphere_3} 

}

\caption{Selected frames of a 2D slice tour of a hollow unit sphere. The anchor for the slice is set to (1, 0, 0). Initially the slice is near the origin, but moves closer to the edge of the sphere as v1 rotates to be near orthogonal to the projection plane.}(\#fig:slice-sphere)
\end{figure}

## Sage display

As the dimension of data increases, the volume of space that contains the data increases exponentially. One effect of this is that points tend to sit close to the edge of the space, with few points near the center. @hastie2009elements gives a good illustration of this, where if we have $N$ uniformly distributed points in a unit ball of dimension $p$ centred at the origin, the median distance from the origin to the closest point is given by the equation

\begin{equation}
d(p, N) = \left( 1- \frac{1}{2}^\frac{1}{N}\right)^\frac{1}{p}
\end{equation}

Counter-intuitively, when we project data from a high-dimensional space to low-dimensions, we see the opposite effect where points tend to crowd towards the center of the projected space. @laa2021burning describes a method for correcting this distortion so that points are less crowded towards the center. It does this by ensuring the relative volume at a given radius $r$ in the original space is preserved in the projected space. The relative volume for a 2-dimensional projection is given by the equation

\begin{equation}
v_2(r; p, R) 
= \frac{V_{2D}(r; p, R)}{V(R,p)}
= 1 - \left(1-\left(\frac{r}{R}\right)^2\right)^\frac{p}{2}
(\#eq:radialcdf2d)
\end{equation}

where $p$ is the dimension of our original data, $R$ is the radius of the p-ball that contains our data, and $r$ is the projected radius within $[0, R]$.

The formula for the corrected radius $r_y^\prime$ is then given as

\begin{equation}
r_y^\prime = R \sqrt{1 - \left(1-\left(\frac{r}{R}\right)^2\right)^\frac{p}{2}}
(\#eq:radial2d)
\end{equation}

\pkg{detourr} uses a slight variation of equation \@ref(eq:radial2d) to calculate the corrected radius, which omits the multiplier of $R$. This is because we always plot the data on the range $r_y^\prime = [0, 1]$, so the multiplier is not needed:

\begin{equation}
r_y^\prime = \sqrt{1 - \left(1-\left(\frac{r}{R}\right)^2\right)^\frac{p}{2}}
(\#eq:radial2d2)
\end{equation}

The full implementation is as follows:

1. Calculate the projected data $\mathbf{Y} = \mathbf{XA}$, where $\mathbf{X}$ has already been scaled.
2. Calculate the trimmed radius of the projected points $r_y^\mathrm{trim} = min(r_y, R)$ and apply the radial transformation described in equation \@ref(eq:radial2d2) to get the corrected radius $r_y^\prime$.
3. Scale the Euclidean point vectors by a factor of $\frac{r_y^\prime}{r_y}$ 

This differs from the original implementation described in @laa2021burning in that we don't convert the Euclidean vectors to polar form, and instead apply the scaling directly to the Euclidean vectors. This removal of the conversion step was primarily to improve performance.

### Extension to 3D

@laa2021burning provides the equation for the relative projected volume at radius $r$ on to a two dimensional disk for the 2-dimensional sage display. In this paper, we extend and implement the scatter, sage, and slice displays in 3D, and to do this we needed to calculate the relative projected volume for the case of a 3-dimensional projection.

In the appendix we show that the relative projected volume for a __sphere__ at radius $r$ is given by:

\begin{equation}
  v_3(r; p, R) = \mathrm{BetaInc}\left(\left(\frac{r}{R}\right)^2, \frac{3}{2}, \frac{p-1}{2}\right) 
\end{equation}

Where $\mathrm{BetaInc}(x, \alpha, \beta)$ is the regularised incomplete beta function. This is important because it represents the radial CDF of points projected to 3 dimensions, and suggests that the radial PDF of the projected points is $\mathrm{Beta}\left(\frac{3}{2}, \frac{p-1}{2}\right)$ assuming the original data is a uniformly distributed ball of radius $R$.

So for the three dimensional case, the full radial transformation for the sage tour is given by

\begin{equation}
r_y^\prime = \sqrt[3]{\mathrm{BetaInc}\left(\left(\frac{r}{R}\right)^2, \frac{3}{2}, \frac{p-1}{2}\right)}
(\#eq:radial3)
\end{equation}

We also show that this generalises to any projection from $p$ to $d$ dimensions with $p>d$ with the equation:

\begin{equation}
v(r; p, R, d) = \mathrm{BetaInc}\left(\left(\frac{r}{R}\right)^2, \frac{d}{2}, \frac{p-d}{2}+1\right)
(\#eq:radiald)
\end{equation}

This also suggests that equation \@ref(eq:radialcdf2d) is a special case of equation \@ref(eq:radiald). 

The 3D sage tour is currently implemented in the `show_sage()` function, and like the scatter and slice displays the correct variant is chosen automatically based on the dimension of the provided tour path. However, this is not implemented for $d>3$ as we don't yet have a display method that can handle higher-dimensional projections. This will be implemented as an extension of a Parallel Coordinates Plot (PCP) or Andrew's plot in future.

An example of the 3D sage tour is shown in Figure \@ref(fig:sage-sphere).




\begin{figure}

{\centering \includegraphics[width=0.32\linewidth]{figures/display_methods/scatter_sphere_3} \includegraphics[width=0.32\linewidth]{figures/display_methods/scatter_sphere_10} \includegraphics[width=0.32\linewidth]{figures/display_methods/scatter_sphere_50} \includegraphics[width=0.32\linewidth]{figures/display_methods/sage_sphere_3} \includegraphics[width=0.32\linewidth]{figures/display_methods/sage_sphere_10} \includegraphics[width=0.32\linewidth]{figures/display_methods/sage_sphere_50} 

}

\caption{(Top) Initial frames of a 3D scatter tour of a 3, 10, and 50 dimensional ball respectively from left to right. (Bottom) Selected frames of a 3D sage tour of similar 3, 10, and 50 dimensional balls. As the dimensionality increases, the standard scatter display crowds the points near the center, whereas the sage display shows a consistent radial distribution of points. All screenshots are at the same zoom level.}(\#fig:sage-sphere)
\end{figure}

# Case study --- MNIST embeddings







A common task when analysing wide or sparse data sets is to generate embeddings; finding a lower dimensional representation of high dimensional data, placing similar objects close together and dissimilar objects far apart in the embedding space. This is especially useful when dealing with text or image data.

An example of this is the algorithm used for facial recognition in FaceNet (@schroff2015facenet). A neural network is trained which maps a vector representation of images of faces to a lower dimensional space. The network minimises the distance between examples of the same class and maximises distances between examples from different classes in the output space. The result is that the euclidean distance between faces can be used as a metric for face similarity, so an unknown face can be classified as belonging to a specific individual if the distance between the unknown face and one or more known faces is small.

The datasets `mnist_embeddings_8d` and `mnist_embeddings_32d` in the \pkg{detourr} package are embeddings trained using a similar algorithm to FaceNet but using the MNIST (@lecun1998mnist) handwritten digits dataset. The training set consists of 60,000 28x28 pixel training images and in the following examples we visualise the test set containing 10,000 examples.

## Scatterplot display



\begin{figure}

{\centering \includegraphics[width=0.49\linewidth]{figures/mnist/mnist-8d-scatter-1} \includegraphics[width=0.49\linewidth]{figures/mnist/mnist-8d-scatter-2} \includegraphics[width=0.49\linewidth]{figures/mnist/mnist-8d-scatter-label} \includegraphics[width=0.49\linewidth]{figures/mnist/mnist-8d-scatter-timeline} 

}

\caption{Selected frames from the 8-dimensional MNIST embeddings data using show\_scatter() as the display method. The colour corresponds to the handwritten digit 0, 1, ..., 9. Despite the large number of data points, the animation of the tour is smooth and interactions are responsive.}(\#fig:mnist-8d-scatter)
\end{figure}

Using the core `show_scatter()` function to display a `grand_tour()` tour path in Figure \@ref(fig:mnist-8d-scatter) we can see quite good separation between the clusters corresponding to each of the 10 digits. Despite the tour animation consisting of 10,000 data points, the animation runs smoothly at 30 FPS in Microsoft Edge on a Macbook Pro 2019. Running a performance profile of the animation indicates the CPU is idle 90% of the time while the animation is playing. The remaining time is divided between scripting (6%, including linear algebra operations), rendering (1.4%), painting (0.8%) and system (1.8%). When running the same tour on the `mnist_embeddings_32d` dataset, the animation is still quite smooth and CPU is 80% idle. 

In the lower left of Figure \@ref(fig:mnist-8d-scatter) is an example of the `label` aesthetic at work. This allows the user to identify which group a set of points belongs to, as well as the precise ID of any outliers that may require further investigation. 

## Sage and Slice display methods



\begin{figure}

{\centering \includegraphics[width=0.49\linewidth]{figures/mnist/mnist-8d-sage-1} \includegraphics[width=0.49\linewidth]{figures/mnist/mnist-8d-sage-2} 

}

\caption{Selected frames from the 8-dimensional MNIST embeddings data using show\_sage() as the display method with a 2D grand tour path. The sage display shows the data points near the surface of the unit ball, which is due to the L2 normalisation of the original embeddings. This structure was not clear in the standard scatter display but is preserved with the sage display.}(\#fig:mnist-8d-sage)
\end{figure}



The `show_scatter()` display method gives the viewer a fairly good sense of the data set, but there is some structure that may not be obvious. The embeddings in the `mnist_embeddings_8d` and `mnist_embeddings_32d` datasets are L2 normalised, so the points sit on the surface of a unit ball in the high-dimensional space. To reveal this structure, we can use the _sage_ (@laa2021burning) or _slice_ (@laa2020slice) display methods, which are implemented as `show_sage()` and `show_slice()` respectively.

The _sage_ display scales points outwards based on their radius so that the relative volume of the circle or sphere in the projected space is the same as in the original space. In the example shown in Figure \@ref(fig:mnist-8d-sage), the `show_sage()` display method is used. The effect is that the projected points tend sit much closer to the surface of the unit circle, giving a much clearer view of the ball-like structure of the original data.



\begin{figure}

{\centering \includegraphics[width=0.49\linewidth]{figures/mnist/mnist-8d-slice-1} \includegraphics[width=0.49\linewidth]{figures/mnist/mnist-8d-slice-2} 

}

\caption{Selected frames of the 8-dimensional MNIST embeddings data using show\_slice() as the display method. The slice display makes the hollowness of this data apparent.}(\#fig:mnist-8d-slice)
\end{figure}


The _slice_ display highlights points based on their proximity to the projection plane. Points that are close to the projection plane are highlighted and those further away are faded out by making them transparent. In the case of the MNIST embedding data in Figure \@ref(fig:mnist-8d-slice) the ball structure of the data manifests as a clear circular void in the middle of the plot, with points highlighted only towards the edges. 

## Linked selection

Plot interactions such as selection and filtering can be helpful for identifying and exploring outliers, clusters, and other interesting features in a dataset. These are enhanced even further when multiple visuals are linked, and selections and filters are applied to all linked visuals. In this example, we compare the tour animation with the result of a T-SNE (@van2008tsne) which was performed using the excellent \CRANpkg{Rtsne} R package (@van2015rtsne) and displayed using \pkg{plotly}. The visuals are linked using the R package \pkg{crosstalk} and a set of filter checkboxes is also added. 

\begin{figure}
\includegraphics[width=\textwidth]{figures/mnist/case-study-linked-brushing-full} \caption{Linked visuals of the tour using detourr (left) compared to a T-SNE dimension reduction (right)}(\#fig:linked-tsne-full)
\end{figure}

Figure \@ref(fig:linked-tsne-full) shows the linked visuals in their initial state with no filtering applied. We can then use the selection tool in either of the visuals to highlight points, and see the highlighting applied to both visuals as in Figure \@ref(fig:linked-tsne-selection).

\begin{figure}
\includegraphics[width=\textwidth]{figures/mnist/case-study-linked-brushing-selection} \caption{Linked visuals with selection applied. Points can be selected in either visual via click-and-drag and the selection will be reflected in both.}(\#fig:linked-tsne-selection)
\end{figure}

Figure \@ref(fig:linked-tsne-filter) shows the result of filtering the visuals using the filter checkboxes on the left. In the filtered visual, outlying points are much easier to see, and they can be easily investigated using tooltips. 

\begin{figure}
\includegraphics[width=\textwidth]{figures/mnist/case-study-linked-brushing-filter} \caption{Linked visuals with filtering applied. Viewing each digit individually makes outlying points much more apparent, and those points can be identified using tooltips.}(\#fig:linked-tsne-filter)
\end{figure}

\pagebreak

The code used to produce figures \@ref(fig:linked-tsne-full), \@ref(fig:linked-tsne-selection), and \@ref(fig:linked-tsne-filter) is shown below. Here each plot is created using a \pkg{crosstalk} `SharedData` object in place of a standard data frame, and linked together using the `bscols` function:


```r
library(crosstalk)
library(Rtsne)
library(plotly)

data(mnist_embeddings_8d)

ts <- select(mnist_embeddings_8d, starts_with("X")) |>
    Rtsne(num_threads = 4)
Y <- as_tibble(ts$Y)
names(Y) <- c("Y1", "Y2")

plot_df <- bind_cols(mnist_embeddings_8d, Y)
shared_mnist <- SharedData$new(plot_df)

detour_plot <- detour(shared_mnist, tour_aes(
    projection = starts_with("X"), color = label,
    label = c(id, label),
)) |>
    tour_path(grand_tour(2)) |>
    show_sage(width = "100%", height = "450px")

tsne_plot <- plot_ly(shared_mnist,
    x = ~Y1,
    y = ~Y2,
    text = paste0("Label: ", plot_df$label, "<br>", "ID: ", plot_df$id),
    color = ~label,
    height = 450,
    colors = viridisLite::viridis(10)
) |>
    highlight(on = "plotly_selected", off = "plotly_doubleclick") |>
    add_trace(type = "scatter", mode = "markers")

bscols(
    list(
        filter_checkbox("label", "Label", shared_mnist, ~label)
    ),
    detour_plot, tsne_plot,
    widths = c(1, 5, 6)
)
```

# Conclusion and future work







In this paper we have introduced \pkg{detourr} which provides interactive, performant, and portable tour visualisations from R. We accomplish these things using web technologies; TensorFlow.js (@abadi2016tensorflow) provides fast linear algebra operations through WebAssembly, Three.js provides GPU rendering via WebGL, and JavaScript & HTML enable good performance and interactive features across the board. We also provide a simplified implementation of the Slice display (@laa2020slice), and have generalised the radial transformation from the Sage display (@laa2021burning) to work with tours of 3 or more dimensions. All of this is done with an intuitive user interface which makes the software accessible to new users.

Looking ahead, the priority for the next stage of development is to leverage \pkg{detourr}'s extensible design to implement additional display methods such as density plots, histograms, parallel coordinates plots, and Andrew's plot. Additional changes could also be made to allow the radial transformation of the sage display and the highlighting of points from the slice display to be incorporated in to these other display methods, rather than being limited to only the scatter plot display. This would also allow the additional information from both the sage and slice tour applied to the same visual.

Further enhancements could be made by implementing facetting; allowing grouped data to be displayed across separate visuals with unified controls and timeline added. This could be taken further by allowing multiple _different_ displays to use the same controls and timeline, for example displaying a scatter plot alongside one or more density plots.

To extend the existing scatter plot displays, the addition of an interactive legend would greatly enhance the user experience. As well as providing context for the point colour / fill, this would allow the user to be able to filter groups without needing to use a separate package like \pkg{shiny} or \pkg{crosstalk}. A shape aesthetic would also be beneficial, and the ability to export the projection matrix at the current frame would make it easier to perform analysis once an interesting projection is found.

As well as being able to display points and lines, support for plotting surfaces would allow for rich visualisations of regression model fits and classification boundaries. Three.js has good support for drawing surfaces, however it's not clear how a decision boundary can be projected down to a lower number of dimensions or whether this is actually feasible.

Support for displaying images or sprites directly on the tour visual or as an extension of the tooltip functionality is possible. A similar feature is implemented in the Tensorboard Embedding Visualiser (@smilkov2016embedding) which also uses Three.js under the hood.

What's more, Three.js has support for VR, which would be an interesting addition for exploring an immersive 3D tour visual. 

\pagebreak






# Radial CDF of hyperspheres projected to 3 dimensions {.appendix}

In order to implement the 3D variant of the sage tour (@laa2021burning), we need an expression for the relative projected volume of a __sphere__ of radius $R$. This is then used as a scaling factor for point radii in the visualisation to prevent points from being crowded towards the centre.

First we denote the volume of a $p$-dimensional hypersphere by:

$$
\frac{2\pi^{p/2}R^p}{p\Gamma(p/2)}
$$

In the appendix of @laa2020hole (equations 7--10) is a derivation for the relative projected volume of a ball of radius $r$.

\begin{align}
    F(r; p, R) &= \frac{V_{inside}(r; p, R)}{ V(p, r) } \\
    &= 1 - \frac{V_{outside}(r; p, R)}{ V(p, r) }
    (\#eq:F2)
\end{align}

And the formula for $V_{outside}(r; p, R)$ for a __circle__ is given as:

\begin{equation}
V_{outside}(r; p, R) =
\int_r^R V(p-2, \sqrt{R^2 - x^2})2\pi x dx 
(\#eq:v-outside)
\end{equation}

To extend this to the 3-dimensional case, we can modify \@ref(eq:v-outside) to express the  volume outside a __sphere__ of radius $r$ as:

\begin{equation}
  V_{outside}(r; p, R) = \int_r^R V(p-3, \sqrt{R^2 - x^2})4\pi x^2 dx
\end{equation}

and it follows that the relative projected volume for a sphere is

\begin{equation}
  F_3(r; p, R) = 1 - 
  \frac{
      \int_r^R V(p-3, \sqrt{R^2 - x^2})4\pi x^2 dx
  }{
      V(p, R)
  } 
\end{equation}

We know $2\Gamma(3/2) = \Gamma(1/2) = \sqrt{ \pi }$ so with some rearranging this can be reduced to:

\begin{equation}
  F_3(r; p, R) = 1 - \frac{2}{R^p} \frac{\Gamma(p/2+1)}{\Gamma(3/2)\Gamma((p-1)/2)}
  \int_r^R (R^2 - x^2)^{(p-3)/2} x^2 dx \\
\end{equation}

Denoting $u = 1 - \left(\frac{x}{R}\right)^2$ and $dx = \frac{R^2}{-2x}du = \frac{R}{-2\sqrt{1-u}}du$ for a change of variable this becomes

\begin{align}
  F_3(r; p, R) &= 1 - \frac{\Gamma(p/2+1)}{\Gamma(3/2)\Gamma((p-1)/2)}
  \int_0^{1-\frac{r^2}{R^2}} u^{(p-3)/2} (1-u)^{1/2} du \\
  &= 1 - \mathrm{BetaInc}\left(1-\left(\frac{r}{R}\right)^2, \frac{p-1}{2}, \frac{3}{2}\right) \\
  &= \mathrm{BetaInc}\left(\left(\frac{r}{R}\right)^2, \frac{3}{2}, \frac{p-1}{2}\right) 
  (\#eq:betainc1)
\end{align}

where _BetaInc_ is the regularised incomplete beta function (the CDF of a Beta distribution).

We can generalise this to any projection from $p$ to $d$ dimensions using the same steps, but with 

\begin{equation}
  V_{outside}(r; p, R, d) = \int_r^R V(p-d, \sqrt{R^2 - x^2}) 
  \frac{2\pi^{d/2}}{\Gamma(d/2)} x^{d-1} dx
\end{equation}

where $\frac{2\pi^{d/2}}{\Gamma(d/2)} x^{d-1}$ is the surface area of a d-ball.

This results in the relative projected volume of a projection from $p$ to $d$ dimensions being given by:

\begin{equation}
F(r; p, R, d) = \mathrm{BetaInc}\left(\left(\frac{r}{R}\right)^2, \frac{d}{2}, \frac{p-d}{2}+1\right)
(\#eq:betainc2)
\end{equation}

Figures \@ref(fig:radial-cdf-p) and \@ref(fig:radial-cdf-d) compare the theoretical results from equations \@ref(eq:betainc1) and \@ref(eq:betainc2) respectively with simulated values.



\begin{figure}

{\centering \includegraphics[width=0.75\linewidth]{detourr_files/figure-latex/radial-cdf-p-1} 

}

\caption{Relative projected volume for projections from p dimensions to d=3 dimensions. The solid line is simulated data, and the dashed line is the theoretical CDF}(\#fig:radial-cdf-p)
\end{figure}

\begin{figure}

{\centering \includegraphics[width=0.75\linewidth]{detourr_files/figure-latex/radial-cdf-d-1} 

}

\caption{Relative projected volume for a projection of p=10 dimensions to d dimensions.  The solid line is simulated data, and the dashed line is the theoretical CDF. This shows the generalisation to d > 3 dimensions}(\#fig:radial-cdf-d)
\end{figure}



# References

