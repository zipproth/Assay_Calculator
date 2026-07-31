library(shiny)
library(ggplot2)
library(plyr)
library(reshape2)
library(grid)
library(shinydashboard)
library(gridExtra)
library(gtools)
library(ggsci)
library(png)
library(readxl)
library(openxlsx)

# The calculations, the plot builders and the download writers live in R/, so
# that they can be tested without starting a Shiny session.
for (module in list.files("R", pattern = "[.][Rr]$", full.names = TRUE)) {
  source(module, local = TRUE)
}

shinyServer(function(input, output, session){

  ############################ settings ############################

  # Seconds between two measurement points.  It scales the time axis and, for
  # the calcium assay, the L/Lmax normalization itself.
  measurement_interval <- reactive({
    value <- input$interval_value
    default <- if (input$assay_type == 1) 10 else 60

    if ((is.null(value))||(is.na(value))||(value <= 0))
      return(default)

    if (isTRUE(input$interval_unit == "min"))
      return(value * 60)

    return(value)
  })

  # Measurement points recorded before the elicitor is added, and how many of
  # them are averaged into the background of a ROS measurement.
  ros_background_points <- reactive({
    value <- input$ros_bg_points
    if ((is.null(value))||(is.na(value))||(value < 2)) return(10)
    return(round(value))
  })

  ros_background_used <- reactive({
    value <- input$ros_bg_used
    if ((is.null(value))||(is.na(value))||(value < 1)) return(5)
    return(min(round(value), ros_background_points() - 1))
  })

  # Unit of the x axis of the kinetics plots.
  time_unit <- reactive({
    if (is.null(input$time_unit))
      return(if (input$assay_type == 1) "s" else "points")
    return(input$time_unit)
  })

  plot_palette <- reactive({
    if (is.null(input$palette)) return("ggplot")
    return(input$palette)
  })

  excluded_wells <- reactive({
    if (is.null(input$excluded_wells)) return(character(0))
    return(input$excluded_wells)
  })

  ############################ data ############################

  calculate <- reactive({

    inputfile <- input$data_file
    if (is.null(inputfile)) return(NULL)

    dc <- if (input$assay_type == 2) 0 else input$dc
    if (is.null(dc)) return(NULL)

    withProgress(message = "Reading and normalizing the measurement", value = NULL, {
      calculate_data(inputfile, dc, input$data_type, input$assay_type,
                     interval = measurement_interval())
    })
  })

  # Mean over all wells of a second plate, drawn into the well curves for
  # comparison.
  calculate_plate_mean <- reactive({

    inputfile <- input$WT_file
    if (is.null(inputfile)) return(NULL)

    dc <- if (input$assay_type == 2) 0 else input$dc
    if (is.null(dc)) return(NULL)

    reference <- calculate_data(inputfile, dc, "2", input$assay_type,
                                interval = measurement_interval())
    if (is.null(reference)) return(NULL)

    return(reference$plate_mean)
  })

  multiple_calculate <- reactive({
    if(is.null(input$data2)&is.null(input$data3)&is.null(input$data4)){
      return(NULL)
    }

    dc <- if (input$assay_type == 2) 0 else input$dc
    if (is.null(dc)) return(NULL)

    output <- list()
    output$data1 <- calculate()

    for (slot in c("data2", "data3", "data4")){
      if(is.null(input[[slot]])) next
      output[[slot]] <- calculate_data(input[[slot]], dc, input$data_type,
                                       input$assay_type,
                                       interval = measurement_interval())
    }

    return(output)
  })

  get_layout <- reactive({
    read_plate_layout(input$layout_file)
  })

  plate_layout <- reactive({

    plate_layout <- get_layout()$plate_layout
    well_plate <- calculate()$well_plate

    if (is.null(plate_layout))
      return(NULL)
    if (is.null(well_plate))
      well_plate = 96

    return(draw_layout(plate_layout, well_plate, input$layout_file$name))
  })

  ############################ summaries ############################

  mean_max_calculate <- reactive({

    plate_layout <- get_layout()$plate_layout
    data <- calculate()

    if ((is.null(plate_layout))||(is.null(data))||(is.null(input$exclude4max)))
      return(NULL)

    withProgress(message = "Averaging the wells and picking the maxima", value = NULL, {
      group_means_maxima(data, plate_layout,
                         exclude4max = input$exclude4max,
                         interval = measurement_interval(),
                         excluded_wells = excluded_wells())
    })
  })

  ROS_calculate <- reactive({

    if(input$assay_type == 1)
      return(NULL)

    plate_layout <- get_layout()$plate_layout
    data <- calculate()

    if ((is.null(plate_layout))||(is.null(data))||(is.null(input$exclude4max)))
      return(NULL)

    withProgress(message = "Blanking against the controls", value = NULL, {
      ros_normalize(data$ms_rawdata, plate_layout,
                    exclude4max = input$exclude4max,
                    bg_values = ros_background_points(),
                    how_many_bg_values = ros_background_used(),
                    excluded_wells = excluded_wells())
    })
  })

  summary_data <- reactive({
    if (input$assay_type == 1) mean_max_calculate() else ROS_calculate()
  })

  ############################ area under curve ############################

  # Start, end and step width of the integration window, in the unit of the
  # x axis of the respective assay.
  auc_time_range <- reactive({

    data <- calculate()

    if (is.null(data))
      return(NULL)

    if (input$assay_type == 1){
      time <- data$ms_normdata$time
      return(c(min(time), max(time), measurement_interval()))
    }

    # ROS measurements are counted in measurement points, starting before the
    # elicitor is added
    last_point <- nrow(data$ms_rawdata) - ros_background_points() - 1
    return(c(-ros_background_points(), last_point, 1))
  })

  # The wells of the current plate, normalized the way the respective assay
  # plots them, together with their time axis.
  analysis_well_values <- reactive({

    data <- calculate()
    plate_layout <- get_layout()$plate_layout

    if ((is.null(data))||(is.null(plate_layout)))
      return(NULL)

    if (input$assay_type == 1){
      return(list("values" = data$ms_normdata[well_columns(data$ms_normdata)],
                  "time" = data$ms_normdata$time))
    }

    raw_plate_layout <- annotated_wells(plate_layout)
    values <- ros_well_values(data$ms_rawdata, raw_plate_layout,
                              bg_values = ros_background_points(),
                              how_many_bg_values = ros_background_used())

    return(list("values" = values,
                "time" = seq(-ros_background_points(),
                             nrow(values) - ros_background_points() - 1)))
  })

  auc_calculate <- reactive({

    plate_layout <- get_layout()$plate_layout
    wells <- analysis_well_values()

    if ((is.null(plate_layout))||(is.null(wells))||(is.null(input$auc_range)))
      return(NULL)

    raw_plate_layout <- annotated_wells(plate_layout)
    raw_plate_layout <- raw_plate_layout[!(raw_plate_layout$well %in% excluded_wells()),]

    method <- if (is.null(input$auc_method)) "trapezoid" else input$auc_method
    spread <- if (input$assay_type == 1) "sd" else "sdom"

    if (input$assay_type == 2){
      # the control wells are the blank, they carry no area of their own
      raw_plate_layout <- raw_plate_layout[raw_plate_layout$elicitor != "control",]
    }

    auc <- group_auc(wells$values, wells$time, raw_plate_layout,
                     from = input$auc_range[1], to = input$auc_range[2],
                     method = method, spread = spread)

    if (is.null(auc))
      return(NULL)

    reference <- if (isTRUE(nzchar(input$auc_reference))) input$auc_reference else NULL

    return(list("auc" = relative_auc(auc, reference),
                "from" = input$auc_range[1],
                "to" = input$auc_range[2],
                "method" = method,
                "reference" = reference,
                "ylabel" = auc_label(input$assay_type, method, reference)))
  })

  ############################ reference curve ############################

  # All curves that can be picked as a reference: either the mean curves of the
  # current plate or the columns of an uploaded file.
  reference_curves <- reactive({

    if (is.null(input$reference_source))
      return(NULL)

    if (input$reference_source == "plate"){
      complete_data <- summary_data()
      if (is.null(complete_data))
        return(NULL)
      return(plate_reference_curves(complete_data, input$assay_type))
    }

    if (input$reference_source == "file"){
      return(read_reference_file(input$reference_file))
    }

    return(NULL)
  })

  reference_curve <- reactive({

    if (is.null(input$reference_source))
      return(NULL)

    # a single well or the mean over a selection of wells
    if (input$reference_source == "wells"){
      wells <- analysis_well_values()
      selected <- input$reference_wells
      if ((is.null(wells))||(is.null(selected)))
        return(NULL)
      selected <- selected[selected %in% colnames(wells$values)]
      if (length(selected) == 0)
        return(NULL)
      return(data.frame(time = wells$time,
                        values = rowMeans(wells$values[selected, drop = FALSE]),
                        stringsAsFactors = FALSE))
    }

    curves <- reference_curves()

    if (is.null(curves))
      return(NULL)

    group <- input$reference_group

    if ((is.null(group))||(!(group %in% colnames(curves))))
      return(NULL)

    return(data.frame(time = curves$time,
                      values = curves[[group]],
                      stringsAsFactors = FALSE))
  })

  ############################ plots ############################

  well_plots <- reactive({

    data <- calculate()

    if ((is.null(data))||(is.null(input$ylim))||(is.null(input$xlim)))
      return(NULL)

    mean_melted <- NULL
    if(isTRUE(input$mean_overlay)) mean_melted <- mean_max_calculate()$mean_melted

    plate_mean <- NULL
    if(isTRUE(input$mean_overlay_plate)) plate_mean <- data$plate_mean

    reference_plate_mean <- NULL
    if(isTRUE(input$mean_overlay_WT)) reference_plate_mean <- calculate_plate_mean()

    return(draw_well_plate(data, get_layout()$empty_wells, excluded_wells(),
                           mean_melted, plate_mean, reference_plate_mean,
                           input$file_name, input$data_file$name,
                           input$xlim, input$ylim))
  })

  bar_plots_max <- reactive({

    complete_data <- summary_data()

    if (is.null(complete_data))
      return(NULL)

    if (input$assay_type == 1){
      all_means <- complete_data$all_means
      header <- "Maxima with SD"
      ylabel <- "L/Lmax"

      max_barplot <- data.frame(
        elicitor = all_means[,1],
        genotype = factor(all_means[,2],
                          levels = unique(annotated_wells(get_layout()$plate_layout)$genotype)),
        values = as.numeric(all_means[,3]),
        sd = as.numeric(all_means[,4]),
        stringsAsFactors = FALSE
      )
    } else {
      header <- "Maxima with SDOM"
      ylabel <- "Relative Luminescence"
      max_barplot <- complete_data$maxima_melted
    }

    return(draw_summary_bars(max_barplot, header, ylabel,
                             input$bar_columns, input$bar_rotation,
                             plot_palette()))
  })

  bar_plots_auc <- reactive({

    complete_data <- auc_calculate()

    if (is.null(complete_data))
      return(NULL)

    header <- paste0("Area under Curve (",
                     complete_data$from, " to ", complete_data$to,
                     if (input$assay_type == 1) " s" else " measurement points",
                     ") with ",
                     if (input$assay_type == 1) "SD" else "SDOM")

    return(draw_summary_bars(complete_data$auc, header, complete_data$ylabel,
                             input$auc_columns, input$auc_rotation,
                             plot_palette()))
  })

  mean_graphs <- reactive({

    complete_data <- summary_data()
    genotypes <- unique(annotated_wells(get_layout()$plate_layout)$genotype)

    if (is.null(complete_data))
      return(NULL)

    if (input$assay_type == 1){
      header <- "Mean with SD"
      ylabel <- "L/Lmax"
      all_means_graph <- complete_data$all_means_graph

      max_lineplot <- data.frame(
        elicitor = all_means_graph[,1],
        genotype = factor(all_means_graph[,2], levels = genotypes),
        values = as.numeric(all_means_graph[,3]),
        sd = as.numeric(all_means_graph[,4]),
        time = as.numeric(all_means_graph[,5]),
        stringsAsFactors = FALSE
      )
    } else {
      header <- "Mean with SDOM"
      ylabel <- "Relative Luminescence"
      max_lineplot <- complete_data$normdata_melted
    }

    time_axis <- scale_time_axis(max_lineplot$time, input$assay_type,
                                 time_unit(), measurement_interval())
    max_lineplot$time <- time_axis$time

    reference <- reference_curve()
    if(is.null(reference) == FALSE){
      reference$time <- scale_time_axis(reference$time, input$assay_type,
                                        time_unit(), measurement_interval())$time
    }

    return(draw_mean_kinetics(max_lineplot, header, ylabel, time_axis$label,
                              input$graph_sorting, 1 %in% input$settings_mean,
                              reference, plot_palette()))
  })

  ############################ outputs ############################

  output$plot <- renderPlot({
    validate(need(input$data_file, "Please upload a data file in the sidebar."))
    validate(need(well_plots(), "The measurement could not be plotted."))
    well_plots()
  })

  output$multiple_wellplots <- renderPlot({
    complete_data <- multiple_calculate()
    validate(need(complete_data,
                  "Please upload at least one additional data file above."))
    validate(need(input$ylim, "Waiting for the axis settings."))

    plots <- list()
    for (dataset in complete_data){
      plots[[length(plots) + 1]] <-
        draw_well_plate(dataset, get_layout()$empty_wells, excluded_wells(),
                        NULL, NULL, NULL,
                        input$file_name, input$data_file$name,
                        input$xlim, input$ylim)
    }

    do.call("grid.arrange", c(plots, ncol=2))
  })

  output$mappingplot <- renderPlot({
    validate(need(input$data_file, "Please upload a data file in the sidebar."))
    well_plots()
  })

  output$plate_layout <- renderPlot({
    plate_layout()
  })

  output$bar_max <- renderPlot({
    validate(need(input$layout_file, "Please upload a plate layout in the sidebar."))
    validate(need(bar_plots_max(), summary_hint()))
    bar_plots_max()
  })

  output$graph_mean <- renderPlot({
    validate(need(input$layout_file, "Please upload a plate layout in the sidebar."))
    validate(need(mean_graphs(), summary_hint()))
    mean_graphs()
  })

  output$bar_auc <- renderPlot({
    validate(need(input$layout_file, "Please upload a plate layout in the sidebar."))
    validate(need(input$auc_range, "Waiting for the integration window."))
    validate(need(bar_plots_auc(), summary_hint()))
    bar_plots_auc()
  })

  # Why a summary could not be calculated.
  summary_hint <- reactive({
    plate_layout <- get_layout()$plate_layout

    if (is.null(plate_layout))
      return("Please upload a plate layout in the sidebar.")

    if ((input$assay_type == 2) &&
        (!("control" %in% annotated_wells(plate_layout)$elicitor)))
      return(paste("The ROS assay needs control wells: write 'control' into the",
                   "elicitor column of the wells used for blanking."))

    return(paste("No genotype and elicitor combination of the layout has wells",
                 "in the measurement file. Please check that the well names",
                 "match."))
  })

  ############################ downloads ############################

  # Everything that influenced the numbers, so that a result stays reproducible.
  current_settings <- reactive({
    values <- list(
      "assay type" = if (input$assay_type == 1) "Calcium" else "ROS",
      "data file" = input$data_file$name,
      "layout file" = input$layout_file$name,
      "measurement interval [s]" = measurement_interval(),
      "measurement points skipped before the peak search" = input$exclude4max,
      "excluded wells" = if (length(excluded_wells()) == 0) "none"
                         else paste(excluded_wells(), collapse = ", ")
    )

    if (input$assay_type == 1){
      values[["discharge measurement points"]] <- input$dc
    } else {
      values[["measurement points before elicitation"]] <- ros_background_points()
      values[["background points used"]] <- ros_background_used()
      values[["background window (measurement points)"]] <-
        range(background_window(ros_background_points(), ros_background_used()))
    }

    auc <- auc_calculate()
    if (is.null(auc) == FALSE){
      values[["area under curve window"]] <- c(auc$from, auc$to)
      values[["area under curve method"]] <- auc$method
      values[["area under curve unit"]] <- auc$ylabel
      values[["area under curve reference"]] <-
        if (is.null(auc$reference)) "none" else auc$reference
    }

    return(export_settings(values))
  })

  # The parts of the analysis, shared by the Excel and the csv download.
  download_parts <- reactive({
    data <- calculate()
    if (is.null(data)) return(NULL)

    auc <- auc_calculate()

    list(
      "ms_rawdata" = data$ms_rawdata,
      "ms_normdata" = if (input$assay_type == 1) data$ms_normdata else NULL,
      "settings" = current_settings(),
      "calcium" = if (input$assay_type == 1) mean_max_calculate() else NULL,
      "ros" = if (input$assay_type == 2) ROS_calculate() else NULL,
      "auc" = auc$auc,
      "wellplot" = well_plots(),
      "layout" = plate_layout(),
      "barplot" = bar_plots_max(),
      "aucplot" = bar_plots_auc(),
      "meangraph" = mean_graphs(),
      "with_plots" = 1 %in% input$settings_data_download1
    )
  })

  download_basename <- reactive({
    name <- input$data_file$name
    if (is.null(name)) return("assay_calculator")
    return(tools::file_path_sans_ext(name))
  })

  output$ui.downloaddata <- renderUI({
    if (is.null(calculate()))
      return(NULL)
    tagList(
      downloadButton('downloadData', 'Download Excel'),
      br(), br(),
      downloadButton('downloadCsv', 'Download CSV (zip)')
    )
  })

  output$downloadData <- downloadHandler(
    filename = function() {paste0("norm_", download_basename(), ".xlsx")},
    content = function(file) {
      workdir <- tempfile("assaycalc-")
      dir.create(workdir)
      on.exit(unlink(workdir, recursive = TRUE), add = TRUE)

      withProgress(message = "Assembling the Excel workbook", value = NULL, {
        path <- build_workbook(workdir, download_parts())
      })

      # a plain rename fails when the temp directory sits on another file
      # system than the download directory
      file.copy(path, file, overwrite = TRUE)
    }
  )

  output$downloadCsv <- downloadHandler(
    filename = function() {paste0("norm_", download_basename(), "_csv.zip")},
    content = function(file) {
      parts <- download_parts()
      workdir <- tempfile("assaycalc-")
      dir.create(workdir)
      on.exit(unlink(workdir, recursive = TRUE), add = TRUE)

      tables <- list(
        "raw data" = parts$ms_rawdata,
        "normalized data" = parts$ms_normdata,
        "settings" = parts$settings,
        "mean data" = parts$calcium$graphs_shaped,
        "maxima" = parts$calcium$all_means_shaped,
        "ros mean data" = parts$ros$normdata,
        "ros maxima" = parts$ros$maxima_melted,
        "area under curve" = parts$auc
      )

      archive <- build_csv_archive(workdir, tables)
      file.copy(archive, file, overwrite = TRUE)
    }
  )

  output$ui.downloadplot <- renderUI({
    if (is.null(calculate()))
      return(NULL)
    downloadButton('downloadPlot', 'Download Plot')
  })

  output$downloadPlot <- downloadHandler(
    filename = function() {paste0("plot_", download_basename(), ".pdf")},
    content = function(file) {
      wellcurves <- well_plots()
      platelayout <- plate_layout()
      bar_plots <- bar_plots_max()
      auc_plots <- bar_plots_auc()
      graph_mean <- mean_graphs()

      pdf(file, width = 29.7, height = 21.0, paper = "a4r")
      invisible(print(wellcurves))
      if (is.null(platelayout) == FALSE){invisible(print(platelayout))}
      if (is.null(bar_plots) == FALSE){invisible(print(bar_plots))}
      if (is.null(auc_plots) == FALSE){invisible(print(auc_plots))}
      if (is.null(graph_mean) == FALSE){invisible(print(graph_mean))}
      dev.off()
    }
  )

  output$downloadCombined <- downloadHandler(
    filename = function(){paste("combined_quadrants", "csv", sep = ".")},
    content = function(file) {

      Q1 <- curateQ(input$Q1E_file$datapath, input$Q1D_file$datapath)
      Q2 <- curateQ(input$Q2E_file$datapath, input$Q2D_file$datapath)
      Q3 <- curateQ(input$Q3E_file$datapath, input$Q3D_file$datapath)
      Q4 <- curateQ(input$Q4E_file$datapath, input$Q4D_file$datapath)

      measurement <- Q1 + Q2 + Q3 + Q4

      write.csv(measurement, file = file, row.names = FALSE)
    }
  )

  example_download <- function(filename){
    downloadHandler(
      filename = function(){filename},
      content = function(file){file.copy(filename, file)}
    )
  }

  output$downloadLicense <- example_download("license.txt")
  output$downloadCaExample <- example_download("CaExample.xlsx")
  output$downloadCaExample_layout <- example_download("CaExample_layout.xlsx")
  output$downloadROSExample <- example_download("ROSExample.xlsx")
  output$downloadROSExample_layout <- example_download("ROSExample_layout.xlsx")
  output$downloadDatatemplate <- example_download("Datatemplate.xlsx")
  output$downloadLayouttemplate <- example_download("Layouttemplate.xlsx")

  ############################ dynamic settings ############################

  output$ui.settings1 <- renderUI({
    if (input$assay_type == 2){
      HTML("<div><span style='color:red'>Keep in mind that the data has to be in the 'Luminoscan' format!</span></div>")
    } else{
      radioButtons(inputId="data_type",
                   label="Show graphs for:",
                   choices = list("raw data" = 1, "normalized data" = 2), selected = 2)
    }
  })

  output$ui.settings2 <- renderUI({
    if (input$assay_type == 2){
      return(NULL)
    }
    sliderInput("dc", "Discharge measurement points", min=0, max=20, value=15)
  })

  output$ui.settings3 <- renderUI({
    yrange <- calculate()$yrange
    if (is.null(yrange)){
      return(NULL)
    }
    ylim <- yrange[2]
    ymin <- yrange[1]
    ylim.steps = round(ylim/20, digits = 2)

    sliderInput("ylim", "y-axis limit", min = floor(ymin), max = ceiling(ylim*2),
                value = ylim, step = ylim.steps)
  })

  output$ui.settings4 <- renderUI({
    xrange <- calculate()$xrange
    if (is.null(xrange)){
      return(NULL)
    }
    sliderInput("xlim", "x-axis limit", min = xrange[1], max = xrange[2],
                value = c(xrange[1], xrange[2]))
  })

  output$ui.settings5 <- renderUI({
    plate_layout <- get_layout()$plate_layout
    if ((is.null(plate_layout))||(input$assay_type == 2))
      return(NULL)
    checkboxInput("mean_overlay", label = "plot respective means", value = FALSE)
  })

  output$ui.interval <- renderUI({
    default <- if (input$assay_type == 1) 10 else 60
    tagList(
      numericInput("interval_value", "Measurement interval",
                   value = default, min = 0, step = 1),
      radioButtons("interval_unit", label = NULL,
                   choices = list("Seconds" = "s", "Minutes" = "min"),
                   selected = "s", inline = TRUE)
    )
  })

  output$ui.ros_background <- renderUI({
    if (input$assay_type == 1)
      return(NULL)
    tagList(
      numericInput("ros_bg_points", "Measurement points before elicitation",
                   value = 10, min = 2, step = 1),
      numericInput("ros_bg_used", "Background points used",
                   value = 5, min = 1, step = 1),
      helpText("Both values are written to the settings sheet of the download.")
    )
  })

  output$ui.excluded_wells <- renderUI({
    data <- calculate()
    if (is.null(data))
      return(NULL)
    selectizeInput("excluded_wells", "Exclude wells from the analysis",
                   choices = well_columns(data$ms_normdata),
                   selected = isolate(input$excluded_wells),
                   multiple = TRUE,
                   options = list(placeholder = "no well excluded"))
  })

  output$ui.time_unit <- renderUI({
    if (input$assay_type == 1){
      radioButtons(inputId="time_unit", label=h4("Time axis"),
                   choices = list("Seconds" = "s", "Minutes" = "min"),
                   selected = "s")
    } else {
      radioButtons(inputId="time_unit", label=h4("Time axis"),
                   choices = list("Measurement points" = "points",
                                  "Seconds" = "s", "Minutes" = "min"),
                   selected = "points")
    }
  })

  output$ui.reference_group <- renderUI({

    if (isTRUE(input$reference_source == "wells")){
      data <- calculate()
      if (is.null(data))
        return(NULL)
      return(selectizeInput("reference_wells",
                            "Reference wells (mean over the selection)",
                            choices = well_columns(data$ms_normdata),
                            selected = isolate(input$reference_wells),
                            multiple = TRUE,
                            options = list(placeholder = "select one or more wells")))
    }

    curves <- reference_curves()

    if (is.null(curves))
      return(NULL)

    groups <- setdiff(colnames(curves), "time")

    if (length(groups) == 0)
      return(NULL)

    selectInput("reference_group", "Reference curve", choices = groups,
                selected = groups[1])
  })

  output$ui.auc_range <- renderUI({
    range <- auc_time_range()
    if (is.null(range))
      return(NULL)

    label <- if (input$assay_type == 1){
      "Integration window [s]"
    } else {
      "Integration window [measurement points]"
    }

    sliderInput("auc_range", label,
                min = range[1], max = range[2],
                value = c(range[1], range[2]), step = range[3])
  })

  output$ui.auc_reference <- renderUI({
    complete_data <- summary_data()

    if (is.null(complete_data))
      return(NULL)

    groups <- setdiff(colnames(plate_reference_curves(complete_data,
                                                      input$assay_type)), "time")

    selectInput("auc_reference", "Show as percent of",
                choices = c("absolute values" = "", groups),
                selected = "")
  })

  ############################ menu ############################

  output$menu1 <- renderMenu({
    if (is.null(input$layout_file))
      return(NULL)

    menuItem("Data Summary", tabName = "data_summary", icon = icon("chart-area"),
             menuSubItem("Mean Maxima", tabName = "norm_data1", icon = icon("chart-column")),
             menuSubItem("Area under Curve", tabName = "norm_data3", icon = icon("chart-area")),
             menuSubItem("Mean Kinetics", tabName = "norm_data2", icon = icon("chart-line"))
    )
  })

  output$menu2 <- renderMenu({
    if (is.null(input$data_file))
      return(NULL)

    menuItem("Download", tabName = "download", icon = icon("cloud-arrow-down"))
  })

  output$ui.plate_layout<-renderUI({
    if (is.null(get_layout()))
      return(NULL)

    box(title = "Plate Layout",
        solidHeader = TRUE,
        status = "success",
        width = 12,
        collapsible = TRUE,
        plotOutput("plate_layout", height = 300))
  })

})
