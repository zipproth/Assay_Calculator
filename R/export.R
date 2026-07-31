# Building the download files.
#
# Everything is assembled below the session temp directory: the application
# directory is not necessarily writable, and two users downloading at the same
# time would otherwise overwrite each other's files.

# Render one plot into a sheet of the workbook.
add_plot_sheet <- function(wb, workdir, plot, sheet, width, height){

  if (is.null(plot))
    return(invisible(NULL))

  image <- file.path(workdir, paste0(sheet, ".png"))
  png(image, width = width, height = height)
  invisible(print(plot))
  dev.off()

  openxlsx::addWorksheet(wb, sheet)
  openxlsx::insertImage(wb, sheet, image, startRow = 2, startCol = 2,
                        width = width, height = height,
                        units = "px", dpi = 72)

  return(invisible(NULL))
}

# Write one data set into a sheet.  Matrices keep their row names when they
# carry information, data frames do not.
add_data_sheet <- function(wb, sheet, data, row_names = FALSE){

  if (is.null(data))
    return(invisible(NULL))

  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, data, colNames = TRUE, rowNames = row_names)

  return(invisible(NULL))
}

# Assemble the Excel download.  Returns the path of the finished workbook.
#
#   parts   named list with the data sets and plots to include, see server.R
build_workbook <- function(workdir, parts){

  wb <- openxlsx::createWorkbook()

  add_data_sheet(wb, "raw data", parts$ms_rawdata)

  if(is.null(parts$ms_normdata) == FALSE)
    add_data_sheet(wb, "normalized data", parts$ms_normdata)

  add_data_sheet(wb, "settings", parts$settings)

  if(is.null(parts$calcium) == FALSE){
    add_data_sheet(wb, "data with names", parts$calcium$ms_normdata_names)
    add_data_sheet(wb, "mean data", parts$calcium$graphs_shaped)
    add_data_sheet(wb, "maxima", parts$calcium$all_means_shaped)
    add_data_sheet(wb, "mean data with sd", parts$calcium$graphs_shaped_w_sd)
    add_data_sheet(wb, "maxima with sd", parts$calcium$all_means_shaped_w_sd)
  }

  if(is.null(parts$ros) == FALSE){
    add_data_sheet(wb, "mean data", parts$ros$normdata)
    add_data_sheet(wb, "maxima", as.data.frame(as.list(parts$ros$maxima),
                                               check.names = FALSE))
    add_data_sheet(wb, "mean data with sdom", parts$ros$normdata_w_sd)
    add_data_sheet(wb, "maxima with sdom", parts$ros$maxima_w_sd, row_names = TRUE)
  }

  add_data_sheet(wb, "area under curve", parts$auc)

  if(isTRUE(parts$with_plots)){
    add_plot_sheet(wb, workdir, parts$wellplot, "wellplots", 1200, 1100)
    add_plot_sheet(wb, workdir, parts$layout, "platelayout", 600, 600)
    add_plot_sheet(wb, workdir, parts$barplot, "maximabarplots", 900, 900)
    add_plot_sheet(wb, workdir, parts$aucplot, "aucbarplots", 900, 900)
    add_plot_sheet(wb, workdir, parts$meangraph, "meangraphs", 1200, 1200)
  }

  path <- file.path(workdir, "workbook.xlsx")
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

  return(path)
}

# The settings sheet: everything that influenced the numbers in this workbook,
# so that a result can be reproduced later.
export_settings <- function(values){
  return(data.frame(setting = names(values),
                    value = vapply(values, function(v) paste(as.character(v),
                                                             collapse = " to "),
                                   character(1)),
                    row.names = NULL,
                    stringsAsFactors = FALSE))
}

# Write every data set of the analysis into one zip file of csv files.
build_csv_archive <- function(workdir, tables){

  written <- character(0)

  for(name in names(tables)){
    if(is.null(tables[[name]])) next
    path <- file.path(workdir, paste0(gsub("[^A-Za-z0-9]+", "_", name), ".csv"))
    write.csv(tables[[name]], path, row.names = FALSE)
    written <- c(written, path)
  }

  archive <- file.path(workdir, "assay_calculator_csv.zip")
  utils::zip(archive, written, flags = "-j9X")

  return(archive)
}
