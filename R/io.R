# Reading the files the user uploads.
#
# Excel workbooks are read with readxl and written with openxlsx.  Both are
# pure R/C++ packages: unlike the XLConnect package that earlier versions of
# this app used, they need no Java runtime, which removes the JVM memory
# tuning and the most common reason for the app to fail on a fresh server.

# Read the first sheet of an Excel workbook as a plain data frame, keeping the
# column names exactly as they are in the file.
read_excel_sheet <- function(path){
  sheet <- readxl::read_excel(path, sheet = 1, .name_repair = "minimal")
  return(as.data.frame(sheet, check.names = FALSE, stringsAsFactors = FALSE))
}

# Strip the "M:" / "M." prefix that the Luminoscan export puts in front of every
# well name, and turn the names into plain well ids (A1, B12, ...).
clean_well_names <- function(names){
  return(sub("^M[.:]", "", names))
}

# Read a measurement table, either as an Excel workbook or as a csv file.
# Returns NULL for anything else.
read_measurement_file <- function(inputfile){

  if (is.null(inputfile))
    return(NULL)

  extension <- tolower(tools::file_ext(inputfile[1]))

  if((extension == "xlsx")||(extension == "xls")){
    rawdata <- read_excel_sheet(inputfile$datapath)
  } else if (extension == "csv"){
    rawdata <- read.csv(inputfile$datapath)
  } else {
    return(NULL)
  }

  colnames(rawdata) <- clean_well_names(colnames(rawdata))
  rawdata[is.na(rawdata)] <- 0

  return(rawdata)
}

# Read the plate layout and derive the plate coordinates from the well names.
read_plate_layout <- function(inputlayout){

  if (is.null(inputlayout))
    return(NULL)

  raw_plate_layout <- read_excel_sheet(inputlayout$datapath)

  required <- c("well", "genotype", "elicitor")
  missing <- setdiff(required, colnames(raw_plate_layout))
  if(length(missing) > 0){
    stop(paste0("The plate layout file needs the columns ",
                paste(required, collapse = ", "), ". Missing: ",
                paste(missing, collapse = ", "),
                ". Please use the plate layout template."))
  }

  raw_plate_layout <- raw_plate_layout[required]

  plate_layout <- mutate(raw_plate_layout,
                        Row=as.numeric(match(toupper(substr(well, 1, 1)), LETTERS)),
                        Column=as.numeric(substr(well, 2, 5)))

  empty_wells <- raw_plate_layout$well[is.na(raw_plate_layout$elicitor) & is.na(raw_plate_layout$genotype)]

  output <- list("empty_wells" = empty_wells,
                "plate_layout" = plate_layout)
  return(output)
}

# The annotated wells of a layout, without the rows that carry no genotype or
# no elicitor.
annotated_wells <- function(plate_layout){

  if (is.null(plate_layout))
    return(NULL)

  raw_plate_layout <- plate_layout[,1:4]
  raw_plate_layout <- raw_plate_layout[complete.cases(raw_plate_layout),] #delete rows with NAs

  return(raw_plate_layout)
}

# Convert the raw .txt export of one Luminoscan plate quadrant into a data
# frame with one column per well.
curateQ <- function(QE = NULL, QD = NULL){
  if(is.null(QE)) return(0)
  if(is.null(QD)) return(0)

  QE <- read.table(QE, sep = "\t", dec = ",", header = FALSE, na.strings = "", as.is = TRUE)
  QE <- QE[,!apply(QE, 2, function(x){all(is.na(x))})]
  QE <- QE[!apply(QE, 1, function(x){all(is.na(x))}),]
  QE <- QE[,2:(ncol(QE))]

  QE_curated <- NULL
  index = 1
  for(i in seq(nrow(QE)/73)){
    if(is.null(QE_curated)) {
      QE_curated <- QE[index:(index+72),1:ncol(QE)]
    } else {
      QE_curated <- cbind(QE_curated, QE[index:(index+72),1:ncol(QE)])
    }
    index = index + 73
  }

  colnames(QE_curated) <- QE_curated[1,]
  QE_curated <- QE_curated[2:nrow(QE_curated),]
  QE_curated <- apply(QE_curated, 2, type.convert, dec = ",", as.is = TRUE)

  QD <- read.table(QD, sep = "\t", dec = ",", header = FALSE, na.strings = "", as.is = TRUE)
  QD <- QD[,!apply(QD, 2, function(x){all(is.na(x))})]
  QD <- QD[!apply(QD, 1, function(x){all(is.na(x))}),]
  QD <- QD[,2:(ncol(QD))]

  QD_curated <- NULL
  index = 1
  for(i in seq(nrow(QD)/7)){
    if(is.null(QD_curated)) {
      QD_curated <- QD[index:(index+6),1:ncol(QD)]
    } else {
      QD_curated <- cbind(QD_curated, QD[index:(index+6),1:ncol(QD)])
    }
    index = index + 7
  }

  colnames(QD_curated) <- QD_curated[1,]
  QD_curated <- QD_curated[2:nrow(QD_curated),]
  QD_curated <- apply(QD_curated, 2, type.convert, dec = ",", as.is = TRUE)

  Q_curated <- rbind(QE_curated, QD_curated)
  Q_curated_df <- data.frame(Q_curated, row.names = NULL, check.names = FALSE)
  Q_curated_df[is.na(Q_curated_df)] = 0
  colnames(Q_curated_df) <- clean_well_names(colnames(Q_curated_df))

  return(Q_curated_df)
}

# Read a reference curve file.  The first column is the time axis, every
# further numeric column is a curve that can be picked as the reference.  The
# "mean data" sheet of the Excel download has exactly this shape, so a previous
# experiment can be used as a reference without any editing.
read_reference_file <- function(inputfile){

  if (is.null(inputfile))
    return(NULL)

  extension <- tolower(tools::file_ext(inputfile[1]))

  if((extension == "xlsx")||(extension == "xls")){
    reference <- read_excel_sheet(inputfile$datapath)
  } else if (extension == "csv"){
    reference <- read.csv(inputfile$datapath, check.names = FALSE)
  } else {
    return(NULL)
  }

  numeric_columns <- vapply(reference, is.numeric, logical(1))

  if((ncol(reference) < 2)||(!numeric_columns[1]))
    return(NULL)

  reference <- reference[, numeric_columns, drop = FALSE]

  if(ncol(reference) < 2)
    return(NULL)

  colnames(reference)[1] <- "time"
  return(reference)
}
