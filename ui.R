#Assay_Calculator
library(shiny)
library(shinydashboard)

dashboardPage(skin = "green",
  dashboardHeader(
    title = "Assay Calculator"

  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Instructions", tabName = "instructions", icon = icon("circle-info"), selected = FALSE),
      menuItem("Data Analysis", tabName = "analysis", icon = icon("calculator"), selected = TRUE,
               menuSubItem("Standard Analysis", tabName = "rawdata", icon = icon("file-lines")),
               menuSubItem("Multiple Measurements", tabName = "multi", icon = icon("clone"))),
      menuItemOutput("menu1"),
      menuItem("Mapping Tools", tabName = "mapping", icon = icon("map"), selected = FALSE,
               menuSubItem("Combine Files", tabName = "combine", icon = icon("clone")),
               menuSubItem("Mapping Wellcurves", tabName = "mapping_wellcurves", icon = icon("calculator"))),
      menuItemOutput("menu2")

    ),
    fileInput("data_file", label = h4("Data file input"), accept = c(".xls", ".xlsx", ".csv")),
    fileInput("layout_file", label = h4("Layout file input (optional)"), accept = c(".xls", ".xlsx"))
  ),

  dashboardBody(
    tags$head(tags$style(HTML('
      .content-wrapper,
      .right-side {
        background-color: #ffffff;
      }
    '))),
    tabItems(
      tabItem(tabName = "instructions",
          HTML("<center><h1><strong>Welcome to Assay Calculator</strong></h1></center>"),
          br(),
          div(class="body", style="font-size:120%",
              HTML("<p>This web application is designed to help you to analyse your data.
                   <br>
                   It can process data obtained from aequorin luminescence measurements (<strong>'Calcium Assay'</strong>) or ROS measurements (<strong>'ROS Assay'</strong>).
                   <br>
                   <br>
                   To view well plots and to start the calculations open the 'Data Analysis' tab.
                   <strong><h3>Here are some things you have to keep in mind:</h3></strong>
                   <li>Input files have to be .xls, .xlsx or .csv files</li>
                   <li>Data has to be formatted accordingly, one column per well</li>
                   <li>The plate layout is optional, but it is required for the ROS normalization and for every mean, maximum and area calculation</li>
                   <li><strong>ROS layout: put 'control' in the elicitor column for the measurements used for blanking</strong></li>
                   <li>The measurement interval defaults to 10 s for the calcium assay and 60 s for the ROS assay and can be changed in the well curve settings; it scales the time axis and the calcium normalization</li>
                   <li>Every setting that influenced the numbers is written to the 'settings' sheet of the download</li></p>"),
              HTML("<h3><strong>Download example data or empty templates:</strong></h3>"),
              tags$li(div(style="display:inline-block",
                    p("Get")),
                downloadLink('downloadCaExample', 'Calcium Assay data'),
                div(style="display:inline-block",
                    p("and")),
                downloadLink('downloadCaExample_layout', 'Calcium Assay plate layout')),
              tags$li(div(style="display:inline-block",
                    p("Get")),
                downloadLink('downloadROSExample', 'ROS Assay data'),
                div(style="display:inline-block",
                    p("and")),
                downloadLink('downloadROSExample_layout', 'ROS Assay plate layout')),
              tags$li(div(style="display:inline-block",
                    p("Get empty")),
                downloadLink('downloadDatatemplate', 'data template'),
                div(style="display:inline-block",
                    p("and")),
                downloadLink('downloadLayouttemplate', 'plate layout template')
              )),
          hr(),
          div(style="display:inline-block",
            p("Assay Calculator was created by Alexander Kutschera (TU Munich) as an OpenPlantScience community project and is maintained in this fork by the Ranf lab.")),
          div(style="display:inline-block",
              p("Please send your ")),
          div(style="display:inline-block",
              a(href="mailto:stefanie.ranf-zipproth@unifr.ch", "feedback!")
          ),
          div(style="display:inline-block",
              p("Assay Calculator is licenced with the ")),
          downloadLink('downloadLicense', 'GNU General Public License v3.0')

      ),
      tabItem(tabName = "rawdata",
        fluidRow(
          box(title = "Well Curves",
              solidHeader = TRUE,
              status = "success",
              width = 9,
              collapsible = FALSE,
              plotOutput("plot", height = 600)),
          box(title = "Well Curves Settings",
              solidHeader = TRUE,
              status = "success",
              width = 3,
              radioButtons(inputId="assay_type", label="Assay Type:",
                          choices = list("Calcium Assay" = 1, "ROS Assay" = 2),
                          selected = 1),
              uiOutput("ui.settings1"),
              hr(),
              h4("Measurement"),
              uiOutput("ui.interval"),
              uiOutput("ui.ros_background"),
              uiOutput("ui.excluded_wells"),
              hr(),
              h4("Options"),
              checkboxInput("file_name", label = "Write filename as header", value = TRUE),
              radioButtons(inputId="palette", label="Colour palette",
                           choices = list("ggplot default" = "ggplot",
                                          "Okabe-Ito (colour blind safe)" = "okabe_ito",
                                          "Viridis (colour blind safe)" = "viridis",
                                          "Nature Publishing Group" = "npg"),
                           selected = "ggplot"),
              uiOutput("ui.settings5"),
              uiOutput("ui.settings2"),
              uiOutput("ui.settings3"),
              uiOutput("ui.settings4")
          )
        ),
        fluidRow(
          uiOutput("ui.plate_layout")
        )
      ),

      tabItem(tabName = "norm_data1",
        fluidRow(
          box(title = "Maxima of the Mean with SD",
              width = 9,
              solidHeader = TRUE,
              status = "success",
              plotOutput("bar_max", height = 600)
              ),
          box(title = "Plot Settings",
              width = 3,
              solidHeader = TRUE,
              status = "success",
              radioButtons(inputId="bar_rotation", label=h4("Bar Rotation"),
                           choices = list("Vertical Bars" = 1, "Horizontal Bars" = 2),
                           selected = 1),
              radioButtons(inputId="bar_columns", label=h4("Plot Arrangement"),
                           choices = list("Vertical Alignment" = 1, "Horizontal Alignment" = 2),
                           selected = 1),
              sliderInput("exclude4max", label = h4("Exclude first Values"), min = 0,
                          max = 20, value = 4)
            )
          )
        ),

      tabItem(tabName = "norm_data3",
        fluidRow(
          box(title = "Area under Curve of the Mean with SD",
              width = 9,
              solidHeader = TRUE,
              status = "success",
              plotOutput("bar_auc", height = 600)
              ),
          box(title = "Plot Settings",
              width = 3,
              solidHeader = TRUE,
              status = "success",
              radioButtons(inputId="auc_rotation", label=h4("Bar Rotation"),
                           choices = list("Vertical Bars" = 1, "Horizontal Bars" = 2),
                           selected = 1),
              radioButtons(inputId="auc_columns", label=h4("Plot Arrangement"),
                           choices = list("Vertical Alignment" = 1, "Horizontal Alignment" = 2),
                           selected = 1),
              hr(),
              h4("Integration"),
              uiOutput("ui.auc_range"),
              radioButtons(inputId="auc_method", label="Method",
                           choices = list("Trapezoidal (area over time)" = "trapezoid",
                                          "Sum of values" = "sum"),
                           selected = "trapezoid"),
              uiOutput("ui.auc_reference")
            )
          )
        ),

      tabItem(tabName = "norm_data2",
          fluidRow(
            box(title = "Mean Kinetics",
                width = 9,
                solidHeader = TRUE,
                status = "success",
                plotOutput("graph_mean", height = 600)
                ),
            box(title = "Plot Settings",
                width = 3,
                solidHeader = TRUE,
                status = "success",
                checkboxGroupInput("settings_mean", label = h5("General Setting"),
                                   choices = list("Show SD (takes time!)" = 1)),
                radioButtons(inputId="graph_sorting", label=h4("Sort graphs by"),
                             choices = list("Elicitor" = 1, "Genotype" = 2),
                             selected = 1),
                uiOutput("ui.time_unit"),
                hr(),
                radioButtons(inputId="reference_source", label=h4("Reference curve"),
                             choices = list("None" = "none",
                                            "Wells from this plate" = "wells",
                                            "Group from this plate" = "plate",
                                            "Uploaded file" = "file"),
                             selected = "none"),
                fileInput("reference_file",
                          label = h6("Reference file (time in the first column)"),
                          accept = c(".xls", ".xlsx", ".csv")),
                uiOutput("ui.reference_group")
                )
          )
        ),


      tabItem(tabName = "combine",
          fluidRow(
            box(title = "Combine raw files",
                solidHeader = TRUE,
                status = "success",
                width = 12,
                collapsible = TRUE,
                div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("Q1E_file", label = h4("Quadrant 1 measurement data input"), accept = c(".txt"))),
                div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("Q2E_file", label = h4("Quadrant 2 measurement data input"), accept = c(".txt"))),
                div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("Q3E_file", label = h4("Quadrant 3 measurement data input"), accept = c(".txt"))),
                div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("Q4E_file", label = h4("Quadrant 4 measurement data input"), accept = c(".txt"))),
                br(),
                div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("Q1D_file", label = h4("Quadrant 1 discharge data input"), accept = c(".txt"))),
                div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("Q2D_file", label = h4("Quadrant 2 discharge data input"), accept = c(".txt"))),
                div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("Q3D_file", label = h4("Quadrant 3 discharge data input"), accept = c(".txt"))),
                div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("Q4D_file", label = h4("Quadrant 4 discharge data input"), accept = c(".txt"))),
                br(),
                downloadButton('downloadCombined', 'Download File')

          )
        )
),
      tabItem(tabName = "mapping_wellcurves",
            fluidRow(
              box(title = "Well Curves Settings",
                  solidHeader = TRUE,
                  status = "success",
                  width = 12,
                  collapsible = TRUE,
                  h4("Upload assay data with wildtype plants for comparison"),
                  div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("WT_file", label = h6("Wildtype data input (optional)"), accept = c(".csv", ".xls", ".xlsx"))),
                  br(),
                  div(style="display: inline-block;vertical-align:mid; width: 150px;", checkboxInput("mean_overlay_WT", label = "plot WT mean", value = FALSE)),
                  div(style="display: inline-block;vertical-align:mid; width: 150px;",checkboxInput("mean_overlay_plate", label = "plot plate mean", value = FALSE))
              )
            ),

          fluidRow(
            box(title = "Mapping Well Curves",
                solidHeader = TRUE,
                status = "success",
                width = 12,
                collapsible = FALSE,
                plotOutput("mappingplot", height = 900))
          )

),

tabItem(tabName = "multi",
        fluidRow(
          box(title = "Well Curves Settings",
              solidHeader = TRUE,
              status = "success",
              width = 12,
              collapsible = TRUE,
              h4("Upload additional datasets for comparison"),
              div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("data2", label = h4("Data file input 2"), accept = c(".xls", ".xlsx"))),
              div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("data3", label = h4("Data file input 3"), accept = c(".xls", ".xlsx"))),
              div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("data4", label = h4("Data file input 4"), accept = c(".xls", ".xlsx"))),
              br(),
              div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("layout2", label = h4("Layout file input 2"), accept = c(".xls", ".xlsx"))),
              div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("layout3", label = h4("Layout file input 3"), accept = c(".xls", ".xlsx"))),
              div(style="display: inline-block;vertical-align:top; width: 300px;",fileInput("layout4", label = h4("Layout file input 4"), accept = c(".xls", ".xlsx")))
          )
        ),

          box(title = "Multiple Well Curves",
              solidHeader = TRUE,
              status = "success",
              width = 12,
              collapsible = FALSE,
              plotOutput("multiple_wellplots")
              )
),


      tabItem(tabName = "download",
             h3("Download Everything!"),
             box(title = "Download Data",
                 width = 4,
                 solidHeader = TRUE,
                 status = "success",
                 uiOutput("ui.downloaddata"),
                 checkboxGroupInput("settings_data_download1", label = h5("General Setting"),
                                    selected = "1",
                                    choices = list("Add graphs to Excel file" = 1))
             ),
             box(title = "Download Plots",
                 width = 4,
                 solidHeader = TRUE,
                 status = "success",
                 uiOutput("ui.downloadplot")
             )

        )
      )
    )
  )
