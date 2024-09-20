fluidPage(
  tags$style(type = "text/css", "html, body {zoom: 0.9;}"),
########## HEADING  ##########
  titlePanel(fluidRow(
    column(width = 10, "ProbDistID Signal Recognition"),
    column(width = 2,
           checkboxInput(
             inputId = "themeToggle",
             label = HTML(paste0(icon("sun"), " / ", icon("moon")))
           ))
  )),
  theme = shinytheme("flatly"),
  
########## SIDEBAR ##########
  sidebarLayout(
    sidebarPanel(
      width = 2,
      radioButtons(
        "option",
        "Choose an option:",
        c("Signal generation", "Data import", "Generate signal dataset")
      ),
      
      # DATA IMPORT
      conditionalPanel(
        "input.option == 'Data import'",
        div(
          HTML("<h4>Data import</h4>"),
          selectInput(
            "file_format",
            "Choose file format",
            choices = c("RData", "JSON", "CSV", "XML", "TXT")
          ),
          fileInput("file", "Choose a file")
        )
      ),
      
      # INDIVIDUAL SIGNAL GENERATION
      conditionalPanel(
        "input.option == 'Signal generation'",
        div(
          HTML("<h4>Signal generation</h4>"),
          uiOutput("selected_distributions_select"),
          numericInput(
            "samples",
            "Number of samples:",
            value = 1000,
            min = 10
          ),
          uiOutput("dist_parameters"),
          actionButton("generate", "Generate", class = "btn-primary"),
        )
      ),
      
     conditionalPanel(
      "input.option == 'Signal generation' | input.option == 'Data import'",
      div(
        tags$hr(style = "border-top: 1px solid #999;"),
        HTML("<h4>DCDF</h4>"),
        # Input field for number of samples
        numericInput(
          "num_samples",
          "Number of samples:",
          value = 1000,
          min = 10
        ),
        # Input field for number of beans
        numericInput("num_bins", "Number of bins:", value = 100, min = 10),
        # Button to run DCDF calculation function
        conditionalPanel(
          "input.option == 'Signal generation'",
          actionButton("dcdf_calculation", "DCDF Calculation", class = "btn-primary")
        ),
      ),
      tags$hr(style = "border-top: 1px solid #999;"),
      
        HTML("<h4>Signal recognition</h4>"),
        actionButton("recognize_signal", "Perform signal fitting", class = "btn-primary")
    ),
    
    # SIGNAL DATASET GENERATION
    conditionalPanel(
      "input.option == 'Generate signal dataset'",
      div(
        HTML("<h4>Generate signal dataset</h4>"),
        uiOutput("selected_distributions_select2"),
        verbatimTextOutput("dist_description"),
        numericInput(
          "numSignals",
          "Number of signals per loop:",
          value = 10,
          min = 1
        ),
        numericInput(
          "numSamples",
          "Number of samples:",
          value = 1000,
          min = 10
        ),
        uiOutput("dist_parameters2"),
        actionButton("generateDataset", "Generate dataset", class = "btn-primary"),
        selectInput(
          "output_file_format",
          "Choose file format",
          choices = c("RData", "JSON", "CSV", "XML")
        ),
        downloadButton("fileDownload", label = "Download File")
      )
    ),
  ),

    ########## MAIN PANEL ##########
    mainPanel(
      width = 10,
      tabsetPanel(
        id = "mainTabset",
        tabPanel(
          title = HTML("<h4>Distribution selection</h4>"),
          value = "tab1",
          HTML("<h4>Distributions</h4>"),
          # Distribution selector
          fluidRow(
            column(
              3,
              style = "border: 1px solid lightgray;",
              # Checkbox group for selecting distributions
              checkboxGroupInput(
                inputId = "distributions",
                label = "Select the distributions to generate or fit",
                choices = names(distribution_map)
              ),
              tags$hr(style = "border-top: 1px solid #999;"),
              checkboxInput(inputId = "select_all", label = "Select all (NOT RECOMENDED)"),
              
              # Button to store selection
              actionButton(
                "store_selection",
                "SELECT",
                icon = icon("check"),
                class = "btn-primary"
              ),
              verbatimTextOutput("selection_output")
              
            ),
            # Placeholder for parameter inputs
            column(6, uiOutput("param_inputs")),
          )
        ),
        
        tabPanel(
          title = HTML("<h4>Signal plot viewer</h4>"),
          value = "tab2",
          tabsetPanel(
            tabPanel("Signal", plotOutput("signalPlot1")),
            tabPanel("PDF", plotOutput("signalPlot2")),
            tabPanel("CDF", plotOutput("signalPlot3"))
          ),
          style = "border: 1px solid lightgray;",
          HTML("<hr/>"),
          plotOutput("dcdfPlot")
        ),
        
        tabPanel(
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
          ),
          title = HTML("<h4>Individual signal fitting results</h4>"),
          value = "tab3",
          HTML("<h4>Model selection values</h4>"),
          tableOutput("table"),
          tags$hr(style = "border-top: 1px solid #999;"),
          # Add a horizontal line
          HTML("<h4>Estimated distribution parameters</h4>"),
          tableOutput("param_table")
        ),
        
        tabPanel(
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
          ),
          title = HTML("<h4>Batch signal fitting results</h4>"),
          value = "tab4",
          # Add some text to be rendered above the table
          verbatimTextOutput("my_text"),
          # Table for dataset
          tableOutput("contents"),
          tableOutput("table2"),
          tags$hr(style = "border-top: 1px solid #999;"),
          # Add a horizontal line
          HTML("<h4>Dataset recognition results:</h4>"),
          tableOutput("param_table2"),
          tags$hr(style = "border-top: 1px solid #999;"),
          verbatimTextOutput("fit_error"),
          # Define the UI elements to display the table outputs
          uiOutput("table_ui"),
          # uiOutput("param_table_ui")
        )
      ),
    )
  ),
  # Add JavaScript code to switch themes
  tags$script(
    "const themes = {
            dark: 'shinythemes/css/darkly.min.css',
            light: 'shinythemes/css/flatly.min.css'
        }

        // function that creates a new link element
        function newLink(theme) {
            let el = document.createElement('link');
            el.setAttribute('rel', 'stylesheet');
            el.setAttribute('text', 'text/css');
            el.setAttribute('href', theme);
            return el;
        }

        // function that remove <link> of current theme by href
        function removeLink(theme) {
            let el = document.querySelector(`link[href='${theme}']`)
            return el.parentNode.removeChild(el);
        }

        // define vars
        const darkTheme = newLink(themes.dark);
        const lightTheme = newLink(themes.light);
        const head = document.getElementsByTagName('head')[0];
        const toggle = document.getElementById('themeToggle');

        // define extra css and add as default
        const extraDarkThemeCSS = '.dataTables_length label, .dataTables_filter label, .dataTables_info {       color: white!important;} .paginate_button { background: white!important;} thead { color: white;}'
        const extraDarkThemeElement = document.createElement('style');
        extraDarkThemeElement.appendChild(document.createTextNode(extraDarkThemeCSS));
        head.appendChild(extraDarkThemeElement);

        toggle.addEventListener('input', function(event) {
            // if checked, switch to light theme
            if (toggle.checked) {
                removeLink(themes.light);
                head.appendChild(extraDarkThemeElement)
                head.appendChild(darkTheme);
            }  else {
                removeLink(themes.dark);
                head.removeChild(extraDarkThemeElement);
                head.appendChild(lightTheme);
            }
        })
        "
  )
)