fluidPage(
  includeCSS("style.css"),
  titlePanel("What Percent of NZ is Within 1 Mile of a Road?"),
    fluidRow(
      column(4, 
        textInput("key", label = "Submit Spreadsheet Key: "), 
        textInput("name", label = "Submit Last Name: ")
      )
    ), 
    fluidRow(
      column(11, 
      tabsetPanel(
          tabPanel("Input My Results", 
                   fluidRow(
                     column(4, 
                            br(), 
                            p("Watch your hard work pay off!
                              These values will update as you populate checkboxes 
                              in the table."),
                            htmlOutput("results"), 
                            br(), 
                            p("Click Download to save your work to CSV."), 
                            actionButton("download", "Download"), 
                            br(), 
                            br(), 
                            p("Click Upload to add your work to the class spreadsheet. 
                              Be sure you've populated the key 
                              and name fields at the top of the page. This action will 
                              overwrite previous work uploaded under your name."), 
                            actionButton("upload", "Upload My Roadless")
                     ), 
                     column(8, DT::dataTableOutput("data"))
                    )
                   ), 
          tabPanel("Get Class Results", 
                   br(),
                   p("Click Refresh to download class-wide results. This could take a minute."), 
                   actionButton("download_class", "Refresh"),
                   br(),
                   br(),
                   htmlOutput('class.results'),
                   plotOutput("plot")
                   )
    )
  )
)
)