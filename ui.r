shinyUI(pageWithSidebar(

  headerPanel(""),

  sidebarPanel(
    fileInput("file1", "File input"),
    downloadButton('downloadData', 'Download'),
    uiOutput("choose_columns"),
    br(),
    a(href = "https://github.com/cirillodavide/psychic-waffle", "Source code")
  ),

  mainPanel(
    sliderInput("zero",
                  "Closeness to zero",
                  min = 0,
                  max = 1,
                  value = 0.05),
    sliderInput("wind",
                  "Smoothing window",
                  min = 1,
                  max = 10,
                  value = 1),
    sliderInput("span",
                  "Span cutoff",
                  min = 0,
                  max = 10,
                  value = 0),
      fluidRow(
        column(6, plotOutput("x2")),
        column(6, DT::dataTableOutput('x1'))
      ),
    sliderInput("range",
                  "Time range",
                  value = c(0,10),
                  min = 0,
                  max = 60, 
                  step = 0.01,
                  round = FALSE),
    plotOutput("x3")
  )
))
