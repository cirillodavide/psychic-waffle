shinyUI(pageWithSidebar(

  headerPanel(""),

  sidebarPanel(
    fileInput("file1", "File input"),
    downloadButton('downloadData', 'Download'),
    uiOutput("choose_columns"),
    br(),
    a(href = "https://gist.github.com/4211337", "Source code")
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
                  "Minimum span",
                  min = 0,
                  max = 10,
                  value = 0),
      fluidRow(
        splitLayout(cellWidths = c("70%", "30%"), plotOutput("distPlot1"), column(2,tableOutput('table')))
      ),
    sliderInput("range",
                  "Time range",
                  min = 0,
                  max = 100,
                  value = c(0,10),
                  round = TRUE),
    plotOutput("distPlot2")
  )
))
