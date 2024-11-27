myFPC <- function(sigma, m) {

  # n & p
  n <- dim(sigma)[1]
  p <- dim(sigma)[2]

  # Eigenvalues / Eigenvectors
  eig <- eigen(sigma)
  lam <- eig$values
  ei <- eig$vectors

  # L
  L <- matrix(NA, nrow = length(ei[,1]), ncol = m)
  for (i in 1:m) {
    L[,i] <- sqrt(lam[i]) * ei[,i]
  }

  # Cumulative Proportion
  plam <- c()
  for (j in 1:p) {
    plam[j] <- lam[j]/p
  }
  cprop <- cumsum(plam)

  # List
  list(L=L, cprop=cprop)
}
R <- as.matrix(OLYMPIC)

library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Shiny Factors"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "factor",
                  label = "How many factors?",
                  min = 1,
                  max = 10,
                  value = 4,
                  step = 1
      )

    ),

    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("tabOut")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  df <- LANGUAGE
  d <- dist(df)


  output$tabOut <- renderTable({
    myFPC(R, input$factor)
  })
}
# Run the application
shinyApp(ui = ui, server = server)
