library(shiny)
library(shinydashboard)
library(deSolve)
library(plotly)

# Define the SEIR model function with ICU beds
seir_model_icu <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N - vacc_rate * S + dist_rate * S
    dE <- beta * S * I / N - sigma * E
    dI <- sigma * E - (1 - mort_rate) * gamma * I - mort_rate * ICU_rate * I
    dR <- (1 - mort_rate) * gamma * I
    dD <- mort_rate * ICU_rate * I
    
    return(list(c(dS, dE, dI, dR, dD)))
  })
}

# Define initial values
initial <- c(S = 0.99, E = 0.01, I = 0, R = 0, D = 0)
parameters <- c(beta = 0.3, sigma = 0.1, gamma = 0.05, vacc_rate = 0.01, dist_rate = 0.02, mort_rate = 0.05, ICU_rate = 0.1, N = 1)

# Time vector
times <- seq(0, 200, by = 1)

# Solve the differential equations
out <- ode(y = initial, times = times, func = seir_model_icu, parms = parameters)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Modelo SIR"),
  dashboardSidebar(
    sliderInput("beta", "Tasa de infección (beta):", min = 0, max = 1, value = 0.3, step = 0.05),
    sliderInput("sigma", "Tasa de incubación (sigma):", min = 0, max = 1, value = 0.1, step = 0.05),
    sliderInput("gamma", "Tasa de recuperación (gamma):", min = 0, max = 1, value = 0.05, step = 0.01),
    sliderInput("vacc_rate", "Tasa de vacunación:", min = 0, max = 1, value = 0.01, step = 0.01),
    sliderInput("dist_rate", "Tasa de distanciamiento social:", min = 0, max = 1, value = 0.02, step = 0.01),
    sliderInput("mort_rate", "Tasa de mortalidad:", min = 0, max = 1, value = 0.05, step = 0.01),
    sliderInput("ICU_rate", "Tasa de ingreso a UCI:", min = 0, max = 1, value = 0.1, step = 0.01)
  ),
  dashboardBody(
    fluidRow(
      column(width = 8,
             plotlyOutput("seir_plot", height = "70vh")
      )
    ),
    fluidRow(
      column(width = 12,
             box(
               title = "Explicación",
               verbatimTextOutput("explanation_text")
             )
      )
    ),
    fluidRow(
      column(width = 12,
             tags$div(
               style = "text-align: center; color: #777; padding: 20px;",
               "Fernando Miranda: fermiraba2790@gmail.com   WA: +505 81909441  https://www.linkedin.com/in/fernando-miranda-ba2b44157/
                    &    Gabriela Lopez: WA: + 505 85104365
               "
             )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$seir_plot <- renderPlotly({
    parameters <- c(beta = input$beta, sigma = input$sigma, gamma = input$gamma, 
                    vacc_rate = input$vacc_rate, dist_rate = input$dist_rate, 
                    mort_rate = input$mort_rate, ICU_rate = input$ICU_rate, N = 1)
    out <- ode(y = initial, times = times, func = seir_model_icu, parms = parameters)
    out_df <- as.data.frame(out)
    plot_ly(data = out_df, x = ~times) %>%
      add_lines(y = ~S, name = "Susceptibles", color = I("blue")) %>%
      add_lines(y = ~E, name = "Expuestos", color = I("orange")) %>%
      add_lines(y = ~I, name = "Infectados", color = I("red")) %>%
      add_lines(y = ~R, name = "Recuperados", color = I("green")) %>%
      add_lines(y = ~D, name = "Fallecidos", color = I("black")) %>%
      layout(title = "Simulación Modelo SIR para COVID-19 con UCI",
             xaxis = list(title = "Tiempo en días: Modelo ajustado por flujos migratorios, proporción  puede ser mayor que 1"),
             yaxis = list(title = "Proporción de población"),
             legend = list(x = 0.85, y = 0.95),
             margin = list(l = 100, r = 100, b = 100, t = 100))
  })
  
  output$explanation_text <- renderText({
    expl_text <- paste(
      "Tasa de infección (beta):", input$beta, "\n",
      "Tasa de incubación (sigma):", input$sigma, "\n",
      "Tasa de recuperación (gamma):", input$gamma, "\n",
      "Tasa de vacunación:", input$vacc_rate, "\n",
      "Tasa de distanciamiento social:", input$dist_rate, "\n",
      "Tasa de mortalidad:", input$mort_rate, "\n",
      "Tasa de ingreso a UCI:", input$ICU_rate
    )
    expl_text
  })
}

# Run the application
shinyApp(ui = ui, server = server)
