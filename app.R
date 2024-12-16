library(shiny)
library(shinythemes)

# Definir la función de costo
costo_func <- function(x1, x2, rendimiento_real, tasa_aprendizaje) {
  # Rendimiento predicho basado en una función simple (lineal por ejemplo)
  rendimiento_predicho <- 2 + 0.05 * x1 + 0.1 * x2  # Simple modelo lineal
  
  # Función de costo
  costo <- (rendimiento_predicho - rendimiento_real)^2 + (x1 + x2) * 0.05  # Costo adicional por insumos
  return(costo)
}

# Definir la actualización de los parámetros con el descenso de gradiente
descenso_gradiente <- function(x1, x2, rendimiento_real, tasa_aprendizaje) {
  # Gradientes
  grad_x1 <- 2 * (2 + 0.05 * x1 + 0.1 * x2 - rendimiento_real) * 0.05 + 0.05  # Gradiente respecto a x1
  grad_x2 <- 2 * (2 + 0.05 * x1 + 0.1 * x2 - rendimiento_real) * 0.1 + 0.05  # Gradiente respecto a x2
  
  # Actualizar valores de x1 y x2
  x1 <- x1 - tasa_aprendizaje * grad_x1
  x2 <- x2 - tasa_aprendizaje * grad_x2
  
  return(c(x1, x2))
}

# Define UI for application
ui <- fluidPage(
  
  # Usar un tema atractivo de shinythemes
  theme = shinytheme("cerulean"),
  
  # Título de la aplicación
  titlePanel(
    h1("Optimización de Insumos para Cultivo de Quinua en Puno", align = "center"),
    windowTitle = "Optimización Agrícola"
  ),
  
  # Layout de la aplicación con barra lateral y panel principal
  sidebarLayout(
    sidebarPanel(
      # Uso de un tema agradable para los controles
      numericInput("rendimiento", "Rendimiento real (toneladas/ha):", value = 2),
      numericInput("tasa", "Tasa de aprendizaje:", value = 0.01, min = 0.001, max = 0.1),
      actionButton("run", "Ejecutar Descenso de Gradiente", class = "btn-primary")
    ),
    
    # Panel principal para mostrar los resultados
    mainPanel(
      h3("Problema de Optimización:"),
      p("En una parcela de quinua en la región de Puno, un agricultor desea encontrar la cantidad óptima de agua de riego (litros por hectárea) y fertilizante (kilogramos por hectárea) para maximizar el rendimiento del cultivo. Actualmente, el rendimiento promedio observado es de 2 toneladas por hectárea, pero varía dependiendo de la combinación de estos insumos. El agricultor quiere determinar la cantidad ideal de insumos para minimizar la diferencia entre el rendimiento real y el rendimiento esperado, sin exceder un presupuesto de 500 soles por hectárea para ambos recursos. ¿Qué cantidades de agua y fertilizante deben aplicarse para optimizar el rendimiento del cultivo?"),
      
      # Gráfico de la evolución del costo
      plotOutput("costoPlot"),
      
      # Resultados de la optimización
      textOutput("result"),
      
      # Tabla de iteraciones
      h4("Iteraciones realizadas:"),
      tableOutput("iteracionesTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Definir los valores iniciales
  x1_inicial <- 10  # Litros de agua inicial
  x2_inicial <- 5   # Kilogramos de fertilizante inicial
  
  # Reaccionar al clic en el botón para ejecutar el descenso de gradiente
  observeEvent(input$run, {
    rendimiento_real <- input$rendimiento
    tasa_aprendizaje <- input$tasa
    
    # Inicializar los valores
    x1 <- x1_inicial
    x2 <- x2_inicial
    
    # Guardar el historial de costos y los valores de las iteraciones
    costos <- c()
    iteraciones <- data.frame(Iteracion = integer(0), Agua = numeric(0), Fertilizante = numeric(0), Costo = numeric(0))
    
    # Ejecutar el descenso de gradiente durante 100 iteraciones
    for (i in 1:100) {
      # Calcular y guardar el costo actual
      costos <- c(costos, costo_func(x1, x2, rendimiento_real, tasa_aprendizaje))
      
      # Registrar la iteración
      iteraciones <- rbind(iteraciones, data.frame(Iteracion = i, Agua = x1, Fertilizante = x2, Costo = costos[i]))
      
      # Actualizar los valores de x1 y x2 usando descenso de gradiente
      params <- descenso_gradiente(x1, x2, rendimiento_real, tasa_aprendizaje)
      x1 <- params[1]
      x2 <- params[2]
    }
    
    # Mostrar el costo final y los valores optimizados
    output$result <- renderText({
      paste("Valor óptimo de agua (litros/ha):", round(x1, 2),
            "\nValor óptimo de fertilizante (kg/ha):", round(x2, 2),
            "\nCosto final:", round(costos[length(costos)], 2))
    })
    
    # Mostrar la gráfica de la evolución del costo
    output$costoPlot <- renderPlot({
      plot(1:100, costos, type = "l", col = "blue", lwd = 2,
           xlab = "Iteraciones", ylab = "Costo",
           main = "Evolución del Costo durante el Descenso de Gradiente")
    })
    
    # Mostrar la tabla de iteraciones
    output$iteracionesTable <- renderTable({
      iteraciones
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
