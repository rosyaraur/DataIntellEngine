install.packages(c("shiny", "tidyverse", "ggrepel"))
library(shiny)
library(tidyverse)
library(ggrepel)

# --- UI Logic ---
ui <- fluidPage(
  titlePanel("GGE Biplot Interactive Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This app performs SVD on environment-centered data (G+GE)."),
      
      # Data Input
      radioButtons("data_source", "Data Source:",
                   choices = c("example" = "example", "Upload CSV" = "upload")),
      
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file1", "Choose CSV File", accept = ".csv"),
        textInput("gen_col", "Genotype Column Name:", value = "Names")
      ),
      
      hr(),
      
      # Scaling Selection
      selectInput("scaling", "Biplot Scaling Method:",
                  choices = list("Genotype-focused (f=1)" = 1,
                                 "Environment-focused (f=0)" = 0,
                                 "Symmetrical (f=0.5)" = 0.5),
                  selected = 0.5),
      
      helpText("f=1: Best for comparing genotypes."),
      helpText("f=0: Best for comparing environments."),
      
      hr(),
      downloadButton("downloadPlot", "Download Biplot")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Biplot", plotOutput("ggePlot", height = "600px")),
        tabPanel("Data Preview", tableOutput("dataPreview")),
        tabPanel("Singular Values", verbatimTextOutput("svdSummary"))
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output) {
  
  # 1. Reactive Data Loading
  rawData <- reactive({
    # FIXED: Changed input.data_source to input$data_source
    if (input$data_source == "example") {
      # Replicating your Wheat Trials data
      tribble(
        ~Names, ~BH93, ~EA93, ~HW93, ~ID93, ~KE93, ~NN93, ~OA93, ~RN93, ~WP93,
        "Ann", 4.460, 4.150, 2.849, 3.084, 5.940, 4.450, 4.351, 4.039, 2.672,
        "Ari", 4.417, 4.771, 2.912, 3.506, 5.699, 5.152, 4.956, 4.386, 2.938,
        "Aug", 4.669, 4.578, 3.098, 3.460, 6.070, 5.025, 4.730, 3.900, 2.621,
        "Cas", 4.732, 4.745, 3.375, 3.904, 6.224, 5.340, 4.226, 4.893, 3.451,
        "Del", 4.390, 4.603, 3.511, 3.848, 5.773, 5.421, 5.147, 4.098, 2.832
      )
    } else {
      req(input$file1)
      read.csv(input$file1$datapath)
    }
  })
  
  # 2. GGE Processing (The Core Logic)
  ggeResults <- reactive({
    df <- rawData()
    gen_col_name <- if(input$data_source == "example") "Names" else input$gen_col
    
    # Separate Labels and Matrix
    genotypes <- df[[gen_col_name]]
    Y_raw <- as.matrix(df[, sapply(df, is.numeric)])
    env_names <- colnames(Y_raw)
    
    # Environment Centering (Subtract column means)
    # This leaves G + GE in the matrix
    Y_centered <- scale(Y_raw, center = TRUE, scale = FALSE)
    
    # Singular Value Decomposition
    svd_res <- svd(Y_centered)
    U <- svd_res$u[, 1:2]
    V <- svd_res$v[, 1:2]
    L <- diag(svd_res$d[1:2])
    
    f <- as.numeric(input$scaling)
    
    # Apply Scaling
    G_coords <- U %*% (L^f)
    E_coords <- V %*% (L^(1-f))
    
    list(
      gen = data.frame(ID = genotypes, PC1 = G_coords[,1], PC2 = G_coords[,2], type = "Genotype"),
      env = data.frame(ID = env_names, PC1 = E_coords[,1], PC2 = E_coords[,2], type = "Environment"),
      d = svd_res$d
    )
  })
  
  # 3. Plotting
  output$ggePlot <- renderPlot({
    res <- ggeResults()
    
    ggplot() +
      # Origin Lines
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
      # Environments (Vectors)
      geom_segment(data = res$env, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
                   arrow = arrow(length = unit(0.2, "cm")), color = "blue", alpha = 0.6) +
      geom_text_repel(data = res$env, aes(x = PC1, y = PC2, label = ID), color = "blue", fontface = "bold") +
      # Genotypes (Points)
      geom_point(data = res$gen, aes(x = PC1, y = PC2), color = "red", size = 3) +
      geom_text_repel(data = res$gen, aes(x = PC1, y = PC2, label = ID), color = "red") +
      # Formatting
      theme_minimal() +
      labs(title = paste("GGE Biplot - Scaling f =", input$scaling),
           subtitle = "Environment Centered (G+GE)",
           x = "PC1", y = "PC2") +
      coord_fixed() # Vital for biplots to maintain geometry
  })
  
  output$dataPreview <- renderTable({ rawData() })
  
  output$svdSummary <- renderPrint({
    d <- ggeResults()$d
    prop <- (d^2) / sum(d^2) * 100
    cat("Singular Values:\n", d, "\n\n")
    cat("Percentage Explained by PC1 & PC2:\n", round(prop[1]+prop[2], 2), "%")
  })
}

shinyApp(ui = ui, server = server)