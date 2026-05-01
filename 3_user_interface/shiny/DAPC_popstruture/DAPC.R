
# Increase the maximum upload size to 50MB (your potato dataset is ~18MB)
options(shiny.maxRequestSize = 50*1024^2)

library(shiny)
library(adegenet)
library(DT)

# Increase the maximum upload size to 50MB (your potato dataset is ~18MB)
options(shiny.maxRequestSize = 50*1024^2)

# --- UI ---
ui <- fluidPage(
  titlePanel("DAPC Population Structure Analysis (adegenet)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload Genotype CSV",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      helpText("Upload the 'new_potato_geno.csv' file. The app expects 'marker', 'chrom', and 'bp' as the first three columns."),
      
      tags$hr(),
      
      # DAPC / K-Means Parameters
      h4("Analysis Parameters"),
      numericInput("n_pca", "Number of PCs to retain (PCA phase):", 
                   value = 50, min = 1, max = 500, step = 1),
      helpText("Tip: Retaining too many PCs can lead to overfitting."),
      
      numericInput("n_clust", "Number of clusters (k) to form:", 
                   value = 3, min = 2, max = 20, step = 1),
      
      numericInput("n_da", "Number of Discriminant Functions to retain:", 
                   value = 2, min = 1, max = 10, step = 1),
      
      actionButton("run_dapc", "Run DAPC Analysis", 
                   class = "btn-primary", style = "width:100%; margin-top:10px;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("DAPC Scatter", 
                 br(),
                 plotOutput("dapcPlot", height = "600px")),
        
        tabPanel("Eigenvalue / Scree Plots", 
                 br(),
                 fluidRow(
                   column(6, plotOutput("pcaScree")),
                   column(6, plotOutput("daScree"))
                 )
        ),
        
        tabPanel("Cluster Assignments", 
                 br(),
                 # ADDED: Download Button
                 downloadButton("downloadClusters", "Download as CSV", class = "btn-success"),
                 br(), br(),
                 DTOutput("clusterTable"))
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # 1. Process the uploaded data
  dataset <- reactive({
    req(input$file1)
    
    # Read the uploaded file
    df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    
    # Extract marker names to use as column names later
    markers <- df$marker
    
    # Drop the first 3 columns (marker, chrom, bp) to isolate numerical genotypes
    geno_data <- df[, -c(1:3)]
    
    # adegenet expects individuals as ROWS and markers as COLUMNS, so we transpose
    t_geno <- t(geno_data)
    colnames(t_geno) <- markers
    
    # Convert the matrix back to numeric
    t_geno <- apply(t_geno, 2, as.numeric)
    rownames(t_geno) <- colnames(geno_data)
    
    # Impute missing values with column (marker) means
    for(i in 1:ncol(t_geno)) {
      if(any(is.na(t_geno[, i]))) {
        t_geno[is.na(t_geno[, i]), i] <- mean(t_geno[, i], na.rm = TRUE)
      }
    }
    
    return(t_geno)
  })
  
  # 2. Run find.clusters and dapc
  dapc_results <- eventReactive(input$run_dapc, {
    req(dataset())
    data <- dataset()
    
    withProgress(message = 'Running adegenet Analysis...', value = 0, {
      
      incProgress(0.2, detail = "Executing K-means (find.clusters)...")
      
      grp_obj <- find.clusters(data, 
                               max.n.clust = 20, 
                               n.pca = input$n_pca, 
                               n.clust = input$n_clust,
                               choose.n.clust = FALSE)
      
      incProgress(0.6, detail = "Computing DAPC...")
      
      dapc_model <- dapc(data, 
                         grp = grp_obj$grp, 
                         n.pca = input$n_pca, 
                         n.da = input$n_da)
      
      incProgress(0.9, detail = "Finalizing results...")
      
      list(model = dapc_model, clusters = grp_obj$grp)
    })
  })
  
  # 3. Output: Scatter Plot
  output$dapcPlot <- renderPlot({
    req(dapc_results())
    dapc_model <- dapc_results()$model
    
    scatter(dapc_model, 
            bg = "white", 
            pch = 20, 
            cstar = 0, 
            col = adegenet::funky(length(levels(dapc_model$grp))), 
            solid = 0.8, 
            cex = 2, 
            clab = 0, 
            leg = TRUE, 
            txt.leg = paste("Cluster", levels(dapc_model$grp)),
            posi.leg = "topright")
  })
  
  # 4. Output: PCA Scree Plot
  output$pcaScree <- renderPlot({
    req(dapc_results())
    model <- dapc_results()$model
    
    barplot(model$pca.eig, 
            main = "PCA Eigenvalues", 
            ylab = "Variance Explained", 
            col = "steelblue", 
            border = NA)
    
    abline(v = input$n_pca, col = "red", lty = 2, lwd = 2)
    legend("topright", legend = paste("Retained PCs:", input$n_pca), 
           col = "red", lty = 2, lwd = 2)
  })
  
  # 5. Output: DA Scree Plot
  output$daScree <- renderPlot({
    req(dapc_results())
    model <- dapc_results()$model
    
    barplot(model$eig, 
            main = "Discriminant Analysis Eigenvalues", 
            ylab = "Discriminant Power", 
            col = "darkgreen", 
            border = NA)
  })
  
  # ADDED: Create a reactive dataframe for the clusters so both the Table and Download can use it
  cluster_df <- reactive({
    req(dapc_results())
    clusters <- dapc_results()$clusters
    data.frame(Sample = names(clusters), 
               Assigned_Cluster = as.character(clusters))
  })
  
  # 6. Output: Cluster Assignation Table
  output$clusterTable <- renderDT({
    datatable(cluster_df(), 
              options = list(pageLength = 15, searchHighlight = TRUE), 
              rownames = FALSE)
  })
  
  # ADDED: Output: Download Handler
  output$downloadClusters <- downloadHandler(
    filename = function() {
      paste("dapc_cluster_assignments_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(cluster_df(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)


# Notes #####################################################
#############################################################
# Part 1: The Method — Discriminant Analysis of Principal Components (DAPC)DAPC,
# implemented in the adegenet R package, is a multivariate statistical approach
# designed to identify and describe genetic clusters (population structure) within 
# a dataset.It is particularly powerful for genomic data (like your potato SNPs) 
# because it combines the strengths of two different statistical tools:Principal
# Component Analysis (PCA) - Data Reduction:The Problem: Genomic datasets usually 
# have vastly more markers (columns) than individual samples (rows), which breaks
# traditional discriminant analysis. Furthermore, genetic markers are often 
# correlated (Linkage Disequilibrium).

# The Solution: DAPC first runs a PCA to 
# transform the raw genotype data into a smaller set of uncorrelated variables
# (Principal Components). This reduces the complexity of the data while retaining 
# the most important genetic variation.K-Means Clustering - Identifying Groups:
# Using the retained Principal Components, the algorithm runs k-means clustering
# (find.clusters) to identify groups of genetically similar individuals. 
# It evaluates different numbers of clusters ($K$) to find the best fit.
# Discriminant Analysis (DA) - Maximizing Separation:Finally, DA is applied to 
# the Principal Components using the clusters identified in Step 2.

# Discriminant Analysis works to maximize the variance between the different 
# clusters while minimizing the variance within them, resulting in clear, visual 
# separation of the potato subpopulations.Why use DAPC? Unlike other popular software
# (like STRUCTURE), DAPC does not assume that populations are in Hardy-Weinberg equilibrium 
# or linkage equilibrium, making it highly robust for complex crops like potatoes, 
# which are polyploid and clonally propagated.

####################################################################
# Part 2: App Instructions
# Here is how to operate the Shiny app you just built.
# PreparationEnsure your new_potato_geno.csv file is downloaded to your computer.

# Run the app.R script in RStudio. A web browser window or RStudio viewer panel will open.

# Step 1: Uploading the DataClick the "Browse..." button under "Upload Genotype CSV".
# Select your new_potato_geno.csv file.Note on the backend: The app will automatically
# clean your data by dropping the marker, chrom, and bp columns, transposing the matrix
# so individuals are rows, and imputing any missing genotype calls using the mean 
# allele frequency.

# Step 2: Setting Analysis ParametersBefore running the analysis, you must define
# three parameters on the left sidebar:Number of PCs to retain (PCA phase): 
# This dictates how much genetic variation is passed to the Discriminant Analysis.
# Rule of thumb: Retaining too many PCs leads to "overfitting" (the model memorizes 
# the data rather than finding true patterns). Retaining too few means you lose 
# important genetic info. The default is set to 50, but you can adjust this based
# on the PCA Scree plot (try to capture the steepest drop-off in eigenvalues).

# Number of clusters (k) to form: This forces the k-means algorithm to group your 
#potatoes into exactly $K$ genetic populations. You can change this to test 
# different hypotheses (e.g., $K=3$ vs $K=5$).

# Number of Discriminant Functions (DA):
# This dictates the dimensions of the final plot. It cannot be greater than the 
# number of clusters minus one ($K - 1$). If you choose 3 clusters, you should 
# retain 2 discriminant functions.

# Step 3: Running the AnalysisClick the blue "Run DAPC Analysis" button.
# A progress bar will appear at the bottom right. Because your dataset is ~18MB,
# this may take 10–30 seconds as the server calculates the k-means and discriminant functions.

# Step 4: Interpreting the OutputsNavigate through the tabs at the top of the main panel
# to view your results:
# Tab 1: DAPC Scatter: This is your primary result. 
# It displays a 2D plot where each point is a potato sample, and colors/ellipses 
# represent the genetic clusters. Points closer together are genetically similar.

# Tab 2: Eigenvalue / Scree Plots: * The blue bar chart (PCA) shows how much 
# variance is captured by each Principal Component. The red dotted line shows your 
# cutoff.The green bar chart (DA) shows the "discriminant power" of the functions 
# separating your clusters.

# Tab 3: Cluster Assignments: This tab displays a 
# searchable table showing exactly which cluster ($1, 2, 3$, etc.) each potato 
# variety was assigned to.Downloading: In Tab 3, click the green "Download as CSV"
# button to save the cluster assignments to your computer for use in publications 
# or downstream analysis.
