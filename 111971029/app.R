library(shiny)
library(ggplot2)
library(ggbiplot)
library(FactoMineR)
library(factoextra)

ui <- fluidPage(
    navbarPage("楊德倫 政大資訊科學系 111971029",
        tabPanel("iris data",
             tabsetPanel(
                 tabPanel("Plot",
                      fluidRow(
                          column(4,
                                 checkboxGroupInput("iris_variables", "請選擇欄位變數:",
                                                    choices = colnames(iris[, 1:4]),
                                                    selected = c("Sepal.Length", "Sepal.Width"))
                          ),
                          column(8,
                                 plotOutput("irisPlot")
                          )
                      )
                 ),
                tabPanel("Summary", verbatimTextOutput("irisSummary")), 
                tabPanel("Table", tableOutput("irisTable"))
            )       
        ),
        tabPanel("PCA",
            tabsetPanel(
                tabPanel("Plot",
                    sidebarLayout(
                         sidebarPanel(
                             checkboxGroupInput("PCs", "請選擇主成分(選擇2個)：", 
                                                choices = c("PC1", "PC2", "PC3", "PC4"),
                                                selected = c("PC1", "PC2")
                                                )
                         ),
                         mainPanel(
                             plotOutput("pcaPlot")
                         )
                    )
                ),
                tabPanel("Summary", verbatimTextOutput("pcaSummary")),
                tabPanel("Table", tableOutput("pcaTable"))
                
            )
        ),
        tabPanel("CA",
             tabsetPanel(
                 tabPanel("Plot",
                          sidebarLayout(
                              sidebarPanel(
                                  checkboxGroupInput("caDims", "請選擇維度：", 
                                                     choices = c("Dim1", "Dim2", "Dim3"),
                                                     selected = c("Dim1", "Dim2"))
                              ),
                              mainPanel(
                                  plotOutput("caPlot")
                              )
                          )
                 ),
                 tabPanel("Summary", verbatimTextOutput("caSummary")),
                 tabPanel("Table", tableOutput("caTable"))
             )
        )
        
    )
    
    
    
)


server <- function(input, output) {
    # iris data -> Plot
    output$irisPlot <- renderPlot({
        req(input$iris_variables)
        if (length(input$iris_variables) < 2) {
            return(NULL)
        }
        
        data(iris)
        x_var <- input$iris_variables[1]
        y_var <- input$iris_variables[2]
        
        ggplot(iris, aes_string(x = x_var, y = y_var, color = "Species")) +
            geom_point() +
            labs(title = paste("Iris Dataset", x_var, "vs", y_var), x = x_var, y = y_var) +
            theme_minimal()
    })
    
    # iris data -> Summary
    output$irisSummary <- renderPrint({
        data(iris)
        summary(iris)
    })
    
    # iris data -> Table
    output$irisTable <- renderTable({
        data(iris)
        iris
    })
    
    
    
    
    
    # pca
    iris.pca <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)
    
    # pca -> Plot
    output$pcaPlot <- renderPlot({
        req(input$PCs)
        if (length(input$PCs) < 2) {
            return(NULL)
        }
        
        selected_PCs <- as.integer(gsub("PC", "", input$PCs))
        ggbiplot(iris.pca, choices = selected_PCs, obs.scale = 1, var.scale = 1,
                 groups = iris$Species, ellipse = TRUE, circle = TRUE) +
            scale_color_discrete(name = '') +
            theme_minimal()
    })
    
    # pca -> Summary
    output$pcaSummary <- renderPrint({
        summary(iris.pca)
    })
    
    # pca -> Table
    output$pcaTable <- renderTable({
        pca_scores <- data.frame(iris.pca$x)
        colnames(pca_scores) <- paste("PC", 1:ncol(pca_scores), sep = "")
        pca_scores <- cbind(pca_scores, Species = iris$Species)
        pca_scores
    })
    
    
    
    
    
    # ca
    iris_ca <- CA(iris[, -5])
    
    # ca -> Plot
    output$caPlot <- renderPlot({
        req(input$caDims)
        if (length(input$caDims) < 2) {
            return(NULL)
        }
        
        selected_dims <- as.integer(gsub("Dim", "", input$caDims))
        fviz_ca_biplot(iris_ca, axes = selected_dims, repel = TRUE, col.row = iris$Species,
                       ggtheme = theme_minimal())
    })
    
    # ca -> Summary
    output$caSummary <- renderPrint({
        summary(iris_ca)
    })
    
    # ca -> Table
    output$caTable <- renderTable({
        ca_scores <- data.frame(iris_ca$row$coord)
        colnames(ca_scores) <- paste("Dim", 1:ncol(ca_scores), sep = "")
        ca_scores <- cbind(ca_scores, Species = iris$Species)
        ca_scores
    })
}
shinyApp(ui = ui, server = server)