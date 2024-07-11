# Load necessary library
library(shiny)   # for the GUI
library(dplyr)   # for group_by function & distinct function
library(cluster)   # for Kmean cluster
library(arules)  # for association rules
library(readxl)  # for read the csv file
library(ggplot2)  # for graphs of the cluster and the association rules
library(arulesViz)  # for plot of the association rules

ui <- fluidPage(titlePanel("Data Analysis Dashboard"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput(
                      "file1",
                      "Choose CSV File",
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                    ),
                    numericInput(
                      "num_clusters",
                      "Number of Clusters",
                      value = 2,
                      min = 2,
                      max = 4
                    ),
                    numericInput(
                      "min_support",
                      "Minimum Support",
                      value = 0.01,
                      min = .001,
                      max = 1,
                      step = 0.01
                    ),
                    numericInput(
                      "min_confidence",
                      "Minimum Confidence",
                      value = 0.01,
                      min = .001,
                      max = 1,
                      step = 0.01
                    ),
                    actionButton("run_analysis", "Run Analysis")
                  ),
                  mainPanel(tabsetPanel(
                    tabPanel(
                      "Cleaning",
                      conditionalPanel(
                        condition = "input.run_analysis > 0",
                        textOutput("descriptionTotal"),
                        plotOutput("boxplotForTotal"),
                        textOutput("descriptionCount"),
                        plotOutput("boxplotForCount"),
                        textOutput("descriptionRnd"),
                        plotOutput("boxplotForRnd"),
                        textOutput("descriptionAge"),
                        plotOutput("boxplotForAge"),
                        verbatimTextOutput("na"),
                        verbatimTextOutput("dataStructureCount"),
                        verbatimTextOutput("dataStructureTotal"),
                        verbatimTextOutput("dataStructureRnd"),
                        verbatimTextOutput("dataStructureAge"),
                        verbatimTextOutput("dataStructureItem"),
                        verbatimTextOutput("dataStructurePaymentType"),
                        verbatimTextOutput("dataStructureCity"),
                        verbatimTextOutput("dataStructureCustomer"),
                        verbatimTextOutput("duplicateBeforeOmit"),
                        verbatimTextOutput("duplicateaAfterOmit")
                      )
                    ),
                    tabPanel("Dashboard", plotOutput("dashboard")),
                    tabPanel(
                      "Cluster Analysis",
                      plotOutput("cluster_plot"),
                      tableOutput("cluster_summary")
                    ),
                    tabPanel(
                      "Association Rules",
                      plotOutput("association_plot"),
                      tableOutput("association_rules")
                    )
                  ))
                ))

server <- function(input, output) {
  data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  # ============================================== #
  
  # make intail value to this variable to not give me an error
  apriori_rules <- reactiveVal(NULL)
  customer_summary <- reactiveVal(NULL)
  clustered_customers <- reactiveVal(NULL)
  
  # ============================================== #
  
  observeEvent(input$run_analysis, {
    # ============================================== #
    
    # Check conditions:
    
    if (input$num_clusters < 2 || input$num_clusters > 4) {
      showModal(
        modalDialog(
          title = "Error",
          "Number of clusters should be between 2 and 4.",
          easyClose = TRUE
        )
      )
      return()
    }
    
    # ============================================== #
    
    if (input$min_support < 0.001 || input$min_support > 1) {
      showModal(
        modalDialog(
          title = "Error",
          "Minimum support threshold should be between 0.001 and 1.",
          easyClose = TRUE
        )
      )
      return()
    }
    
    # ============================================== #
    
    if (input$min_confidence < 0.001 || input$min_confidence > 1) {
      showModal(
        modalDialog(
          title = "Error",
          "Minimum confidence threshold should be between 0.001 and 1.",
          easyClose = TRUE
        )
      )
      return()
    }
    
    # ============================================== #
    
    # Data Cleaning
    dataset <- data()
    dataset1 <- unique(dataset)  # for duplicated
    
    # ============================================== #
    
    # group the data by the customer for perform the Kmean:
    customer_transactions <- dataset1 %>%
      group_by(customer) %>%
      summarise(total_spent = sum(total),
                ageOfCustomer = mean(age))
    
    # Perform K-means clustering on aggregated data
    kmeans_result <-
      kmeans(customer_transactions[, c("total_spent", "ageOfCustomer")], centers = input$num_clusters)
    
    # Add cluster labels to the aggregated dataset
    clustered_customers_data <-
      cbind(customer_transactions, Cluster = kmeans_result$cluster)
    
    # Group by customer name for table
    customer_summary_data <- clustered_customers_data %>%
      group_by(customer) %>%
      summarise(
        Total = mean(total_spent),
        Age = mean(ageOfCustomer),
        cluster = Cluster
      )
    
    # ============================================== #
    
    # Convert the dataset to transaction format
    tdata <- strsplit(dataset1$items, ",")
    
    # to turn the items to transactions
    tdata <- as(tdata, 'transactions')
    
    # ============================================== #
    
    # Use built-in apriori function
    apriori_rules_data <- apriori(
      tdata,
      parameter = list(
        support = input$min_support,
        confidence = input$min_confidence,
        minlen = 2
      )
    )
    
    # ============================================== #
    
    apriori_rules(apriori_rules_data)
    customer_summary(customer_summary_data)
    clustered_customers(clustered_customers_data)
    
    # ============================================== #
    
    # box plot check for outlier:
    output$descriptionTotal <- renderText({
      req(input$file1)
      "Box plot for Total"
    })
    
    output$boxplotForTotal <- renderPlot({
      req(input$file1)
      boxplot(dataset1$total)
    })
    
    output$descriptionCount <- renderText({
      req(input$file1)
      "Box plot for Count"
    })
    
    output$boxplotForCount <- renderPlot({
      req(input$file1)
      boxplot(dataset1$count)   # there is a outliers here
    })
    
    output$descriptionRnd <- renderText({
      req(input$file1)
      "Box plot for Rnd"
    })
    
    output$boxplotForRnd <- renderPlot({
      req(input$file1)
      boxplot(dataset1$rnd)
    })
    
    output$descriptionAge <- renderText({
      req(input$file1)
      "Box plot for Age"
    })
    
    output$boxplotForAge <- renderPlot({
      req(input$file1)
      boxplot(dataset1$age)
    })
    
    # ============================================== #
    # print na
    output$na <- renderText({
      req(input$file1)
      total_na <- sum(is.na(dataset1))
      paste("Total NA values in the data:", total_na)
    })
    
    # ============================================== #
    
    # print Data structure:
    
    output$dataStructureCount <- renderText({
      req(input$file1)
      strCount <-  is.integer(dataset1$count)
      paste("Structure of Count is: ", strCount)
    })
    
    # ============================================== #
    
    output$dataStructureTotal <- renderText({
      req(input$file1)
      strTotal <- is.integer(dataset1$total)
      paste("Structure of Total is: ", strTotal)
    })
    
    # ============================================== #
    
    output$dataStructureRnd <- renderText({
      req(input$file1)
      strRnd <- is.integer(dataset1$rnd)
      paste("Structure of Rnd is: ", strRnd)
    })
    
    # ============================================== #
    
    output$dataStructureAge <- renderText({
      req(input$file1)
      strAge <- is.integer(dataset1$age)
      paste("Structure of Age is: ", strAge)
    })
    
    # ============================================== #
    
    output$dataStructureItems <- renderText({
      req(input$file1)
      strItems <- is.character(dataset1$items)
      paste("Structure of Items is: ", strItems)
    })
    
    # ============================================== #
    
    output$dataStructurePaymentType <- renderText({
      req(input$file1)
      strPaymentType <- is.character(dataset1$paymentType)
      paste("Structure of Payment type is: ", strPaymentType)
    })
    
    # ============================================== #
    
    output$dataStructureCity <- renderText({
      req(input$file1)
      strCity <- is.character(dataset1$city)
      paste("Structure of City is: ", strCity)
    })
    
    # ============================================== #
    
    output$dataStructureCustomer <- renderText({
      req(input$file1)
      strCustomer <- is.character(dataset1$customer)
      paste("Structure of Customer is: ", strCustomer)
    })
    
    # ============================================== #
    
    # duplicate check:
    output$duplicateBeforeOmit <- renderText({
      req(input$file1)
      sumOFDuplicate <- sum(duplicated(dataset))
      paste("The number Of duplicate before omit it: ", sumOFDuplicate)
    })
    
    # ============================================== #
    
    output$duplicateaAfterOmit <- renderText({
      req(input$file1)
      sumOFDuplicate <- sum(duplicated(dataset1))
      paste("The number Of duplicate after omit it: ", sumOFDuplicate)
    })
    
    # ============================================== #
    
  })   # the end of the run analysis button
  
  # ============================================== #
  
  output$dashboard <- renderPlot({
    # Data Visualization
    dataset1 <- data()
    
    par(mfrow = c(2, 2))   # to make the dashboard
    
    # ============================================== #
    
    # Pie chart
    x <- table(dataset1$paymentType)
    percentage <- paste(round(100 * x / sum(x)), "%")
    pie(
      x,
      labels = percentage,
      main = "Compare cash and credit totals",
      col = c("yellow", "green")
    )
    legend(
      "bottomright",
      legend = c("Cash", "Credit"),
      fill = c("yellow", "green")
    )
    
    # ============================================== #
    
    # scatter by age
    sorted_data_by_age <- dataset1 %>% group_by(age) %>%
      summarise(age = age,
                total = sum(total),
                .groups = 'drop') %>%
      distinct()
    
    plot(
      y = sorted_data_by_age$total,
      x = sorted_data_by_age$age,
      col = "black",
      main = "Total Spending by Age",
      xlab = "age",
      ylab = "Total Spending"
    )
    
    
    # ============================================== #
    
    # Barplot by city
    sorted_data_by_city <- dataset1 %>% group_by(city) %>%
      summarise(city = city,
                total = sum(total),
                .groups = 'drop') %>%
      distinct() %>%
      arrange(desc(total)) # total descending
    
    barplot(
      height = sorted_data_by_city$total,
      # for Y
      name = sorted_data_by_city$city,
      # for X
      col = "skyblue",
      main = "Total Spending by City",
      xlab = "Cities",
      ylab = "Total spending",
      cex.names = 0.59   # the size of the X
    )
    
    # ============================================== #
    
    # Boxplot for total spending
    boxplot(dataset1$total,
            xlab = "Total spending",
            ylab = "Spending",
            main = "Distribution of total spending")
    
    # ============================================== #
    
    par(mfrow = c(1, 1)) # Reset layout
  })
  
  # ============================================== #
  
  # the plot of the cluster:
  output$cluster_plot <- renderPlot({
    ggplot(clustered_customers(),
           aes(
             x = total_spent,
             y = ageOfCustomer,
             color = factor(Cluster)
           )) +
      geom_point() +
      labs(x = "Total Spent", y = "Age of Customer", color = "Cluster") +
      ggtitle("Cluster Analysis") +
      theme_minimal()
  })
  
  # ============================================== #
  
  # the table of the cluster:
  output$cluster_summary <- renderTable({
    customer_summary()
  })
  
  # ============================================== #
  
  # the plot of the association:
  output$association_plot <- renderPlot({
    plot(apriori_rules(),
         method = "graph",
         control = list(type = "items"))
  })
  
  # ============================================== #
  
  # the table of the association:
  output$association_rules <- renderTable({
    inspect(head(
      sort(apriori_rules(), by = "confidence", decreasing = TRUE),
      n = 10
    ))
  })
  
  # ============================================== #
}

shinyApp(ui, server)
