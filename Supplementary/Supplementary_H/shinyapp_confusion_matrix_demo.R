library(shiny)
library(tidyverse)
library(broom)

# Salespeople example (from https://peopleanalytics-regression-book.org)
salespeople_full <- read_csv("salespeople.csv") 
set.seed(130)
salespeople <- salespeople_full |> slice_sample(n=300) # Sample of 300 observations
salespeople_new <- salespeople_full |> anti_join(salespeople)



# Fit model
model1 <- glm(promoted ~ sales, data = salespeople, family = "binomial")
predictions <- augment(model1, type.predict = "response", newdata=salespeople_new)

# Function to find sales cutoff for a given threshold
find_cutoff_sales <- function(model, threshold, sales_range) {
  sales_grid <- seq(min(sales_range), max(sales_range), length.out = 1000)
  pred_grid <- predict(model, 
                       newdata = data.frame(sales = sales_grid), 
                       type = "response")
  idx <- which.min(abs(pred_grid - threshold))
  sales_grid[idx]
}

ui <- fluidPage(
  titlePanel("Logistic Regression Threshold Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("threshold",
                  "Classification Threshold (c):",
                  min = 0.01,
                  max = 0.99,
                  value = 0.5,
                  step = 0.01,
                  animate = animationOptions(interval = 100, loop = TRUE)),
      
      hr(),
      
      h4("Decision Rule"),
      textOutput("decision_rule"),
      
      hr(),
      
      h4("Confusion Matrix"),
      htmlOutput("confusion_matrix"),
      
      h4("Metrics"),
      verbatimTextOutput("metrics")
    ),
    
    mainPanel(
      plotOutput("threshold_plot", height = "600px")
    )
  )
)

server <- function(input, output) {
  
  # Salespeople example (from https://peopleanalytics-regression-book.org)
  salespeople_full <- read_csv("salespeople.csv") 
  set.seed(130)
  salespeople <- salespeople_full |> slice_sample(n=300) # Sample of 300 observations
  salespeople_new <- salespeople_full |> anti_join(salespeople)
  
  
  
  # Calculate cutoff sales value
  cutoff_sales <- reactive({
    find_cutoff_sales(model1, input$threshold, predictions$sales)
  })
  
  # Reactive data based on threshold
  classified_data <- reactive({
    cutoff <- cutoff_sales()
    predictions |>
      mutate(
        predicted_class = ifelse(sales >= cutoff, 1, 0),
        fill_color = case_when(
          promoted == 1 & predicted_class == 1 ~ "True Positive",
          promoted == 0 & predicted_class == 0 ~ "True Negative",
          promoted == 1 & predicted_class == 0 ~ "False Negative",
          promoted == 0 & predicted_class == 1 ~ "False Positive"
        )
      )
  })
  
  # Main plot
  output$threshold_plot <- renderPlot({
    data <- classified_data()
    cutoff <- cutoff_sales()
    
    # Calculate label positions
    x_range <- range(predictions$sales)
    x_red <- mean(c(x_range[1], cutoff))  # midpoint of red region
    x_green <- mean(c(cutoff, x_range[2]))  # midpoint of green region
    y_label <- 0.5  # middle of y-axis
    
    ggplot(data, aes(x = sales, y = .fitted)) +
      # Red region (left - predict no promotion)
      annotate("rect", 
               xmin = -Inf, xmax = cutoff, 
               ymin = -Inf, ymax = Inf,
               fill = "lightcoral", alpha = 0.3) +
      # Green region (right - predict promotion)
      annotate("rect", 
               xmin = cutoff, xmax = Inf, 
               ymin = -Inf, ymax = Inf,
               fill = "lightgreen", alpha = 0.3) +
      # Labels on the regions
      annotate("text", x = x_red, y = y_label, 
               label = "Predict No\nPromotion", 
               size = 6, fontface = "bold", color = "darkred") +
      annotate("text", x = x_green, y = y_label, 
               label = "Predict\nPromotion", 
               size = 6, fontface = "bold", color = "darkgreen") +
      # Vertical cutoff line
      geom_vline(xintercept = cutoff, 
                 linetype = "dashed", color = "black", size = 1.5) +
      # Horizontal threshold line
      geom_hline(yintercept = input$threshold, 
                 linetype = "dashed", color = "black", size = 1.5) +
      # Intersection point
      geom_point(x = cutoff, y = input$threshold,
                 color = "red", size = 5, shape = 4, stroke = 2) +
      # S-curve
      geom_line(color = "blue", size = 1.5) +
      # TRUE VALUES: Points at y=0 or y=1 based on actual promotion status
      geom_point(aes(x = sales, y = promoted, color = fill_color), 
                 size = 4, alpha = 0.8) +
      scale_color_manual(
        name = "Classification",
        values = c(
          "True Positive" = "darkgreen",
          "True Negative" = "darkred",
          "False Positive" = "orange",
          "False Negative" = "purple"
        )
      ) +
      labs(
        title = paste("Threshold c =", round(input$threshold, 3), 
                      "→ Sales cutoff =", round(cutoff, 1)),
        x = "Sales",
        y = "Probability of Promotion"
      ) +
      scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = c("0\n(Not Promoted)", "0.25", "0.5", "0.75", "1\n(Promoted)")) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  # Decision rule text
  output$decision_rule <- renderText({
    cutoff <- cutoff_sales()
    paste0("If Sales < ", round(cutoff, 1), " → Predict No Promotion (0)\n",
           "If Sales ≥ ", round(cutoff, 1), " → Predict Promotion (1)")
  })
  
  # Confusion matrix as HTML table
  output$confusion_matrix <- renderUI({
    data <- classified_data()
    
    # Calculate counts
    TP <- sum(data$promoted == 1 & data$predicted_class == 1)
    FN <- sum(data$promoted == 1 & data$predicted_class == 0)
    FP <- sum(data$promoted == 0 & data$predicted_class == 1)
    TN <- sum(data$promoted == 0 & data$predicted_class == 0)
    
    # Create HTML table
    HTML(paste0(
      '<table border="1" cellpadding="5" style="border-collapse: collapse; text-align: center;">
        <tr>
          <th></th>
          <th colspan="2">Actual</th>
        </tr>
        <tr>
          <th>Predicted</th>
          <th>1 (Promoted)</th>
          <th>0 (Not Promoted)</th>
        </tr>
        <tr>
          <td><strong>1 (Promoted)</strong></td>
          <td style="background-color: lightgreen;"><strong>', TP, '</strong><br>(True Positive)</td>
          <td style="background-color: lightyellow;"><strong>', FP, '</strong><br>(False Positive)</td>
        </tr>
        <tr>
          <td><strong>0 (Not Promoted)</strong></td>
          <td style="background-color: lightyellow;"><strong>', FN, '</strong><br>(False Negative)</td>
          <td style="background-color: lightcoral;"><strong>', TN, '</strong><br>(True Negative)</td>
        </tr>
      </table>'
    ))
  })
  
  # Metrics
  output$metrics <- renderText({
    data <- classified_data()
    
    TP <- sum(data$promoted == 1 & data$predicted_class == 1)
    FN <- sum(data$promoted == 1 & data$predicted_class == 0)
    FP <- sum(data$promoted == 0 & data$predicted_class == 1)
    TN <- sum(data$promoted == 0 & data$predicted_class == 0)
    
    accuracy <- (TP + TN) / nrow(data)
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    
    paste(
      sprintf("Accuracy:    %.3f\n", accuracy),
      sprintf("Sensitivity: %.3f (TPR)\n", sensitivity),
      sprintf("Specificity: %.3f (TNR)\n", specificity),
      sprintf("\nTP: %d  FP: %d\n", TP, FP),
      sprintf("FN: %d  TN: %d", FN, TN)
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)