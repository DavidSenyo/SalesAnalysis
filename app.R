
######################################################################################################################
# This a shiny dashboard report with a simulated marketing data on sale with ads units and products
# 
# Again, this is for showcasing what I can do with shiny to be showcased to employers/recruiters

######################################################################################################################

# Load required packages
library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(plotly)
library(bslib)
library(shinythemes)


########################
# SIMULATED DATA
#######################
set.seed(2025)

n <- 1000
dates <- seq(as.Date("2023-01-01"), as.Date("2024-12-31"), length.out = n)

sim_data <- tibble(
  transaction_id = 1:n,
  date = sample(dates, n, replace = TRUE),
  region = sample(c("North", "South", "East", "West"), n, replace = TRUE, prob = c(0.3, 0.25, 0.25, 0.2)),
  product = sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  units = rpois(n, lambda = 5) + 1,
  price = round(runif(n, 10, 100), 1),
  ad_spend = round(runif(n, 50, 500), 0),
  customer_segment = sample(c("Premium", "Regular", "Budget", NA), n, replace = TRUE, prob = c(0.4, 0.4, 0.15, 0.05))
) %>%
  mutate(
    sales = units * price,
    revenue = sales + ad_spend * 0.2 + rnorm(n, 0, 50)
  ) %>%
  select(-sales) %>%
  rename(sales = revenue)

sim_data$price[sample(1:n, 20)] <- NA

#####################
### UI
#####################
ui <- page_navbar(
  title = "SALES ANALYSIS",
  theme = bs_theme(bootswatch = "flatly"),
  
  
  nav_panel(
    "Trends by Regions",
    sidebarLayout(
                sidebarPanel(
                            selectInput("cohort_var", 
                                        "Cohort (Region)", 
                                        choices = c("All", unique(sim_data$region))
                                        ),
                dateRangeInput("date_range", 
                                 "Date Range",
                                 start = min(sim_data$date), 
                                 end = max(sim_data$date)
                                 ),
                sliderInput("n_months", 
                              "Number of recent months to show",
                              min = 3, 
                              max = 12, 
                              value = 6, 
                              step = 1
                            )
                            ),
      mainPanel(h4("Sale Trend"),
                plotlyOutput("trend_plot"),
                br(),
                h4("Monthly Average Sales by Region (Last N Months)"),
                DTOutput("cohort_table"), br(), hr(), hr()
                )
                 )
       ),
  
  nav_panel(
            "Regression Analysis",
            sidebarLayout(
                          sidebarPanel(
                                       selectInput("target", 
                                                   "Target Variable", 
                                                   choices = c("sales", "units")
                                                   ),
                                       checkboxGroupInput("predictors", 
                                                          "Predictors",
                                                          choices = c("price", "ad_spend", "units", "product", "region"),
                                                          selected = c("price", "ad_spend", "units")
                                                          ),
                                       actionButton("run_reg", "Run Regression", class = "btn-primary")
                                       ),
            mainPanel(
                      h4("Model Summary"),
                      verbatimTextOutput("reg_summary"), hr(),
                      h4("Residual Plot"),
                      plotOutput("reg_residuals"), br(), hr(), hr()
                     )
                         )
             ),
  
  nav_panel(
    "Sensitivity Analysis",
    sidebarLayout(
                  sidebarPanel(
        sliderInput("price_sim", 
                    "Price ($)", 
                    min = 10, 
                    max = 100, 
                    value = 50, 
                    step = 5
                    ),
        sliderInput("ad_spend_sim", 
                    "Ad Spend ($)", 
                    min = 50, 
                    max = 500, 
                    value = 200, 
                    step = 10
                    ),
        numericInput("units_sim",
                     "Units Sold", 
                     value = 5, 
                     min = 1, 
                     max = 20
                     ),
        hr(),
        helpText("Using a linear model fitted on the full data (sales ~ price + ad_spend + units).")
      ),
      mainPanel(
                h3("Predicted Sales"),
                h4(textOutput("pred_sales")),
                br(),
                plotlyOutput("sensitivity_plot"), br(), hr(), hr()
               )
                  )
          )
  
  # nav_panel(
  #          "Data Quality",
  #         fluidRow(
  #                 column(6, DTOutput("missing_table")),
  #                 column(6, plotOutput("outliers_plot"))
  #                 ),  br(),
  #  
  #   fluidRow(
  #           column(12, DTOutput("summary_stats")), br(), hr(), hr()
  #           )
  #         )
)

#######################
# SERVER
######################
server <- function(input, output, session) {
  
  filtered_data <- reactive({
                            req(input$date_range)
                            data <- sim_data %>%
                              filter(date >= input$date_range[1], date <= input$date_range[2])
                            
                            if (input$cohort_var != "All") {
                              data <- data %>% filter(region == input$cohort_var)
                            }
                            
                            if (nrow(data) == 0) {
                              showNotification("No data available for the selected filters.", type = "warning")
                            }
                            
                            data
                             })
  
  ## Trend plot
  output$trend_plot <- renderPlotly({
                                    req(nrow(filtered_data()) > 0)
                                    
                                    df <- filtered_data() %>%
                                      mutate(year_month = floor_date(date, "month")) %>%
                                      group_by(year_month) %>%
                                      summarise(total_sales = sum(sales), .groups = "drop")
                                    
                                    p <- ggplot(df, aes(x = year_month, y = total_sales)) +
                                      geom_line(color = "#2c3e50", size = 1.2) +
                                      geom_point(color = "#e74c3c", size = 1.5) +
                                      labs(x = "Month", y = "Total Sales ($)") +
                                      theme_minimal()
                                    ggplotly(p, dynamicTicks = TRUE)
                                 })
  
  ## Cohort table (FIXED) 
  output$cohort_table <- renderDT({
                                  req(nrow(filtered_data()) > 0)
                                  
                                  df_long <- filtered_data() %>%
                                                                mutate(month = format(floor_date(date, "month"), "%Y-%m")) %>%
                                                                group_by(region, month) %>%
                                                                summarise(avg_sales = mean(sales), .groups = "drop")
                                  
                                  recent_months <- df_long %>%
                                                              distinct(month) %>%
                                                              arrange(desc(month)) %>%
                                                              slice_head(n = input$n_months) %>%
                                                              pull(month)
                                  
                                  df_wide <- df_long %>%
                                                        filter(month %in% recent_months) %>%
                                                        pivot_wider(names_from = month, values_from = avg_sales, values_fill = NA) %>%
                                                        arrange(region)
                                  
                                  numeric_cols <- names(df_wide)[sapply(df_wide, is.numeric)]
                                  
                                  datatable(df_wide,
                                            options = list(scrollX = TRUE, pageLength = 10, dom = 'Bfrtip'),
                                            rownames = FALSE) %>%
                                    formatRound(columns = numeric_cols, digits = 2)
                                                              })
  
  ## Regression
  reg_model <- eventReactive(input$run_reg, {
                                            req(input$predictors)
                                            fmla <- as.formula(paste(input$target, "~", paste(input$predictors, collapse = "+")))
                                            data_clean <- sim_data %>% select(all_of(c(input$target, input$predictors))) %>% na.omit()
                                            if (nrow(data_clean) == 0) return(NULL)
                                            lm(fmla, data = data_clean)
                                            })
  
  output$reg_summary <- renderPrint({
                                     req(reg_model())
                                     summary(reg_model())
                                     })
  
  output$reg_residuals <- renderPlot({
                                      req(reg_model())
                                      model <- reg_model()
                                      df <- data.frame(resid = residuals(model), fitted = fitted(model))
                                      ggplot(df, aes(x = fitted, y = resid)) +
                                            geom_point(alpha = 0.5) +
                                            geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                                            labs(x = "Fitted Values", y = "Residuals") +
                                            theme_minimal()
                                    })
  
  ## Sensitivity
  sim_model <- reactive({
                        lm(sales ~ price + ad_spend + units, data = sim_data %>% na.omit())
                        })
  
  output$pred_sales <- renderText({
                                  model <- sim_model()
                                  newdata <- data.frame(price = input$price_sim,
                                                        ad_spend = input$ad_spend_sim,
                                                        units = input$units_sim)
                                  pred <- predict(model, newdata)
                                  paste0("$", round(pred, 2))
                                 })
  
  output$sensitivity_plot <- renderPlotly({
    price_seq <- seq(10, 100, length.out = 30)
    preds <- sapply(price_seq, function(p) {
      predict(sim_model(), newdata = data.frame(price = p,
                                                ad_spend = input$ad_spend_sim,
                                                units = input$units_sim))
    })
    df_plot <- data.frame(price = price_seq, predicted_sales = preds)
    p <- ggplot(df_plot, aes(x = price, y = predicted_sales)) +
      geom_line(color = "#2980b9", size = 1.2) +
      geom_point(data = data.frame(x = input$price_sim, y = preds[which.min(abs(price_seq - input$price_sim))]),
                 aes(x = x, y = y), color = "red", size = 3) +
      labs(x = "Price ($)", y = "Predicted Sales ($)") +
      theme_minimal()
    ggplotly(p)
                                          })
  
  ##Data quality
  output$missing_table <- renderDT({
    miss <- sim_data %>%
                        summarise(across(everything(), ~ sum(is.na(.)))) %>%
                        pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
                        mutate(Missing_Pct = round(100 * Missing_Count / nrow(sim_data), 2))
    datatable(miss, options = list(dom = "t"), rownames = FALSE) %>%
             formatStyle("Missing_Pct", background = styleColorBar(c(0,100), "lightcoral"))
                                  })
  
  output$outliers_plot <- renderPlot({
    numeric_cols <- sim_data %>% select(where(is.numeric)) %>% names()
    df_long <- sim_data %>%
      select(all_of(numeric_cols)) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
      group_by(variable) %>%
      mutate(Q1 = quantile(value, 0.25, na.rm = TRUE),
             Q3 = quantile(value, 0.75, na.rm = TRUE),
             IQR = Q3 - Q1,
             outlier = value < (Q1 - 1.5 * IQR) | value > (Q3 + 1.5 * IQR)) %>%
      ungroup()
    
    ggplot(df_long, aes(x = variable, y = value)) +
      geom_boxplot(fill = "skyblue", alpha = 0.6) +
      geom_point(data = filter(df_long, outlier), aes(x = variable, y = value), color = "red", size = 1.5) +
      labs(x = "", y = "Value", title = "Outliers (red points) by variable") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$summary_stats <- renderDT({
    sim_data %>%
      select(where(is.numeric)) %>%
      summary() %>%
      as.data.frame() %>%
      rownames_to_column("Statistic") %>%
      datatable(options = list(dom = "t", pageLength = 10), rownames = FALSE)
  })
}

######################
# RUN THE APP
#####################

shinyApp(ui, server)
