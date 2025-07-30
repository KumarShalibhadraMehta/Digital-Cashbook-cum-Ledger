library(shiny)
library(shinyWidgets)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(readr)
library(writexl)

ui <- navbarPage("Advanced Ledger Analysis",
                 
                 tabPanel("Upload CSV",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("file_upload", "Upload Ledger CSV", accept = ".csv"),
                              helpText("CSV format: Date, Description, Amount, Type")
                            ),
                            mainPanel(
                              tableOutput("uploaded_table")
                            )
                          )
                 ),
                 
                 tabPanel("Transactions",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("description", "Description"),
                              numericInput("amount", "Amount", 0),
                              selectInput("type", "Type", choices = c("Credit", "Debit")),
                              dateInput("date", "Transaction Date", value = Sys.Date()),
                              actionButton("add", "Add Transaction", class = "btn-primary"),
                              numericInput("index", "Row Index to Delete/Modify", 1, min = 1),
                              actionButton("delete", "Delete Transaction", class = "btn-danger"),
                              actionButton("modify", "Modify Transaction", class = "btn-warning"),
                              downloadButton("download_excel", "Export as Excel"),
                              h3("Total Balance: ", textOutput("total_balance", inline = TRUE))
                            ),
                            mainPanel(
                              h3("Transaction Ledger"),
                              tableOutput("ledger_table")
                            )
                          )
                 ),
                 
                 tabPanel("Data Analysis",
                          plotlyOutput("monthly_summary_plot"),
                          plotlyOutput("yearly_summary_plot"),
                          plotlyOutput("commodity_analysis_plot")
                 ),
                 
                 tabPanel("Real-time Balance Overview",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("slider_txn", "Show first N transactions:",
                                          min = 1, value = 10, step = 1, ticks = FALSE, width = '100%',
                                          max = 100)
                            ),
                            mainPanel(
                              plotlyOutput("realtime_bar_graph")
                            )
                          )
                 )
)

server <- function(input, output, session) {
  ledger <- reactiveVal(data.frame())
  
  # Read uploaded CSV
  observeEvent(input$file_upload, {
    req(input$file_upload)
    df <- read_csv(input$file_upload$datapath, show_col_types = FALSE)
    df <- df %>%
      mutate(Date = as.Date(Date, format = "%d-%m-%Y")) %>%
      mutate(Serial = row_number()) %>%
      select(Serial, everything())
    ledger(df)
  })
  
  # Display uploaded file
  output$uploaded_table <- renderTable({
    req(ledger())
    head(ledger())
  })
  
  # Add transaction
  observeEvent(input$add, {
    req(ledger())
    new_entry <- data.frame(
      Serial = nrow(ledger()) + 1,
      Date = input$date,
      Description = input$description,
      Amount = input$amount,
      Type = input$type
    )
    updated_ledger <- bind_rows(ledger(), new_entry) %>%
      mutate(Serial = row_number()) %>%
      select(Serial, everything())
    ledger(updated_ledger)
  })
  
  # Delete transaction
  observeEvent(input$delete, {
    req(ledger())
    if (input$index > 0 & input$index <= nrow(ledger())) {
      updated_ledger <- ledger()[-input$index, ] %>%
        mutate(Serial = row_number()) %>%
        select(Serial, everything())
      ledger(updated_ledger)
    }
  })
  
  # Modify transaction
  observeEvent(input$modify, {
    req(ledger())
    if (input$index > 0 & input$index <= nrow(ledger())) {
      updated_ledger <- ledger()
      updated_ledger$Description[input$index] <- input$description
      updated_ledger$Amount[input$index] <- input$amount
      updated_ledger$Type[input$index] <- input$type
      updated_ledger$Date[input$index] <- input$date
      ledger(updated_ledger)
    }
  })
  
  # Display total balance
  output$total_balance <- renderText({
    req(ledger())
    sum(ledger()$Amount * ifelse(ledger()$Type == "Credit", 1, -1), na.rm = TRUE)
  })
  
  # Display ledger table
  output$ledger_table <- renderTable({
    req(ledger())
    ledger() %>%
      mutate(
        Date = format(Date, "%Y-%B-%d"),
        Amount = ifelse(Type == "Credit",
                        paste0("+", Amount),
                        paste0("-", Amount))
      )
  })
  
  # Monthly Summary
  output$monthly_summary_plot <- renderPlotly({
    req(ledger())
    summary <- ledger() %>%
      mutate(Month = floor_date(Date, "month")) %>%
      group_by(Month, Type) %>%
      summarise(Total = sum(Amount), .groups = 'drop') %>%
      mutate(Type = factor(Type, levels = c("Credit", "Debit")))
    
    p <- ggplot(summary, aes(x = Month, y = Total, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Credit" = "green", "Debit" = "red")) +
      theme_minimal() +
      labs(title = "Monthly Credit/Debit Summary", x = "Month", y = "Total Amount")
    
    ggplotly(p)
  })
  
  
  # Yearly Summary
  output$yearly_summary_plot <- renderPlotly({
    req(ledger())
    summary <- ledger() %>%
      mutate(Year = year(Date)) %>%
      group_by(Year, Type) %>%
      summarise(Total = sum(Amount), .groups = 'drop') %>%
      mutate(Type = factor(Type, levels = c("Credit", "Debit")))
    
    p <- ggplot(summary, aes(x = factor(Year), y = Total, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Credit" = "green", "Debit" = "red")) +
      theme_minimal() +
      labs(title = "Yearly Credit/Debit Summary", x = "Year", y = "Total Amount")
    
    ggplotly(p)
  })
  
  
  # Commodity Analysis
  output$commodity_analysis_plot <- renderPlotly({
    req(ledger())
    summary <- ledger() %>%
      group_by(Description, Type) %>%
      summarise(Total = sum(Amount), .groups = 'drop') %>%
      mutate(Type = factor(Type, levels = c("Credit", "Debit")))
    
    p <- ggplot(summary, aes(x = reorder(Description, Total), y = Total, fill = Type)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("Credit" = "green", "Debit" = "red")) +
      theme_minimal() +
      labs(title = "Commodity-wise Analysis", x = "Commodity", y = "Total Amount")
    
    ggplotly(p)
  })
  
  
  # Real-time balance overview slider update
  observe({
    req(ledger())
    updateSliderInput(session, "slider_txn", max = nrow(ledger()), value = min(10, nrow(ledger())))
  })
  
  # Real-time graph
  output$realtime_bar_graph <- renderPlotly({
    req(ledger())
    df <- ledger()[1:input$slider_txn, ]
    
    credit <- sum(df$Amount[df$Type == "Credit"], na.rm = TRUE)
    debit <- sum(df$Amount[df$Type == "Debit"], na.rm = TRUE)
    balance <- credit - debit
    
    summary_df <- data.frame(
      Category = factor(c("Credit", "Balance", "Debit"), levels = c("Credit", "Balance", "Debit")),
      Value = c(credit, balance, debit)
    )
    
    plot_ly(summary_df, x = ~Category, y = ~Value, type = "bar",
            marker = list(color = c("green", "blue", "red"))) %>%
      layout(title = paste("Real-Time Ledger Summary (First", input$slider_txn, "Transactions)"),
             yaxis = list(title = "Amount"),
             xaxis = list(title = "Category"))
  })
  
  
  
  # Export ledger as Excel
  output$download_excel <- downloadHandler(
    filename = function() { "exported_ledger.xlsx" },
    content = function(file) {
      write_xlsx(ledger(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
