server <- function(input, output, session){
  
  output$flag <- renderImage({
    return(list(src = "www/flag.png",contentType = "image/png",
                width = 80,       # Specify width
                height = 27))     # Specify height))
  }, deleteFile = FALSE)
  

  
  

# Equally -----------------------------------------------------------------

  
  
  price_box_1 <- reactive({
    req(input$Price_box_1)
    as.numeric(input$Price_box_1) 
  })
  
   number_prizes_1 <- reactive({
    req(input$Dropdown_var_1)
    as.numeric(input$Dropdown_var_1) 
  })
  
   observeEvent(list(input$Price_box_1, input$Dropdown_var_1),{
     req(!is.null(input$Price_box_1)) 
     req(input$Dropdown_var_1)
     as.numeric(input$Price_box_1)
     updateNumericInput(session, "Budget_cereal_1",
                       value = price_box_1() * number_prizes_1())
     
   })
   
   budget_box_1 <- reactive({
   req(!is.null(input$Price_box_1)) 
   req(input$Dropdown_var_1)
   req(input$Price_box_1)
   as.numeric(input$Budget_cereal_1)
   })
   
   simulations_cereal_boxes_1 <- reactive({
     #browser()
     req(!is.null(input$Price_box_1)) 
     req(input$Dropdown_var_1)
     req(input$Budget_cereal_1)
     
     get_prizes_1 <- c()
     vector_prizes_1 <- 1:number_prizes_1()
     budget_1 <- budget_box_1()
     vect_all_pr_1 <- c()
     cost_cum_1 <- 0
     tries_1 <- 0
     
     first_time <- rep(NA, length(vector_prizes_1))
     names(first_time) <- vector_prizes_1
     
     while ((length(get_prizes_1) < length(vector_prizes_1)) && 
            (budget_1 >= price_box_1())) {
       
       h <- sample(1:number_prizes_1(), size = 1)
       
       if (!(h %in% get_prizes_1)) {
         get_prizes_1 <- c(get_prizes_1, h)
         first_time[as.character(h)] <- tries_1 + 1
       }
       
       vect_all_pr_1 <- c(vect_all_pr_1, h)
       cost_cum_1 <- cost_cum_1 + price_box_1()
       budget_1 <- budget_1 - price_box_1()
       tries_1 <- tries_1 + 1 
     }
     
     if (length(get_prizes_1) < length(vector_prizes_1)) {
       msg_1 <- "😢 You ran out of budget before completing the collection"
     } else {
       msg_1 <- "🎉 Congratulations! You completed the collection"
     }
     
     # Data frame de frecuencias
     freq_df_1 <- as.data.frame(table(vect_all_pr_1))
     colnames(freq_df_1) <- c("Prize", "Frequency")
     freq_df_1$Prize <- as.numeric(as.character(freq_df_1$Prize))
     
     # Tabla resumen
     summary_df_1 <- data.frame(
       Metric = c("Amount spent", "Remaining budget", "Number of tries"),
       Value  = c(cost_cum_1, budget_1, tries_1)
     )
     
     # Tabla de primera aparición
     first_time_df_1 <- data.frame(
       Prize = as.numeric(names(first_time)),
       First_Appearance = as.numeric(first_time)
     )
     
     
     list(
       freq_df_1 = freq_df_1,
       summary_df_1 = summary_df_1,
       first_time_df_1 = first_time_df_1,
       msg_1 = msg_1,
       cost_cum_1 = cost_cum_1,
       budget_1 = budget_1,
       tries_1 = tries_1
     )
   })
   
   output$result_1 <- renderUI({
     req(simulations_cereal_boxes_1())
     
     HTML(paste0(
       simulations_cereal_boxes_1()$msg_1
     ))
   })
   
   result_2 <- reactive({
     req(simulations_cereal_boxes_1())
     simulations_cereal_boxes_1()$first_time_df_1 
   })
   
   observe({
     req(result_2())
     print(result_2())
   })

   output$results_1 <- renderUI({
     req(simulations_cereal_boxes_1())
     HTML(knitr::kable(simulations_cereal_boxes_1()$freq_df_1, format = "html", table.attr = "class='table table-hover table-bordered'") %>%
            kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed")))
   })   
   
   output$summary_1 <- gt::render_gt({
     
     req(simulations_cereal_boxes_1())
     
     tabla <- simulations_cereal_boxes_1()$summary_df_1
     
     gt::gt(tabla) %>%
       
       gt::data_color(
         columns = Value,
         colors = function(x) {
           
           # aplicamos regla según la métrica
           mapply(function(valor, metrica) {
             
             if (metrica == "Amount spent") {
               if (valor > .75 * budget_box_1()) "red"
               else if (valor > .50 * budget_box_1()) "orange"
               else "green"
               
             } else if (metrica == "Remaining budget") {
               if (valor > 0.50 * budget_box_1() ) "green"
               else if (valor > 0.30 * budget_box_1()) "orange"
               else "red"
               
             } else if (metrica == "Number of tries") {
               if (valor > budget_box_1() / price_box_1() * 0.8) "red"
               else if (valor > budget_box_1() / price_box_1() * 0.5) "orange"
               else "green"
             }
             
           }, x, tabla$Metric)
           
         }
       ) })
     output$first_time_count_1 <- DT::renderDataTable({
       DT::datatable(simulations_cereal_boxes_1()$first_time_df_1 ,
                     options = list(
                       pageLength = 8,
                       scrollX = TRUE,
                       dom = 'Bfrtip',
                       buttons = c('csv'),
                       autoWidth = TRUE
                     ),
                     class = 'cell-border stripe hover',
                     extensions = 'Buttons',
                     rownames = FALSE)
     })
     
     expected_trials <- function(n) {
       n * sum(1 / (1:n))
     }
     
     output$comparison_plot_1 <- renderPlotly({
       
       req(simulations_cereal_boxes_1())
       
       n <- number_prizes_1()
       expected <- expected_trials(n)
       observed <- simulations_cereal_boxes_1()$tries_1
       
       df_compare <- data.frame(
         Type = c("Expected", "Observed"),
         Value = c(expected, observed)
       )
       
       plot_ly(
         data = df_compare,
         x = ~Type,
         y = ~Value,
         type = "bar",
         color = ~Type,
         text = ~round(Value, 2),
         textposition = "outside"
       ) %>%
         layout(
           title = "Expected vs Observed Trials",
           yaxis = list(title = "Number of boxes"),
           xaxis = list(title = ""),
           showlegend = FALSE
         )
     })
  
     
     output$budget_plot_1 <- renderPlotly({
       
       req(simulations_cereal_boxes_1())
       
       remaining_budget <- simulations_cereal_boxes_1()$budget_1
       amount_spent <- simulations_cereal_boxes_1()$cost_cum_1
       expected_cost <- expected_trials(number_prizes_1()) * price_box_1()
       
       df_budget <- data.frame(
         Type = c("Remaining budget", "Amount spent", "Expected cost"),
         Value = c(remaining_budget, amount_spent, expected_cost)
       )
       
       plot_ly(
         data = df_budget,
         x = ~Type,
         y = ~Value,
         type = "bar",
         color = ~Type,
         text = ~round(Value, 2),
         textposition = "outside"
       ) %>%
         layout(
           title = "Budget status vs Expected cost",
           yaxis = list(title = "USD"),
           xaxis = list(title = ""),
           showlegend = FALSE
         )
     })
     

# Inequally ---------------------------------------------------------------

     output$probabilidades_ui <- renderUI({
       
       n <- input$n_premios
       
       lapply(1:n, function(i) {
         numericInput(
           inputId = paste0("p_", i),
           label = paste("Prize ", i, " probability" ),
           value = round(1 / n, 3),
           min = 0,
           max = 1,
           step = 0.01
         )
       })
       
     })     
     
     probs_i <- reactive({
       req(input$n_premios)
       
       sapply(1:input$n_premios, function(i) {
         input[[paste0("p_", i)]]
       })
     })
     
     
     sum_probs <- reactive({
       sum(probs_i(), na.rm = TRUE)
     })
     
     validacion_estado <- eventReactive(input$cerealine, {
       total <- sum_probs()
       
       if (abs(total - 1) > 0.001) {
         list(
           ok = FALSE,
           total = total
         )
       } else {
         list(
           ok = TRUE,
           total = total
         )
       }
     })
     
     
     output$validacion <- renderUI({
       v <- validacion_estado()
       
       if (!v$ok) {
         div(
           style = "color:red;",
           paste("❌ Sum must be 1. Current:", round(v$total, 3))
         )
       } else {
         div(
           style = "color:green;",
           "✅ Probabilities are valid"
         )
       }
     })
     
     price_box_2 <- reactive({
       req(input$Price_box_2)
       as.numeric(input$Price_box_2) 
     })
     
     number_prizes_2 <- reactive({
       req(input$n_premios)
       as.numeric(input$n_premios) 
     })
     
     observeEvent(list(input$Price_box_2, input$n_premios),{
       req(!is.null(input$Price_box_2)) 
       req(input$n_premios)
       as.numeric(input$Price_box_2)
       updateNumericInput(session, "Budget_cereal_2",
                          value = price_box_2() * number_prizes_2())
       
     })
     
     budget_box_2 <- reactive({
       req(!is.null(input$Price_box_2)) 
       req(input$n_premios)
       req(input$Price_box_2)
       as.numeric(input$Budget_cereal_2)
     })
     
     simulations_cereal_boxes_2 <- eventReactive(input$cerealine,{
       #browser()
       req(!is.null(input$Price_box_2)) 
       req(input$n_premios)
       req(input$Budget_cereal_2)
       
       p <- probs_i()  # tu vector de probabilidades
       
       validate(
         need(abs(sum(p) - 1) < 0.001, 
              "Probabilities must sum to 1")
       )
       
       get_prizes_2 <- c()
       vector_prizes_2 <- 1:number_prizes_2()
       budget_2 <- budget_box_2()
       vect_all_pr_2 <- c()
       cost_cum_2 <- 0
       tries_2 <- 0
       
       first_time <- rep(NA, length(vector_prizes_2))
       names(first_time) <- vector_prizes_2
       
       while ((length(get_prizes_2) < length(vector_prizes_2)) && 
              (budget_2 >= price_box_2())) {
         
         h <- sample(1:number_prizes_2(), size = 1, prob = probs_i())
         
         if (!(h %in% get_prizes_2)) {
           get_prizes_2 <- c(get_prizes_2, h)
           first_time[as.character(h)] <- tries_2 + 1
         }
         
         vect_all_pr_2 <- c(vect_all_pr_2, h)
         cost_cum_2 <- cost_cum_2 + price_box_2()
         budget_2 <- budget_2 - price_box_2()
         tries_2 <- tries_2 + 1 
       }
       
       if (length(get_prizes_2) < length(vector_prizes_2)) {
         msg_2 <- "😢 You ran out of budget before completing the collection"
       } else {
         msg_2 <- "🎉 Congratulations! You completed the collection"
       }
       
       # Data frame de frecuencias
       freq_df_2 <- as.data.frame(table(vect_all_pr_2))
       colnames(freq_df_2) <- c("Prize", "Frequency")
       freq_df_2$Prize <- as.numeric(as.character(freq_df_2$Prize))
       
       # Tabla resumen
       summary_df_2 <- data.frame(
         Metric = c("Amount spent", "Remaining budget", "Number of tries"),
         Value  = c(cost_cum_2, budget_2, tries_2)
       )
       
       # Tabla de primera aparición
       first_time_df_2 <- data.frame(
         Prize = as.numeric(names(first_time)),
         First_Appearance = as.numeric(first_time)
       )
       
       
       list(
         freq_df_2 = freq_df_2,
         summary_df_2 = summary_df_2,
         first_time_df_2 = first_time_df_2,
         msg_2 = msg_2,
         cost_cum_2 = cost_cum_2,
         budget_2 = budget_2,
         tries_2 = tries_2
       )
     })
     
     output$result_2 <- renderUI({
       req(simulations_cereal_boxes_2())
       
       HTML(paste0(
         simulations_cereal_boxes_2()$msg_2
       ))
     })
     
     result_3 <- reactive({
       req(simulations_cereal_boxes_2())
       simulations_cereal_boxes_2()$first_time_df_2 
     })
     
     observe({
       req(result_3())
       print(result_3())
     })
     
     output$results_2 <- renderUI({
       req(simulations_cereal_boxes_2())
       HTML(knitr::kable(simulations_cereal_boxes_2()$freq_df_2, format = "html", table.attr = "class='table table-hover table-bordered'") %>%
              kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed")))
     })   
     
     output$summary_2 <- gt::render_gt({
       
       req(simulations_cereal_boxes_2())
       
       tabla <- simulations_cereal_boxes_2()$summary_df_2
       
       gt::gt(tabla) %>%
         
         gt::data_color(
           columns = Value,
           colors = function(x) {
             
             # aplicamos regla según la métrica
             mapply(function(valor, metrica) {
               
               if (metrica == "Amount spent") {
                 if (valor > .75 * budget_box_2()) "red"
                 else if (valor > .50 * budget_box_2()) "orange"
                 else "green"
                 
               } else if (metrica == "Remaining budget") {
                 if (valor > 0.50 * budget_box_2() ) "green"
                 else if (valor > 0.30 * budget_box_2()) "orange"
                 else "red"
                 
               } else if (metrica == "Number of tries") {
                 if (valor > budget_box_2() / price_box_2() * 0.8) "red"
                 else if (valor > budget_box_2() / price_box_2() * 0.5) "orange"
                 else "green"
               }
               
             }, x, tabla$Metric)
             
           }
         ) })
     output$first_time_count_2 <- DT::renderDataTable({
       DT::datatable(simulations_cereal_boxes_2()$first_time_df_2 ,
                     options = list(
                       pageLength = 8,
                       scrollX = TRUE,
                       dom = 'Bfrtip',
                       buttons = c('csv'),
                       autoWidth = TRUE
                     ),
                     class = 'cell-border stripe hover',
                     extensions = 'Buttons',
                     rownames = FALSE)
     })
     
     expected_trials_unequal <- function(p) {
       n <- length(p)
       total <- 0
       
       for (k in 1:n) {
         combs <- combn(1:n, k)
         
         for (j in 1:ncol(combs)) {
           subset_indices <- combs[, j]
           prob_sum <- sum(p[subset_indices])
           
           total <- total + ((-1)^(k + 1)) * (1 / prob_sum)
         }
       }
       
       return(total)
     }
     
     output$comparison_plot_2 <- renderPlotly({
       
       req(simulations_cereal_boxes_2())
       p <- probs_i()
       
       validate(
         need(abs(sum(p) - 1) < 0.001, 
              "Probabilities must sum to 1")
       )
       
       expected <- expected_trials_unequal(p)
       observed <- simulations_cereal_boxes_2()$tries_2
       
       df_compare <- data.frame(
         Type = c("Expected", "Observed"),
         Value = c(expected, observed)
       )
       
       plot_ly(
         data = df_compare,
         x = ~Type,
         y = ~Value,
         type = "bar",
         color = ~Type,
         text = ~round(Value, 2),
         textposition = "outside"
       ) %>%
         layout(
           title = "Expected vs Observed Trials",
           yaxis = list(title = "Number of boxes"),
           xaxis = list(title = ""),
           showlegend = FALSE
         )
     })
     
     
     output$budget_plot_2 <- renderPlotly({
       
       req(simulations_cereal_boxes_2())
       p <- probs_i()
       remaining_budget <- simulations_cereal_boxes_2()$budget_2
       amount_spent <- simulations_cereal_boxes_2()$cost_cum_2
       expected_cost <- expected_trials_unequal(p) * price_box_2()
       
       df_budget <- data.frame(
         Type = c("Remaining budget", "Amount spent", "Expected cost"),
         Value = c(remaining_budget, amount_spent, expected_cost)
       )
       
       plot_ly(
         data = df_budget,
         x = ~Type,
         y = ~Value,
         type = "bar",
         color = ~Type,
         text = ~round(Value, 2),
         textposition = "outside"
       ) %>%
         layout(
           title = "Budget status vs Expected cost",
           yaxis = list(title = "USD"),
           xaxis = list(title = ""),
           showlegend = FALSE
         )
     })
     
     
}