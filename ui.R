ui <- dashboardPage(
  dashboardHeader(title = "Cereal box",
                  titleWidth = 310,
                  disable = FALSE,
                  tags$li(class = "dropdown", 
                          tags$span("English Version", style = "font-weight: bold; margin-right: 15px; font-size: 14px; color: white;"),
                          imageOutput("flag", width = "10%",height = "30px"))),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Equally likely", tabName = "tab1", icon = icon("ellipsis-h")),
      menuItem("Inequally likely", tabName = "tab2", icon = icon("chart-area")))
  ),
  
  dashboardBody(
    useShinyjs(),
    #Custom CSS to change page border to red and blue
    tags$style(HTML("
      
      body {
        border: 8px solid #8B0000;
      }
      
      .wrapper {
        border: 8px solid #00008B;
      }

      .skin-blue .main-header .navbar {
        background-color: #8B0000 !important;
      }

      .skin-blue .main-header .logo {
        background-color: #8B0000 !important;
        color: white !important;
        font-weight: bold;
      }

      .skin-blue .main-sidebar {
        background-color: #1C2833 !important;
      }

      .sidebar-menu > li > a {
        color: #FFD700 !important;
        font-weight: bold;
      }

      .sidebar-menu > li.active > a {
        background-color: #8B0000 !important;
        color: white !important;
        border-left: 4px solid #FFD700;
      }

      .sidebar-menu > li:hover > a {
        background-color: #00008B !important;
        color: white !important;
      }

      .content-wrapper {
        background-color: #ECEFF4 !important;
      }

      h2 {
        color: #2E8B57;
        font-style: italic;
      }
      
          .irs-bar {
      background: #DC143C !important; /* rojo */
      border-top: 1px solid #DC143C !important;
      border-bottom: 1px solid #DC143C !important;
    }

    .irs-bar-edge {
      background: #DC143C !important;
      border: 1px solid #DC143C !important;
    }

    .irs-slider {
      background: #DC143C !important;
      border: 1px solid #DC143C !important;
    }
  
    ")),
    tabItems(
      tabItem(tabName = "tab1", 
              
              # 🔹 Título centrado
              div(style = "text-align: center;",
                  h2("Equally likely"),
                  h4("You can simulate how many boxes you need to open to complete the collection.")
              ),
              
              br(),
              
              # 🔹 Inputs alineados en una sola fila
              fluidRow(
                column(
                  width = 4,
                  offset = 2,
                  selectInput(
                    inputId = "Dropdown_var_1",
                    label = "Select the number of prizes",
                    choices = c(2:12)
                  )
                ),
                column(
                  width = 4,
                  numericInput(
                    inputId = "Price_box_1",
                    label = "Price per box (USD)",
                    value = 2.5,
                    min = 0
                  )
                )
              ),
              
              # 🔹 Budget centrado
              fluidRow(
                column(
                  width = 4,
                  offset = 4,
                  numericInput(
                    inputId = "Budget_cereal_1",
                    label = "Enter your budget for cereal boxes (USD)",
                    value = 10000,
                    min = 0.01
                  )
                )
              ),
              
              br(),
              
              # 🔹 Mensaje centrado
              fluidRow(
                column(
                  width = 12,
                  div(style = "text-align: center; font-size: 16px;",
                      htmlOutput("result_1")
                  )
                )
              ),
              
              br(),
              
              # 🔹 Resultados balanceados
              fluidRow(
                column(
                  width = 4,
                  div(style = "text-align: center;",
                      h4("Prizes collected"),
                      div(style = "display: flex; justify-content: center;",
                          uiOutput("results_1")
                      )
                  )
                ),
                column(
                  width = 4,
                  div(style = "text-align: center;",
                      h4("Simulation summary"),
                      div(style = "display: flex; justify-content: center;",
                          gt::gt_output("summary_1")
                      )
                  )
                ),
                column(
                  width = 4,
                  div(style = "text-align: center;",
                      h4("First appearance of each prize"),
                      div(style = "display: flex; justify-content: center;",
                          DT::dataTableOutput("first_time_count_1")
                      )
                  )
                )),
                fluidRow(
                  column(
                    width = 6,
                    div(style = "text-align: center;",
                        h4("Prizes collected"),
                        div(style = "display: flex; justify-content: center;",
                            plotlyOutput("comparison_plot_1")
                        )
                    )
                  ),
                  column(
                    width = 6,
                    div(style = "text-align: center;",
                        h4("Remaining Budget"),
                        div(style = "display: flex; justify-content: center;",
                            plotlyOutput("budget_plot_1")
                        )
                    
                  )
                )
                
              )
      ),
      tabItem(tabName = "tab2", 
              h2("Inequally likely"),
              h3("In this section, prizes may have different probabilities of appearing.
You can explore how rare prizes affect the cost and time needed to complete the collection."),
              sidebarLayout(
              sidebarPanel(
                
                numericInput(
                  inputId = "Price_box_2",
                  label = "Price per box (USD)",
                  value = 2.5,
                  min = 0
                ),
                numericInput(
                  inputId = "Budget_cereal_2",
                  label = "Enter your budget for cereal boxes (USD)",
                  value = 10000,
                  min = 0.01
                ),
                numericInput(
                  inputId = "n_premios",
                  label = "Prizes number:",
                  value = 4,
                  min = 2,
                  step = 1,
                  max = 12
                ),
                uiOutput("probabilidades_ui"),
                actionButton(inputId = "cerealine", label = "Generate analysis", disabled = FALSE)),
              mainPanel(  width = 8,
                         fluidRow(
                           column(
                             width = 12,
                             div(style = "text-align: center; font-size: 16px;",
                                 uiOutput("validacion")))),
                         fluidRow(
                           column(
                  width = 12,
                  div(style = "text-align: center; font-size: 16px;",
                      htmlOutput("result_2")
                  ))
                ),
                fluidRow(
                  column(
                    width = 6,
                    div(style = "text-align: center;",
                        h4("Prizes collected"),
                        div(style = "display: flex; justify-content: center;",
                            uiOutput("results_2")
                        )
                    )
                  ),
                  column(
                    width = 6,
                    div(style = "text-align: center;",
                        h4("Simulation summary"),
                        div(style = "display: flex; justify-content: center;",
                            gt::gt_output("summary_2")
                        )
                    )
                  )
                ),
                
                br(),
                
                # 🔹 Segunda fila (1 columna centrada)
                fluidRow(
                  column(
                    width = 6,
                    offset = 3,  # 👈 esto lo centra
                    div(style = "text-align: center;",
                        h4("First appearance of each prize"),
                        div(style = "display: flex; justify-content: center;",
                            DT::dataTableOutput("first_time_count_2")
                        )
                    )
                  )
                ),
                  fluidRow(
                    column(
                      width = 6,
                      div(style = "text-align: center;",
                          h4("Prizes collected"),
                          div(style = "display: flex; justify-content: center;",
                              plotlyOutput("comparison_plot_2")
                          )
                      )
                    ),
                    column(
                      width = 6,
                      div(style = "text-align: center;",
                          h4("Remaining Budget"),
                          div(style = "display: flex; justify-content: center;",
                              plotlyOutput("budget_plot_2")
                          )
                      )
                    )
                  ))
              
              )
      
    ))
  )
)