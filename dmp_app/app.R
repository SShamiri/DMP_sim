library(shiny);library(shinydashboard);library(shinyWidgets)

ui <- dashboardPage(
    dashboardHeader(title = "S&M model Simualtion", titleWidth = 550, disable = FALSE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            column(width = 4,
                   box(title = "Info", status = "success", solidHeader = TRUE, width = 12,
                       collapsible = FALSE, collapsed = FALSE,
                       p("Direction:"),
                       switchInput(
                           inputId = "switch",
                           label = "",
                           value = TRUE,
                           onLabel = "Increase",
                           offLabel = "Decrease",
                           onStatus = "success", 
                           offStatus = "danger"
                       ),
                       awesomeRadio(
                           inputId = "param",
                           label = "Simualtion",
                           c("steatdy state" = "ss",
                             "output of the worker (y)"= "prod", 
                             "efficiency of the matching process (A)" = "a",
                             "cost to advertise a job vacancy (κ)" = "kappa", 
                             "workers' degree of bargaining power in wage negotiations (β)" = "beta",
                             "unemployment benefits (b)" = "b", 
                             "reflects the exogenous separation rate (λ)" = "lambda"),
                           checkbox = TRUE
                       )
                       
                   ),
                   verbatimTextOutput("param")
                   
            ),
            column(width = 8,
                   fluidRow(
                       box(title = "DMP framework", status = "success", solidHeader = TRUE, width = 12,
                           collapsible = TRUE, collapsed = FALSE,
                           p("The DMP model of unemployment is widely accepted as the most realistic account of unemployment based on a careful
                             and full statement of the underlying economic principles governing labour turnover, job-finding rates, 
                             and wage determination. Its building blocks are;"),
                           tags$ul(
                               tags$li("Two type of agents:",
                                       tags$ul(tags$li("workers"),
                                       tags$li("Firm"))),
                               tags$li("Workers has two possible states:",
                                       tags$ul(tags$li("Unemployed: obtain a flow utility of unemployed benefit"),
                                               tags$li("Employed: obtain a flow utility of wage"))),
                               tags$li("Firms has two possible states:",
                                       tags$ul(tags$li("Vacant"),
                                               tags$li("Employing a worker"))),
                               tags$li("Workers and firms try to match and form employment relationships.") 
                           )
                       )
                   ),
                   fluidRow(
                       box(title = "Wage and Vacancy curve", status = "success", solidHeader = TRUE, width = 6,
                           collapsible = FALSE, collapsed = FALSE,
                           echarts4rOutput("ws_vs_plot")
                       ),
                       box(title = "Beveridge curve", status = "success", solidHeader = TRUE, width = 6,
                           collapsible = FALSE, collapsed = FALSE,
                           echarts4rOutput("bc_plot")
                       )
                   )
            )
        )
    )
    
    
)

server <- function(input, output) {
    
     #output$param <- renderPrint({ input$param_increase })
    
    # bc plot
    output$bc_plot <- renderEcharts4r({
        
        u_range <- c(0.08,0.16)
        
        dat %>% filter(between(u,u_range[1], u_range[2])) %>%
            e_chart(u_bc) %>% 
            e_line(theta_bc, lineStyle = list(type = "solid", color = "#3D3DDD"),symbol= 'none', name = 'BC') %>%
            e_x_axis(min = 0.06, name = "Unemployment rate", 
                     nameLocation= 'middle', 
                     axisLabel = list(color = 'white'), 
                     nameGap = 25) %>%
            e_y_axis(max = 6,
                    name = "market tightness (θ)", 
                     nameLocation= 'middle', 
                     #axisLine = F, 
                     #axisLabel = list(color = 'white'), 
                     nameGap = 25) %>%
            e_hide_grid_lines() %>% 
            e_mark_p(
                type = "line",
                serie_index = 1,
                data = list(
                    list(xAxis = 0.1155, yAxis = 1.64256),
                    list(xAxis = 0.1155, yAxis = 0,
                         value = "Uss")
                ),
                lineStyle = list(type = "solid", color = "gray")
            ) %>% 
            e_mark_p(
                type = "line",
                serie_index = 1,
                data = list(
                    list(xAxis = 0.1155, yAxis = 1.64256),
                    list(xAxis = 0.06, yAxis = 1.64256,
                         value = "θss")
                ),
                lineStyle = list(type = "solid", color = "gray")
            ) %>% 
            e_tooltip()
    })
    
    toListen <- reactive({
        list(input$param,input$switch)
    })
    
    output$param <- renderPrint({toListen() })

    observeEvent(toListen(), {
        u_range <- c(0.08,0.16)

        if(grepl("ss|kappa|b|beta|y",input$param)){
            echarts4rProxy("bc_plot", data = dat %>% filter(between(u,u_range[1], u_range[2])), x = u_bc) %>%
                e_remove_serie("BC'") %>%
                e_execute()
        }

        if(input$param == 'a' && input$switch == TRUE){
            echarts4rProxy("bc_plot", data = dat %>% filter(between(u,u_range[1], u_range[2])), x = u_bc) %>%
                e_line(theta_bc_a_plus, lineStyle = list(type = "dashed", color = "#3D3DDD"),symbol= 'none', name = "BC'") %>%
                e_remove_serie("BC'") %>%
                e_execute()
        }

        if(input$param == 'a' && input$switch == FALSE){
            echarts4rProxy("bc_plot", data = dat %>% filter(between(u,u_range[1], u_range[2])), x = u_bc) %>%
                e_remove_serie("BC'") %>%
                e_line(theta_bc_a_minus, lineStyle = list(type = "dashed", color = "#3D3DDD"),symbol= 'none', name = "BC'") %>%
                e_execute()
        }

        if(input$param == 'lambda' && input$switch == TRUE){
            echarts4rProxy("bc_plot", data = dat %>% filter(between(u,u_range[1], u_range[2])), x = u_bc) %>%
                e_remove_serie("BC'") %>%
                e_line(theta_bc_lambda_plus, lineStyle = list(type = "dashed", color = "#3D3DDD"),symbol= 'none', name = "BC'") %>%
                e_execute()
        }

        if(input$param == 'lambda' && input$switch == FALSE){
            echarts4rProxy("bc_plot", data = dat %>% filter(between(u,u_range[1], u_range[2])), x = u_bc) %>%
                e_remove_serie("BC'") %>%
                e_line(theta_bc_lambda_minus, lineStyle = list(type = "dashed", color = "#3D3DDD"),symbol= 'none', name = "BC'") %>%
                e_execute()
        }
    })
    
    # ws & vs plot
    output$ws_vs_plot <- renderEcharts4r({
        
        w_range <- c(0.85,0.98)
        
        dat %>% filter(between(w,w_range[1], w_range[2])) %>%
            e_chart(w) %>% 
            e_line(theta_ws, lineStyle = list(type = "solid", color = "#8d89f5"),symbol= 'none',name = 'Wage') %>%
            e_line(theta_vs, lineStyle = list(type = "solid", color = "#B80000"),symbol= 'none', name = 'Vacancy') %>% 
            e_hide_grid_lines() %>% 
            e_x_axis(min = 0.82) %>% 
            e_y_axis(max = 6) %>% 
            e_mark_p(
                type = "line",
                serie_index = 1,
                data = list(
                    list(xAxis = 0.9065, yAxis = 1.64256),
                    list(xAxis = 0.9065, yAxis = 0,
                         value = "Wss")
                ),
                lineStyle = list(type = "solid", color = "gray")
            )  %>% 
            e_mark_p(
                type = "line",
                serie_index = 1,
                data = list(
                    list(xAxis = 0.9065, yAxis = 1.64256),
                    list(xAxis = 0.82, yAxis =  1.64256,
                         value = "θss")
                ),
                lineStyle = list(type = "solid", color = "gray")
            )  %>% 
            e_tooltip()
    })
    
    observeEvent(toListen(), {
        w_range <- c(0.85,0.98)

        if(input$param == 'ss'){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
                e_remove_serie("Wage'") %>%
                e_remove_serie("Vacancy'") %>%
                e_execute()
        }

        # if(input$param == 'ss' && input$switch == FALSE ){
        #     echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
        #         e_remove_serie("Wage'") %>%
        #         e_remove_serie("Vacancy'") %>%
        #         e_execute()
        # }

        if(input$param == 'prod' && input$switch == TRUE){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
                e_remove_serie("Wage'") %>%
                e_remove_serie("Vacancy'") %>%
                e_line(theta_ws_y_plus, name = "Wage'", lineStyle = list(type = "dashed", color = "#8d89f5"),symbol= 'none') %>%
                e_line(theta_vs_y_plus, name = "Vacancy'", lineStyle = list(type = "dashed", color = "#B80000"),symbol= 'none') %>%
                e_execute()
        }

        if(input$param == 'prod' && input$switch == FALSE){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
                e_remove_serie("Wage'") %>%
                e_remove_serie("Vacancy'") %>%
                e_line(theta_ws_y_minus, name = "Wage'", lineStyle = list(type = "dashed", color = "#8d89f5"),symbol= 'none') %>%
                e_line(theta_vs_y_minus, name = "Vacancy'", lineStyle = list(type = "dashed", color = "#B80000"),symbol= 'none') %>%
                e_execute()
        }

        if(input$param == 'a' && input$switch == TRUE){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
                e_remove_serie("Wage'") %>%
                e_remove_serie("Vacancy'") %>%
                e_line(theta_vs_a_plus, name = "Vacancy'", lineStyle = list(type = "dashed", color = "#B80000"),symbol= 'none') %>%
                e_execute()
        }

        if(input$param == 'a' && input$switch == FALSE){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
                e_remove_serie("Wage'") %>%
                e_remove_serie("Vacancy'") %>%
                e_line(theta_vs_a_minus, name = "Vacancy'", lineStyle = list(type = "dashed", color = "#B80000"),symbol= 'none') %>%
                e_execute()
        }

        if(input$param == 'kappa' && input$switch == TRUE){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
                e_remove_serie("Wage'") %>%
                e_remove_serie("Vacancy'") %>%
                e_line(theta_ws_kappa_plus, name = "Wage'", lineStyle = list(type = "dashed", color = "#8d89f5"),symbol= 'none') %>%
                e_line(theta_vs_kappa_plus, name = "Vacancy'", lineStyle = list(type = "dashed", color = "#B80000"),symbol= 'none') %>%
                e_execute()
        }

        if(input$param == 'kappa' && input$switch == FALSE){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
                e_remove_serie("Wage'") %>%
                e_remove_serie("Vacancy'") %>%
                e_line(theta_ws_kappa_minus, name = "Wage'", lineStyle = list(type = "dashed", color = "#8d89f5"),symbol= 'none') %>%
                e_line(theta_vs_kappa_minus, name = "Vacancy'", lineStyle = list(type = "dashed", color = "#B80000"),symbol= 'none') %>%
                e_execute()
        }

        if(input$param == 'beta' && input$switch == TRUE){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
                e_remove_serie("Wage'") %>%
                e_remove_serie("Vacancy'") %>%
                e_line(theta_ws_beta_plus, name = "Wage'", lineStyle = list(type = "dashed", color = "#8d89f5"),symbol= 'none') %>%
                e_execute()
        }

        if(input$param == 'beta' && input$switch == FALSE){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
                e_remove_serie("Wage'") %>%
                e_remove_serie("Vacancy'") %>%
                e_line(theta_ws_beta_minus, name = "Wage'", lineStyle = list(type = "dashed", color = "#8d89f5"),symbol= 'none') %>%
                e_execute()
        }

        if(input$param == 'b' && input$switch == TRUE){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
                e_remove_serie("Wage'") %>%
                e_remove_serie("Vacancy'") %>%
                e_line(theta_ws_b_plus, name = "Wage'", lineStyle = list(type = "dashed", color = "#8d89f5"),symbol= 'none') %>%
                e_execute()
        }

        if(input$param == 'b' && input$switch == FALSE){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
                e_remove_serie("Wage'") %>%
                e_remove_serie("Vacancy'") %>%
                e_line(theta_ws_b_plus, name = "Wage'", lineStyle = list(type = "dashed", color = "#8d89f5"),symbol= 'none') %>%
                e_execute()
        }

        if(input$param == 'lambda' && input$switch == TRUE){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
                e_remove_serie("Wage'") %>%
                e_remove_serie("Vacancy'") %>%
                e_line(theta_vs_lambda_plus, name = "Vacancy'", lineStyle = list(type = "dashed", color = "#B80000"),symbol= 'none') %>%
                e_execute()
        }

        if(input$param == 'lambda' && input$switch == FALSE){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>%
                e_remove_serie("Wage'") %>%
                e_remove_serie("Vacancy'") %>%
                e_line(theta_vs_lambda_minus, name = "Vacancy'", lineStyle = list(type = "dashed", color = "#B80000"),symbol= 'none') %>%
                e_execute()
        }
     })
}

# Run the application 
shinyApp(ui = ui, server = server)
