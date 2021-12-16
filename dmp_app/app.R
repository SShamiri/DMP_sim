
library(shiny)
library(htmlwidgets)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DMP model Simualtion"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            radioGroupButtons(
                inputId = "param_increase",
                label = "Simualtion",
                #selected = "",
                #choices = c("y","A", "κ", "β", "b","λ"),
                c("steatdy state" = "ss","y"= "prod", "A" = "a",
                  "κ" = "kappa", "β" = "beta",
                  "b" = "b", "λ" = "lambda"),
                checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square",
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o",
                                style = "color: steelblue"))
            ),
            #verbatimTextOutput("param"),
            radioGroupButtons(
                inputId = "param_decline",
                label = "Simualtion",
                #selected = "",
                #choices = c("y","A", "κ", "β", "b","λ"),
                c("steatdy state" = "ss","y"= "prod", "A" = "a",
                  "κ" = "kappa", "β" = "beta",
                  "b" = "b", "λ" = "lambda"),
                checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square",
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o",
                                style = "color: steelblue"))
            ),
        ),
        mainPanel(
            fluidRow(
                column(6, echarts4rOutput("bc_plot")),
                column(6, echarts4rOutput("ws_vs_plot"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
   # output$param <- renderPrint({ input$param_increase })
    
    # bc plot
    output$bc_plot <- renderEcharts4r({
        
        u_range <- c(0.08,0.16)
        
        dat %>% filter(between(u,u_range[1], u_range[2])) %>%
            e_chart(u_bc) %>% 
            e_line(theta_bc, lineStyle = list(type = "solid", color = "#3D3DDD"),symbol= 'none') %>%
            e_x_axis(min = 0.06) %>% 
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
    
    observeEvent(input$param_increase, {
        u_range <- c(0.08,0.16)

        if(grepl("ss|kappa|b|beta|y",input$param_increase)){
            echarts4rProxy("bc_plot", data = dat %>% filter(between(u,u_range[1], u_range[2])), x = u_bc) %>%
                e_remove_serie("theta_bc_a_plus") %>%
                e_remove_serie("theta_bc_a_minus") %>%
                e_remove_serie("theta_bc_lambda_plus") %>%
                e_remove_serie("theta_bc_lambda_minus") %>%
                e_execute()
        }

        if(input$param_increase == 'a'){
            echarts4rProxy("bc_plot", data = dat %>% filter(between(u,u_range[1], u_range[2])), x = u_bc) %>%
                e_line(theta_bc_a_plus, lineStyle = list(type = "dashed", color = "#3D3DDD"),symbol= 'none') %>%
                e_remove_serie("theta_bc_a_minus") %>%
                e_remove_serie("theta_bc_lambda_plus") %>%
                e_remove_serie("theta_bc_lambda_minus") %>%
                e_execute()
        }

        if(input$param_increase == 'lambda'){
            echarts4rProxy("bc_plot", data = dat %>% filter(between(u,u_range[1], u_range[2])), x = u_bc) %>%
                e_line(theta_bc_lambda_plus, lineStyle = list(type = "dashed", color = "#3D3DDD"),symbol= 'none') %>%
                e_remove_serie("theta_bc_a_plus") %>%
                e_remove_serie("theta_bc_a_minus") %>%
                e_remove_serie("theta_bc_lambda_minus") %>%
                e_execute()
        }
    })
    
    # ws & vs plot
    output$ws_vs_plot <- renderEcharts4r({
        
        w_range <- c(0.85,0.98)
        
        dat %>% filter(between(w,w_range[1], w_range[2])) %>%
            e_chart(w) %>% 
            e_line(theta_ws, lineStyle = list(type = "solid", color = "#8d89f5"),symbol= 'none') %>%
            e_line(theta_vs, lineStyle = list(type = "solid", color = "#B80000"),symbol= 'none') %>% 
            e_hide_grid_lines() %>% 
            e_x_axis(min = 0.82) %>% 
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
    
    observeEvent(input$param_increase, {
        w_range <- c(0.85,0.98)
        
        if(input$param_increase == 'ss'){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>% 
                e_remove_serie("theta_ws_y_plus") %>%
                e_remove_serie("theta_vs_y_plus") %>%  
                e_remove_serie("theta_vs_a_minus") %>%
                e_remove_serie("theta_ws_beta_plus") %>%
                e_remove_serie("theta_bc_a_plus") %>%
                e_remove_serie("theta_ws_kappa_plus") %>%
                e_remove_serie("theta_ws_beta_minus") %>%
                e_remove_serie("theta_ws_y_minus") %>%
                e_remove_serie("theta_vs_kappa_plus") %>%
                e_remove_serie("theta_ws_b_plus") %>%
                e_remove_serie("theta_vs_y_minus") %>%
                e_remove_serie("theta_ws_kappa_minus") %>%
                e_remove_serie("theta_ws_b_minus") %>%
                e_remove_serie("theta_vs_a_plus") %>%
                e_remove_serie("theta_vs_kappa_minus") %>%
                e_remove_serie("theta_vs_lambda_plus") %>%
                e_remove_serie("theta_vs_lambda_minus") %>%
                e_execute()
        }
        
        if(input$param_increase == 'prod'){
        echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>% 
            e_line(theta_ws_y_plus) %>%
            e_line(theta_vs_y_plus) %>% 
            e_remove_serie("theta_vs_a_minus") %>%
            e_remove_serie("theta_ws_beta_plus") %>%
            e_remove_serie("theta_bc_a_plus") %>%
            e_remove_serie("theta_ws_kappa_plus") %>%
            e_remove_serie("theta_ws_beta_minus") %>%
            e_remove_serie("theta_ws_y_minus") %>%
            e_remove_serie("theta_vs_kappa_plus") %>%
            e_remove_serie("theta_ws_b_plus") %>%
            e_remove_serie("theta_vs_y_minus") %>%
            e_remove_serie("theta_ws_kappa_minus") %>%
            e_remove_serie("theta_ws_b_minus") %>%
            e_remove_serie("theta_vs_a_plus") %>%
            e_remove_serie("theta_vs_kappa_minus") %>%
            e_remove_serie("theta_vs_lambda_plus") %>%
            e_remove_serie("theta_vs_lambda_minus") %>%
            e_execute()
        }
        
        if(input$param_increase == 'a'){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>% 
                e_line(theta_vs_a_plus) %>%  
                e_remove_serie("theta_ws_y_plus") %>%
                e_remove_serie("theta_vs_y_plus") %>%
                e_remove_serie("theta_vs_a_minus") %>%
                e_remove_serie("theta_ws_beta_plus") %>%
                e_remove_serie("theta_ws_kappa_plus") %>%
                e_remove_serie("theta_ws_beta_minus") %>%
                e_remove_serie("theta_ws_y_minus") %>%
                e_remove_serie("theta_vs_kappa_plus") %>%
                e_remove_serie("theta_ws_b_plus") %>%
                e_remove_serie("theta_vs_y_minus") %>%
                e_remove_serie("theta_ws_kappa_minus") %>%
                e_remove_serie("theta_ws_b_minus") %>%
                e_remove_serie("theta_vs_a_plus") %>%
                e_remove_serie("theta_vs_kappa_minus") %>%
                e_remove_serie("theta_vs_lambda_plus") %>%
                e_remove_serie("theta_vs_lambda_minus") %>%
                e_execute()
        }
        
        if(input$param_increase == 'kappa'){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>% 
                e_line(theta_ws_kappa_plus) %>% 
                e_line(theta_vs_kappa_plus) %>% 
                e_remove_serie("theta_ws_y_plus") %>%
                e_remove_serie("theta_vs_y_plus") %>%
                e_remove_serie("theta_ws_y_minus") %>%
                e_remove_serie("theta_vs_y_minus") %>%
                e_remove_serie("theta_vs_a_plus") %>%
                e_remove_serie("theta_vs_a_minus") %>%
                e_remove_serie("theta_ws_kappa_minus") %>%
                e_remove_serie("theta_vs_kappa_minus") %>%
                e_remove_serie("theta_ws_beta_plus") %>%
                e_remove_serie("theta_ws_beta_minus") %>%
                e_remove_serie("theta_ws_b_plus") %>%
                e_remove_serie("theta_ws_b_minus") %>%
                e_remove_serie("theta_vs_lambda_plus") %>%
                e_remove_serie("theta_vs_lambda_minus") %>%
                e_execute()
        }
        
        if(input$param_increase == 'beta'){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>% 
                e_line(theta_ws_beta_plus) %>%  
                e_remove_serie("theta_ws_y_plus") %>%
                e_remove_serie("theta_vs_y_plus") %>%
                e_remove_serie("theta_ws_y_minus") %>%
                e_remove_serie("theta_vs_y_minus") %>%
                e_remove_serie("theta_vs_a_plus") %>%
                e_remove_serie("theta_vs_a_minus") %>%
                e_remove_serie("theta_ws_kappa_plus") %>%
                e_remove_serie("theta_vs_kappa_plus") %>%
                e_remove_serie("theta_ws_kappa_minus") %>%
                e_remove_serie("theta_vs_kappa_minus") %>%
                e_remove_serie("theta_ws_beta_plus") %>%
                e_remove_serie("theta_ws_beta_minus") %>%
                e_remove_serie("theta_ws_b_minus") %>%
                e_remove_serie("theta_vs_lambda_plus") %>%
                e_remove_serie("theta_vs_lambda_minus") %>%
                e_execute()
        }
        
        if(input$param_increase == 'b'){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>% 
                e_line(theta_ws_b_plus) %>%
                e_remove_serie("theta_ws_y_plus") %>%
                e_remove_serie("theta_vs_y_plus") %>%
                e_remove_serie("theta_ws_y_minus") %>%
                e_remove_serie("theta_vs_y_minus") %>%
                e_remove_serie("theta_vs_a_plus") %>%
                e_remove_serie("theta_vs_a_minus") %>%
                e_remove_serie("theta_ws_kappa_plus") %>%
                e_remove_serie("theta_vs_kappa_plus") %>%
                e_remove_serie("theta_ws_kappa_minus") %>%
                e_remove_serie("theta_vs_kappa_minus") %>%
                e_remove_serie("theta_ws_beta_plus") %>%
                e_remove_serie("theta_ws_beta_minus") %>%
                e_remove_serie("theta_ws_b_minus") %>%
                e_remove_serie("theta_vs_lambda_plus") %>%
                e_remove_serie("theta_vs_lambda_minus") %>%
                e_execute()
        }
        
        if(input$param_increase == 'lambda'){
            echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>% 
                e_line(theta_vs_lambda_plus) %>% 
                e_remove_serie("theta_ws_y_plus") %>%
                e_remove_serie("theta_vs_y_plus") %>%
                e_remove_serie("theta_ws_y_minus") %>%
                e_remove_serie("theta_vs_y_minus") %>%
                e_remove_serie("theta_vs_a_plus") %>%
                e_remove_serie("theta_vs_a_minus") %>%
                e_remove_serie("theta_ws_kappa_plus") %>%
                e_remove_serie("theta_vs_kappa_plus") %>%
                e_remove_serie("theta_ws_kappa_minus") %>%
                e_remove_serie("theta_vs_kappa_minus") %>%
                e_remove_serie("theta_ws_beta_plus") %>%
                e_remove_serie("theta_ws_beta_minus") %>%
                e_remove_serie("theta_ws_b_plus") %>%
                e_remove_serie("theta_ws_b_minus") %>%
                e_remove_serie("theta_vs_lambda_minus") %>%
                e_execute()
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
