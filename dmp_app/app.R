
library(shiny)
library(htmlwidgets)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DMP model"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            radioGroupButtons(
                inputId = "param",
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
            verbatimTextOutput("param"),
            #textOutput("switch"),
            
            # checkboxGroupButtons(
            #     inputId = "param", label = "Simulation :", 
            #     choices = c("Choice A", "Choice B", " Choice C", "Choice D"), 
            #     justified = TRUE, status = "primary",
            #     checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            # )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            echarts4rOutput("bc_plot"),
            echarts4rOutput("ws_vs_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$param <- renderPrint({ input$param })
    
    # param <- switch(input$parm,
    #                prod = y_plaus,
    #                a = a_plus,
    #                kappa = kappa_plus,
    #                beta = beta_plus,
    #                b = b_plus,
    #                lambda = lambda_plus
    #                )
    # 
    # output$switch <- renderText({ paste0("use",param) })
    
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
                    list(xAxis = 0.1, yAxis = 2.3),
                    list(xAxis = 0.1, yAxis = 0,
                         value = "Uss")
                ),
                lineStyle = list(type = "solid", color = "gray")
            ) %>% 
            e_mark_p(
                type = "line",
                serie_index = 1,
                data = list(
                    list(xAxis = 0.1, yAxis = 2.3),
                    list(xAxis = 0.06, yAxis = 2.3,
                         value = "θss")
                ),
                lineStyle = list(type = "solid", color = "gray")
            ) %>% 
        e_tooltip()
    })
    
    observeEvent(input$param, {
        u_range <- c(0.08,0.16)

        if(grepl("ss|kappa|b|beta|y",input$param)){
            echarts4rProxy("bc_plot", data = dat %>% filter(between(u,u_range[1], u_range[2])), x = u_bc) %>%
                e_remove_serie("theta_bc_a_plus") %>%
                e_remove_serie("theta_bc_a_minus") %>%
                e_remove_serie("theta_bc_lambda_plus") %>%
                e_remove_serie("theta_bc_lambda_minus") %>%
                e_execute()
        }

        if(input$param == 'a'){
            echarts4rProxy("bc_plot", data = dat %>% filter(between(u,u_range[1], u_range[2])), x = u_bc) %>%
                e_line(theta_bc_a_plus, lineStyle = list(type = "dashed", color = "#3D3DDD"),symbol= 'none') %>%
                e_remove_serie("theta_bc_a_minus") %>%
                e_remove_serie("theta_bc_lambda_plus") %>%
                e_remove_serie("theta_bc_lambda_minus") %>%
                e_execute()
        }

        if(input$param == 'lambda'){
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
            e_line(theta_ws, lineStyle = list(type = "solid", color = "#C3F7F7"),symbol= 'none') %>%
            e_line(theta_vs, lineStyle = list(type = "solid", color = "#2A29A6"),symbol= 'none') %>% 
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
    
    observeEvent(input$param, {
        w_range <- c(0.85,0.98)
        
        if(input$param == 'ss'){
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
        
        if(input$param == 'prod'){
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
        
        if(input$param == 'a'){
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
        
        if(input$param == 'kappa'){
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
        
        if(input$param == 'beta'){
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
        
        if(input$param == 'b'){
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
        
        if(input$param == 'lambda'){
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
