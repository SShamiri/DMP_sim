
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
    
    output$bc_plot <- renderEcharts4r({
        
        u_range <- c(0.08,0.16)
        
        dat %>% filter(between(u,u_range[1], u_range[2])) %>%
            e_chart(u_bc) %>% 
            e_line(theta_bc) %>%
            e_x_axis(min = 0.06) %>% 
            e_mark_p(
                type = "line",
                data = list(
                    list(xAxis = 0.1, yAxis = 2.3),
                    list(xAxis = 0.1, yAxis = 0,
                         value = "Uss")
                ),
                #lineStyle = list(type = "solid", color = "yellow")
            ) %>% 
            e_mark_p(
                type = "line",
                data = list(
                    list(xAxis = 0.1, yAxis = 2.3),
                    list(xAxis = 0.06, yAxis = 2.3,
                         value = "θss")
                )
            ) %>% 
        e_tooltip()
    })
    
    output$ws_vs_plot <- renderEcharts4r({
        
        w_range <- c(0.85,0.98)
        
        dat %>% filter(between(w,w_range[1], w_range[2])) %>%
            e_chart(w) %>% 
            e_line(theta_ws) %>%
            e_line(theta_vs) %>% 
            e_x_axis(min = 0.82) %>% 
            e_mark_p(
                type = "line",
                data = list(
                    list(xAxis = 0.9065, yAxis = 1.64256),
                    list(xAxis = 0.9065, yAxis = 0,
                         value = "Wss")
                )
            )  %>% 
            e_mark_p(
                type = "line",
                data = list(
                    list(xAxis = 0.9065, yAxis = 1.64256),
                    list(xAxis = 0.82, yAxis =  1.64256,
                         value = "θss")
                )
            )  %>% 
            e_tooltip()
    })
    
    observeEvent(input$param, {
        w_range <- c(0.85,0.98)
        
        if(input$param == 'prod'){
            
        echarts4rProxy("ws_vs_plot", data = dat %>% filter(between(w,w_range[1], w_range[2])), x = w) %>% 
            e_line(theta_ws_y_plus) %>%
            e_line(theta_vs_y_plus) %>%   
            e_execute()
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
