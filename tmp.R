
############################################
# bc plot

dat %>% filter(between(u,u_range[1], u_range[2])) %>%
  e_chart(u_bc) %>% 
  e_line(theta_bc) %>%
  e_x_axis(min = 0.06) %>% 
  # e_mark_line(data = list(xAxis = 0.1, yAxis = 2.4))  %>% 
  #e_mark_line(data = list(xAxis = 0.1)) %>% 
  # e_mark_area( color ='red',
  #   data = list(
  #     list(xAxis = 0.06, yAxis = 0), 
  #     list(xAxis = 0.1, yAxis = 2.3)
  #   )
  # ) %>% 
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
    ),
    #lineStyle = list(type = "solid", color = "yellow")
  ) 



dat %>% filter(between(w,w_range[1], w_range[2])) %>%
  e_chart(w) %>% 
  e_line(theta_ws) %>%
  e_line(theta_vs) %>% 
  e_line(theta_ws_y_plus) %>%
  e_line(theta_vs_y_plus) %>% 
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
