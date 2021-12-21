
############################################
# bc plot

dat %>% filter(between(u,u_range[1], u_range[2])) %>%
  e_chart(u_bc) %>% 
  e_line(theta_bc, lineStyle = list(type = "sold", color = "#3D3DDD"),symbol= 'none') %>%
  e_line(theta_bc_lambda_plus, lineStyle = list(type = "dashed", color = "#3D3DDD"),symbol= 'none') %>%
  e_hide_grid_lines() %>% 
  e_x_axis(min = 0.06) %>% 
  e_y_axis(name = "market tightness", 
           nameLocation= 'middle', 
           #axisLine = T,
           axisLabel = list(color = 'white'), 
           nameGap = 25) %>% 
  e_mark_p(
    type = "line",
    #serie_index = 1,
    data = list(
      list(xAxis = 0.1155, yAxis = 1.64256),
      list(xAxis = 0.1155, yAxis = 0,
           value = "Uss")
      ),
      lineStyle = list(type = "solid", color = "red")
   
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
  e_mark_p(
    type = "line",
    serie_index = 2,
    data = list(
      list(xAxis = 0.1, yAxis = 4),
      list(xAxis = 0.1, yAxis = 0,
           value = "Uss")
    ),
    lineStyle = list(type = "solid", color = "blue")
  ) %>% 
  e_mark_p(
    type = "line",
    serie_index = 2,
    data = list(
      list(xAxis = 0.1, yAxis = 4),
      list(xAxis = 0.06, yAxis = 4,
           value = "Uss")
    ),
    lineStyle = list(type = "solid", color = "blue")
  ) 




dat %>% filter(between(w,w_range[1], w_range[2])) %>%
  e_chart(w) %>% 
  e_line(theta_ws, lineStyle = list(type = "solid", color = "#8d89f5"),symbol= 'none',name = 'Wage') %>%
  e_line(theta_vs, lineStyle = list(type = "solid", color = "#B80000"),symbol= 'none', name = 'Vacancy') %>% 
  e_line(theta_ws_y_plus, name = 'Wage', lineStyle = list(type = "dashed", color = "#8d89f5"),symbol= 'none') %>%
  e_line(theta_vs_y_plus, name = 'Vacancy', lineStyle = list(type = "dashed", color = "#B80000"),symbol= 'none') %>% 
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



Dashed svg path:
  'path://M180 1000 l0 -40 200 0 200 0 0 40 0 40 -200 0 -200 0 0 -40z, M810 1000 l0 -40 200 0 200 0 0 40 0 40 -200 0 -200 0 0 -40zm, M1440 1000 l0 -40 200 0 200 0 0 40 0 40 -200 0 -200 0 0 -40z'
Dotted svg path:
  'path://M335 1316 c-63 -28 -125 -122 -125 -191 0 -71 62 -164 127 -192 18 -7 58 -13 90 -13 72 0 125 28 168 88 27 39 30 52 30 117 0 65 -3 78 -30 117 -43 61 -95 88 -170 87 -33 0 -73 -6 -90 -13z, M1035 1313 c-76 -40 -115 -103 -115 -188 0 -121 85 -205 205 -205 121 0 205 84 205 205 0 84 -39 148 -112 186 -46 24 -140 25 -183 2z, M1714 1298 c-61 -42 -94 -102 -94 -173 0 -71 33 -131 94 -172 41 -28 57 -33 107 -33 76 0 115 16 161 68 76 84 76 190 0 274 -46 52 -85 68 -161 68 -50 0 -66 -5 -107 -32z'
