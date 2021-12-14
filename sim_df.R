library(tidyverse)
library(echarts4r)

# Set number of points to plot
N = 500

# Initial parameter values
kappa= 0.43
beta= 0.2
lambda= 0.102
a= 0.6
b= 0.71
y= 1;

# Initial steady state
theta_ss= 1.61685
u_ss= 0.11793
w_ss= 0.90705

# Parameter values after change
y_plus= 1+.05
y_minus= 1-.05

a_plus= 0.6+0.2
a_minus= 0.6-0.125

kappa_plus= 0.43+0.25
kappa_minus= 0.43-0.135

beta_plus= 0.2+0.1
beta_minus= 0.2-0.05

b_plus= 0.71+0.075
b_minus= 0.71-0.075

lambda_plus= 0.102+0.025
lambda_minus= 0.102-0.025

# Length of transition paths
periods = 15
t = 5

theta_tick_pos = 1.61685
theta_range = c(0.75,2.75)

u_tick_pos = 0.11793
u_range =  c(0.08,0.16)

w_tick_pos = 0.90705
w_range = c(0.85,0.98)

dat = data.frame(i = 2: N) %>%
  mutate(u = i/N,
         u_bc = u,
         theta_bc = lambda^2/a^2 * ((1-u)/u)^2,
         w = i/N,
         w_ws_vs = w,
         theta_ws = (w-beta*y-(1-beta)*b)/beta/kappa,
         theta_vs = (a/kappa/lambda*(y-w))^2,
         theta_ws_y_plus = (w-beta*y_plus-(1-beta)*b)/beta/kappa,
         theta_vs_y_plus = (a/kappa/lambda*(y_plus-w))^2
  )

head(dat)
