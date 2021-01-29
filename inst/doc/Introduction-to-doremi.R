## ---- include = FALSE---------------------------------------------------------
 library(ggplot2)
 library(data.table)
 library(doremi)
set.seed(1)
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  fig.align="center",
  fig.width = 5,
  fig.height = 5,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# create a time vector
time <- 0:90
# create an excitation mechanism
exc <- rep(c(0,1,0),c(11,30,50))

# generate simulated data
set.seed(123)
variable <- generate.panel.1order(time = time,
                           excitation = exc,
                           tau = 5,
                           nind = 1,
                           intranoise = 1)

plot(variable)

## -----------------------------------------------------------------------------
est_result <- analyze.1order(data = variable,
                             input = "excitation",
                             time = "time",
                             signal = "signal",
                             dermethod = "gold",
                             derparam = 13)

## -----------------------------------------------------------------------------
plot(est_result)+
  ggplot2::geom_line(data = variable,aes(time,signalraw,color = "underlying model"))+
  ggplot2::labs(y = "",
       title = "estimation of first order\n differential equation parameters")

## -----------------------------------------------------------------------------
set.seed(123)
variable2 <- generate.panel.2order(time = time,
                                  excitation = exc,
                                  period = 30,
                                  y0 = 0,
                                  xi = 0.1,
                                  nind = 1,
                                  intranoise = 0.8)

plot(variable2)

## -----------------------------------------------------------------------------
est_result2 <- analyze.2order(variable2,
                              input = "excitation",
                             time = "time",
                             signal = "signal",
                             dermethod = "glla",
                             derparam = 14)

plot(est_result2)+
  ggplot2::geom_line(data = variable2,aes(time,signalraw,color = "underlying model"))+
  ggplot2::labs(color = "",
       y = "",
       title = "estimation of second order \ndifferential equation parameters")

## ----fig.width = 5, fig.height = 4, fig.align = "center", echo = FALSE--------
time <- seq(0,100,0.1)
exc <- as.numeric(time >= 10 & time <= 40)
variable <- generate.1order(time = time,
                         excitation = exc,
                         y0 = 0,
                         tau = 10,
                         k = 1)

ggplot2::ggplot(data = as.data.table(variable)) +
ggplot2::ggtitle( "First order differential equation solution")+
  ggplot2::geom_line(ggplot2::aes(t,y, colour = "variable"))+
  ggplot2::geom_line(ggplot2::aes(t,exc, colour = "Excitation"),size = 1.5)+
  ggplot2::geom_hline(yintercept=0.63*max(variable$y), linetype="dashed", colour = "gray")+
  ggplot2::geom_hline(yintercept=0.37*max(variable$y), linetype="dashed", colour = "gray")+
  ggplot2::geom_vline(xintercept=variable$t[variable$y==max(variable$y)], colour = "gray")+
  ggplot2::geom_vline(xintercept=50, colour = "gray")+
  ggplot2::geom_vline(xintercept=19, colour = "gray")+
  ggplot2::geom_vline(xintercept=10, colour = "gray")+
  ggplot2::annotate("segment", x = 10, xend = 19, y = -0.1, yend = -0.1, colour = "dark green", size = 1)+
  ggplot2::annotate("text", x = 15, y = -0.2, label = "tau", parse = TRUE, colour = "dark green")+
  
  ggplot2::annotate("segment", x = 40, xend = 50, y = -0.1, yend = -0.1, colour = "dark green", size = 1)+
  ggplot2::annotate("text", x = 45, y = -0.2, label = "tau", parse = TRUE, colour = "dark green")+

  ggplot2::annotate("text", x = 75, y = 0.7, label = "63% diff. max and eq. value", colour = "gray")+
  ggplot2::annotate("text", x = 75, y = 0.3*max(variable$y), label = "37% diff. max and eq. value", colour = "gray")+
  ggplot2::labs(x = "Time (arb. unit)",
           y = "variable (arb. unit)",
           colour = "Legend")+
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.position = "top", plot.title = ggplot2::element_text(hjust = 0.5))

## ----fig.width = 5, fig.height = 4, fig.align = "center", echo = FALSE--------
time <- 0:130
excitation <- c(rep(0,30),rep(1,50),rep(0,51))
variable <- generate.2order(time = time,
                            excitation = excitation,
                            xi = 0.2,
                            period = 15,
                            k = 1)

ggplot2::ggplot(variable)+
  ggplot2::ggtitle( "Second order differential equation solution")+
  ggplot2::geom_line(aes(t,y,color = "variable"))+
  ggplot2::geom_line(aes(time,excitation,color = "excitation"))+
  ggplot2::geom_vline(xintercept=80, colour = "gray")+
  ggplot2::annotate("text", x = 100, y = 1, label = "DLO", parse = TRUE, colour = "gray")+
  ggplot2::labs(x = "Time (arb. unit)",
           y = "variable (arb. unit)",
           colour = "Legend")+
  ggplot2::theme_bw() + 
  ggplot2::theme(legend.position = "top", plot.title = ggplot2::element_text(hjust = 0.5))

