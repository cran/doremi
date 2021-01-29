## ----options, include = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.align="center",
  fig.width = 5,
  fig.height = 5,
  comment = "#>"
)

## ----libraries, setup---------------------------------------------------------
library(doremi)

## ----different forms according to xi,fig.width = 5, fig.height = 4, fig.align = "center"----
data11a <- data.table::rbindlist(lapply(seq(0,2,0.2), 
                            function(eps){
                              generate.2order(time = 0:49, 
                                              y0 = 1, 
                                              xi = eps, 
                                              period = 20)[,xi := eps][]
                            }))
# plot
ggplot2::ggplot(data11a,ggplot2::aes(t,y,color = as.factor(xi)))+
  ggplot2::geom_line() +
  ggplot2::labs(x = "time (arb. unit)", y = "signal (arb. unit)", colour = "xi")

## -----------------------------------------------------------------------------
test <- generate.2order(time = 0:100,y0 = 10,v0 = 0,period = 10,xi = 0.2)
plot(test$t,test$y)

## ----simulation example 1-----------------------------------------------------
time <- 0:100
set.seed(123)
data1 <- generate.panel.2order(time = time,
                               y0 = 10,
                               xi = 0.1,
                               period = 30,
                               yeq = 2,
                               nind = 6,
                               internoise = 0.1,
                               intranoise = 0.3)
data1

## ----dlo plot data1,fig.width = 7, fig.height = 6, fig.align = "center"-------
plot(data1) +
  ggplot2::geom_hline(yintercept=0)

## ----simulation example2------------------------------------------------------
set.seed(123)
data2a <- generate.panel.2order(time = 0:99,
                               y0 = 1,
                               period = 30,
                               nind = 1,
                               intranoise = 0.2)
set.seed(123)
data2b <- generate.panel.2order(time = 0:99,
                               y0 = 1,
                               xi = 1,
                               period = 30,
                               intranoise = 0.2)
set.seed(123)
data2c <- generate.panel.2order(time = 0:99,
                               y0 = 1,
                               xi = 2,
                               period = 30,
                               intranoise = 0.2)


## ----plot example2,fig.width = 7, fig.height = 5, fig.align = "center"--------
gridExtra::grid.arrange(plot(data2a)+
               ggplot2::ggtitle("undamped, xi=0"), 
             plot(data2b)+
               ggplot2::ggtitle("critically damped, xi=1"), 
             plot(data2c)+
               ggplot2::ggtitle("overdamped, xi=2"), ncol= 3)

## ----analysis, example1-------------------------------------------------------
res1 <- analyze.2order(data = data1,
                        id = "id",
                        time ="time",
                        signal = "signal",
                        dermethod = "glla",
                        derparam = 13,
                        order = 2)

## ----analysis plot res1,fig.width = 7, fig.height = 6, fig.align = "center"----
plot(res1)

## ----print res1---------------------------------------------------------------
res1

## ----print res1 and components------------------------------------------------
res1$resultid

## ----2nd order with excitation example1---------------------------------------
time <- 0:100
data1 <- generate.panel.2order(time = time,
                               excitation = as.numeric(time>20),
                               xi = 0.1,
                               period = 30,
                               k = 1,
                               nind = 5)

## ----plot data1,fig.width = 7, fig.height = 6, fig.align = "center"-----------
plot(data1)

## ----example2-----------------------------------------------------------------
# Generation of signals with intra and inter-noise
time <- 0:100
data2 <- generate.panel.2order(time = time,
                               excitation = as.numeric(time>20),
                               xi = 0.1,
                               period = 30,
                               k = 1,
                               nind = 5,
                               internoise = 0.2,
                               intranoise = 0.3)

## ----plot data2,fig.width = 7, fig.height = 6, fig.align = "center"-----------
plot(data2)

## ----example3-----------------------------------------------------------------
time <- 0:99
data3 <- generate.panel.2order(time = time,
                         excitation = as.numeric(time>20),
                         xi = 0.3,
                         period = 30,
                         k = 1,
                         y0 = 2,
                         v0 = 1,
                         t0 = 15,nind = 1)
plot(data3)

## ----simulation plot example4, fig.width = 5, fig.height = 4, fig.align = "center"----
t <- 0:99
excitation <- 5*sin(2*pi*t/10)
driven_dlo <- generate.panel.2order(time = t, 
                              excitation = excitation, 
                              y0 = 10, 
                              xi = 0.2, 
                              period = 20,
                              nind = 1)
plot(driven_dlo)

## ----analysis example1--------------------------------------------------------
res1 <- analyze.2order(data = data1[id==1],
                      input = "excitation",
                      time ="time",
                      signal = "signal",
                      dermethod = "gold",
                      derparam = 3,
                      verbose=T)

## ----plot res1,fig.width = 6, fig.height = 4, fig.pos = 0.5, fig.align = "center"----
plot(res1)

## ----res2---------------------------------------------------------------------
res2 <- analyze.2order(data = data2,
                        id = "id",
                        input ="excitation",
                        time ="time",
                        signal = "signal",
                        dermethod = "gold",
                        derparam = 5,
                        order = 4)

## ----plot res2,fig.width = 7, fig.height = 6, fig.align = "center"------------
plot(res2)

## ----res3---------------------------------------------------------------------
res3 <- optimum_param (data=data2,
                      id="id",
                      input="excitation",
                      time="time",
                      signal="signal",
                      model = "2order",
                      dermethod = "glla",
                      order = 2,
                      pmin = 5,
                      pmax = 17,
                      pstep = 2)
res3$analysis
res3$summary_opt
res3$d

## ----plot res3, fig.width = 7, fig.height = 6, fig.align = "center"-----------
plot(res3)

## ----optimum analysis res3b---------------------------------------------------
res3b <- analyze.2order(data = data2,
                        id = "id",
                        input ="excitation",
                        time ="time",
                        signal = "signal",
                        dermethod = "glla",
                        derparam = res3$d,
                        order = 2)
res3b

## ----plot res3b,fig.width = 7, fig.height = 6, fig.align = "center"-----------
plot(res3b)

## ----example4-----------------------------------------------------------------
#Simulating data with these hypothesis
#Generating the three excitation signals:
time <- 1:100
u1 <- as.numeric(time < 20 & time > 10)
u2 <- as.numeric(time < 40 & time > 30)
u3 <- as.numeric(time < 80 & time > 70)
# Arbitrarily choosing a = 1, b = 2 and c = 5 for the first individual
et1 <- u1 + 3*u2 + 5*u3

y1 <- generate.2order(time = time,
                      excitation = et1)$y
#as we are using the $y argument of the object generated

#Signals for the second individual;
# Arbitrarily choosing a = 1, b = 2.5 and c = 4 for the second individual
et2 <- u1 + 2.5*u2 + 4*u3
y2 <- generate.2order(time = time,
                      excitation = et2)$y 

#Generating table with signals
dataa4 <- data.table::data.table(id = rep(c(1, 2), c(length(et1), length(et2))), 
                 time = c(time, time),
                 excitation1 = rep(u1,2),
                 excitation2 = rep(u2,2),
                 excitation3 = rep(u3,2),
                 signal_no_noise = c(y1, y2))
dataa4[,signal := signal_no_noise + rnorm(.N,0,0.5)]
dataa4[,excitation := excitation1 + excitation2 + excitation3]

## ----plot example4,fig.width = 7, fig.height = 4, fig.align = "center"--------
#Plotting signals
ggplot2::ggplot( data = dataa4) +
  ggplot2::geom_line(ggplot2::aes(time,signal_no_noise, colour = "Signal_no_noise"))+
  ggplot2::geom_point(ggplot2::aes(time,signal, colour = "Signal"))+
  ggplot2::geom_line(ggplot2::aes(time,excitation,colour = "Total excitation"))+
  ggplot2::facet_wrap(~id)+
  ggplot2::labs(x = "Time (s)",
           y = "Signal (arb. unit)",
           colour = "")

## ----res4---------------------------------------------------------------------
#Analyzing signals
res4 <- analyze.2order(data = dataa4,
                       id = "id",
                       input = c("excitation1", "excitation2", "excitation3"),
                       time = "time",
                       signal = "signal",
                       dermethod = "glla",
                       derparam = 7)

#Looking for the calculation of the coefficients of the excitation
res4
res4$resultid


## ----plot res4,fig.width = 7, fig.height = 4, fig.align = "center"------------
#Plotting signals
plot(res4)

