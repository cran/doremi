## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.align="center",
  fig.width = 5,
  fig.height = 5,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(doremi)

## -----------------------------------------------------------------------------
timevec <- seq(-5,5,0.2)
noisevec <- rnorm(length(timevec),0,sd = 1)
signal <- timevec^2
signal_noise <- signal + noisevec

plot(timevec,signal_noise)

## -----------------------------------------------------------------------------
der_est <- calculate.gold(time = timevec,
               signal = signal_noise,
               embedding = 9,
               n = 2)

plot(der_est$dtime,
     der_est$dsignal[,2]
     ,xlab = "time",
     ylab = "first derivative")
lines(timevec,2*timevec,col = 2)

## -----------------------------------------------------------------------------
conditions <- data.table::CJ(embedding = 3:10,
                 method = c("GLLA","GOLD","FDA"),
                 der = 0:2)
derivative_example <- conditions[,.(time = timevec),by = .(embedding,method,der)]
derivative_example[,signal := time^2]
derivative_example[,signal_noise := signal + noisevec]

ggplot2::ggplot(derivative_example)+
  ggplot2::geom_point(ggplot2::aes(time,signal_noise))+
  ggplot2::theme_bw()+
  ggplot2::labs(y = "y", 
               x = "t",
               title = "example variable for the estimation of derivatives")

## -----------------------------------------------------------------------------
# assigning the true derivative for comparison
derivative_example[der  == 1,truder := 2*time]
derivative_example[der  == 2,truder := 2]
derivative_example[der  == 0,truder := signal]

## -----------------------------------------------------------------------------
derivative_example[method == "GOLD",
                   derivate := calculate.gold(time = time,
                                              signal = signal_noise,
                                              embedding = embedding,
                                              n = 2)$dsignal[,(der[1]+1)],
                   by =  .(embedding,der)]

## -----------------------------------------------------------------------------
# set the corresponding derivative time
derivative_example[method == "GOLD",
                   timeder := calculate.gold(time = time,
                                             signal = signal_noise,
                                             embedding = embedding,n = 2)$dtime,
                   by =  .(embedding,der)]

## ----fig.height=7,fig.width=6-------------------------------------------------

# calculation of the derivatives with calculate.glla
derivative_example[method == "GLLA",
                   derivate := calculate.glla(time = time,
                                              signal = signal_noise,
                                              embedding = embedding,
                                              n = 2)$dsignal[,(der[1]+1)],
                   by =  .(embedding,der)]
derivative_example[method == "GLLA",
                   timeder := calculate.glla(time = time,
                                             signal = signal_noise,
                                             embedding = embedding,
                                             n = 2)$dtime,
                   by =  .(embedding,der)]
# calculation of the derivatives with calculate.fda
derivative_example[,spar :=( embedding-3 )/7]
derivative_example[method == "FDA",
                   derivate := calculate.fda(time = time,
                                             signal = signal_noise,
                                             spar = spar)$dsignal[,(der[1]+1)],
                   by = .(spar,der)]
derivative_example[method == "FDA",
                   timeder := calculate.fda(time = time,
                                            signal = signal_noise,
                                            spar = spar)$dtime,
                   by = .(spar,der)]

derivative_example[,derlegend := factor(der,levels = 0:2,labels = c("0th order","first order","second order"))]
# plot of the results
ggplot2::ggplot(derivative_example)+
  ggplot2::geom_line(ggplot2::aes(timeder,derivate,color = as.factor(embedding)),size = 0.8)+
  ggplot2::geom_line(ggplot2::aes(time,truder),color = "black",size = 0.1)+
  ggplot2::facet_grid(derlegend~method,scales = "free")+
  ggplot2::labs(color = "Embedding",
       x = "time",
       y = "")+
  ggplot2::scale_color_viridis_d(option = "C")+
  ggplot2::theme_bw()

