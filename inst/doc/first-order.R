## ----options, include=FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  fig.align="center",
  fig.width = 5,
  fig.height = 5,
  comment = "#>"
)

## ----libraries, setup---------------------------------------------------------
library(doremi)

## -----------------------------------------------------------------------------
test <- generate.1order(0:100,y0 = 10,tau = 10)
plot(test$t,test$y)

## ----simulation example1a-----------------------------------------------------
res1a <- generate.panel.1order(time = 0:99,
                               y0 = 1,
                               tau = 10,
                               nind = 4)

## ----res1a--------------------------------------------------------------------
res1a

## ----plot res_1a, fig.align="center", fig.height=6, fig.width=7---------------
plot(res1a)

## ----changing initial condition res1b-----------------------------------------
timevec <- 0:49
set.seed(123)
res1b <- generate.panel.1order(time = timevec,
                               excitation = as.numeric(timevec > 20),
                               y0 = 3,
                               tau = 5,
                               yeq = 1.5,
                               nind = 4)

## ----changing initial condition plot res1b,fig.width = 7, fig.height = 6, fig.align = "center"----
plot(res1b) + 
  ggplot2::scale_y_continuous(limits = c(0, 3))

## ----noise res2a--------------------------------------------------------------
# Generation of signals with intra and inter-noise
set.seed(123)
res2a <- generate.panel.1order(time = 0:49,
                               y0 = 1,
                               tau = 5,
                               nind = 6,
                               internoise = 0.4,
                               intranoise = 0.1)

## ----noise plot res2a, fig.width = 7, fig.height = 6, fig.align = "center"----
plot(res2a)

## ----analysis example3--------------------------------------------------------
#Simulating data with these hypothesis
set.seed(123)
data3 <- generate.panel.1order(time = 0:50,
                               y0 = 0.5,
                               tau = 10,
                               nind = 3,
                               internoise = 0.2,
                               intranoise = 0.1)

## ----analysis res3------------------------------------------------------------
#Analyzing

res3 <- analyze.1order(data = data3,
                      id = "id",
                      time = "time",
                      signal = "signal",
                      dermethod = "fda",
                      derparam = 0.7)

## -----------------------------------------------------------------------------
res3

## -----------------------------------------------------------------------------
res3$resultid

## ----analysis plot res3,fig.width = 7, fig.height = 6, fig.align = "center"----
#Plotting
plot(res3)

## -----------------------------------------------------------------------------
set.seed(123)
U <- generate.excitation(nexc = 3, # number of square pulses
                           duration = 10, # pulse duration
                           deltatf = 1, # time spacing between points
                           tmax = 100, # maximum time
                           minspacing = 20) # minimum spacin between square pulses
plot(U$t,U$exc)

## ----excitation term example1a,fig.width = 7, fig.height = 6, fig.align = "center"----

res1a <- generate.panel.1order(time = U$t,
                               excitation = U$exc,
                               tau = 10,
                               k = 1,
                               nind = 4)
plot(res1a)

## ----changing initial condition-----------------------------------------------

res1b <- generate.panel.1order(time = U$t,
                               excitation = U$exc,
                               y0 = 5,
                               tau = 10,
                               k = 3,
                               yeq = 2,
                               nind = 4)

## ----plot res1b,fig.width = 7, fig.height = 6, fig.align = "center"-----------
plot(res1b)

## ----example2-----------------------------------------------------------------
# Generation of signals with intra and inter-noise
res2a <- generate.panel.1order(time = U$t,
                               excitation = U$exc,
                               tau = 10,
                               k = 1,
                               nind = 4,
                               yeq = 2,
                               y0 = 2,
                               internoise = 0.2,
                               intranoise = 2)

## ----plot res2a,fig.width = 7, fig.height = 6, fig.align = "center"-----------
plot(res2a)

## ----example3-----------------------------------------------------------------
data3 <- res2a[id==1]

## ----res3---------------------------------------------------------------------
#Analyzing
res3 <- analyze.1order(data = data3,
                      input = "excitation",
                      time ="time",
                      signal = "signal",
                      verbose=T)
res3

## ----plot res3,fig.width = 6, fig.height = 4, fig.pos = 0.5, fig.align = "center"----
#Plotting 
plot(res3)

## ----analysis res3a-----------------------------------------------------------
res3a <- analyze.1order(data = res2a,
                        id = "id",
                        input ="excitation",
                        time ="time",
                        signal = "signal",
                        dermethod = "gold",
                        derparam = 3)

## ----analysis res3a print-----------------------------------------------------
res3a 

## ----summary res3a------------------------------------------------------------
summary(res3a) 

## ----head res3a---------------------------------------------------------------
head(res3a$data)

## ----components of res3a------------------------------------------------------
res3a$regression

## ----resultmean---------------------------------------------------------------
res3a$resultmean

## ----resultid-----------------------------------------------------------------
res3a$resultid

## ----plot res3a,fig.width = 7, fig.height = 6, fig.align = "center"-----------
plot(res3a)

## ----more plot res3a,fig.width = 7, fig.height = 6, fig.align = "center"------
plot(res3a, id = 3)
plot(res3a, id = c(1,4))

## ----res3b--------------------------------------------------------------------
res3b <- optimum_param (data = res2a,
                      id = "id",
                      input = "excitation",
                      time = "time",
                      signal = "signal",
                      model = "1order",
                      dermethod = "gold",
                      pmin = 3,
                      pmax = 21,
                      pstep = 2)

res3b$summary_opt
res3b$d

## ----plot res3b,fig.width = 7, fig.height = 6, fig.align = "center"-----------
plot(res3b)

## ----example4-----------------------------------------------------------------
#Simulating data with these hypothesis
#Generating the three excitation signals:
t <- 0:100
u1 <- as.numeric(t>20 & t<40)
u2 <- as.numeric(t>50 & t<70)
u3 <- as.numeric(t>80 & t<100)
# Arbitrarily choosing a = 1, b = 2 and c = 5 for the first individual
et1 <- u1 + 3 * u2 + 5 * u3
y1 <- generate.1order(time = t,
                      excitation = et1,
                      tau = 10,
                      k = 1)$y
#as we are using the $y argument of the object generated

#Signals for the second individual;
# Arbitrarily choosing a = 1, b = 2.5 and c = 4 for the second individual
et2 <- u1 + 2.5 * u2 + 4 * u3
y2 <- generate.1order(time = t,
                      excitation = et2,
                      tau = 10,
                      k = 1)$y 

#Generating a table with the signals
data4 <- data.frame(id = rep(c(1, 2), c(length(et1), length(et2))), 
                 time = c(t, t),
                 excitation1 = c(u1, u1),
                 excitation2 = c(u2, u2),
                 excitation3 = c(u3, u3),
                 signalcol = c(y1, y2))

## ----plot example4,fig.width = 7, fig.height = 4, fig.align = "center"--------
#Plotting signals
ggplot2::ggplot( data = data4) +
  ggplot2::geom_line(ggplot2::aes(time,signalcol, colour = "Signal-no noise"))+
  ggplot2::geom_line(ggplot2::aes(time,excitation1,colour = "excitation 1"))+
  ggplot2::geom_line(ggplot2::aes(time,excitation2,colour = "excitation 2"))+
  ggplot2::geom_line(ggplot2::aes(time,excitation3,colour = "excitation 3"))+
  ggplot2::facet_wrap(~id)+
  ggplot2::labs(x = "Time (s)",
           y = "Signal (arb. unit)",
           colour = "")

## ----res4---------------------------------------------------------------------
#Analyzing signals
res4 <- analyze.1order(data = data4,
                       id = "id",
                       input = c("excitation1", "excitation2", "excitation3"),
                       time = "time",
                       signal = "signalcol",
                       dermethod = "fda",
                       derparam = 0.1)

#Looking for the calculation of the coefficients of the excitation

res4$resultid


## ----fig.width = 7, fig.height = 4, fig.align = "center"----------------------
#Plotting signals
plot(res4)

## ----example5-----------------------------------------------------------------
t <- 0:200
set.seed(123)
data5 <- generate.panel.1order(time = t,
                               excitation = as.numeric(t>50 & t<100),
                               tau = 10,
                               k = 1,
                               nind = 6,
                               internoise = 0.4,
                               intranoise = 0.1)

## ----plot data5, fig.width = 7, fig.height = 6, fig.align = "center"----------
plot(data5)

## ----missing data-------------------------------------------------------------
#Keeping one third of the rows selected randomly from the full data set
set.seed(123)
data5rd <- data5[sample(nrow(data5), nrow(data5)/3), ]
data5rd <- data5rd[order(id,time)]

## ----plot missing data,fig.width = 7, fig.height = 6, fig.align = "center"----
plot(data5rd)

## ----res7---------------------------------------------------------------------
res7 <- analyze.1order(data = data5rd,
                       id = "id",
                       input = "excitation",
                       time ="time",
                       signal = "signal")

## ----plot res7,fig.width = 7, fig.height = 6, fig.align = "center"------------
plot(res7)+
  ggplot2::geom_line(data = data5,
                     ggplot2::aes(time,signalraw, colour = "Original signal"))

## ----cardio data--------------------------------------------------------------
resc1a <- analyze.1order(data = cardio,
                 id = "id",
                 input = "load",
                 time ="time",
                 signal = "hr",
                 dermethod = "glla",
                 derparam = 5)

## ----plot cardio data,fig.width = 7, fig.height = 16, fig.align = "center"----
plot(resc1a, id = 1:21) + 
  ggplot2::facet_wrap(~id,ncol=3,scales="free")

## ----cardio multiple excitations generate-------------------------------------
mydata <- cardio[id %in% 1:10]
# create a index indicate which step of the exercise test 
mydata[,load_idx := data.table::rleid(load),by = id]
# transforming to large format, to have one column per workload step
mydata_large <- data.table::dcast(id + time + hr  ~ paste0("load_",load_idx),data = mydata,value.var = "load")
# replacing NAs by 0s
load_cols <- paste0("load_",1:max(mydata$load_idx))
mydata_large[,c(load_cols) := lapply(.SD,function(col) data.table::fifelse(is.na(col),0,col)),.SDcols = load_cols]

head(mydata_large)

## ----cardio multiple excitations----------------------------------------------
# analyzing
resc1b <- analyze.1order(data = mydata_large,
                        id = "id",
                        input = load_cols,
                        time = "time",
                        signal = "hr",
                        dermethod = "gold",
                        derparam = 5)

## -----------------------------------------------------------------------------
resc1b

## ----plot cardio mult excitations,fig.width = 7, fig.height = 16, fig.align = "center"----
plot(resc1b,id=1:10)+ ggplot2::facet_wrap(~id,ncol=3,scales="free")

## ----mental rotation data-----------------------------------------------------
dermethod<- "fda"
pmin = 0.1
pmax = 1
pstep = 0.1

restemp <- optimum_param(data = rotation,
                          id = "id",
                          time ="days",
                          signal = "meanRT",
                          dermethod = dermethod,
                          model = "1order",
                          pmin = pmin,
                          pmax = pmax,
                          pstep = pstep)
restemp$summary_opt
resc2a <- analyze.1order(data = rotation,
                 id = "id",
                 time ="days",
                 signal = "meanRT",
                 dermethod = dermethod,
                 derparam = restemp$d)

## ----plot menta rotation data,fig.width = 7, fig.height = 6, fig.align = "center"----
plot(resc2a, id = 1:17)

