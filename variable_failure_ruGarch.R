library(rugarch) 

set.seed(1) 

x1 <- rnorm(1000,5,1) 
x2 <- rnorm(1000,3,3) 

y    <- .5*(x1*x2) + rnorm(1000,1,3) 
dat  <- data.frame(x1,x2,y) 

var1 <- c("x1","x2") 
var2 <- c("x2","x1") 

# setbounds(spec)<-list(vxreg1=c(-1,1)) 
model_maker <- function(x_name){ 
  temp <- dat[,c("y",x_name)] 

  spec <- ugarchspec(variance.model      = list(model = "sGARCH", 
                                                garchOrder = c(1,0)), 

                     mean.model          = list(armaOrder = c(2,2), 
                                                external.regressors = as.matrix(temp[,x_name]), 
                                                include.mean= T), 

                     distribution.model  = "std") 

  fit         <- ugarchfit(spec = spec, data = as.matrix(temp$y),solver = "hybrid") 
  return(fit@fit$robust.matcoef)} 

model_maker(var1) 
model_maker(var2)
