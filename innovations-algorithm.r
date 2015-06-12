###
### source("http://faculty.washington.edu/dbp/s519/R-code/innovations-algorithm.R")
###
### given a vector acvf of length h+1 with values of an autocovariance
### function (theoretical or sample) at lags 0, 1, ..., h, the function
### innovations.algorithm runs the innovations algorithm n.max times,
### where n.max is an integer less than or equal to h (if not supplied,
### n.max is set to h by default).  See below for an example of what
### this function returns.

innovations.algorithm <- function(acvf,n.max=length(acvf)-1)
  {
    thetas <- vector(mode="list",length=n.max)
    vs <- rep(acvf[1],n.max+1)
    for(n in 1:n.max)
      {
        thetas[[n]] <- rep(0,n)
        thetas[[n]][n] <- acvf[n+1]/vs[1]
        if(n>1)
          {
            for(k in 1:(n-1))
              {
                js <- 0:(k-1)
                thetas[[n]][n-k] <- (acvf[n-k+1] - sum(thetas[[k]][k-js]*thetas[[n]][n-js]*vs[js+1]))/vs[k+1]
              }
          }
        js <- 0:(n-1)
        vs[n+1] <- vs[n+1] - sum(thetas[[n]][n-js]^2*vs[js+1])
      }
    structure(list(vs=vs,thetas=thetas))
  }

### > AR2.acvf <- as.vector(ARMAacf(ar=c(3/4,-1/2),lag=3))*16/9
### > (results <- innovations.algorithm(AR2.acvf))
### $vs
### [1] 1.777778 1.333333 1.000000 1.000000
### 
### $thetas
### $thetas[[1]]
### [1] 0.5
### 
### $thetas[[2]]
### [1]  0.750 -0.125
### 
### $thetas[[3]]
### [1]  0.75000  0.06250 -0.34375
### 
### 
### > results$vs  # v_0, v_1, v_2, v_3
### [1] 1.777778 1.333333 1.000000 1.000000
### > results$thetas[[1]]  # theta_{1,1}
### [1] 0.5
### > results$thetas[[2]]  # theta_{2,1}, theta_{2,2}
### [1]  0.750 -0.125
### > results$thetas[[3]]  # theta_{3,1}, theta_{3,2}, theta_{3,3}
### [1]  0.75000  0.06250 -0.34375
