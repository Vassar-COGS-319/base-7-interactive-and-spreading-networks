# https://waltervanheuven.net/jiam/

decay <- 0.07
max.activation <- 1.0
min.activation <- -0.2
threshold <- 0

feature.to.digit.weight <- 0.3
digit.to.digit.weight <- -0.2

weights <- matrix(c(
  c(0,0, feature.to.digit.weight, feature.to.digit.weight, 0, 0, 0, 0, digit.to.digit.weight
  c(0,0,0,1),
  c(0,0,0,-1),
  c(0,0,-1,0)
), byrow=T, nrow=4)

activations <- rep(0, 4)

input <- c(1,1,0,0)

cycle <- function(input, activations){
  
  current.activity <- pmin(max.activation, activations + input)
  
  net.i <- current.activity %*% weights
  
  delta <- rep(0, length(activations))
  
  for(i in 1:length(activations)){
    if(net.i[i] > 0){
      delta[i] <- (max.activation - activations[i])*net.i[i] - decay*activations[i]
    } else {
      delta[i] <- (activations[i] - min.activation)*net.i[i] - decay*activations[i]
    }
  }
  
  return(activations + delta)
}  

net.a <- activations
for(i in 1:100){
  net.a <- cycle(input, net.a)
  print(net.a)
}


  