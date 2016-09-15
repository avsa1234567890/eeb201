#create a function that outputs 3 responses based on the value of the input

takesNumber <- function(num){
  if(num <= -1){
    cat("small");
  }
  else if((num > -1) && (num < 1)){
    cat("medium");
  }
  else if (num >= 1){
    cat("big");
  }
}

takesNumber(1)