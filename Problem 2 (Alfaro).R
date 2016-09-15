#for loop that prints the following: "*&*&*&*&*&*&*&*&*&*"

for (addAmpersand in 1:10){
  if(addAmpersand == 10){
    cat("*");
  }
  else {
    cat("*&");
  }
  }