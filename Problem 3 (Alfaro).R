dogs <- 10;
for (i in 1:5){
  dogs <- dogs + 1; 
}
#dogs: initial = 10, final = 15

meatloaf <- 0; 
for (i in 5:9){
  meatloaf <- meatloaf - i + 1;
  cat(meatloaf) 
}
#meatloaf: initial = 0, final = -4 -9 -15 -22 -30

bubbles <- 12;
for (i in -1:-4){
  bubbles <- i;
}
#bubbles: initial = 12, final = -4