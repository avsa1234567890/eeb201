get_heights <- function(num){
  heights <- rnorm(num, mean = 69, sd = 10);
  heightsMean <- mean(heights, trim = 0, na.rm = FALSE);
  return(heightsMean);
}

#create a for-loop for a sample size of 100

mean_heights_100 <- numeric(1000);
for(counter in 1:1000){
  mean_heights_100[counter] <- get_heights(100);
}

#create a for-loop for a sample size of 1000

mean_heights_1000 <- numeric(1000);
for(counter in 1:1000){
  mean_heights_1000[counter] <- get_heights(1000);
}

mean_heights_100bin <- hist(mean_heights_100, breaks = seq(65,73,by=0.5))$counts
mean_heights_1000bin <- hist(mean_heights_1000, breaks = seq(65,73,by=0.5))$counts
pdf(file= "both_barplot.pdf", width=6,height=6)
par(mfrow=c(1,1), mar=c(4, 4, 3, 2))

#note, sequence ends at 72 and not 73 becuase the sequence is asking for the number of "buckets" to put data in; the range above was for the individual borders...
barplot(rbind(mean_heights_100bin, mean_heights_1000bin), col=c(2,4), beside = T, names.arg = seq(65,72.5,by=0.5), xlab="Average height (inches)",ylab="Count")

#make the legend after you make the barplot
legend(0, 300, c("n = 100", "n = 1000"), fill = c("red", "blue"));
#shut down
dev.off()

