#Change the code to clarify presidential vs. congressional elections.

years <- c( 2015, 2016, 2018, 2020, 2021)
for(ii in 1:length(years)){
  if(years[ii] %% 4 == 0){
    cat(years[ii], 'Hooray, presidential and congressional elections!', sep = '\t', fill = T)
  }
  else if (years[ii] %% 2 == 0){
    cat(years[ii], 'Hooray, congressional elections!', sep = '\t', fill = T)
  }
}