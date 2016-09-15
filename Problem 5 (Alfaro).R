#initial error found: "object 'bankAccounts' not found;


bankAccounts <- c(10, 9.2, 5.6, 3.7, 8.8, 0.5);
compounded <- rep(0, length(bankAccounts));

interestRate <- 0.0125;
for (i in 1:length(bankAccounts)) {
  compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i];
}
cat(compounded);

#HINT: variables must be initialized before you can perform operations on them
#HINT 2: look at the rep() function and see if you can use that to initialize a variable that will help you.