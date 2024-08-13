#Question 1
Durango <- read.csv('DUR.csv')# Reads data from 'DUR.csv' and stores it in the Durango variable
zcfbyhs<- read.csv('zcfbyhs.csv')# Reads data from 'zcfbyhs.csv' and stores it in the zcfbyhs variable

gi <- 0.5# Assigns the value 0.5 to the variable gi
gs <- 1# Assigns the value 1 to the variable gs
t <- 31.4e6# Assigns the value 31.4 million to the variable t in terms of year
pd <- 300000# Assigns the value 300,000 to the variable pd in terms of per cm^-2
λ <- log(2)/4.47e9# Calculates the decay constant and assigns it to the variable λ in terms of per year
ps1 <- (Durango$Ns)# Extracts the values from the 'Ns' column of Durango and stores them in ps1
pi1 <- (Durango$Ni)# Extracts the values from the 'Ni' column of Durango and stores them in pi1

S <- (exp(t*λ)-1)/((gi/gs)*λ*pd*(ps1/pi1)) #Calculates the variable S (Calbration factor) in terms of year*cm^2

print("Calibration Factor (S):")
print(S)# Prints the value of S (Calibration factor) in terms of year*cm^2

#Question 2
m <- mean(S)# Calculates the mean of S (Calibration factor) and stores it in the variable m
ps2 <- (zcfbyhs$Ns)# Extracts the values from the 'Ns' column of zcfbyhs and stores them in ps2
pi2 <- (zcfbyhs$Ni)# Extracts the values from the 'Ni' column of zcfbyhs and stores them in pi2

age_of_zcfbyhs = (1/λ)*log(1+(gi/gs)*λ*m*pd*(ps2/pi2)) #Calculates the age_of_zcfbyhs in terms of years
mean_age_of_zcfbyhs = mean(age_of_zcfbyhs) # Calculates the mean of age_of_zcfbyhs and stores it in mean_age_of_zcfbyhs in terms of years
print("Age of zcfbyhs:")
print(age_of_zcfbyhs)# Prints the values of age_of_zcfbyhs
print("mean age of zcfbyhs:")
print(mean_age_of_zcfbyhs)

#Question 3
uncertainty_Ns <- sqrt(ps2)# Calculates the square root of ps2 and stores it in uncertainty_Ns
uncertainty_Ni <- sqrt(pi2)# Calculates the square root of pi2 and stores it in uncertainty_Ni

analytic_uncertainty <- age_of_zcfbyhs*sqrt((uncertainty_Ni/pi2)^2 + (uncertainty_Ns/ps2)^2)# Calculates analytic_uncertainty
print("Analytic Uncertainty:")
print(analytic_uncertainty)

#Question 4
standard_deviation_of_zcfbyhs <- sd(age_of_zcfbyhs)# Calculates the standard deviation of age_of_zcfbyhs and stores it in standard_deviation_of_zcfbyhs
standard_error_of_zcfbyhs <- standard_deviation_of_zcfbyhs/sqrt(length(age_of_zcfbyhs))# Calculates the standard error of age_of_zcfbyhs and stores it in standard_error_of_zcfbyhs

percentage_of_uncertainty_U238Pb206 <- (13.20389/1066.5638)*100 # Calculates the percentage of uncertainty for U238Pb206
percentage_of_uncertainty_U235Pb207 <- (15.79126/1064.37509)*100 # Calculates the percentage of uncertainty for U235Pb207
percentage_of_uncertainty_40Ar39Ar <- (0.1438594/100.076)*100 # Calculates the percentage of uncertainty for 40Ar39Ar
percentage_of_uncertainty_age <- (analytic_uncertainty/age_of_zcfbyhs)*100 # Calculates the percentage of uncertainty for age
percentage_of_standard_error_over_mean_age <- (standard_error_of_zcfbyhs/mean_age_of_zcfbyhs)*100 # Calculates the percentage of standard error over mean age

print("Percentage of uncertainty for U238Pb206:")
print(percentage_of_uncertainty_U238Pb206)
print("Percentage of uncertainty for U235Pb207:")
print(percentage_of_uncertainty_U235Pb207)
print("Percentage of uncertainty for 40Ar39Ar:")
print(percentage_of_uncertainty_40Ar39Ar)
print("Percentage of uncertainty for age of zcfbyhs:")
print(percentage_of_uncertainty_age)
print("Percentage of standard error over mean age of zcfbyhs:")
print(percentage_of_standard_error_over_mean_age)

# The relative precision of analytical methods can be inferred by examining the ratio of analytic uncertainty to age, with lower values indicating greater precision. In this context, the 40Ar/39Ar and U-Pb methods exhibit significantly lower uncertainty percentages compared to the fission track method, as evident from the calculated values.The precision ranking of the methods due to percentage uncertainty (40Ar/39Ar > U-Pb > fission track)
# Moreover, a comparison of the standard error across these methods 40Ar/39Ar, U-Pb, and fission track—reveals potential discrepancies in precision, with 40Ar/39Ar displaying superior precision followed by U-Pb and then fission track. The standard error serves as a measure of the variability or precision inherent in the mean age derived from multiple measurements. Notably, the higher standard error associated with the fission track method implies lesser precision in determining the mean age from fission track data.The precision ranking of the methods due to percentage stand error (40Ar/39Ar > U-Pb > fission track)
# In conclusion, the substantial difference in precision observed between the fission track method and the 40Ar/39Ar and U-Pb methods may be attributed to the smaller size of the data sample used in the fission track calculations compared to those utilized in the 40Ar/39Ar and U-Pb analyses.


