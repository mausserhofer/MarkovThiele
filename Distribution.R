
# berechne VAR
source("markovFunctions.R")
prob <- recDist(currentAge, "alive", riskValue)
print(prob)

