# ATIVIDADE 01 - IND 2622

library(data.table)
library(dplyr)

df <- read.csv("/Users/brandao/Documents/Mestrado/I SEMESTRE/Statistical Learning/Auto")
df <- read.csv("ds_salaries.csv")

head(df)
tail(df)

View(df)

Y = length(df$salary_in_usd)

#APLICAÇÃO DE UMA TÉCNICA DE AMOSTRAGEM (BOOTSTRAP)
transpose(sample(transpose(df), size = 30, replace = TRUE))

#APLICAR TÉCNICA DE DISCRETIZAÇÃO 

QT25 <- quantile(df$salary_in_usd) [2]
QT50 <- quantile(df$salary_in_usd) [3]
QT75 <- quantile(df$salary_in_usd) [4]

for (i in 1:Y) {
  if (df$salary_in_usd[i] <= QT25) {
    df$salary_in_usd_disc[i] <- 1
  } else if (df$salary_in_usd[i] <= QT50 && df$salary_in_usd[i] > QT25) {
    df$salary_in_usd_disc[i] <- 2
  } else if (df$salary_in_usd[i] <= QT75 && df$salary_in_usd[i] > QT50) { 
    df$salary_in_usd_disc <- 3
  } else if (df$salary_in_usd[i] > QT75) {
    df$salary_in_usd_disc[i] <-4
  }


  head (df)
  
  #TÉCNICA DE FEATURE ENGINEERING
  
  ano=2016
  mes = 04
  
  dmes <- c ()
  for ( i in 1:Y) {
    dmes[i] = ((df$work_year[i] - ano) * 12) + (df$work_month[i]- mes + 1)
  }
  
  df$dmes = dmes
  
  tail(df)
  
  
