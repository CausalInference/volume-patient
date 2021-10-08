
results <- function(){
  install.packages("splines"); library(splines)
  
  # Formulas and functions
  source("functions-formulas.R")
  
  # Models
  m1 <- glm(data=pancreas,
            family="binomial",
            formula=as.formula(paste0("death90 ~ ", formula.model1))
  )
  
  m2 <- glm(data=pancreas,
            family="binomial",
            formula=as.formula(paste0("death90 ~ ", formula.model2))
  )
  m3 <- glm(data=pancreas,
            family="binomial",
            formula=as.formula(paste0("death90 ~ ", formula.model3))
  )
  m4 <- glm(data=pancreas,
            family="binomial",
            formula=as.formula(paste0("death90 ~ ", formula.model4))
  )
  
  # Estimates
  vector.svol = c(2, as.numeric(quantile(pancreas$surgeon.volume, probs=c(0.5, 0.9, 0.95))))
  vector.hvol = c(2, as.numeric(quantile(pancreas$hospital.volume, probs=c(0.5, 0.9))))
  results <- list()
  results$trial1.outcomes.table <- outcome.table.fun(vector.svol = vector.svol, 
                                             vector.hvol = NULL, 
                                             model=m1, dataset=pancreas,
                                             intervention="surgeon.only", dynamic=FALSE)
  results$trial2.outcomes.table <- outcome.table.fun(vector.svol = vector.svol,
                                             vector.hvol = vector.hvol,
                                             model=m2, dataset=pancreas,
                                             intervention="joint", dynamic=FALSE)
  results$trial3.outcomes.table <- outcome.table.fun(vector.svol = vector.svol,
                                             vector.hvol = vector.hvol,
                                             model=m3, dataset=pancreas,
                                             intervention="joint", dynamic=TRUE)
  results$trial4.outcomes.table <- outcome.table.fun(vector.svol = vector.svol,
                                             vector.hvol = vector.hvol,
                                             model=m4, dataset=pancreas,
                                             intervention="joint", dynamic=TRUE)
  return(results)
}

results()

