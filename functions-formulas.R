
# Formulas
formula.model1 <- "female  + 
ns(age, knots=quantile(age, probs=c(0.35, 0.65)), Boundary.knots=quantile(age, probs=c(0.05,0.95))) + 
race + year_source + inpatient + comorb_ami + comorb_dementia + comorb_afib + comorb_ckd + 
comorb_copd + comorb_chf + comorb_diabetes + comorb_cad + comorb_stroketia + 
ns(hospital.volume, knots=quantile(hospital.volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(hospital.volume, probs=c(0.05,0.95))) +
ns(surgeon.volume, knots=quantile(surgeon.volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(surgeon.volume, probs=c(0.05,0.95)))"
formula.model2 <- "female  + 
ns(age, knots=quantile(age, probs=c(0.35, 0.65)), Boundary.knots=quantile(age, probs=c(0.05,0.95))) + 
race + year_source + inpatient + comorb_ami + comorb_dementia + comorb_afib + comorb_ckd + 
comorb_copd + comorb_chf + comorb_diabetes + comorb_cad + comorb_stroketia + 
ns(hospital.volume, knots=quantile(hospital.volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(hospital.volume, probs=c(0.05,0.95))) +
ns(surgeon.volume, knots=quantile(surgeon.volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(surgeon.volume, probs=c(0.05,0.95)))"
formula.model3 <- "female  + 
ns(age, knots=quantile(age, probs=c(0.35, 0.65)), Boundary.knots=quantile(age, probs=c(0.05,0.95))) + 
race + year_source + inpatient + comorb_ami + comorb_dementia + comorb_afib + comorb_ckd + 
comorb_copd + comorb_chf + comorb_diabetes + comorb_cad + comorb_stroketia + 
ns(hospital.volume, knots=quantile(hospital.volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(hospital.volume, probs=c(0.05,0.95))) +
ns(surgeon.volume, knots=quantile(surgeon.volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(surgeon.volume, probs=c(0.05,0.95)))"
formula.model4 <- "female  + 
ns(age, knots=quantile(age, probs=c(0.35, 0.65)), Boundary.knots=quantile(age, probs=c(0.05,0.95))) + 
race + year_source + inpatient + comorb_ami + comorb_dementia + comorb_afib + comorb_ckd + 
comorb_copd + comorb_chf + comorb_diabetes + comorb_cad + comorb_stroketia + 
ns(hospital.volume, knots=quantile(hospital.volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(hospital.volume, probs=c(0.05,0.95))) +
ns(surgeon.volume, knots=quantile(surgeon.volume, probs=c(0.35, 0.65)), Boundary.knots=quantile(surgeon.volume, probs=c(0.05,0.95))) +
ns(hosp.totalbeds, knots=quantile(hosp.totalbeds, probs=c(0.35, 0.65)), Boundary.knots=quantile(hosp.totalbeds, probs=c(0.05,0.95))) +
hosp.teaching + hosp.controltype + hosp.cicu + hosp.p_medicare +
ns(surgeon.age, knots=quantile(surgeon.age, probs=c(0.35, 0.65)), Boundary.knots=quantile(surgeon.age, probs=c(0.05,0.95))) + 
md_female + mmem"



# Outcome function for all target trials and sensitivity analyses
outcome.table.fun <- function(vector.svol, vector.hvol, model, dataset,
                              intervention="joint", dynamic){
  if (intervention=="joint"){
    pred.matrix <- expand.grid(svol=vector.svol, hvol=vector.hvol)
    if (dynamic==TRUE){
      trial.outcomes <- data.frame(pred.matrix, death90=sapply(1:dim(pred.matrix)[1], function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(surgeon.volume=case_when(max.close.surgeon.volume_num_90 >= pred.matrix[x,]$svol ~ pred.matrix[x,]$svol,
                                                                         max.close.surgeon.volume_num_90 < pred.matrix[x,]$svol ~ max.close.surgeon.volume_num_90,
                                                                         is.na(max.close.surgeon.volume_num_90) ~ surgeon.volume),
                                                hospital.volume=case_when(max.close.hospital.volume_num_90 >= pred.matrix[x,]$hvol ~ pred.matrix[x,]$hvol,
                                                                          max.close.hospital.volume_num_90 < pred.matrix[x,]$hvol ~ max.close.hospital.volume_num_90,
                                                                          is.na(max.close.hospital.volume_num_90) ~ hospital.volume)
                     )))
      }))
    }
    if (dynamic==FALSE){
      trial.outcomes <- data.frame(pred.matrix, death90=sapply(1:dim(pred.matrix)[1], function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(surgeon.volume=(pred.matrix[x,]$svol),
                                                hospital.volume=(pred.matrix[x,]$hvol))))
      }))
    }
  }
  
  if (intervention=="surgeon.only"){
    pred.matrix.svol <- vector.svol
    if (dynamic==TRUE){
      trial.outcomes <- data.frame(pred.matrix.svol, death90=sapply(1:length(pred.matrix.svol), function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(surgeon.volume=case_when(max.close.surgeon.volume_num_90 >= pred.matrix.svol[x] ~ pred.matrix.svol[x],
                                                                         max.close.surgeon.volume_num_90 < pred.matrix.svol[x] ~ max.close.surgeon.volume_num_90,
                                                                         is.na(max.close.surgeon.volume_num_90) ~ surgeon.volume)
                     )))
      }))
    }
    if (dynamic==FALSE){
      trial.outcomes <- data.frame(pred.matrix.svol, death90=sapply(1:length(pred.matrix.svol), function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(surgeon.volume=pred.matrix.svol[x])))
      }))
    }
  }
  
  if (intervention=="hospital.only"){
    pred.matrix.hvol <- vector.hvol
    if (dynamic==TRUE){
      trial.outcomes <- data.frame(pred.matrix.hvol, death90=sapply(1:length(pred.matrix.hvol), function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(hospital.volume=case_when(max.close.hospital.volume_num_90 >= pred.matrix.hvol[x] ~ pred.matrix.hvol[x],
                                                                          max.close.hospital.volume_num_90 < pred.matrix.hvol[x] ~ max.close.hospital.volume_num_90,
                                                                          is.na(max.close.hospital.volume_num_90) ~ hospital.volume)
                     )))
      }))
    }
    if (dynamic==FALSE){
      trial.outcomes <- data.frame(pred.matrix.hvol, death90=sapply(1:length(pred.matrix.hvol), function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(hospital.volume=pred.matrix.hvol[x])))
      }))
    }
  }
  return(trial.outcomes)
}






outcome.table.fun.sens_180 <- function(vector.svol, vector.hvol, model, dataset,
                              intervention="joint", dynamic){
  if (intervention=="joint"){
    pred.matrix <- expand.grid(svol=vector.svol, hvol=vector.hvol)
    if (dynamic==TRUE){
      trial.outcomes <- data.frame(pred.matrix, death90=sapply(1:dim(pred.matrix)[1], function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(surgeon.volume=case_when(max.close.surgeon.volume_num_180 >= pred.matrix[x,]$svol ~ pred.matrix[x,]$svol,
                                                                         max.close.surgeon.volume_num_180 < pred.matrix[x,]$svol ~ max.close.surgeon.volume_num_180,
                                                                         is.na(max.close.surgeon.volume_num_180) ~ surgeon.volume),
                                                hospital.volume=case_when(max.close.hospital.volume_num_180 >= pred.matrix[x,]$hvol ~ pred.matrix[x,]$hvol,
                                                                          max.close.hospital.volume_num_180 < pred.matrix[x,]$hvol ~ max.close.hospital.volume_num_180,
                                                                          is.na(max.close.hospital.volume_num_180) ~ hospital.volume)
                     )))
      }))
    }
    if (dynamic==FALSE){
      trial.outcomes <- data.frame(pred.matrix, death90=sapply(1:dim(pred.matrix)[1], function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(surgeon.volume=(pred.matrix[x,]$svol),
                                                hospital.volume=(pred.matrix[x,]$hvol))))
      }))
    }
  }
  
  if (intervention=="surgeon.only"){
    pred.matrix.svol <- vector.svol
    if (dynamic==TRUE){
      trial.outcomes <- data.frame(pred.matrix.svol, death90=sapply(1:length(pred.matrix.svol), function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(surgeon.volume=case_when(max.close.surgeon.volume_num_180 >= pred.matrix.svol[x] ~ pred.matrix.svol[x],
                                                                         max.close.surgeon.volume_num_180 < pred.matrix.svol[x] ~ max.close.surgeon.volume_num_180,
                                                                         is.na(max.close.surgeon.volume_num_180) ~ surgeon.volume)
                     )))
      }))
    }
    if (dynamic==FALSE){
      trial.outcomes <- data.frame(pred.matrix.svol, death90=sapply(1:length(pred.matrix.svol), function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(surgeon.volume=pred.matrix.svol[x])))
      }))
    }
  }
  
  if (intervention=="hospital.only"){
    pred.matrix.hvol <- vector.hvol
    if (dynamic==TRUE){
      trial.outcomes <- data.frame(pred.matrix.hvol, death90=sapply(1:length(pred.matrix.hvol), function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(hospital.volume=case_when(max.close.hospital.volume_num_180 >= pred.matrix.hvol[x] ~ pred.matrix.hvol[x],
                                                                          max.close.hospital.volume_num_180 < pred.matrix.hvol[x] ~ max.close.hospital.volume_num_180,
                                                                          is.na(max.close.hospital.volume_num_180) ~ hospital.volume)
                     )))
      }))
    }
    if (dynamic==FALSE){
      trial.outcomes <- data.frame(pred.matrix.hvol, death90=sapply(1:length(pred.matrix.hvol), function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(hospital.volume=pred.matrix.hvol[x])))
      }))
    }
  }
  return(trial.outcomes)
}



outcome.table.fun.sens_360 <- function(vector.svol, vector.hvol, model, dataset,
                                       intervention="joint", dynamic){
  if (intervention=="joint"){
    pred.matrix <- expand.grid(svol=vector.svol, hvol=vector.hvol)
    if (dynamic==TRUE){
      trial.outcomes <- data.frame(pred.matrix, death90=sapply(1:dim(pred.matrix)[1], function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(surgeon.volume=case_when(max.close.surgeon.volume_num_360 >= pred.matrix[x,]$svol ~ pred.matrix[x,]$svol,
                                                                         max.close.surgeon.volume_num_360 < pred.matrix[x,]$svol ~ max.close.surgeon.volume_num_360,
                                                                         is.na(max.close.surgeon.volume_num_360) ~ surgeon.volume),
                                                hospital.volume=case_when(max.close.hospital.volume_num_360 >= pred.matrix[x,]$hvol ~ pred.matrix[x,]$hvol,
                                                                          max.close.hospital.volume_num_360 < pred.matrix[x,]$hvol ~ max.close.hospital.volume_num_360,
                                                                          is.na(max.close.hospital.volume_num_360) ~ hospital.volume)
                     )))
      }))
    }
    if (dynamic==FALSE){
      trial.outcomes <- data.frame(pred.matrix, death90=sapply(1:dim(pred.matrix)[1], function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(surgeon.volume=(pred.matrix[x,]$svol),
                                                hospital.volume=(pred.matrix[x,]$hvol))))
      }))
    }
  }
  
  if (intervention=="surgeon.only"){
    pred.matrix.svol <- vector.svol
    if (dynamic==TRUE){
      trial.outcomes <- data.frame(pred.matrix.svol, death90=sapply(1:length(pred.matrix.svol), function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(surgeon.volume=case_when(max.close.surgeon.volume_num_360 >= pred.matrix.svol[x] ~ pred.matrix.svol[x],
                                                                         max.close.surgeon.volume_num_360 < pred.matrix.svol[x] ~ max.close.surgeon.volume_num_360,
                                                                         is.na(max.close.surgeon.volume_num_360) ~ surgeon.volume)
                     )))
      }))
    }
    if (dynamic==FALSE){
      trial.outcomes <- data.frame(pred.matrix.svol, death90=sapply(1:length(pred.matrix.svol), function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(surgeon.volume=pred.matrix.svol[x])))
      }))
    }
  }
  
  if (intervention=="hospital.only"){
    pred.matrix.hvol <- vector.hvol
    if (dynamic==TRUE){
      trial.outcomes <- data.frame(pred.matrix.hvol, death90=sapply(1:length(pred.matrix.hvol), function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(hospital.volume=case_when(max.close.hospital.volume_num_360 >= pred.matrix.hvol[x] ~ pred.matrix.hvol[x],
                                                                          max.close.hospital.volume_num_360 < pred.matrix.hvol[x] ~ max.close.hospital.volume_num_360,
                                                                          is.na(max.close.hospital.volume_num_360) ~ hospital.volume)
                     )))
      }))
    }
    if (dynamic==FALSE){
      trial.outcomes <- data.frame(pred.matrix.hvol, death90=sapply(1:length(pred.matrix.hvol), function(x){
        mean(predict(object=model,
                     type="response",
                     newdata=dataset %>% mutate(hospital.volume=pred.matrix.hvol[x])))
      }))
    }
  }
  return(trial.outcomes)
}
