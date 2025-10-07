#summary for datasets
#how to copy down demographics (fn to apply to variable?)


hbq_summary <- function(input_data){
  #check inputs- should be a dataframe of width 26
  if( !is.data.frame(input_data)) { 
    print("not a data frame")  #fix print syntax here
    return(NULL)
  } 
  if(ncol(input_data) != 26) {
    print("check input width")
    return(NULL)
  }
  #create additional columns for the reversed-value questions
  input_data <- mutate(input_data, rev6 = as.integer(10-input_data[,6]), rev8 = as.integer(10-input_data[,8]),
                       rev16 = as.integer(10-input_data[,16]), rev17 = as.integer(10-input_data[,17]), rev18 = as.integer(10-input_data[,18]))
  
  #create mean scores
  input_data <- input_data %>% mutate(susceptibility = rowMeans(cbind(input_data[,1], input_data[,7], input_data$rev16, input_data[,22])))
  input_data <- mutate(input_data, severity = rowMeans(cbind(input_data[,2], input_data[,12], input_data[,23])))
  input_data <- mutate(input_data, benefits = rowMeans(cbind(input_data[,3], input_data$rev8, input_data$rev17, input_data[,20], input_data[,24])))
  input_data <- mutate(input_data, barriers = rowMeans(cbind(input_data[,4], input_data[,9], input_data[,11], input_data[,13], input_data[,15], input_data$rev18, input_data[,21], input_data[,26])))
  input_data <- mutate(input_data, cues_action = rowMeans(cbind(input_data[,5], input_data[,14], input_data[,19])))
  input_data <- mutate(input_data, efficacy = rowMeans(cbind(input_data$rev6, input_data[,10], input_data[,25])))
  
  output_data <- select(input_data, susceptibility, severity, benefits, barriers, cues_action, efficacy)
  return(output_data)
}

hhia_summary <- function(input_data) {
  #check inputs- should be a dataframe of width 10
  if( !is.data.frame(input_data)) { 
    print("not a data frame")  #fix print syntax here
    return(NULL)
  } 
  if(ncol(input_data) != 10) {
    print("check input width")
    return(NULL)
  }
  
  input_data <- mutate(input_data, hhia_sum = rowSums(input_data[,1:10]), na.rm = T)

  return(input_data$hhia_sum)

}

hhie_summary <- function(input_data) {
  #check inputs- should be a dataframe of width 10
  if( !is.data.frame(input_data)) { 
    print("not a data frame")  #fix print syntax here
    return(NULL)
  } 
  if(ncol(input_data) != 10) {
    print("check input width")
    return(NULL)
  }
  
  input_data <- mutate(input_data, hhie_sum = rowSums(input_data[,1:10]), na.rm = T)

return(input_data$hhie_sum)

}

#submethods for aphab

aphab_convert <- function(values) {
  output <- case_when(
    values == 1 ~ 0.99,
    values == 2 ~ 0.87,
    values == 3 ~ 0.75,
    values == 4 ~ 0.5,
    values == 5 ~ 0.25,
    values == 6 ~ 0.12,
    values == 7 ~ 0.01,
    TRUE ~ NA_real_
  )
  
  return(output)
}

aphab_reverse <- function(values) {
  output <- case_when(
    values == 7 ~ 0.99,
    values == 6 ~ 0.87,
    values == 5 ~ 0.75,
    values == 4 ~ 0.5,
    values == 3 ~ 0.25,
    values == 2 ~ 0.12,
    values == 1 ~ 0.01,
    TRUE ~ NA_real_
  )
  
  return(output)
}

aphab_summary <- function(input_data) {
  #check inputs- should be a dataframe of width 24
  if( !is.data.frame(input_data)) { 
    print("not a data frame")  #fix print syntax here
    return(NULL)
  } 
  if(ncol(input_data) != 24) {
    print("check input width")
    return(NULL)
  }
  
  #create new cols
  
  input_data <- mutate(input_data, ECscore = rowMeans(cbind(aphab_convert(input_data[,4]), aphab_convert(input_data[,10]), aphab_convert(input_data[,12]), aphab_convert(input_data[,14]), aphab_convert(input_data[,15]), aphab_convert(input_data[,23]))))
  input_data <- mutate(input_data, BNscore = rowMeans(cbind(aphab_reverse(input_data[,1]), aphab_convert(input_data[,6]),  aphab_convert(input_data[,7]),  aphab_reverse(input_data[,16]), aphab_reverse(input_data[,19]), aphab_convert(input_data[,24]))))
  input_data <- mutate(input_data, RVscore = rowMeans(cbind(aphab_convert(input_data[,2]), aphab_convert(input_data[,5]),  aphab_convert(input_data[,9]),  aphab_reverse(input_data[,11]), aphab_convert(input_data[,18]), aphab_reverse(input_data[,18]))))
  input_data <- mutate(input_data, AVscore = rowMeans(cbind(aphab_convert(input_data[,3]), aphab_convert(input_data[,8]),  aphab_convert(input_data[,13]), aphab_convert(input_data[,17]), aphab_convert(input_data[,20]), aphab_convert(input_data[,22]))))
  input_data <- mutate(input_data, global = rowMeans(input_data[,25:27]))
  
  output_data <- select(input_data, ECscore, BNscore, RVscore, AVscore, global)
  return(output_data)
}

