#summary for datasets
#how to copy down demographics (fn to apply to variable?)


hbq_summary <- function(input_data) {
  # --- checks ---
  if (!is.data.frame(input_data)) {
    message("hbq_summary: input is not a data.frame/tibble.")
    return(NULL)
  }
  if (ncol(input_data) < 26) {
    message("hbq_summary: need at least 26 columns.")
    return(NULL)
  }
  
  # only take the first 26 columns (as requested)
  input_data <- input_data[, 1:26, drop = FALSE]
  
  # coerce all to numeric once (avoids list/character issues)
  input_num <- dplyr::mutate(
    input_data,
    dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x)))
  )
  
  # reverse-coded items (0–10 scale; change 10 if your max differs)
  rev6  <- 10 - input_num[[6]]
  rev8  <- 10 - input_num[[8]]
  rev16 <- 10 - input_num[[16]]
  rev17 <- 10 - input_num[[17]]
  rev18 <- 10 - input_num[[18]]
  
  # composite scores
  susceptibility <- rowMeans(cbind(input_num[[1]],  input_num[[7]],  rev16,            input_num[[22]]), na.rm = TRUE)
  severity       <- rowMeans(cbind(input_num[[2]],  input_num[[12]], input_num[[23]]),                   na.rm = TRUE)
  benefits       <- rowMeans(cbind(input_num[[3]],  rev8,            rev17,            input_num[[20]],  input_num[[24]]), na.rm = TRUE)
  barriers       <- rowMeans(cbind(input_num[[4]],  input_num[[9]],  input_num[[11]], input_num[[13]],
                                   input_num[[15]], rev18,           input_num[[21]], input_num[[26]]), na.rm = TRUE)
  cues_action    <- rowMeans(cbind(input_num[[5]],  input_num[[14]], input_num[[19]]),                   na.rm = TRUE)
  efficacy       <- rowMeans(cbind(rev6,            input_num[[10]], input_num[[25]]),                   na.rm = TRUE)
  
  dplyr::tibble(
    susceptibility = susceptibility,
    severity       = severity,
    benefits       = benefits,
    barriers       = barriers,
    cues_action    = cues_action,
    efficacy       = efficacy
  )
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

