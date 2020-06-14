

test_auto_stats = function(y, ...) UseMethod("test_auto_stats")

test_auto_stats.default = function(y, nb_x, paired) {
  
  if(missing(y)) stop("The y argument is missing.")
  
  if(missing(nb_x)) stop("The nb_x argument is missing.")
  
  if(missing(paired)) stop("The paired argument is missing.")
  
  if(!(y == "qualitative" || y == "quantitative"))
    stop('Y must be either "qualitative" or "quantitative".')
  
  if(!(nb_x == 0 || nb_x == 1 || nb_x == 2))
    stop("The number of X must be either 0, 1 or 2.")
  
  if(!(paired == "none" || paired == "first" || paired == "second" || paired == "both"))
    stop('The paired argument must be either "none", "first", "second" or "both".')
  
  
  ### Datasets for qualitative Y
  
  if(y == "qualitative") {
  
  if(nb_x == 0 && paired == "none") {
    
    y1_cochran_not_ok = c("No", "No", "No", "Yes", "No")
    `QUALITATIVE: No X - Y with 2 levels - Cochran Rule = OK` <<-
      data.frame(Y = y1_cochran_not_ok)
    
    y1_cochran_ok = rep(y1_cochran_not_ok, 3)
    `QUALITATIVE: No X - Y with 2 levels - Cochran Rule = NOT OK` <<- 
      data.frame(Y = y1_cochran_ok)
    
    # Display
    cat('Tests:
          
          auto_stats(`QUALITATIVE: No X - Y with 2 levels - Cochran Rule = OK`,
                      y = "Y", paired = "none")
          
          auto_stats(`QUALITATIVE: No X - Y with 2 levels - Cochran Rule = NOT OK`,
                      y = "Y", paired = "none")')
    
  }
  
  else if(nb_x == 0 && paired == "first" || nb_x == 0 && paired == "second" || nb_x == 0 && paired == "both")
    stop('No factor (X): the paired argument must be "none".')
  
  
  else if(nb_x == 1 && paired == "none") {
    
    # Independent: Y with two levels & X1 with two levels
    y2_cochran_not_ok = c("No", "No", "No", "Yes", "No", "Yes", "No", "No")
    x2_cochran_not_ok = c("Group 1", "Group 2", "Group 1", "Group 2", "Group 1", "Group 2", "Group 1", "Group 2")
    `QUALITATIVE: One independent X - Y & X1 with 2 levels - Cochran Rule = NOT OK` <<- 
      data.frame(Y = y2_cochran_not_ok, X1 = x2_cochran_not_ok)
    
    y2_cochran_ok = rep(y2_cochran_not_ok, 10)
    x2_cochran_ok = rep(x2_cochran_not_ok, 10)
    `QUALITATIVE: One independent X - Y & X1 with 2 levels - Cochran Rule = OK` <<- 
      data.frame(Y = y2_cochran_ok, X1 = x2_cochran_ok)
    
    
    # Independent: Y with more than two levels & X1 with two levels  
    y_3_cochran_not_ok = c("No", "Maybe", "No", "Yes", "No", "Yes", "No", "Maybe", "Yes", "No")
    x_3_cochran_not_ok = c("Group 1", "Group 2", "Group 1", "Group 2", "Group 1", "Group 2", "Group 1", "Group 2", "Group 1", "Group 2")
    `QUALITATIVE: One independent X - Y with more than 2 levels & X1 with 2 levels - Cochran Rule = NOT OK` <<- 
      data.frame(Y = y_3_cochran_not_ok, X1 = x_3_cochran_not_ok)
    
    y_3_cochran_ok = rep(y_3_cochran_not_ok, 10)
    x_3_cochran_ok = rep(x_3_cochran_not_ok, 10)
    `QUALITATIVE: One independent X - Y with more than 2 levels & X1 with 2 levels - Cochran Rule = OK` <<- 
      data.frame(Y = y_3_cochran_ok, X1 = x_3_cochran_ok)
    
    
    # Independent: Y with two levels & X1 with more than two levels            
    y_4_cochran_not_ok = c("No", "No", "No", "Yes", "No", "Yes", "No", "No", "Yes") 
    x_4_cochran_not_ok = c("Group 1", "Group 2", "Group 3", "Group 2", "Group 1", "Group 3","Group 1", "Group 2", "Group 3")
    `QUALITATIVE: One independent X - Y with 2 levels & X1 with more than 2 levels - Cochran Rule = NOT OK` <<- 
      data.frame(Y = y_4_cochran_not_ok, X1 = x_4_cochran_not_ok)
    
    y_4_cochran_ok = rep(y_4_cochran_not_ok, 10)
    x_4_cochran_ok = rep(x_4_cochran_not_ok, 10)
    `QUALITATIVE: One independent X - Y with 2 levels & X1 with more than 2 levels - Cochran Rule = OK` <<- 
      data.frame(Y = y_4_cochran_ok, X1 = x_4_cochran_ok)
    
    
    # Independent: Y with more than two levels & X1 with more than two levels   
    y_5_cochran_not_ok = c("No", "No", "Maybe", "Yes", "No", "Yes", "Maybe", "No", "Yes")
    x_5_cochran_not_ok = c("Group 1", "Group 2", "Group 3", "Group 2", "Group 1", "Group 3", "Group 1", "Group 2", "Group 3")
    `QUALITATIVE: One independent X - Y & X1 with more than 2 levels - Cochran Rule = NOT OK` <<- 
      data.frame(Y = y_5_cochran_not_ok, X1 = x_5_cochran_not_ok)
    
    y_5_cochran_ok = rep(y_5_cochran_not_ok, 10)
    x_5_cochran_ok = rep(x_5_cochran_not_ok, 10)
    `QUALITATIVE: One independent X - Y & X1 with more than 2 levels - Cochran Rule = OK` <<- 
      data.frame(Y = y_5_cochran_ok, X1 = x_5_cochran_ok)
    
    
    # Display
    cat('Test:
          
          auto_stats(`QUALITATIVE: One independent X - Y & X1 with 2 levels - Cochran Rule = NOT OK`,
                      y = "Y", x1 = "X1", paired = "none")
          
          auto_stats(`QUALITATIVE: One independent X - Y & X1 with 2 levels - Cochran Rule = OK`,
                      y = "Y", x1 = "X1", paired = "none")
          
          auto_stats(`QUALITATIVE: One independent X - Y with more than 2 levels & X1 with 2 levels - Cochran Rule = NOT OK`,
                      y = "Y", x1 = "X1", paired = "none")
          
          auto_stats(`QUALITATIVE: One independent X - Y with more than 2 levels & X1 with 2 levels - Cochran Rule = OK`,
                      y = "Y", x1 = "X1", paired = "none")
          
          auto_stats(`QUALITATIVE: One independent X - Y with 2 levels & X1 with more than 2 levels - Cochran Rule = NOT OK`,
                      y = "Y", x1 = "X1", paired = "none")
          
          auto_stats(`QUALITATIVE: One independent X - Y with 2 levels & X1 with more than 2 levels - Cochran Rule = OK`,
                      y = "Y", x1 = "X1", paired = "none")
          
          auto_stats(`QUALITATIVE: One independent X - Y & X1 with more than 2 levels - Cochran Rule = NOT OK`,
                      y = "Y", x1 = "X1", paired = "none")
          
          auto_stats(`QUALITATIVE: One independent X - Y & X1 with more than 2 levels - Cochran Rule = OK`,
                      y = "Y", x1 = "X1", paired = "none")')
    
  }
  
  else if(nb_x == 1 && paired == "first") {
    
    # Paired: Y with two levels & X1 with two levels
    y_6 = c("No", "No", "No", "Yes", "No", "Yes", "No", "No")
    x_6 = c("Before", "After", "Before", "After", "Before", "After", "Before", "After")
    id_6 = c("Subject 1", "Subject 1", "Subject 2", "Subject 2", "Subject 3", "Subject 3", "Subject 4", "Subject 4")
    `QUALITATIVE: One paired X - Y & X1 with 2 levels` <<-
      data.frame(Y = y_6, X1 = x_6, ID = id_6)
    
    
    # Paired: Y with more than two levels & X1 with two levels
    y_9 = c("No", "No", "Maybe", "Yes", "No", "Yes", "No", "Maybe")
    x_9 = c("Before", "After", "Before", "After", "Before", "After", "Before", "After")
    id_9 = c("Subject 1", "Subject 1", "Subject 2", "Subject 2", "Subject 3", "Subject 3", "Subject 4", "Subject 4")
    `QUALITATIVE: One paired X - Y with more than 2 levels & X1 with 2 levels` <<- 
      data.frame(Y = y_9, X1 = x_9, ID = id_9)
    
    
    # Paired: Y with two levels & X1 with more than two levels
    y_10 = c("No", "No", "No", "Yes", "No", "Yes", "No", "Yes", "Yes")
    x_10 = c("Before", "After", "During", "Before", "After", "During", "Before", "After", "During")
    id_10 = c("Subject 1", "Subject 1", "Subject 1", "Subject 2", "Subject 2", "Subject 2", "Subject 3", "Subject 3", "Subject 3")
    `QUALITATIVE: One paired X - Y with 2 levels & X1 with more than 2 levels` <<- 
      data.frame(Y = y_10, X1 = x_10, ID = id_10)
    
    
    # Paired: Y with more than two levels & X1 with more than two levels
    y_11 = c("No", "No", "Maybe", "Yes", "No", "Yes", "No", "Maybe", "Yes")
    x_11 = c("Before", "After", "During", "Before", "After", "During", "Before", "After", "During")
    id_11 = c("Subject 1", "Subject 1", "Subject 1", "Subject 2", "Subject 2", "Subject 2", "Subject 3", "Subject 3", "Subject 3")
    `QUALITATIVE: One paired X - Y & X1 with more than 2 levels` <<- 
      data.frame(Y = y_11, X1 = x_11, ID = id_11)
    
    
    # Display
    cat('Tests:
          
          auto_stats(`QUALITATIVE: One paired X - Y & X1 with 2 levels`,
                      y = "Y", x1 = "X1", id = "ID", paired = "first")
          
          auto_stats(`QUALITATIVE: One paired X - Y with more than 2 levels & X1 with 2 levels`,
                      y = "Y", x1 = "X1", id = "ID", paired = "first")
          
          auto_stats(`QUALITATIVE: One paired X - Y with 2 levels & X1 with more than 2 levels`,
                      y = "Y", x1 = "X1", id = "ID", paired = "first")
          
          auto_stats(`QUALITATIVE: One paired X - Y & X1 with more than 2 levels`,
                      y = "Y", x1 = "X1", id = "ID", paired = "first")')
    
  }
  
  #else if(nb_x == 1 && paired == "second" || nb_x == 1 && paired == "both")
  # stop('Only one factor (X): the paired argument must be either "none" or "first".')
  
  else if(nb_x == 2 && paired == "none") {
    
    # INDEPENDENT: Y & X1 with 2 levels
    y_12 = c("No", "No", "No", "Yes", "No", "Yes", "No", "No")
    x1_12 = c("Group 1", "Group 2", "Group 1", "Group 2", "Group 1", "Group 2", "Group 1", "Group 2")
    x2_12 = c("Adult female", "Adult female", "Adult male", "Adult male", "Teenage boy", "Teenage boy", "Teenage girl", "Teenage girl")
    `QUALITATIVE: Two independent X - Y & X1 with 2 levels - Woolf test = OK` <<- 
      data.frame(Y = y_12, X1 = x1_12, X2 = x2_12)
    
    `QUALITATIVE: Two independent X - Y & X1 with 2 levels - Woolf test = NOT OK - Additive model` <<- 
      data.frame(Y = rep(y_12, 1000), X1 = rep(x1_12, 1000), X2 = rep(x2_12, 1000))
    
    y_12_m = c(rep("Yes", 1755),rep("No", 2771))
    x1_12_m = c(rep("Female", 557),rep("Male", 1198),rep("Female", 1278),rep("Male", 1493))
    x2_12_m = c(rep("Group 1", 89),rep("Group 2", 17),rep("Group 3", 202),
                rep("Group 4", 131),rep("Group 5", 94),rep("Group 6", 24),
                rep("Group 1", 512),rep("Group 2", 353),rep("Group 3", 120),
                rep("Group 4", 138),rep("Group 5", 53),rep("Group 6", 22),
                rep("Group 1", 19),rep("Group 2", 8),rep("Group 3", 391),
                rep("Group 4", 244),rep("Group 5", 299),rep("Group 6", 317),
                rep("Group 1", 313),rep("Group 2", 207),rep("Group 3", 205),
                rep("Group 4", 279),rep("Group 5", 138),rep("Group 6", 351))
    `QUALITATIVE: Two independent X - Y & X1 with 2 levels - Woolf test = NOT OK - Multiplicative model` <<- 
      data.frame(Y = y_12_m, X1 = x1_12_m, X2 = x2_12_m)
    
    
    # INDEPENDENT: Y with more than 2 levels & X1 with 2 levels
    y_13 = c("No", "No", "Maybe", "Yes", "No", "Yes", "No", "No")
    x1_13 = c("Group 1", "Group 2", "Group 1", "Group 2", "Group 1", "Group 2", "Group 1", "Group 2")
    x2_13 = c("Adult female", "Adult female", "Adult male", "Adult male", "Teenage boy", "Teenage boy", "Teenage girl", "Teenage girl")
    `QUALITATIVE: Two independent X - Y with more than 2 levels & X1 with 2 levels - Woolf test = OK` <<- 
      data.frame(Y = y_13, X1 = x1_13, X2 = x2_13)
    
    `QUALITATIVE: Two independent X - Y with more than 2 levels & X1 with 2 levels - Woolf test = NOT OK` <<- 
      data.frame(Y = rep(y_13, 1000), X1 = rep(x1_13, 1000), X2 = rep(x2_13, 1000))
    
    
    # INDEPENDENT: Y with 2 levels & X1 with more than 2 levels (Additive model better)
    y_14 = c("No", "No", "No", "Yes", "No", "Yes", "No", "No")
    x1_14 = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 1", "Group 2", "Group 3", "Group 4")
    x2_14 = c("Adult female", "Adult female", "Adult male", "Adult male", "Teenage boy", "Teenage boy", "Teenage girl", "Teenage girl")
    `QUALITATIVE: Two independent X - Y with 2 levels & X1 with more than 2 levels` <<- 
      data.frame(Y = y_14, X1 = x1_14, X2 = x2_14)
    
    
    # INDEPENDENT: Y1 & X1 with more than 2 levels (Additive model better)
    y_15 = c("No", "No", "Maybe", "Yes", "No", "Yes", "No", "Yes")
    x1_15 = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 1", "Group 2", "Group 3", "Group 4")
    x2_15 = c("Adult female", "Adult male", "Adult male", "Adult male", "Teenage boy", "Teenage boy", "Teenage girl", "Teenage girl")
    `QUALITATIVE: Two independent X - Y1 & X1 with more than 2 levels` <<- 
      data.frame(Y = y_15, X1 = x1_15, X2 = x2_15)
    
    
    # Display
    cat('Tests:
          
          auto_stats(`QUALITATIVE: Two independent X - Y & X1 with 2 levels - Woolf test = OK`,
                      y = "Y", x1 = "X1", x2 = "X2", paired = "none")
          
          auto_stats(`QUALITATIVE: Two independent X - Y & X1 with 2 levels - Woolf test = NOT OK - Additive model`,
                      y = "Y", x1 = "X1", x2 = "X2", paired = "none")
          
          auto_stats(`QUALITATIVE: Two independent X - Y & X1 with 2 levels - Woolf test = NOT OK - Multiplicative model`,
                      y = "Y", x1 = "X1", x2 = "X2", paired = "none")
          
          auto_stats(`QUALITATIVE: Two independent X - Y with more than 2 levels & X1 with 2 levels - Woolf test = OK`,
                      y = "Y", x1 = "X1", x2 = "X2", paired = "none")
          
          auto_stats(`QUALITATIVE: Two independent X - Y with more than 2 levels & X1 with 2 levels - Woolf test = NOT OK`,
                      y = "Y", x1 = "X1", x2 = "X2", paired = "none")
          
          auto_stats(`QUALITATIVE: Two independent X - Y with 2 levels & X1 with more than 2 levels`,
                      y = "Y", x1 = "X1", x2 = "X2", paired = "none")
          
          auto_stats(`QUALITATIVE: Two independent X - Y1 & X1 with more than 2 levels`,
                      y = "Y", x1 = "X1", x2 = "X2", paired = "none")')
    
  }
  
  else if(nb_x == 2 && paired == "first" || nb_x == 2 && paired == "second" || nb_x == 2 && paired == "both") 
    stop("The function auto_stats does not support such design. Therefore, no datasets have been created for it.")
    
  }
  
  ### Datasets for quantitative Y
  
  else 
   stop("
   The function auto_stats does not support analysis of data with quantitative dependent variable yet. 
   Therefore, no datasets have been created for it yet.")
  
}





