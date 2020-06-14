auto_stats = function(y, ...) UseMethod("auto_stats")

auto_stats = function(data, y, x1 = NULL, x2 = NULL, paired = "none", id = NULL, digits = 3, apa = F) {
  
  ## Initialisation
  
  fct.env = new.env()
  
  if(!(y %in% names(data)))
    stop("The dataframe does not contain Y. Check the spelling.")
  
  if(!is.null(x1) && !(x1 %in% names(data)))
    stop("The dataframe does not contain X1. Check the spelling.")
  
  if(!is.null(x2) && !(x2 %in% names(data)))
    stop("The dataframe does not contain X2. Check the spelling.")
  
  if(!is.null(id) && !(id %in% names(data)))
    stop("The dataframe does not contain ID. Check the spelling.")
  
  Y = data[, y]
  
  if(!is.null(x1))  X1 = data[, x1]
  
  if(!is.null(x2))  X2 = data[, x2]
  
  if(!is.null(id))  ID = data[, id]
  
  if(length(unique(Y)) == 1)
    stop("Y is constant while it must have at least two levels.")
  
  if(!is.null(id) && length(unique(ID)) == 1)
    stop("The ID of the subjects is constant while it must have at least two levels.")
  
  if(!(is.character(Y) || is.factor(Y) || is.integer(Y) || is.numeric(Y)))
    stop("Y must be either a character, a factor, an integer or a number. Check it with class(Y),
          and change it using as.character(y) etc, depending on which type you want it to be.")
  
  if(!is.null(x1) && !(is.character(X1) || is.factor(X1) || is.integer(X1) || is.numeric(X1)))
    stop("X1 must be either a character, a factor, an integer or a number. Check it with class(X1),
          and change it using as.character(X1) etc, depending on which type you want it to be.")
  
  if(!is.null(x2) && !(is.character(X2) || is.factor(X2) || is.integer(X2) || is.numeric(X2)))
    stop("X2 must be either a character, a factor, an integer or a number. Check it with class(X2),
          and change it using as.character(X2) etc, depending on which type you want it to be.")
  
  if(!is.null(id) && !(is.character(ID) || is.factor(ID) || is.integer(ID) || is.numeric(ID)))
    stop("ID must be either a character, a factor, an integer or a number. Check it with class(ID),
          and change it using as.character(ID) etc, depending on which type you want it to be.")
  
  if(!(paired == "none" || paired == "first" || paired == "second" || paired == "both"))
    stop('The "paired" argument only takes those values: "none", "first", "second", "both".')
  
  
  ## Functions
  
  display = function(tab = NULL, prob1 = NULL, prob2 = NULL, vali1 = NULL, vali2 = NULL, vali3 = NULL, vali4 = NULL, 
                     test1 = NULL, test2 = NULL, apa_test1 = NULL, apa_test2 = NULL, asso1 = NULL, asso2 = NULL, apa_asso1 = NULL,
                     apa_asso2 = NULL, other_asso = NULL, name_ph1 = NULL, name_ph2 = NULL, name_ph3 = NULL, ph1 = NULL, 
                     ph2 = NULL, ph3 = NULL, mes1 = NULL, mes2 = NULL, mes3 = NULL, mes4 = NULL, dig = digits) {
    if(!is.null(tab)) {
      cat("------------------------------", "TABLE", "-----------------------------", fill = T)
      cat(" ", fill = T)
      print(tab, digits = dig)
      cat(" ", fill = T)
    }
    if(!is.null(prob1)) {
      cat("--------------------------", "PROBABILITIES", "-------------------------", fill = T)
      cat(" ", fill = T)
      cat(prob1, fill = T)
      cat(" ", fill = T)
      cat(prob2, fill = T)
      cat(" ", fill = T)
    }
    if(!is.null(vali1)) {
      cat("----------------------", "ASSUMPTION(S) TESTING", "---------------------", fill = T)
      cat(" ", fill = T)
      cat(vali1, fill = T)
      if(!is.null(vali2)) {cat(" ", fill = T) 
        cat(vali2, fill = T)}
      if(!is.null(vali3)) {cat(" ", fill = T) 
        cat(vali3, fill = T)}
      if(!is.null(vali4)) {cat(" ", fill = T) 
        cat(vali4, fill = T)}
      cat(" ", fill = T)
    }
    if(!is.null(test1)) {
      cat("--------------------------", "MAIN TEST(S)", "--------------------------", fill = T)
      cat(" ", fill = T)
      cat(test1, fill = T)
      if(!is.null(apa_test1)) {
        cat(" ", fill = T)
        cat(paste("APA code ->", apa_test1), fill = T)
        if(is.null(apa_test2)) { cat(" ", fill = T)
          cat("(Copy it + Paste it in an R Notebook + Click on Preview and save it + Click on Knit + Copy it anywhere else)", fill = T)}}
      if(!is.null(test2)) {cat(" ", fill = T) 
        cat(test2, fill = T)}
      if(!is.null(apa_test2)) {
        cat(" ", fill = T)
        cat(paste("APA code ->", apa_test2), fill = T)
        cat(" ", fill = T)
        cat("(Copy it + Paste it in an R Notebook + Click on Preview and save it + Click on Knit + Copy it anywhere else)", fill = T)}
      cat(" ", fill = T)
    }
    if(!is.null(asso1)) {
      cat("--------------------", "MEASURE(S) OF ASSOCIATION", "-------------------", fill = T)
      cat(" ", fill = T)
      cat(asso1, fill = T)
      if(!is.null(apa_asso1)) {
        cat(" ", fill = T)
        cat(paste("APA code ->", apa_asso1), fill = T)}
      if(!is.null(asso2)) {cat(" ", fill = T) 
        cat(asso2, fill = T)} 
      if(!is.null(apa_asso2)) {
        cat(" ", fill = T)
        cat(paste("APA code ->", apa_asso2), fill = T)}
      if(!is.null(other_asso)) {cat(" ", fill = T)
        print(other_asso)}
      cat(" ", fill = T)
    }
    
    if(!is.null(ph1)) {
      cat("------------------------", "POST-HOC ANALYSIS", "-----------------------", fill = T)
      cat(" ", fill = T)
      cat(name_ph1,fill = T)
      cat(" ", fill = T)
      print(ph1, digits = dig)
      if(!is.null(ph2)) {cat(" ", fill = T) 
        cat(name_ph2,fill = T)
        cat(" ", fill = T)
        print(ph2, digits = dig)}
      if(!is.null(ph3)) {cat(" ", fill = T) 
        cat(name_ph3,fill = T)
        cat(" ", fill = T)
        print(ph3, digits = dig)}
      cat(" ", fill = T)
    }
    if(!is.null(mes1) || !is.null(mes2) || !is.null(mes3) || !is.null(mes4)) {
      cat("--------------------------", "MESSAGE(S)", "---------------------------", fill = T)
      cat("", fill = F)
      if(!is.null(mes1)) {cat(" ", fill = T) 
        cat(mes1, fill = T)}
      if(!is.null(mes2)) {cat(" ", fill = T) 
        cat(mes2, fill = T)}
      if(!is.null(mes3)) {cat(" ", fill = T) 
        cat(mes3, fill = T)}
      if(!is.null(mes4)) {cat(" ", fill = T) 
        cat(mes4, fill = T)}
      cat(" ", fill = T)
    }
    cat("------------------------------------------------------------------", fill = T)
  }
  
  r = function(r) {
    
    if(is.numeric(r) || is.integer(r)) r = round(r, digits = digits)
    r
  }
  
  x_apa = function(x) {
    if(is.numeric(x) || is.integer(x)) x = round(x, digits = 3)
    x
  }
  
  x1_apa = function(x) {
    if(is.numeric(x) || is.integer(x)) 
      if(x != 0) x = substr(round(x, digits = 3), 2, digits + 2)
      else x = x
      x
      
  }
  
  p_apa = function(p) {
    
    if(p >= 0.001 && p < 0.01)
      p = "< .01"
    
    else if(p < 0.001)
      p = "< .001"
    
    else 
      p = substr(round(p, digits = 3), 2, 5)
    p
    
  }
  
  s = function(p) {
    
    if(p >= 0.05 && p < 0.1)    significance = " (.)"
    
    else if(p >= 0.01 && p < 0.05)    significance = " (*)"
    
    else if(p >= 0.001 && p < 0.01)    significance = " (**)"
    
    else if(p < 0.001)    significance = " (***)"
    
    else significance = " (ns)"
    
    significance
    
  }
  
  m = function(x, lim1, lim2, lim3, OR = F) {
    
    if(is.numeric(x) || is.integer(x)) {
      
      if(OR && x < 1) magnitude = " (Less odds)"
      
      else if(is.infinite(x)) magnitude = ""
      
      else if(x < lim1) magnitude = " (Negligible effect)"
      
      else if(x >= lim1 && x < lim2) magnitude = " (Small effect)"
      
      else if(x >= lim2 && x < lim3) magnitude = " (Medium effect)"
      
      else magnitude = " (Large effect)" 
      
    }
    
    else magnitude = ""
    
    magnitude
    
  }
  
  mv = function(v, df) {
    
    if(df == 2) magnitude = m(v, lim1 = 0.07, lim2 = 0.2, lim3 = 0.35)
    
    else if(df == 3) magnitude = m(v, lim1 = 0.06, lim2 = 0.17, lim3 = 0.29)
    
    else if(df == 4) magnitude = m(v, lim1 = 0.05, lim2 = 0.15, lim3 = 0.25)
    
    else if(df == 5) magnitude = m(v, lim1 = 0.05, lim2 = 0.13, lim3 = 0.22)
    
    else if(df > 5) {
      
      magnitude = m(v, lim1 = 0.05, lim2 = 0.13, lim3 = 0.22)
      message2 = assign("message2", "No information on how to interpret Cramer's V for contigency tables having more than 5 columns\nand/or rows exist to my knowledge. The magnitude given will maybe slightly underestimate the magnitude\n(e.g. medium instead of large).", envir = fct.env)
      
    }
    
    magnitude
    
  }
  
  cm = function(mess) {
    
    if(exists(mess, envir = fct.env)) mess = get(mess, envir = fct.env)
    
    else mess = NULL
    
  }
  
  
  ## Qualitative Y
  
  if(is.factor(y) || is.character(y)) {
    
    ### No X
    
    if(is.null(x1) && is.null(x2)) {
      
      # Independence
      
      if(paired == "none") {
        
        if(length(unique(Y)) > 2) 
          stop("Impossible to compare more than 2 proportions in a binomial test. Consider analysing each pairs of levels separately.")
        
        else {
          
          tab_n1 = sort(table(Y), decreasing = T)
          tab_n2 = sort(table(Y), decreasing = F)
          tab_prop = sort(prop.table(table(Y)), decreasing = T) 
          cochran = suppressWarnings(table(chisq.test(tab_n1)$expected > 5))
          
          ### Test
          
          # Violation of Cochran's Rule
          
          if(names(cochran) == "FALSE") {
            
            vali = "Cochran's rule: 100% of expected counts are below 5 -> Not satisfied"
            
            # One-sample Chi-squared Test with Yates' correction
            
            test1 = suppressWarnings(prop.test(tab_n1)) 
            test2 = suppressWarnings(prop.test(tab_n2))
            name = "One-sample Chi-squared Test with Yates' correction"
            
          }
          
          # Satisfied Cochran's Rule
          
          else {
            
            vali = "Cochran's rule: 0% of expected counts are below 5 -> Satisfied"
            
            # One-sample Chi-squared Test
            
            test1 = suppressWarnings(prop.test(tab_n1, correct = F)) 
            test2 = suppressWarnings(prop.test(tab_n2, correct = F)) 
            name = "One-sample Chi-squared Test"
            
          }
          
          # Effect size: Cohen's H
          
          effect_size = abs(2 * asin(sqrt(tab_prop[[1]])) - 2 * asin(sqrt(tab_prop[[2]])))
          
          # Probability 
          
          prob1 = test1[[4]][[1]]
          prob2 = test2[[4]][[1]]
          ci1 = test1[[6]][1:2]
          ci2 = test2[[6]][1:2]
          
          ### Displaying results
          
          var_type = "Variable type: Qualitative dependent variable (Y) with two levels.\nNo factors (X)."
          
          prob_ci1 = paste("Probality of ", names(tab_n1)[1], " = ", r(prob1), " 95% CI [", r(ci1[1]), ", ", r(ci1[2]), "]", sep = "")
          prob_ci2 = paste("Probality of ", names(tab_n2)[1], " = ", r(prob2), " 95% CI [", r(ci2[1]), ", ", r(ci2[2]), "]", sep = "")
          
          prob_ci1_apa = paste("Probality of ", names(tab_n1)[1], " = ", x1_apa(prob1), 
                               ", 95% CI [", x1_apa(ci1[1]), ", ", x1_apa(ci1[2]), "]", sep = "")
          prob_ci2_apa = paste("Probality of ", names(tab_n2)[1], " = ", x1_apa(prob2), 
                               " 95% CI [", x1_apa(ci2[1]), ", ", x1_apa(ci2[2]), "]", sep = "")
          
          test = paste(name, ": X-squared = ", r(test1[[1]][[1]]), ", df = ", test1[[2]][[1]], ", p-value = ", r(test1[[3]]), s(test1[[3]]), sep = "")
          
          test_apa = paste("&chi;^2^(", test1[[2]][[1]], ") = ", x_apa(test1[[1]][[1]]), ", *p* = ", p_apa(test1[[3]]), sep = "")
          
          eff = paste("Cohen's h = ", r(effect_size), m(effect_size, lim1 = 0.2, lim2 = 0.5, lim3 = 0.8), sep = "")
          
          eff_apa = paste("Cohen's h = ", x_apa(effect_size), m(effect_size, lim1 = 0.2, lim2 = 0.5, lim3 = 0.8), sep = "")
          
          if(apa) display(tab = tab_n1, prob1 = prob_ci1_apa, prob2 = prob_ci2_apa, vali1 = var_type, vali2 = vali, 
                          test1 = test, apa_test1 = test_apa, asso1 = eff_apa)
          
          else display(tab = tab_n1, prob = prob_ci1, prob2 = prob_ci2, vali1 = var_type, vali2 = vali, test1 = test, asso1 = eff)
          
        } 
        
      }
      
      # Repeated measures: ERROR
      
      else stop("Y cannot be paired. Add a repeated factor in the formula.")
      
    }
    
    ### One X
    
    if(!is.null(x1) && is.null(x2)) {
      
      # Independence
      
      if(paired == "none") {
        
        tab_n = table(X1, Y)
        
        # X and Y have two levels each
        
        if(length(unique(X1)) == 2 && length(unique(Y)) == 2) {
          
          # Exact Fisher's Test
          
          test1 = fisher.test(tab_n)
          
          ### Effect size
          
          # Violation of Cochran's Rule
          
          cochran = suppressWarnings(table(chisq.test(tab_n)$expected > 5))
          
          if(length(cochran) == 1 && names(cochran) == "FALSE" ||
             length(cochran) == 1 && cochran[[1]] < round(0.8*nrow(tab_n) * ncol(tab_n)) ||
             length(cochran) == 2 && cochran[[2]] < round(0.8*nrow(tab_n) * ncol(tab_n))) {
            
            # Effect size: Phi coefficient
            
            phi = suppressWarnings(sqrt(prop.test(tab_n)[[1]][[1]]/sum(tab_n)))
            
            warning = "No effect size have been specifically designed for use after a Fisher's Exact Test to my knowlegde.\nThe Phi coefficient based on the Chi-squared with Yates correction (Cochran's rule NOT OK) was used instead." 
            
          }
          
          # Satisfied Cochran's Rule 
          
          else {
            
            # Effect size: Phi coefficient
            
            phi = suppressWarnings(sqrt(prop.test(tab_n, correct = F)[[1]][[1]]/sum(tab_n)))
            
            warning = "No effect size have been specifically designed for use after a Fisher's Exact Test to my knowlegde.\nThe Phi coefficient based on the Chi-squared without Yates correction (Cochran's rule OK) was used instead."
            
          }
          
          # Odds ratio and their CI
          
          # Sample size <= 1000
          
          if(sum(tab_n) <= 1000) {
            
            # fisher.exact (more precise CI)
            
            odds = fisher.exact(tab_n)[[3]][[1]]
            
            ci_odds = fisher.exact(tab_n)[[2]][1:2]
            
          }
          
          # Sample size > 1000
          
          else {
            
            # fisher.test (fastest computation)
            
            odds = test1[[3]][[1]]
            
            ci_odds = test1[[2]][1:2]
            
          }
          
          ### Displaying results
          
          var_type = "Variable type: Qualitative dependent variable (Y) with two levels.\nOne factor (X1) with two independent levels."
          
          test = paste("Fisher's Exact Test: p-value = ", r(test1[[1]]), s(test1[[1]]), sep = "")
          
          test_apa = paste("Fisher's Exact Test: *p* = ", p_apa(test1[[1]]), sep = "")
          
          eff = paste("Phi coefficient = ", r(phi), m(phi, lim1 = 0.1, lim2 = 0.3, lim3 = 0.5), sep = "")
          
          eff_apa = paste("&phi; = ", x_apa(phi), sep = "")
          
          odd = paste("Odds ratio = ", r(odds), ", 95% CI [", r(ci_odds[[1]]), ", ", 
                      r(ci_odds[[2]]), "]", m(odds, lim1 = 1.55, lim2 = 2.8, lim3 = 5, OR = T), sep = "")
          
          odds_apa = paste("OR = ", r(odds), ", 95% CI [", x_apa(ci_odds[1]), ", ", 
                           x_apa(ci_odds[2]), "]", sep = "")
          
          if(apa) display(tab = tab_n, vali1 = var_type, test1 = test, apa_test1 = test_apa, asso1 = eff, 
                          apa_asso1 = eff_apa, asso2 = odds_apa, mes1 = warning)
          
          else display(tab = tab_n, vali1 = var_type, test1 = test, asso1 = eff, asso2 = odd, mes1 = warning)
          
        }
        
        # X and/or Y have more than two levels
        
        else {
          
          if(length(unique(Y)) == 2 && length(unique(X1)) == 2)
            
            var_type = "Variable type: Qualitative dependent variable (Y) with two levels.\nOne factor (X1) with more than two independent levels."
          
          else if(length(unique(Y)) > 2 && length(unique(X1)) == 2)
            
            var_type = "Variable type: Qualitative dependent variable (Y) with more than two levels.\nOne factor (X1) with two independent levels."
          
          else  var_type = "Variable type: Qualitative dependent variable (Y) with more than two levels.\nOne factor with more than two independent levels(X1)."
          
          # Exact Fisher's Test
          
          test1 = fisher.test(tab_n)
          
          # Effect size
          
          # Violation of Cochran's Rule
          
          cochran = suppressWarnings(table(chisq.test(tab_n)$expected > 5))
          
          if(length(cochran) == 1 && names(cochran) == "FALSE" ||
             length(cochran) == 1 && cochran[[1]] < round(0.8*nrow(tab_n) * ncol(tab_n)) ||
             length(cochran) == 2 && cochran[[2]] < round(0.8*nrow(tab_n) * ncol(tab_n))) {
            
            # Effect size: Cramer's V
            
            v = cramerV(tab_n, bias.correct = T)
            
            message1 = "No effect size have been specifically designed for use after a Fisher's Exact Test to my knowlegde.\nThe Cramer's V with a bias correction (violation of Cochran's rule) was used instead."
            
          }
          
          # Satisfied Cochran's Rule 
          
          else {
            
            # Effect size: Cramer's V
            
            v = cramerV(tab_n)
            
            message1 = "No effect size have been specifically designed for use after a Fisher's Exact Test to my knowlegde.\nThe Cramer's V without bias correction (Cochran's rule satisfied) was used instead."
            
          }
          
          df = suppressWarnings(chisq.test(tab_n))[[2]][[1]]
          
          magnitude = mv(v = v, df = df)
          
          # Post-hoc
          
          # X has two levels
          
          if(length(unique(X1)) == 2) {
            
            # Post-hoc: Pairwise Fisher's Exact Test
            
            ph = r(fisher.multcomp(tab_n)[[4]])
            
          }
          
          # X has multiple levels
          
          else {
            
            if(length(unique(Y)) == 2) var_type = "Variable type: Qualitative dependent variable with two levels (Y)\nOne factor (X1) with more than two independent levels."
            
            else var_type = "Variable type: Qualitative dependent variable with more than two levels (Y)\nOne factor (X1) with more than two independent levels."
            
            # Post-hoc: Pairwise Fisher's Exact Test and Cramer's V
            
            ph = pairwiseNominalIndependence(tab_n, compare = "row", gtest = F, chisq = F, cramer = T)
            
            magn = vector(length = length(ph[[4]]))
            
            for(i in 1:length(ph[[4]])) {
              magn[i] = mv(ph[[4]][[i]], df = ncol(tab_n))
            }
            
            ph = data.frame(Comparison = ph[[1]], p_value = r(ph[[3]]), Cramer_V = r(ph[[4]]), Magnitude = magn)
            colnames(ph) = c("Comparison", "p-value", "Cramer's V", "Magnitude")
            
          }
          
          ### Display results
          
          test = paste("Fisher's Exact Test: p-value = ", r(test1[[1]]), s(test1[[1]]), sep = "")
          
          test_apa = paste("Fisher's Exact Test: *p* = ", p_apa(test1[[1]]), sep = "")
          
          eff = paste("Cramer's V = ", r(v), magnitude, sep = "")
          
          eff_apa = paste("Cramer's V = ", x1_apa(v), magnitude, sep = "")
          
          name_ph = "Pairwise Fisher's exact test (fdr adjustment method) :"
          
          if(apa) display(tab = tab_n, vali1 = var_type, test1 = test, apa_test1 = test_apa, asso1 = eff_apa, name_ph1 = name_ph, 
                          ph1 = ph, mes1 = message1, mes2 = cm("message2"))
          
          else display(tab = tab_n, vali1 = var_type, test1 = test, asso1 = eff, name_ph1 = name_ph,
                       ph1 = ph, mes1 = message1, mes2 = cm("message2"))
          
        }
        
      }
      
      else if(paired == "second" || paired == "both")
        stop('Only one factor: the paired argument must be either "none" or "first".')
      
      else if(paired == "first" && is.null(id))
        stop('Please provide the ID of the subjects in the ID argument, or change the paired argument to "none".')
      
      # Repeated measures
      
      else {
        
        data_long = group_by(ID, .data = data.frame(Y, X1, ID)) %>% 
          mutate(Number_of_measure = n(), Number_of_levels = length(unique(X1))) %>% arrange(ID, X1)
        
        data_long$ID = factor(data_long$ID, ordered = T)
        
        if(length(unique(data_long$Number_of_measure)) != 1 || length(unique(data_long$Number_of_levels)) != 1) {
          print(as.data.frame(data_long))
          stop("All the individuals haven't been measured during all sessions (see above). Revise your data or the paired argument.") }
        
        else if(length(unique(data_long$X1)) != data_long$Number_of_measure[1]) 
          stop("The levels of the factor change accross subjects. Please check the spelling of the factor's levels.") 
        
        else {
          
          data_long = data_long[ , -which(names(data_long) %in% c("Number_of_measure", "Number_of_levels"))]
          
          data_wide = data_long %>% spread(X1, Y)
          
          tab_n = table(data_wide[,-1])
          
          tab_f = ftable(data_wide[,-1])
          
          # X has two levels
          
          if(length(unique(X1)) == 2) {
            
            # Y has two levels (2 x 2)
            
            if(length(unique(Y)) == 2) {
              
              # Exact McNemar's Test
              
              test1 = mcnemar.exact(tab_n)
              
              # Effect size: Cohen's g
              
              g = (tab_n[1, 2]/(tab_n[1, 2]+tab_n[2, 1])) # 0.5
              
              # Odds ratio and their CI
              
              o = mcnemar.exact(tab_n)[[5]][[1]]
              
              ci_o = mcnemar.exact(tab_n)[[4]][1:2]
              
              ### Displaying result
              
              var_type = "Variable type: Qualitative dependent variable with two levels (Y)\nOne factor (X1) with two levels of repeated measures."
              
              test = paste("Exact McNemar's Test: b = ", r(test1[[1]][[1]]), ", c = ", 
                           r(test1[[2]][[1]]), ", p-value = ", r(test1[[3]][[1]]), s(test1[[3]][[1]]), sep = "")
              
              test_apa = paste("Exact McNemar's Test: b = ", x_apa(test1[[1]][[1]]), ", c = ", 
                               x_apa(test1[[2]][[1]]), ", *p* = ", p_apa(test1[[3]][[1]]), sep = "")
              
              eff = paste("Cohen's g = ", r(g), m(g, lim1 = 0.05, lim2 = 0.15, lim3 = 0.25), sep = "")
              
              eff_apa = paste("Cohen's *g* = ", x1_apa(g), sep = "")
              
              odds = paste("Odds ratio = ", r(o), ", 95% CI [", r(ci_o[[1]]), ", ", r(ci_o[[2]]), "]",
                           m(o, lim1 = 1.22, lim2 = 1.86, lim3 = 3, OR = T), sep = "")
              
              odds_apa = paste("OR = ", x_apa(o), ", 95% CI [", x_apa(ci_o[[1]]), ", ", x_apa(ci_o[[2]]), "]", sep = "")
              
              if(apa) display(tab = tab_n, vali1 = var_type, test1 = test, apa_test1 = test_apa, asso1 = eff, 
                              apa_asso1 = eff_apa, asso2 = odds_apa)
              
              else display(tab = tab_n, vali1 = var_type, test1 = test, asso1 = eff, asso2 = odds_apa)
              
            }
            
            # Y has more than two levels (n x n)
            
            else {
              
              # Asymptotic General Symmetry Test
              
              test1 = symmetry_test(Y ~ as.factor(X1) | ID, data_long)
              Z = test1@statistic@teststatistic
              p_value1 = pvalue(test1)
              
              # Stuart-Maxwell Marginal Homogeneity Test
              
              test2 = mh_test(tab_n)
              x_squared = test2@statistic@teststatistic
              df = test2@statistic@df
              p_value2 = pvalue(test2)
              
              # Effect size: Cohen's G & Odds ratio (global and pairwise)
              
              g_global = cohenG(tab_n)[[1]][[4]]
              
              o_global = cohenG(tab_n)[[1]][[3]]
              
              eff_pairwise = data.frame(Comparison = cohenG(tab_n)[[2]][[1]], Odds_ratio = r(cohenG(tab_n)[[2]][[2]]), 
                                        Cohen_g = r(cohenG(tab_n)[[2]][[4]]))
              colnames(eff_pairwise) = c("Comparison", "Odds-ratio", "Cohen's G")
              
              # Post-hoc: Pairwise Symmetry Test
              
              ph = nominalSymmetryTest(tab_n)
              
              ph = data.frame(Comparison = ph[[2]][[1]], p_value = r(ph[[2]][[3]]))
              colnames(ph) = c("Comparison", "p-value")
              
              ### Displaying results
              
              var_type = "Variable type: Qualitative dependent variable with more than two levels (Y)\nOne factor (X1) with two levels of repeated measures."
              
              test1_n = paste("Asymptotic General Symmetry Test: Z = ", r(Z), ", p-value = ", r(p_value1), s(p_value1), sep = "")
              
              test1_apa = paste("Asymptotic General Symmetry Test: Z = ", x_apa(Z), ", *p* = ", p_apa(p_value1), sep = "")
              
              test2_n = paste("Stuart-Maxwell Marginal Homogeneity Test: X-squared = ", r(x_squared), ", df = ", df,
                              ", p-value = ", r(p_value2), s(p_value2), sep = "")
              
              test2_apa = paste("Stuart-Maxwell Marginal Homogeneity Test: &chi;^2^(", df, ") = ", x_apa(x_squared), 
                                ", *p* = ", p_apa(p_value2), sep = "")
              
              g = paste("Cohen's g = ", r(g_global), m(g_global, lim1 = 0.05, lim2 = 0.15, lim3 = 0.25), sep = "")
              
              g_apa = paste("Cohen's *g* = ", x1_apa(g_global), sep = "")
              
              o = paste("Odds ratio = ", r(o_global), m(o_global, lim1 = 1.22, lim2 = 1.86, lim3 = 3, OR = T), sep = "")
              
              o_apa = paste("OR = ", x_apa(o_global), sep = "")
              
              name_ph = "Pairwise Symmetry Tests (fdr adjustment method):"
              
              if(apa) display(tab = tab_n, vali1 = var_type, test1 = test1_n, apa_test1 = test1_apa, test2 = test2_n,
                              apa_test2 = test2_apa, asso1 = g, apa_asso1 = g_apa, asso2 = o, apa_asso2 = o_apa,
                              other_asso = eff_pairwise, ph1 = ph)
              
              else display(tab = tab_n, vali1 = var_type, test1 = test1_n, test2 = test2_n, asso1 = g, asso2 = o,
                           other_asso = eff_pairwise, name_ph1 = name_ph, ph1 = ph)
              
            }
            
          }
          
          # X has more than two levels
          
          else {
            
            # Asymptotic General Symmetry Test
            
            test1 = symmetry_test(Y ~ as.factor(X1) | ID, data_long)
            Z = test1@statistic@teststatistic
            p_value1 = pvalue(test1)
            
            # Cochran's Q Test 
            
            if(length(unique(Y)) == 2) assign("test2", cochran.qtest(Y ~ X1 | ID, data = data_long), envir = fct.env)
            
            # Post-hoc
            
            ph = pairwisePermutationSymmetry(Y ~ X1 | ID, data_long)
            ph = data.frame(Comparison = ph[1], p_value = r(ph[4]))
            colnames(ph) = c("Comparison", "p-value")
            name_ph = "Pairwise two-sample permutation symmetry tests (fdr adjustment method)"
            
            ### Displaying results
            
            var_type = "Variable type: Qualitative dependent variable with two levels (Y)\nOne factor (X1) with more than two levels of repeated measures."
            
            test1_n = paste("Asymptotic General Symmetry Test: Z = ", r(Z), ", p-value = ", r(p_value1), s(p_value1), sep = "")
            
            test1_apa = paste("Asymptotic General Symmetry Test: Z = ", x_apa(Z), ", *p* = ", p_apa(p_value1), sep = "")
            
            if(exists("test2", envir = fct.env)) 
              assign("test2_n", paste("Cochran's Q test: Q = ", r(get("test2", envir = fct.env)[[3]][[1]]), ", df = ", 
                                      get("test2", envir = fct.env)[[4]][[1]], ", p-value = ", 
                                      r(get("test2", envir = fct.env)[[7]][[1]]), 
                                      s(get("test2", envir = fct.env)[[7]][[1]]), sep = ""), envir = fct.env)
            
            if(exists("test2", envir = fct.env)) 
              assign("test2_apa", paste("Cochran's Q test: Q(", get("test2", envir = fct.env)[[4]][[1]], ") = ", 
                                        x_apa(get("test2", envir = fct.env)[[3]][[1]]), ", *p* = ", 
                                        p_apa(get("test2", envir = fct.env)[[7]][[1]]), sep = ""), envir = fct.env)
            
            if(apa) display(tab = tab_f, vali1 = var_type, test1 = test1_n, apa_test1 = test1_apa, 
                            test2 = cm("test2_n"),
                            apa_test2 = cm("test2_apa"), name_ph1 = name_ph, ph1 = ph)
            
            else display(tab = tab_f, vali1 = var_type, test1 = test1_n, 
                         test2 = cm("test2_n"), name_ph1 = name_ph, ph1 = ph)
            
          }
          
        }
        
      }
      
    }
    
    ### Two X 
    
    else if(!is.null(x1) && !is.null(x2)) {
      
      # Both X are independent measures
      
      if(paired == "none") {
        
        tab_n = table(X1, Y, X2)
        
        tab_f = ftable(tab_n) 
        
        # Pairwise odds ratio 
        
        if(any(tab_f[1:dim(tab_f)[1], 1:dim(tab_f)[2]] == 0)) {
          
          o_p = data.frame(Log_odds_ratio_with_continuity_correction = t(t(r(loddsratio(tab_n, correct = T, log = F)[[1]]))))
          colnames(o_p) = "Odds ratio with continuity correction"
          
        }
        
        else {
          
          o_p = data.frame(Log_odds_ratio = t(t(r(loddsratio(tab_n, log = F)[[1]]))))
          colnames(o_p) = "Odds ratio without continuity correction"
          
        }
        
        # Post-hoc: Groupewise Exact Fisher's Test
        
        name_ph = "Groupewise Exact Fisher's Tests (fdr adjustment method):"
        ph = groupwiseCMH(tab_n, group = 3, fisher = T)
        ph =  data.frame(ph[1], r(ph[4]))
        colnames(ph) = c("Group", "p-value")
        
        # Woolf's Test: Homogeneity of odds ratios across levels of X2
        
        vali = woolf_test(tab_n)
        
        if(vali[[3]] > 0.05 && length(unique(X1)) == 2) {
          
          # 2 x 2 x k Table
          
          if(dim(tab_n)[2] == 2) {
            
            var_type = "Variable type: Qualitative dependent variable (Y) with two levels\nTwo independent factors (X) and X1 has two levels."
            
            if(sum(tab_n) <= 1000) {
              
              # Exact Mantel-Haenszel Test (only for small dataset)
              
              test1 = mantelhaen.test(tab_n, exact = T)
              
              test = paste("Exact conditional test of independence: S = ", r(test1[[1]][[1]]), ", p-value = ", r(test1[[2]][[1]]),
                           s(test1[[2]][[1]]), sep = "")
              
              test_apa = paste("Exact M-H Test: S = ", test1[[2]][[1]], ", *p* = ", p_apa(test1[[2]][[1]]), sep = "")
              
            }
            
            else {
              
              # Mantel-Haenszel Test with continuity correction (for large dataset)
              
              test1 = mantelhaen.test(tab_n)
              
              test = paste("Mantel-Haenszel Chi-squared Test with continuity correction: X-squared = ", r(test1[[1]][[1]]), 
                           ", df = ", test1[[2]][[1]], ", p-value = ", r(test1[[3]][[1]]), s(test1[[3]][[1]]), sep = "")
              
              test_apa = paste("M-H c.c. Test: &chi;^2^(", test1[[2]][[1]], ") = ", x_apa(test1[[1]][[1]]),
                               ", *p* = ", p_apa(test1[[3]][[1]]), sep = "")
              
            }
            
          }
          
          # 2 x n x k Table (n > 2)
          
          else {
            
            var_type = "Variable type: Qualitative dependent variable (Y) with more than two levels\nTwo independent factors (X) and X1 has two levels."
            
            test1 = mantelhaen.test(tab_n)
            
            test = paste("Cochran-Mantel-Haenszel Test: M^2^ = ", r(test1[[1]][[1]]), 
                         ", df = ", test1[[2]][[1]], ", p-value = ", r(test1[[3]][[1]]), s(test1[[3]][[1]]), sep = "")
            
            test_apa = paste("CMH Test: M^2^(", test1[[2]][[1]], ") = ", x_apa(test1[[1]][[1]]),
                             ", *p* = ", p_apa(test1[[3]][[1]]), sep = "")
            
          }
          
          # General Odds ratio
          
          if(dim(tab_n)[2] == 2) {
            
            assign("o_g", paste("Odds ratio (conditional Maximum Likelihood Estimate) = " , r(exp(test1[[4]][[1]])), " 95% CI [", 
                                r(exp(test1[[3]][1])), ", ", r(exp(test1[[3]][2])), "]", 
                                m(exp(test1[[4]][[1]]), lim1 = 1.55, lim2 = 2.8, lim3 = 5, OR = T), sep = ""), envir = fct.env)
            
            assign("o_g_apa", paste("OR = ", x_apa(exp(test1[[4]][[1]])), " 95% CI [", x_apa(exp(test1[[4]][[1]])), ", ", 
                                    x_apa(exp(test1[[4]][[1]])), "]", sep = ""), fct.env)
            
          }
          
          ### Displaying results
          
          val = paste("Woolf's Test: X-squared = ", r(vali[[1]][[1]]), ", df = ", vali[[2]][[1]],
                      ", p-value = ", r(vali[[3]][[1]]), " -> Satisfied -> Homogeneity of odds ratios across levels of X2 (p-value > 0.05)", sep = "")
          
          mess1 = "This test has been designed to know if there is an association between the 1st factor (X1)\nand the dependant variable (Y). As the 2nd factor (X2) is used for adjustement, you should\nchange the position in the formula if this is the variable of main interest."
          
          if(apa) display(tab = tab_f, vali1 = var_type, vali2 = val, test1 = test, apa_test1 = test_apa, 
                          asso1 = cm("o_g"), apa_asso1 = cm("o_g_apa"), other_asso = o_p, 
                          name_ph1 = name_ph, ph1 = ph, mes1 = mess1)
          
          else display(tab = tab_f, vali1 = var_type, vali2 = val, test1 = test, asso1 = cm("o_g"), 
                       other_asso = o_p, name_ph1 = name_ph, ph1 = ph, mes1 = mess1)
          
        }
        
        # Impossible to run tests above or Woolf's Test significant
        
        else {
          
          if(length(unique(X1)) == 2) {
            
            var_type = var_type = "Variable type: Qualitative dependent variable (Y) with two levels\nTwo independent factors (X)."
            
            assign("vali2", paste("Woolf's Test: X-squared = ", r(vali[[1]][[1]]), ", df = ", vali[[2]][[1]],
                                  ", p-value = ", r(vali[[3]][[1]]), " -> Not satisfied -> Heterogeneity of odds ratios across levels of X2 (p-value <= 0.05)", sep = ""))
          }
          
          else var_type = "Variable type: Qualitative dependent variable (Y) with more than two levels\nTwo independent factors (X)."
          
          data$Y = as.factor(data$Y)
          
          mod_simple = suppressWarnings(glm(Y ~ X1 + X2, family = binomial, data))
          
          mod_interaction = suppressWarnings(glm(Y ~ X1 * X2, family = binomial, data))
          
          dev = anova(mod_simple, mod_interaction, test = "LRT")
          
          # Wald's Test on an additive model (logistic regression) 
          
          if(is.na(dev[[5]][2]) || dev[[5]][2] > 0.05) {
            
            vali3_type = " -> Additive model."
            
            lrt = Anova(mod_simple, type = 2)
            
            test = paste("Likelihood Ratio Test on Logistic Regression model (Type II tests):\nX1 -> X-squared = ", r(lrt[[1]][1]), ", df = ", lrt[[2]][1], 
                         ", p-value = ", r(lrt[[3]][1]), s(lrt[[3]][1]), "\nX2 -> X-squared = ", r(lrt[[1]][2]), 
                         ", df = ", lrt[[2]][2], ", p-value = ", r(lrt[[3]][2]), s(lrt[[3]][2]), sep = "")
            
            test_apa = paste("LRT Test:\nX1 -> &chi;^2^(", lrt[[2]][1], ") = ", x_apa(lrt[[1]][1]), ", *p* = ", 
                             p_apa(lrt[[3]][1]), "\nX2 -> &chi;^2^(", lrt[[2]][2], ") = ", x_apa(lrt[[1]][2]), ", *p* = ", 
                             p_apa(lrt[[3]][2]), sep = "")
            
          }
          
          # Wald's Test on a multiplicative model (logistic regression) 
          
          else {
            
            vali3_type = " -> Multiplicative model."
            
            if(Anova(mod_interaction, type = 3)[[3]][3] > 0.05) {
              
              type = " (Type II tests)"
              
              lrt = Anova(mod_interaction, type = 2)
              
            }
            
            else {
              
              type = " (Type III tests)"
              
              lrt = Anova(mod_interaction, type = 3)
              
            }
            
            test = paste("Likelihood Ratio Test on Logistic Regression model", type, ":\nX1 -> X-squared = ", r(lrt[[1]][1]), ", df = ", lrt[[2]][1], 
                         ", p-value = ", r(lrt[[3]][1]), s(lrt[[3]][1]), "\nX2 -> X-squared = ", r(lrt[[1]][2]), 
                         ", df = ", lrt[[2]][2], ", p-value = ", r(lrt[[3]][2]), s(lrt[[3]][2]), 
                         "\nX1:X2 -> X-squared = ", r(lrt[[1]][3]), ", df = ", lrt[[2]][3], ", p-value = ", 
                         r(lrt[[3]][3]), s(lrt[[3]][3]), sep = "")
            
            test_apa = paste("LRT Test:\nX1 -> &chi;^2^(", lrt[[2]][1], ") = ", x_apa(lrt[[1]][1]), ", *p* = ", 
                             p_apa(lrt[[3]][1]), "\nX2 -> &chi;^2^(", lrt[[2]][2], ") = ", x_apa(lrt[[1]][2]), ", *p* = ", 
                             p_apa(lrt[[3]][2]), "\nX1:X2 -> &chi;^2^(", lrt[[2]][3], ") = ", x_apa(lrt[[1]][3]), ", *p* = ", 
                             p_apa(lrt[[3]][3]), sep = "")
            
          }
          
          vali3 = paste("Likelihood Ratio Test on Logistic Regression model:\nDeviance between additive (Y ~ X1 + X2) & multiplicative model (Y ~ X1 * X2) = ", 
                        r(dev[[4]][2]), ", p-value = ", ifelse(is.na(dev[[5]][2]), 1, r(dev[[5]][2])), 
                        vali3_type , sep = "")
          
          if(apa) display(tab = tab_f, vali1 = var_type, vali2 = cm("vali2"), vali3 = vali3, test1 = test, apa_test1 = test_apa,
                          other_asso = o_p, name_ph1 = name_ph, ph1 = ph)
          
          else display(tab = tab_f, vali1 = var_type, vali2 = cm("vali2"), vali3 = vali3, test1 = test, 
                       other_asso = o_p, name_ph1 = name_ph, ph1 = ph)
          
        }
        
      }
      
      # One or two X are repeated measures: NOT SUPPORTED
      
      else
        stop("This function does not support design with a qualitative Y and one or both paired factors. However, 
              you can test the effect of the factors (repeated or not) on Y one after the other using this function.")
      
    }
    
  }
  
  ### Quantitative Y
  
  else stop("
            For the moment, the auto_stats function can only analyse data if Y is qualitative.
            It will soon be available if Y is quantitative.
            ")
  
}


######################################## Code in progress if Y is quantitative #############################################

### Quantitative Y
#
#else if(is.numeric(y) || is.integer(y)) 
#  
#  else 
#    
#    stop("Y variable must belong to the class numeric, integer, character or factor. 
#                 Check it with is.[...] and change it with as.[...]).")
#
### No X
#
#if(is.null(X1) && is.null(X2) && is.null(MU))
#  stop("No analysis of Y could be conducted if Y is quantitative and X1, X2, ID or MU have not been filled.")
#
#
#else if(is.null(X1) && is.null(X2) && !is.null(MU))
#  
#  # Independence
#  
#  if(paired == "none")
#    
#    if(is.null(theoric_mean))
#      theoric_mean = 0
#    warning("No theoric mean have been provided to compare it with the mean of Y. 
#                Therefore, the theoric mean was automatically set to 0.")
#    
#    
#    else
#      
#     # Non-normality of Y (Shapiro-Wilk Test) and/or outliers (Boxplot Method)
#      
#      if(shapiro.test(Y)[2] > 0.05 || identify_outliers(nrow(as.data.frame(Y)) != 0)
#         
#         if(identify_outliers(nrow(as.data.frame(Y)) != 0)
#           warning("There was outliers : please check for measurement and/or experimental errors. If so, remove those values 
#                        from the dataframe. Otherwise, a non-parametric method (more robust) was used to deal with those outliers.")
#            
#            # One-sample Wilcoxon's Test 
#            
#           wilcox.test(Y, mu = theoric_mean)
#           
#           # Effect size : r
#           
#           wilcox_effsize(Y ~ 1, mu = theoric_mean, data = as.data.frame(Y))
#           
#           # Pseudo-median of Y and its CI
#           
#           wilcox.test(Y, mu = theoric_mean, conf.int = T)[[9]][[1]]
#           wilcox.test(Y, mu = theoric_mean, conf.int = T)[[8]][1:2]
#           
#           # Normality of Y (Shapiro-Wilk Test) and no outliers (Boxplot Method)
#           
#           else
#             
#             # One-sample Student's T-Test
#             
#             t.test(Y, mu = theoric_mean)
#           
#            # Effect size: Cohen's D
#           
#           cohens_d(Y ~ 1, mu = theoric_mean, data = as.data.frame(Y))
#            
#           # Mean of Y and its CI
#           
#           t.test(Y, mu = theoric_mean)[[5]][[1]]
#           t.test(Y, mu = theoric_mean)[[4]][1:2]
#           
#           # Repeated measures: ERROR
#           
#           else
#             
#             stop("Y cannot be paired. Add a repeated factor in the formula.")
#            
#           ### Un X
#           
#           else if(!is.null(X1) && is.null(X2))
#             
#             # Qualitative X
#             
#             if(is.factor(x) ||
#                is.character(x))
#               
#               # X has two levels
#               
#               else if(length(unique(X1)) == 2)
#                 
#                 mod.lm = lm(Y ~ X1, data)
#           
#           #  Independence
#           
#           if(paired == "none")
#             
#             # Normality of Y (Shapiro-Wilk Test)
#             
#             if(shapiro.test(Y)[2] > 0.05)
#               
#               # Outliers (Boxplot Method)
#               
#               if(nrow(group_by(X1, .data = data) %>% identify_outliers(Y)) != 0) 
#                 
#                 warning("There was outliers : please check for measurement and/or experimental errors. If so, remove those values 
#                             from the dataframe. Otherwise, a robust method was used to deal with those outliers.")
#           
#           # Yuen's Test (20% trimmed means)
#           
#           yuen(Y ~ X1, data = data)
#           
#           # Effect size: Robust Cohen's D
#           
#           yuen(Y ~ X1, data = data)$effsize
#           
#           # Difference between the means and its CI
#           
#           yuen(Y ~ X1, data = data)$diff
#           
#           yuen(Y ~ X1, data = data)$conf.int
#           
#           # No outliers (Boxplot Method)
#           
#           else
#             
#             # Homoscedasticity (Fisher-Snedecor Test)
#             
#             if(var.test(Y ~ X1)[3] > 0.05)
#               
#               # Two-sample Student's T-Test
#               
#               t.test(Y ~ X1, var.equal = T)
#           
#           # Effect size: Cohen's D
#           
#           cohens_d(Y~X1, data = data, var.equal = T)
#           
#           # Difference between the means and its CI
#           
#           t.test(Y ~ X1, var.equal = T)$estimate[1] - t.test(Y ~ X1, var.equal = T)$estimate[2]
#           
#           t.test(Y ~ X1, var.equal = T)$conf.int
#           
#           # Sous-sujet 4
#           
#           # Heteroscedasticity (Fisher-Snedecor Test)
#           
#           else
#             
#             # Two-sample Student's T-Test with Welch correction
#             
#             t.test(Y ~ X1)
#           
#           # Effect size: Cohen's D
#           
#           cohens_d(Y~X1, data = data)
#           
#           # Difference between the means and its CI
#           
#           t.test(Y ~ X1)$estimate[1] - t.test(Y ~ X1)$estimate[2]
#           
#           t.test(Y ~ X1)$conf.int
#           
#           # Non-normality of Y (Shapiro-Wilk Test)
#           
#           else
#             
#             # Two-sample Wilcoxon's Test
#             
#             wilcox.test(Y ~ X1)
#           
#           # Effect size: r
#           
#           wilcox_effsize(Y~X1, data = data)
#           
#           # Difference between the means and its CI
#           
#           wilcox.test(Y ~ X1)$estimate
#           
#           wilcox.test(Y ~ X1, conf.int=T)$conf.int
#           
#           # Repeated measures
#           
#           else if(paired == "both" || paired == "second") 
#             stop('There is only one factor: please state paired == "first".')
#           
#           else if(paired == "first") 
#             
#             if(ID == NULL)
#               warning("IMPORTANT: Please, make sure that the dataframe is ordered so that the 1st observation of 
#                          Group 1 is the same subject as the 1st observation of Group 2 and so on.")
#           
#           else
#             group_by(X1, .data = data) %>% arrange(X1, ID)
#           
#           if(var(table(data$X1) != 0)
#              stop("There is not two observations per subject. Verify the paired argument or your data.")
#              
#              data.long = reshape(data, timevar = "X1", direction = "wide") %>% select(starts_with("Y"))
#              
#              diff = c(data.long[1] # data.long[2])[[1]]
#                       
#                       data.diff = cbind(data.long, diff)
#                       
#                       # Normality of the difference (Shapiro-Wilk Test)
#                       
#                       if(shapiro.test(diff) > 0.05)
#                         
#                         # Outliers (Boxplot Method)
#                         
#                         if(nrow(identify_outliers(diff, data = data.diff)) != 0)
#                           
#                           warning("There was outliers : please check for measurement and/or experimental errors. 
#                                       If so, remove those values from the dataframe. Otherwise, a robust method was used to deal 
#                                       with those outliers.")
#                       
#                       # Paired Yuen's Test (20% trimmed means)
#                       
#                       yuend(data.long[1], data.long[2])
#                       
#                       # Taille d'effet : Robust Cohen's d
#                       
#                       yuend(data.long[1], data.long[2])$effsize
#                       
#                       # Difference between means and its CI
#                       
#                       yuend(data.long[1], data.long[2])$diff
#                       
#                       yuend(data.long[1], data.long[2])$conf.int
#                       
#                       # No outliers (Boxplot Method)
#                       
#                       else
#                          
#                         # Paired Two-sample Student's T-Test
#                         
#                         t.test(Y ~ X1, paired = T)
#                       
#                       # Effect size: Cohen's D
#                       
#                       cohens_d(Y~X1, data = data, paired = T)
#                       
#                       # Difference between means and its CI
#                       
#                       t.test(Y ~ X1, paired = T)$estimate
#                       
#                       t.test(Y ~ X1, paired = T)$conf.int
#                       
#                       # Non-normality of the difference (Shapiro-Wilk Test)
#                       
#                       else if(shapiro.test(diff) <= 0.05)
#                         
#                         # Paired Two-sample Wilcoxon's Test
#                         
#                         wilcox.test(Y ~ X1, paired = T)
#                       
#                       # Effect size: r
#                       
#                       wilcox_effsize(Y ~ X1, data = data, paired = T)
#                       
#                       # Difference between means and its CI
#                       
#                       wilcox.test(Y ~ X1, paired = T)$estimate
#                       
#                       wilcox.test(Y ~ X1, paired = T, conf.int=T)$conf.int
#                       
#                       # X has more than two levels
#                       
#                       else if(length(unique(X1)) > 2)
#                         
#                         mod.lm = lm(Y ~ X1, data)
#                       mod.aov = aov(Y ~ X1, data)
#                       resids = summary(mod.lm)$residuals
#                       
#                       # Independence
#                       
#                       if(paired == "none")
#                         
#                         # Non-normality of the residuals (Shapiro-Wilk Test)
#                         
#                         if(shapiro.test(resids)$p.value <= 0.05)
#                           
#                           # Kruskal-Wallis Test
#                           
#                          kruskal.test(Y ~ X1, data)
#                      
#                       # Effect size: Eta-squared (based on H-statistic)
#                       
#                       kruskal_effsize(Y ~ X1, data = data)
#                       
#                       # Post-hoc: Dunn's Test
#                       
#                      dunn_test(Y ~ X1, p.adjust.method = "holm", data = data, detailed = T)
#                     
#                     # Normality of the residuals (Shapiro-Wilk Test)
#                    
#                    else
#                      
#                      # Outliers (Boxplot Method)
#                      
#                          if( nrow(group_by(X1, .data = data) %>% identify_outliers(Y)) != 0 )
#                             warning("There was outliers : please check for measurement and/or experimental errors. 
#                                        If so, remove those values from the dataframe. Otherwise, a robust method was used to 
#                                         deal with those outliers.")
#                        
#                        # Robust One-way ANOVA (20% trimmed means)
#                        
#                       robust_ANOVA = t1way(Y ~ X1, data)
#                       
#                       # Robust effect size
#                       
#                       robust_ANOVA[5]
#                       
#                       # Post-hoc: Pairwise comparison
#                       
#                       lincon(Y ~ X1, data)
#                       
#                       # No outliers (Boxplot Method)
#                       
#                       else
#                         
#                         # Heteroscedasticity (Levene's Test)
#                         
#                         if( levene_test(Y ~ X1, data = data)[[4]]  <= 0.05 )
#                           
#                           # Welch ANOVA
#                           
#                           welch_anova_test(Y ~ X1, data = data)
#                       
#                       # Effect size: Global Eta-squared
#                       
#                       anova_test(Y ~ X1, data = data, effect.size = "ges")[[7]]
#                       
#                       warning("The effect size might be slightly inacurrate since it was calculated with an homoscedastic model. 
#                                   Indeed, the effect size to use after a Welch ANOVA is still being discussed and has not been coded 
#                                   in R yet (2020) to my knowledge.")
#                       
#                         #  Post-hoc: Games-Howell's Test
#                       
#                       games_howell_test(Y ~ X1, data = data, detailed = T)
#                       
#                       # Homoscedasticity (Levene's Test)
#                       
#                       else
#                         
#                         # One-way ANOVA
#                         
#                         anova_test(Y ~ X1, data = data)
#                       
#                       # Effect size: Global Eta-squared
#                        
#                        anova_test(Y ~ X1, data = data, effect.size = "ges")[[7]]
#                       
#                       # Post-hoc: Tukey's Test
#                       
#                       tukey = TukeyHSD(model.aov, ordered = T)[[1]]
#                       tukey[order(-tukey[,1]),]
#                       
#                       # Repeated measures
#                       
#                       else if(paired == "both")
#                         
#                         if(ID == NULL)
#                           stop("Please provide the name of the column containing the blocking variable (i.e. id of the subjects) 
#                                    in the ID argument.")
#                       
#                       # Non-normality of Y by groups (Shapiro-Wilk Test)
#                       
#                       grouped_normality = group_by(X1, .data = data) %>%
#                         shapiro_test(Y)
#                       
#                       if( any(grouped_normality[5] <= 0.05)
#                           
#                           # Friedman's Test
#                           
#                           friedman.test(Y ~ X1 |ID, data = selfesteem)
#                           
#                           # Effect-size: Kendall's W
#                           
#                           friedman_effsize(Y ~ X1 |ID)
#                           
#                           # Pairwise comparison (Wilcoxon's Tests)
#                           
#                           wilcox_test(Y ~ X1, paired = T, p.adjust.method = "holm", data = data)
#                           
#                           warning("IMPORTANT: Please, make sure that dataframe is ordered so that the 1st observation of 
#                                       Group 1 is the same subject as the 1st observation of Group 2 and so on. The post-hoc analysis 
#                                       would otherwise be completely wrong.")
#                           
#                           # Normality of Y by groups (Shapiro-Wilk Test)
#                           
#                           else
#                              
#                             # Outliers (Boxplot Method)
#                             
#                             if(nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0)
#                               
#                               warning("There was outliers : please check for measurement and/or experimental errors. 
#                                           If so, remove those values from the dataframe. Otherwise, a robust method have been used 
#                                           to deal with those outliers.")
#                           
#                           # Robust paired One-way ANOVA
#                           
#                           rmanova(Y, X1, ID)
#                           
#                           # Effect size: Global Eta-squared
#                           
#                           anova_test(dv = Y, wid = ID, within = X1, data = data, effect.size = "ges")[[1]][7]
#                           
#                           warning("No packages offering functions specifically designed to measure effect size of a 
#                                       robust one-way repeated model are implemented in R to my knowledge. The effect size given 
#                                       here is a partial eta squared of a parametric one-way repeated model.")
#                           
#                           # Pairwise comparison
#                           
#                           rmmcp(Y, X1, ID)
#                           
#                           # No outliers (Boxplot Method)
#                           
#                           # One-way paired ANOVA
#                           
#                           paired_anova = anova_test(dv = Y, wid = ID, within = X1, data = data, effect.size = "ges") 
#                           
#                           mauchly_test = paired_anova[2]
#                           
#                           corrected_anova = get_anova_table(paired_anova, correction = "auto")
#                           
#                           # Effect size: Global Eta-squared
#                           
#                           corrected_anova[[7]]
#                           
#                           # Pairwise comparison (Student's T-Tests)
#                           
#                           pairwise_t_test(Y ~ X1, data = data, paired = T, p.adjust.method = "holm")
#                           
#                           # Quantitative X
#                           
#                           else
#                             
#                             # Linear relationship between the mean of Y and the residuals of the model (Linearity Test)
#                             
#                             if(gvlma(mod.lm)$GlobalTest$GlobalStat4$pvalue <= 0.05)
#                             { warning("Non-linear relationship between the factor and Y, which may decrease accuracy. Therefore, 
#                                           you should consider transforming the variables (e.g. log, sqrt, polynomial regression) or 
#                                           using a GAM (i.e performing a non-linear regression).")
#                               
#                               # Absence of residuals' autocorrelation (Durbin-Watson Test)
#                               
#                               if(dwtest(mod.lm) <= 0.05)
#                               { warning("Residuals are autocorelated, which may decrease accuracy. This is mainly problematic 
#                                             for Time Series Data but it can be fixed by adding a factor containing lagged residuals to 
#                                             the original model.") }
#                               
#                               # Independence
#                               
#                               if(paired == "none")
#                                 
#                                 # Heteroscedasticity (Breusch-Pagan Test) and/or non-normality of the residuals (Shapiro-Wilk Test)
#                                 
#                                 if( bptest(mod.lm)$p.value > 0.05 || shapiro.test(resids)$p.value > 0.05)
#                                   
#                                   # Linear Robust Regression
#                                   
#                                   anova(lmRob(Y ~ X1, data))
#                               
#                               # Kendall's correlation
#                               
#                               cor(X1, Y, method = "kendall")
#                               
#                                # Homoscedasticity (Breusch-Pagan Test) and normality of the residuals (Shapiro-Wilk Test)
#
#                               else
#                                 
#                                 # Outliers (Bonferroni Outlier Test)
#                                 
#                                 if(outlierTest(mod.lm)[3] <= 0.05)
#                                   
#                                   warning("There was outliers : please check for measurement and/or experimental errors. 
#                                               If so, remove those values from the dataframe. Otherwise, a robust method was used to 
#                                               deal with those outliers.")
#                               
#                               # Winsorized correlation (20% winsorization)
#                               
#                               wincor(X1, Y)
#                               
#                               # No outliers (Bonferroni Outlier Test)
#                               
#                               else
#                                 
#                                 # Simple Linear Regression
#                                 
#                                 anova(mod.lm)
#                               
#                               # Pearson's correlation
#                               
#                               cor(X1, Y)
#                               
#                               # Repeated measures: ERROR
#                               
#                               else
#                                 
#                                 stop("The factor cannot be continuous and paired.")
#                               
#                               ### Two X
#                               
#                               else if(!is.null(X1) && !is.null(X2))
#                                 
#                                 mod.lm = lm(Y ~ X1*X2, data)
#                               mod.aov = aov(Y ~ X1*X2, data)
#                               resids = summary(mod.lm)$residuals
#                               
#                               # Both X are qualitative
#                               
#                               if(length(unique(X1)) == 1)
#                                 
#                                 stop("The 1st factor is constant while it must have at least two modalities.")
#
#                               else if(length(unique(X2)) == 1)
#                                 
#                                 stop("The 2nd factor is constant while it must have at least two modalities.")
#                               
#                                else
#                                 
#                                 # Both X are independent measures
#                                 
#                                 if(paired == "none")
#                                   
#                                   # Non-normality of the residuals (Shapiro-Wilk Test)
#                                   
#                                   if(shapiro.test(resids)$p.value <= 0.05)
#                                     
#                                     # Presence of missing values
#                                     
#                                     if(anyNA(Y) || anyNA(X1) || anyNA(X2))
#                                       
#                                       warning("No packages offering functions specifically designed for post-hoc analysis 
#                                                   after a permutation test have been coded in R to my knowledge. A pairwise Wilcoxon Test 
#                                                   was used instead of it.")
#                               
#                               # Monte-Carlo Test (permutation)
#                               
#                               permutation = ezPerm(dv = Y, wid = ID, between = c(X1, X2), data)
#                               
#                               # Effect size: Partial Eta-squared
#                               
#                               if(anova(mod.lm)[5][[1]][3] > 0.05 {
#                                 
#                                 ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 2, data = data, effect.size = "pes") }
#                                 
#                                 else ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 1, data = data, effect.size = "pes")
#                                 
#                                 ANOVA[[7]]
#                                 
#                                 warning("The standardized effect size was not calculated for a specific use after a 
#                                             permutation test. The result might be slightly inaccurate since it uses a parametric model.")
#                                 
#                                  # Post-hoc
#                                  
#                                 # Significant interaction effect
#                                 
#                                  if(permutation[[2]][3] <= 0.05)
#                                   
#                                   # Interaction plot
#                                   
#                                   plot(allEffects(mod.lm), multiline=T, ci.style="bars")
#                                 
#                                 # Grouped (2nd X) pairwise comparison of the 1st X (Wilcoxon's Tests)
#                                 
#                                 group_by(X2, .data = data) %>%
#                                   pairwise_wilcox_test(Y ~ X1,
#                                                        p.adjust.method = "holm"
#                                   )
#                                 
#                                 # Grouped (1st X) pairwise comparison of the 2nd X (Wilcoxon's Tests)
#                                 
#                                 group_by(X1, .data = data) %>%
#                                   pairwise_wilcox_test(Y ~ X2,
#                                                        p.adjust.method = "holm"
#                                   )
#                                                                  
#                                 # Significant main effects and non-significant interaction effect
#                                 
#                                 else if(permutation[[2]][1] <= 0.05 ||
#                                         permutation[[2]][2] <= 0.05)
#                                   
#                                   # Pairwise comparison of the 1st X (Wilcoxon's Tests)
#                                   
#                                   pairwise_wilcox_test(Y ~ X1, data = data, p.adjust.method = "holm")
#                                 
#                                 # Pairwise grouped comparison of the 2nd X (Wilcoxon's Tests)
#                                 
#                                 pairwise_wilcox_test(Y ~ X2, data = data, p.adjust.method = "holm")
#                                 
#                                 # Absence of missing values
#                                 
#                                 else
#                                   
#                                   # Two-way ANOVA on aligned rank model
#                                   
#                                   art_model = art(Y ~ X1*X2, data)
#                                 
#                                 if(anova(art_model)[[7]][3] > 0.05) {
#                                   ANOVA = anova(art_model, type = 2) }
#                                 
#                                 else ANOVA = anova(art_model, type = 1)
#                                 
#                                 # Effect size: Partial Eta-squared
#                                 
#                                 with(anova(model), `Sum Sq`/(`Sum Sq` + `Sum Sq.res`))
#                                 
#                                 # Post-hoc
#                                 
#                                 # Significant interaction effect
#                                 
#                                 if(ANOVA[[7]][3] <= 0.05)
#                                   
#                                   # Interaction plot
#                                   
#                                   plot(allEffects(mod.lm), multiline=T, ci.style="bars")
#                                 
#                                 # Pairwise comparison of the interaction (Tukey's Test)
#                                 
#                                 art_lm = artlm(art_model, colnames(art_model$aligned)[3])
#                                 
#                                 marginal_X1_X2 = emmeans(art_lm, ~ X1:X2)
#                                 
#                                 contrast(marginal_X1_X2, method="pairwise", adjust="tukey")
#                                 
#                                 # Significant main effects and non-significant interaction effect
#                                 
#                                 else if(ANOVA[[7]][1] <= 0.05 ||
#                                         ANOVA[[7]][2] <= 0.05)
#                                   
#                                   # Pairwise comparison of the 1st X (Tukey's Test)
#                                   
#                                   art_lm = artlm(art_model, colnames(art_model$aligned)[1])
#                                 
#                                 marginal_X1 = emmeans(model.lm, ~X1)
#                                 
#                                 pairs(marginal_X1, adjust = "tukey")
#                                 
#                                 # Pairwise comparison of the 2nd X (Tukey's Test)
#                                 
#                                 art_lm = artlm(art_model, colnames(art_model$aligned)[2])
#                                 
#                                 marginal_X2 = emmeans(model.lm, ~X2)
#                                 
#                                 pairs(marginal_X2, adjust = "tukey")
#                                 
#                                 # Normality of the residuals (Shapiro-Wilk Test)
#                                 
#                                 else
#                                   
#                                   # Outliers (Boxplot Method)
#                                   
#                                   if( nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0 )
#                                     warning("There was outliers : please check for measurement and/or experimental errors. If so, 
#                                                 remove those values from the dataframe. Otherwise, a robust method was used to deal with 
#                                                 those outliers.")
#                                 
#                                 # Robust Two-way ANOVA (20% trimmed means)
#                                  
#                                 robust_ANOVA = t2way(Y ~ X1*X2, data)
#                                 
#                                 # Effect size: Partial Eta-squared
#                                 
#                                 if(anova(mod.lm)[5][[1]][3] > 0.05 {
#                                   
#                                   ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 2, data = data, effect.size = "pes") }
#                                   
#                                   ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 1, data = data, effect.size = "pes")
#                                   
#                                   ANOVA[[7]]
#                                   
#                                   warning("No packages offering functions specifically designed to measure effect size of a 
#                                               two-way trimmed model are implemented in R to my knowledge. The effect size given here is a 
#                                               partial eta squared of a parametric model without correction of homoscedasticity.")
#                                   
#                                   # Post-hoc
#                                   
#                                   # Pairwise comparison and legend matrix
#                                   
#                                   comp = mcp2atm(Y ~ X1*X2, data = data)
#                                   comp
#                                   model.matrix(comp)
#                                   
#                                   # Significant interaction effect
#                                   
#                                   if(robust_ANOVA[6] <= 0.05)
#                                     
#                                     # Interaction plot
#                                     
#                                     plot(allEffects(mod.lm), multiline=T, ci.style="bars") }
#                           
#                           # No outliers (Boxplot Method)
#                           
#                           else
#                             
#                             # Test
#                             
#                             # Heteroscedasticity (Levene's Test)
#                             
#                             if( levene_test(Y ~ X1*X2, data = data)[[4]]  <= 0.05 )
#                               
#                               # Two-way ANOVA with heteroscedasticity correction (Hubert-White covariance matrix) 
#                               
#                               if(anova(mod.lm)[5][[1]][3] > 0.05 {
#                                 
#                                 ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 2, white.adjust = T, data = data, effect.size = "pes") }
#                                 
#                                 else ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 1, white.adjust = T, data = data, effect.size = "pes")
#                                 
#                                 # Homoscedasticity (Levene's Test)
#                                 
#                                 else
#                                   
#                                   # Two-way ANOVA
#                                   
#                                   if(anova(mod.lm)[5][[1]][3] > 0.05 {
#                                     
#                                     ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 2, data = data, effect.size = "pes") }
#                                     
#                                     else ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 1, data = data, effect.size = "pes")
#                                     
#                                     # Post-hoc
#                                     
#                                     # Effect size: Partial Eta-squared
#                                     
#                                     ANOVA[[7]]
#                                     
#                                     # Significant interaction effect
#                                     
#                                     if(ANOVA[[7]][3] <= 0.05)
#                                       
#                                       # Interaction plot
#                                       
#                                       plot(allEffects(mod.lm), multiline=T, ci.style="bars")
#                                      
#                                     # Grouped (2nd X) ANOVA models with 1st X only
#                                     
#                                     group_by(X2, .data = data) %>%
#                                       anova_test(Y ~ X1, error = model)
#                                     
#                                      # Grouped (1st X) ANOVA models with 2nd X only
#                                     
#                                     group_by(X1, .data = data) %>%
#                                       anova_test(Y ~ X2, error = model)
#                                      
#                                     # Tukey's Test
#                                     
#                                     tukey_X1.X2 = TukeyHSD(model.aov, ordered = T)[[3]]
#                                     tukey_X1.X2[order(-tukey_X1.X2[,1]),]
#                                     
#                                      # Significant main effects and non-significant interaction effect
#                                     
#                                     else if(ANOVA[[7]][1] <= 0.05 ||
#                                             ANOVA[[7]][2] <= 0.05)
#                                       
#                                       # Tukey's Test on the 1st X
#                                       
#                                       tukey_X1 = TukeyHSD(mod.aov, ordered = T)[[1]]
#                                     {if(nrow(tukey_X1) != 1) { tukey_X1[order(-tukey_X1[,1]),] } else {tukey_X1} }
#                                    
#                                     # Tukey's Test on the 2nd X
#                                     
#                                     tukey_X2 = TukeyHSD(mod.aov, ordered = T)[[2]]
#                                     {if(nrow(tukey_X2) != 1) { tukey_X2[order(-tukey_X2[,1]),] } else {tukey_X2} }
#                                     
#                                     # Both X are repeated measures
#                                     
#                                     else if(paired == "both")
#                                       
#                                       if(ID == NULL)
#                                         stop("Please provide the name of the column containing the blocking variable 
#                                                  (i.e. id of the subjects) in the ID argument.")
#                                     
#                                      # Non-normality of Y by groups (Shapiro-Wilk Test) and/or outliers (Boxplot Method)
#                                     
#                                     grouped_normality = group_by(X1, X2, .data = data) %>%
#                                       shapiro_test(Y)
#                                     
#                                     if( any(grouped_normality[5] <= 0.05) ||
#                                         nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0 )
#                                       
#                                       if(nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0)
#                                         warning("There was outliers : please check for measurement and/or experimental errors. 
#                                                     If so, remove those values from the dataframe. Otherwise, a robust method or a non-parametric method have been used to deal with those outliers.")
#                                     
#                                     # Paired Monte-Carlo Test (permutation)
#                                     
#                                     permutation = ezPerm(dv = Y, wid = ID, within = c(X1, X2), data)
#                                     
#                                     # Effect size: Partial Eta-squared
#                                     
#                                     paired_anova = anova_test(dv = Y, wid = ID, within = c(X1, X2), data = data, effect.size = "pes") }
#
#get_anova_table(paired_anova, correction = "auto")[[7]]

#warning("The standardized effect size was not calculated for a specific use after a permutation test. The result might be 
#       slightly inaccurate since it uses a parametric model.")
#
## Post-hoc
#
## Significant interaction effect
#
i#f(permutation[[2]][3] <= 0.05)
#
# # Interaction plot
# 
# plot(allEffects(mod.lm), multiline=T, ci.style="bars") }
#
## Grouped (2nd X) pairwise comparison of the 1st X (Paired Wilcoxon's Tests)
#
#group_by(X1, .data = data) %>%
#pairwise_wilcox_test(Y ~ X2, paired = T,
#                       p.adjust.method = "holm"
# )
#
## Grouped (1st X) pairwise comparison of the 2nd X (Paired Wilcoxon's Tests)
#
#group_by(X2, .data = data) %>%
# pairwise_wilcox_test(Y ~ X1, paired = T,
#                    p.adjust.method = "holm"
#  )
#
## Significant main effects and non-significant interaction effect
#
#else if(permutation[[2]][1] <= 0.05 ||
#        permutation[[2]][2] <= 0.05)
# 
# # Pairwise comparison of the 1st X (Paired Wilcoxon's Tests)
# 
# pairwise_wilcox_test(Y ~ X1, data = data, paired = T, p.adjust.method = "holm")
#
## Pairwise comparison of the 1st X (Paired Wilcoxon's Tests)
#
#pairwise_wilcox_test(Y ~ X2, data = data, paired = T, p.adjust.method = "holm")
#
## Normality of Y by groups (Shapiro-Wilk Test) and no outliers (Boxplot Method)
#
#else
# 
# # Two-way paired ANOVA
# 
# paired_anova = anova_test(dv = Y, wid = ID, within = c(X1, X2), data = data, effect.size = "pes", type = 1) 
#
#mauchly_test = paired_anova[2]
#
#corrected_anova = get_anova_table(paired_anova, correction = "auto")
#
## Effect size: Partial Eta-squared
#
#corrected_anova[[7]]
#
## Post-hoc
#
## Significant interaction effect
#
#if(corrected_anova[[5]][3] <= 0.05)
# 
# # Interaction plot
# 
# plot(allEffects(mod.lm), multiline=T, ci.style="bars")
#
## Grouped (2nd X) pairwise comparison of the 1st X (Paired Student's T-Tests)
#
#group_by(X2, .data = data) %>%
# pairwise_t_test(Y ~ X1, paired = T,
#                 p.adjust.method = "holm")
#
## Grouped (1st X) pairwise comparison of the 2nd X (Paired Student's T-Tests)
#
#group_by(X2, .data = data) %>%
# pairwise_t_test(Y ~ X1, paired = T,
#                 p.adjust.method = "holm"
# )
#
## Significant main effects and non-significant interaction effect
#
#else if(ANOVA[[7]][1] <= 0.05 ||
#       ANOVA[[7]][2] <= 0.05)
# 
# # Pairwise comparison of the 1st X (Paired Student's T-Tests)
# 
# pairwise_t_test(Y ~ X1, data = data, paired = T, p.adjust.method = "holm")
#
## Pairwise comparison of the 2nd X (Paired Student's T-Tests)
#
#pairwise_t_test(Y ~ X2, data = data, paired = T, p.adjust.method = "holm")
#
##
## Mixed (one X is independent measures and the other repeated measures)
#
#else if(paired == "first" || paired == "second")
# 
#
# if(ID == NULL)
#   stop("Please provide the name of the column containing the blocking variable (i.e. id of the subjects) in the ID argument.")
#
## A ADAPTER : Homog^2^n^2^it^2^ des covariances
#
#if(paired == "first") {
# if(box_m(data[, "Y", drop = F], data$X2)[2] < 0.001)
#   warning("The homogeneity of covariance assumption is not satisfied. This could be fixed by increasing the sampling effort and/or 
#           transforming the data. Another strategy could be to perform repeated measure ANOVA for each group of the independent variable.  
#           This problem is usually linked with non-normality: non-parametric can therefore resolve this problem.") }
#
#if(paired == "second") {
# if(box_m(data[, "Y", drop = F], data$X1)[2] < 0.001)
#   warning("The homogeneity of covariance assumption is not satisfied. This could be fixed by increasing 
#            the sampling effort and/or transforming the data. Another strategy could be to perform repeated measure ANOVA 
#           for each group of the independent variable.  This problem is usually linked with non-normality: non-parametric can 
#           therefore resolve this problem.") }
#
## Non-normality of Y by groups (Shapiro-Wilk Test) and/or heteroscedasticity by groups (Levene's Test) and/or outliers (Boxplot Method)
#
#grouped_normality = group_by(X1, X2, .data = data) %>%
# shapiro_test(Y)
#
#grouped_homogeneity = group_by(X1, X2, .data = data) %>%
# levene_test(Y)
#
#if(any(grouped_normality[5] <= 0.05) ||
#  any(grouped_homogeneity[5] <= 0.05) ||
#  nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0)
# 
# warning("No packages offering functions specifically designed for post-hoc analysis after a permutation test or 
#         a robust test (trimmed means) have been coded in R to my knowledge. A pairwise Wilcoxon Test was used instead of it.") }
#
#if(nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0)
# warning("There was outliers : please check for measurement and/or experimental errors. If so, remove those values 
#         from the dataframe. Otherwise, a robust method or a non-parametric method have been used to deal with those outliers.")
#
## Test
#
## Non-normality and/or heteroscedasticity
#
#if(any(grouped_normality[5] <= 0.05) ||
#  any(grouped_homogeneity[5] <= 0.05)
#  
#  # Mixed Monte-Carlo Test (permutation)
#  
#  if(paired == "first") {
#    permutation = ezPerm(dv = Y, wid = ID, within = X1, between = X2, data) }
#  
#  else if(paired == "second") {
#    permutation = ezPerm(dv = Y, wid = ID, within = X2, between = X1, data) }
#  
#  # Outliers
#  
#  else if(nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0)
#    
#    # ANOVA on the trimmed means (20%)
#    
#    bwtrim(Y ~ X1*X2, id = ID, data) ]
#
## Effect size: Partial Eta-squared
#
#if(paired == "first") {
# paired_anova = anova_test(dv = Y, wid = ID, within = X1, between = X2, data = data, effect.size = "pes") }
#
#else if(paired == "second") {
# paired_anova = anova_test(dv = Y, wid = ID, within = X2, between = X1, data = data, effect.size = "pes") }
#
#get_anova_table(paired_anova, correction = "auto")[[7]]
#
#warning("The standardized effect size was not calculated for a specific use after a permutation test or a robust mixed ANOVA. 
#       The result might be slightly inaccurate since it uses a parametric model.")
#
## Post-hoc
#
## Significant interaction effect
#
#if(permutation[[2]][3] <= 0.05)
#  
# # Interaction plot
# 
# plot(allEffects(mod.lm), multiline=T, ci.style="bars")
#
## Grouped (1st X and/or 2nd X) pairwise comparison of the 1st X and/or 2nd X (Paired or not Wilcoxon's Tests)
#
#if(paired == "first") {
# group_by(X2, .data = data) %>%
#   pairwise_wilcox_test(Y ~ X1,
#                        p.adjust.method = "holm") 
# 
# group_by(X1, .data = data) %>%
#   pairwise_wilcox_test(Y ~ X2, paired = T, p.adjust.method = "holm") }
#
#else if(paired == "second") {
# group_by(X1, .data = data) %>%
#   pairwise_wilcox_test(Y ~ X2,
#                        p.adjust.method = "holm") 
# 
# group_by(X2, .data = data) %>%
#   pairwise_wilcox_test(Y ~ X1, paired = T, p.adjust.method = "holm") }
#
# Significant main effects and non-significant interaction effect
#
#else if(permutation[[2]][1] <= 0.05 ||
#       permutation[[2]][2] <= 0.05)
# 
# # Pairwise comparison of the 1st X (Paired or not Wilcoxon's Tests)
# 
# if(paired == "first") {
#   pairwise_wilcox_test(Y ~ X1, data = data, paired = T, p.adjust.method = "holm") }
#
#if(paired == "second") {
# pairwise_wilcox_test(Y ~ X1, data = data, p.adjust.method = "holm") }
#
## Pairwise comparison of the 2nd X (Paired or not Wilcoxon's Tests)
#
#if(paired == "first") {
# pairwise_wilcox_test(Y ~ X2, data = data, p.adjust.method = "holm") }
#
#if(paired == "second") {
# pairwise_wilcox_test(Y ~ X2, data = data, paired = T, p.adjust.method = "holm") }
#
## Normality of Y by groups (Shapiro-Wilk Test) and heteroscedasticity by groups (Levene's Test) and no outliers (Boxplot Method)
#
#else
# 
# # Mixed Two-way ANOVA
# 
# if(paired == "first") {
#   paired_anova = anova_test(dv = Y, wid = ID, within = X1, between = X2, data = data, effect.size = "pes") }
#
#else if(paired == "second") {
# paired_anova = anova_test(dv = Y, wid = ID, within = X2, between = X1, data = data, effect.size = "pes") }
#
#mauchly_test = paired_anova[2]
#
#corrected_anova = get_anova_table(paired_anova, correction = "auto")
#
## Effect size: Partial Eta-squared
#
#corrected_anova[[7]]
#
## Post-hoc
#
## Significant interaction effect
#
#if(corrected_anova[[5]][3] <= 0.05)
# 
# # Interaction plot
# 
# plot(allEffects(mod.lm), multiline=T, ci.style="bars")
#
## Grouped (1st X and/or 2nd X) pairwise comparison of the 1st X and/or 2nd X (Paired or not Student's T-Tests)
#
#if(paired == "first") {
# group_by(X2, .data = data) %>%
#   pairwise_t_test(Y ~ X1,
#                   p.adjust.method = "holm") 
# 
# group_by(X1, .data = data) %>%
#   pairwise_t_test(Y ~ X2, paired = T,
#                   p.adjust.method = "holm") }
#
#else if(paired == "second") {
# group_by(X1, .data = data) %>%
#   pairwise_t_test(Y ~ X2,
#                   p.adjust.method = "holm") 
# 
# group_by(X2, .data = data) %>%
#   pairwise_t_test(Y ~ X1, paired = T,
#                   p.adjust.method = "holm") }
#
## Significant main effects and non-significant interaction effect
#
#else if(ANOVA[[7]][1] <= 0.05 ||
#       ANOVA[[7]][2] <= 0.05)
# 
# # Pairwise comparison of the 1st X (Paired or not Student's T-Tests)
# 
# if(paired == "first") {
#   pairwise_t_test(Y ~ X1, data = data, paired = T, p.adjust.method = "holm") }
#
#if(paired == "second") {
# pairwise_t_test(Y ~ X1, data = data, p.adjust.method = "holm") }
#
## Pairwise comparison of the 2nd X (Paired or not Student's T-Tests)
#
#if(paired == "first") {
# pairwise_t_test(Y ~ X2, data = data, p.adjust.method = "holm") }
#
#if(paired == "second") {
# pairwise_t_test(Y ~ X2, data = data, paired = T, p.adjust.method = "holm") }
#
## Both X are quantitative
#
## Linear relationship between the mean of Y and the residuals of the model (Linearity Test)
#
#if(gvlma(mod.lm)$GlobalTest$GlobalStat4$pvalue <= 0.05)
# 
#{ warning("Non-linear relationship between the factor(s) and Y, which may decrease accuracy. 
#         Therefore, you should consider transforming the variables (e.g. log, sqrt, polynomial regression) or using a 
#         GAM (i.e performing a non-linear regression).")
# 
# # Absence of multicolinearity (VIF)
# 
# if(vif(mod.lm)[1]>4) {warning("Multicolinearity between factors. Please consider removing the 1st factor from the formula (highest VIF)") }
# 
# else if(vif(mod.lm)[2]>4) {warning("Multicolinearity between factors. Please consider removing the 2nd factor from the formula (highest VIF)") }
# 
# else
#   
#   # Absence of correlation between the residuals and one or both X (Kendall's correlation)
#   
#   if(cor.test(X1, resids, method= "kendall")$p.value <= 0.05 && cor.test(X2, resids, method= "kendall")$p.value <= 0.05) {
#     { warning("Both factors are correlated with the residuals. OLS is biased so you should consider using a Model II Regression".) }
#     
#     
#     else if(cor.test(X1, resids, method= "kendall")$p.value <= 0.05) { warning("The first predictor is correlated with the residuals. 
#                                                                                OLS is biased so you should consider using a Model II Regression".) } }
# 
# else if(cor.test(X2, resids, method= "kendall")$p.value <= 0.05)
# { warning("Second factor correlated with the residuals. OLS is biased so you should consider using a Model II Regression".) }
# 
# else
#   
#   # Absence of residuals' autocorrelation (Durbin-Watson Test)
#   
#   if(dwtest(mod.lm) <= 0.05)
#   { warning("Residuals are autocorelated, which may decrease accuracy. This is mainly problematic for Time Series Data but it 
#             can be fixed by adding a factor containing lagged residuals to the original model.") }
# 
# # Independence
# 
# if(paired == "none")
#   
#   # Non-normality of the residuals (Shapiro-Wilk Test) and/or heteroscedasticity (Breusch-Pagan Test) and/or outliers (Bonferroni Outlier Test) 
#   
#   if( bptest(mod.lm)$p.value > 0.05 ||
#       shapiro.test(resids)$p.value > 0.05 ||
#       outlierTest(mod.lm)[3] <= 0.05)
#     
#     if(outlierTest(mod.lm)[3] <= 0.05)
#       warning("There was outliers : please check for measurement and/or experimental errors. If so, remove those values from the dataframe. 
#               Otherwise, a robust method was used to deal with those outliers.")
# 
# # Multivariate robust regression
# 
# anova(lmRob(Y ~ X1*X2, data)
#       
#       # Effect size: Partial Cohen's f-squared
#       
#       if(anova(lmRob(Y ~ X1*X2, data))[[3]][2] <= 0.05) {
#         R2_X1 = summary(lmRob(Y~X1))$r.squared
#         R2_X1_X2 = summary(lmRob(Y~X1+X2))$r.squared
#         p_cohen_f2_X1 = (R2_X1_X2 # R2_X1)/(1 # R2_X1_X2)
#                          p_cohen_f2_X1 }
#       
#       else if(anova(lmRob(Y ~ X1*X2, data))[[3]][3] <= 0.05) {
#         R2_X2 = summary(lm(Y~X2))$r.squared
#         R2_X1_X2 = summary(lm(Y~X1+X2))$r.squared
#         p_cohen_f2_X2 = (R2_X1_X2 # R2_X2)/(1 # R2_X1_X2)
#                          p_cohen_f2_X2 }
#       
#       # Correlation matrix
#       
#       # Outliers (Bonferroni Outlier Test)
#       
#       if(outlierTest(mod.lm)[3] <= 0.05)
#         
#         # Winsorized correlation matrix (20% winsorization)
#         
#         winall(data.frame(Y, X1, X2))
#       
#       # No outliers (Bonferroni Outlier Test)
#       
#       else
#         
#         # Kendall's correlation matrix 
#         
#         cor(data.frame(Y, X1, X2), method = "kendall")
#       
#       # Significant interaction effect
#       
#       if(anova(lmRob(Y ~ X1*X2, data))[[3]][4] <= 0.05)
#         
#         # Interaction plot
#         
#         plot(allEffects(mod.lm), multiline=T, ci.style="bars")
#       
#       # Normality of the residuals (Shapiro-Wilk Test) and homoscedasticity (Breusch-Pagan Test) and no outliers (Bonferroni Outlier Test)
#       
#       else
#         
#         # Multivariate regression
#         
#         anova(mod.lm)
#       
#       # Effect size: Partial Cohen's f-squared
#       
#       if(anova(mod.lm)[[5]][1] <= 0.05) {
#         R2_X1 = summary(lm(Y~X1))$r.squared
#         R2_X1_X2 = summary(lm(Y~X1+X2))$r.squared
#         p_cohen_f2_X1 = (R2_X1_X2 # R2_X1)/(1 # R2_X1_X2)
#                          p_cohen_f2_X1 }
#       
#       else if(anova(mod.lm)[[5]][2] <= 0.05) {
#         R2_X2 = summary(lm(Y~X2))$r.squared
#         R2_X1_X2 = summary(mod.lm)$r.squared
#         p_cohen_f2_X2 = (R2_X1_X2 # R2_X2)/(1 # R2_X1_X2)
#                          p_cohen_f2_X2 }
#       
#       # Correlation matrix
#       
#       cor(data.frame(Y, X1, X2))
#       
#       # Significant interaction effect
#       
#       if(anova(mod.lm)[[5]][3]<= 0.05)
#         
#         # Interaction plot
#         
#         plot(allEffects(mod.lm), multiline=T, ci.style="bars")
#       
#       # Repeated measures: ERROR
#       
#       else if(paired != "none") {
#         if(id != NULL)
#           stop("Please, provide data in a long format instead of a wide format, if you wanted to perform a paired test.") }
#       
#       else 
#         stop("The factor(s) cannot be continuous and paired.")
#       
#       # One X is qualitative and the other is quantitative: NOT SUPPORTED
#       
#       if(is.numeric(X1) || is.integer(X1))
#         if(is.factor(X2) || is.character(X2)) {
#           
#           stop("The 1st X was a number/integer while the 2nd X was a character/factor. This is not supported by this function. 
#                You might want to convert the 1st X in a character or a factor (as.[...]) and run this function again. 
#                You could also want to perform a MANOVA, which are also not supported by this function.")
#           
#           else if(is.factor(X1) || is.character(X1))
#             if(is.numeric(X2) || is.integer(X2)) {
#               
#               stop("The 1st X was a character/factor while the 2nd X was a number/integer. 
#                    This is not supported by this function. You might want to convert the 2nd X in a character or a factor (as.[...]) 
#                    and run this function again. You could also want to perform a MANOVA, which is not supported by this function.")
#               
#               
#             }




