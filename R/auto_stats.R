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
  
  n = paste(" (N = ", length(Y), "): ", sep = "")
  
  n_apa = paste(", N = ", length(Y), ") = ", sep = "")
  
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
  
  display = function(tab = NULL, gs1 = NULL, gs2 = NULL, vali1 = NULL, vali2 = NULL, vali3 = NULL, vali4 = NULL, 
                     test1 = NULL, test2 = NULL, apa_test1 = NULL, apa_test2 = NULL, asso1 = NULL, asso2 = NULL, 
                     apa_asso1 = NULL, apa_asso2 = NULL, other_asso = NULL, name_ph1 = NULL, name_ph2 = NULL, name_ph3 = NULL,
                     ph1 = NULL, ph2 = NULL, ph3 = NULL, mes1 = NULL, mes2 = NULL, mes3 = NULL, mes4 = NULL, dig = digits) {
    if(!is.null(tab)) {
      cat("------------------------------", "TABLE", "-----------------------------", fill = T)
      cat(" ", fill = T)
      print(tab, digits = dig)
      cat(" ", fill = T)
    }
    if(!is.null(gs1)) {
      cat("---------------------------", "GLOBAL STATS", "-------------------------", fill = T)
      cat(" ", fill = T)
      cat(gs1, fill = T)
      if(!is.null(gs2)) {cat(" ", fill = T) 
        cat(gs2, fill = T)} 
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
    if(!is.null(asso1) || !is.null(other_asso)) {
      cat("--------------------", "MEASURE(S) OF ASSOCIATION", "-------------------", fill = T)
      cat(" ", fill = T)
      if(!(is.null(asso1) && !is.null(other_asso))) cat(asso1, fill = T)
      if(!is.null(apa_asso1)) {
        cat(" ", fill = T)
        cat(paste("APA code ->", apa_asso1), fill = T)}
      if(!is.null(asso2)) {cat(" ", fill = T) 
        cat(asso2, fill = T)} 
      if(!is.null(apa_asso2)) {
        cat(" ", fill = T)
        cat(paste("APA code ->", apa_asso2), fill = T)}
      if(!is.null(other_asso)) {
        if(!is.null(asso1) || !is.null(asso2)) {cat(" ", fill = T)
          print(other_asso, row.names = F)}
        else print(other_asso, row.names = F) }
      cat(" ", fill = T)
    }
    
    if(!is.null(ph1)) {
      cat("------------------------", "POST-HOC ANALYSIS", "-----------------------", fill = T)
      cat(" ", fill = T)
      cat(name_ph1,fill = T)
      cat(" ", fill = T)
      print(ph1, digits = dig, row.names = F)
      if(!is.null(ph2)) {cat(" ", fill = T) 
        cat(name_ph2,fill = T)
        cat(" ", fill = T)
        print(ph2, digits = dig, row.names = F)}
      if(!is.null(ph3)) {cat(" ", fill = T) 
        cat(name_ph3,fill = T)
        cat(" ", fill = T)
        print(ph3, digits = dig, row.names = F)}
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
            name = paste("One-sample Chi-squared Test with Yates' correction", n, sep = "")
            
          }
          
          # Satisfied Cochran's Rule
          
          else {
            
            vali = "Cochran's rule: 0% of expected counts are below 5 -> Satisfied"
            
            # One-sample Chi-squared Test
            
            test1 = suppressWarnings(prop.test(tab_n1, correct = F)) 
            test2 = suppressWarnings(prop.test(tab_n2, correct = F)) 
            name = paste("One-sample Chi-squared Test", n, sep = "")
            
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
          
          prob_ci1 = paste("Probality of ", names(tab_n1)[1], " = ", r(prob1), ", 95% CI [", r(ci1[1]), ", ", r(ci1[2]), "]", sep = "")
          
          prob_ci2 = paste("Probality of ", names(tab_n2)[1], " = ", r(prob2), " 95% CI [", r(ci2[1]), ", ", r(ci2[2]), "]", sep = "")
          
          prob_ci1_apa = paste("Probality of ", names(tab_n1)[1], " = ", x1_apa(prob1), 
                               ", 95% CI [", x1_apa(ci1[1]), ", ", x1_apa(ci1[2]), "]", sep = "")
          
          prob_ci2_apa = paste("Probality of ", names(tab_n2)[1], " = ", x1_apa(prob2), 
                               " 95% CI [", x1_apa(ci2[1]), ", ", x1_apa(ci2[2]), "]", sep = "")
          
          test = paste(name, "X-squared = ", r(test1[[1]][[1]]), ", df = ", test1[[2]][[1]], ", p-value = ", r(test1[[3]]), s(test1[[3]]), sep = "")
          
          test_apa = paste("&chi;^2^(", test1[[2]][[1]], n_apa, x_apa(test1[[1]][[1]]), ", *p* ", p_apa(test1[[3]]), sep = "")
          
          eff = paste("Cohen's h = ", r(effect_size), m(effect_size, lim1 = 0.2, lim2 = 0.5, lim3 = 0.8), sep = "")
          
          eff_apa = paste("Cohen's *h* = ", x_apa(effect_size), sep = "")
          
          if(apa) display(tab = tab_n1, gs1 = prob_ci1_apa, gs2 = prob_ci2_apa, vali1 = var_type, vali2 = vali, 
                          test1 = test, apa_test1 = test_apa, asso1 = eff_apa)
          
          else display(tab = tab_n1, gs1 = prob_ci1, gs2 = prob_ci2, vali1 = var_type, vali2 = vali, test1 = test, asso1 = eff)
          
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
          
          test = paste("Fisher's Exact Test", n, "p-value = ", r(test1[[1]]), s(test1[[1]]), sep = "")
          
          test_apa = paste("Fisher's Exact Test", n, "*p* ", p_apa(test1[[1]]), sep = "")
          
          eff = paste("Phi coefficient = ", r(phi), m(phi, lim1 = 0.1, lim2 = 0.3, lim3 = 0.5), sep = "")
          
          eff_apa = paste("&phi; = ", x1_apa(phi), sep = "")
          
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
          
          else  var_type = "Variable type: Qualitative dependent variable (Y) with more than two levels.\nOne factor (X1) with more than two independent levels."
          
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
          
          test = paste("Fisher's Exact Test", n, "p-value = ", r(test1[[1]]), s(test1[[1]]), sep = "")
          
          test_apa = paste("Fisher's Exact Test", n, "*p* ", p_apa(test1[[1]]), sep = "")
          
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
              
              test = paste("Exact McNemar's Test", n, "b = ", r(test1[[1]][[1]]), ", c = ", 
                           r(test1[[2]][[1]]), ", p-value = ", r(test1[[3]][[1]]), s(test1[[3]][[1]]), sep = "")
              
              test_apa = paste("Exact McNemar's Test", n, "b = ", x_apa(test1[[1]][[1]]), ", c = ", 
                               x_apa(test1[[2]][[1]]), ", *p* ", p_apa(test1[[3]][[1]]), sep = "")
              
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
              
              test1_n = paste("Asymptotic General Symmetry Test", n, "Z = ", r(Z), ", p-value = ", r(p_value1), s(p_value1), sep = "")
              
              test1_apa = paste("Asymptotic General Symmetry Test", n, "Z = ", x_apa(Z), ", *p* ", p_apa(p_value1), sep = "")
              
              test2_n = paste("Stuart-Maxwell Marginal Homogeneity Test", n, "X-squared = ", r(x_squared), ", df = ", df,
                              ", p-value = ", r(p_value2), s(p_value2), sep = "")
              
              test2_apa = paste("Stuart-Maxwell Marginal Homogeneity Test: &chi;^2^(", df, n_apa, x_apa(x_squared), 
                                ", *p* ", p_apa(p_value2), sep = "")
              
              g = paste("Cohen's g = ", r(g_global), m(g_global, lim1 = 0.05, lim2 = 0.15, lim3 = 0.25), sep = "")
              
              g_apa = paste("Cohen's *g* = ", x1_apa(g_global), sep = "")
              
              o = paste("Odds ratio = ", r(o_global), m(o_global, lim1 = 1.22, lim2 = 1.86, lim3 = 3, OR = T), sep = "")
              
              o_apa = paste("OR = ", x_apa(o_global), sep = "")
              
              name_ph = "Pairwise Symmetry Tests (fdr adjustment method):"
              
              if(apa) display(tab = tab_n, vali1 = var_type, test1 = test1_n, apa_test1 = test1_apa, test2 = test2_n,
                              apa_test2 = test2_apa, asso1 = g, apa_asso1 = g_apa, asso2 = o, apa_asso2 = o_apa,
                              other_asso = eff_pairwise, name_ph1 = name_ph, ph1 = ph)
              
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
            
            test1_n = paste("Asymptotic General Symmetry Test", n, "Z = ", r(Z), ", p-value = ", r(p_value1), s(p_value1), sep = "")
            
            test1_apa = paste("Asymptotic General Symmetry Test", n, "Z = ", x_apa(Z), ", *p* ", p_apa(p_value1), sep = "")
            
            if(exists("test2", envir = fct.env)) 
              assign("test2_n", paste("Cochran's Q test", n, "Q = ", r(get("test2", envir = fct.env)[[3]][[1]]), ", df = ", 
                                      get("test2", envir = fct.env)[[4]][[1]], ", p-value = ", 
                                      r(get("test2", envir = fct.env)[[7]][[1]]), 
                                      s(get("test2", envir = fct.env)[[7]][[1]]), sep = ""), envir = fct.env)
            
            if(exists("test2", envir = fct.env)) 
              assign("test2_apa", paste("Cochran's Q test: Q(", get("test2", envir = fct.env)[[4]][[1]], n_apa, 
                                        x_apa(get("test2", envir = fct.env)[[3]][[1]]), ", *p* ", 
                                        p_apa(get("test2", envir = fct.env)[[7]][[1]]), sep = ""), envir = fct.env)
            
            if(apa) display(tab = tab_f, vali1 = var_type, test1 = test1_n, apa_test1 = test1_apa, 
                            test2 = cm("test2_n"), apa_test2 = cm("test2_apa"), name_ph1 = name_ph, ph1 = ph)
            
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
        
        magn = vector(length = dim(loddsratio(tab_n))[3])
        
        if(length(unique(X1)) == 2 && length(unique(Y)) == 2) {
          
          for(i in 1:dim(loddsratio(tab_n))[3]) {
            
            magn[i] = m(exp(loddsratio(tab_n)[[1]][[i]]), lim1 = 1.55, lim2 = 2.8, lim3 = 5, OR = T)
            
          }
          
          o_p = data.frame(t(t(r(exp(loddsratio(tab_n)[[1]])))), magn)
          colnames(o_p) = c("Odds ratio", "Magnitude")
          
        }
        
        else if(length(unique(X1)) > 2 && length(unique(Y)) == 2 || length(unique(X1)) == 2 && length(unique(Y)) > 2) 
          o_p = loddsratio(tab_n, log = F)
        
        else o_p = data.frame(as.data.frame(loddsratio(tab_n))[1:3], round(as.data.frame(loddsratio(tab_n, log = F))[4], digits))
        
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
              
              test = paste("Exact conditional test of independence", n, "S = ", r(test1[[1]][[1]]), ", p-value = ", r(test1[[2]][[1]]),
                           s(test1[[2]][[1]]), sep = "")
              
              test_apa = paste("Exact conditional test of independence", n, "S = ", test1[[2]][[1]], ", *p* ", p_apa(test1[[2]][[1]]), sep = "")
              
              # General Odds ratio
              
              if(dim(tab_n)[2] == 2) {
                
                assign("o_g", paste("Odds ratio (Mantel-Haenszel estimate) = " , r(test1[[4]][[1]]), " 95% CI [", 
                                    r(test1[[3]][1]), ", ", r(test1[[3]][2]), "]", 
                                    m(test1[[4]][[1]], lim1 = 1.55, lim2 = 2.8, lim3 = 5, OR = T), sep = ""), envir = fct.env)
                
                assign("o_g_apa", paste("OR = ", x_apa(test1[[4]][[1]]), " 95% CI [", x_apa(test1[[3]][1]), ", ", 
                                        x_apa(test1[[3]][2]), "]", sep = ""), fct.env)
                
              }
              
            }
            
            else {
              
              # Mantel-Haenszel Test with continuity correction (for large dataset)
              
              test1 = mantelhaen.test(tab_n)
              
              test = paste("Mantel-Haenszel Chi-squared Test with continuity correction", n, "X-squared = ", r(test1[[1]][[1]]), 
                           ", df = ", test1[[2]][[1]], ", p-value = ", r(test1[[3]][[1]]), s(test1[[3]][[1]]), sep = "")
              
              test_apa = paste("M-H c.c. Test: &chi;^2^(", test1[[2]][[1]], n_apa, x_apa(test1[[1]][[1]]),
                               ", *p* ", p_apa(test1[[3]][[1]]), sep = "")
              
              # General Odds ratio
              
              if(dim(tab_n)[2] == 2) {
                
                assign("o_g", paste("Odds ratio (Mantel-Haenszel estimate) = " , r(test1[[5]][[1]]), " 95% CI [", 
                                    r(test1[[4]][1]), ", ", r(test1[[4]][2]), "]", 
                                    m(test1[[5]][[1]], lim1 = 1.55, lim2 = 2.8, lim3 = 5, OR = T), sep = ""), envir = fct.env)
                
                assign("o_g_apa", paste("OR = ", x_apa(test1[[5]][[1]]), " 95% CI [", x_apa(test1[[4]][1]), ", ", 
                                        x_apa(test1[[4]][2]), "]", sep = ""), fct.env)
                
              }
              
            }
            
          }
          
          # 2 x n x k Table (n > 2)
          
          else {
            
            var_type = "Variable type: Qualitative dependent variable (Y) with more than two levels\nTwo independent factors (X) and X1 has two levels."
            
            test1 = mantelhaen.test(tab_n)
            
            test = paste("Cochran-Mantel-Haenszel Test", n, "M^2 = ", r(test1[[1]][[1]]), 
                         ", df = ", test1[[2]][[1]], ", p-value = ", r(test1[[3]][[1]]), s(test1[[3]][[1]]), sep = "")
            
            test_apa = paste("CMH Test: M^2^(", test1[[2]][[1]], n_apa, x_apa(test1[[1]][[1]]),
                             ", *p* ", p_apa(test1[[3]][[1]]), sep = "")
            
          }
          
          ### Displaying results
          
          val = paste("Woolf's Test", n, "X-squared = ", r(vali[[1]][[1]]), ", df = ", vali[[2]][[1]],
                      ", p-value = ", r(vali[[3]][[1]]), " -> Satisfied (p-value > 0.05)\nNote: Homogeneity of odds ratios across levels of X2 (strata).", sep = "")
          
          mess1 = "This test has been designed to know if there is an association between the 1st factor (X1)\nand the dependant variable (Y). As the 2nd factor (X2) is used for adjustment, you should\nchange the position in the formula if this is the variable of main interest."
          
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
            
            assign("vali2", paste("Woolf's Test", n, "X-squared = ", r(vali[[1]][[1]]), ", df = ", vali[[2]][[1]],
                                  ", p-value = ", r(vali[[3]][[1]]), " -> Not satisfied (p-value <= 0.05)\nNote: Heterogeneity of odds ratios across levels of X2 (strata).", sep = ""))
          }
          
          else var_type = "Variable type: Qualitative dependent variable (Y) with more than two levels\nTwo independent factors (X)."
          
          data$Y = as.factor(data$Y)
          
          mod_simple = suppressWarnings(glm(Y ~ X1 + X2, family = binomial, data))
          
          mod_interaction = suppressWarnings(glm(Y ~ X1 * X2, family = binomial, data))
          
          dev = anova(mod_simple, mod_interaction, test = "LRT")
          
          # LRT Test on an additive model (logistic regression) 
          
          if(is.na(dev[[5]][2]) || dev[[5]][2] > 0.05) {
            
            vali3_type = " -> Additive model (non-significant difference)"
            
            lrt = Anova(mod_simple, type = 2)
            
            test = paste("Type II Likelihood Ratio Test on Logistic Regression model", n, "\nX1 -> X-squared = ", r(lrt[[1]][1]), ", df = ", lrt[[2]][1], 
                         ", p-value = ", r(lrt[[3]][1]), s(lrt[[3]][1]), "\nX2 -> X-squared = ", r(lrt[[1]][2]), 
                         ", df = ", lrt[[2]][2], ", p-value = ", r(lrt[[3]][2]), s(lrt[[3]][2]), sep = "")
            
            test_apa = paste("LRT Test:\nX1 -> &chi;^2^(", lrt[[2]][1], n_apa, x_apa(lrt[[1]][1]), ", *p* ", 
                             p_apa(lrt[[3]][1]), "\nX2 -> &chi;^2^(", lrt[[2]][2], n_apa, x_apa(lrt[[1]][2]), ", *p* ", 
                             p_apa(lrt[[3]][2]), sep = "")
            
          }
          
          # LRT Test on a multiplicative model (logistic regression) 
          
          else {
            
            vali3_type = " -> Multiplicative model (significant difference)"
            
            if(Anova(mod_interaction, type = 3)[[3]][3] > 0.05) {
              
              type = "Type II "
              
              lrt = Anova(mod_interaction, type = 2)
              
            }
            
            else {
              
              type = "Type III "
              
              lrt = Anova(mod_interaction, type = 3)
              
            }
            
            test = paste(type, "Likelihood Ratio Test on Logistic Regression model", n, "\nX1 -> X-squared = ", r(lrt[[1]][1]), ", df = ", lrt[[2]][1], 
                         ", p-value = ", r(lrt[[3]][1]), s(lrt[[3]][1]), "\nX2 -> X-squared = ", r(lrt[[1]][2]), 
                         ", df = ", lrt[[2]][2], ", p-value = ", r(lrt[[3]][2]), s(lrt[[3]][2]), 
                         "\nX1:X2 -> X-squared = ", r(lrt[[1]][3]), ", df = ", lrt[[2]][3], ", p-value = ", 
                         r(lrt[[3]][3]), s(lrt[[3]][3]), sep = "")
            
            test_apa = paste("LRT Test:\nX1 -> &chi;^2^(", lrt[[2]][1], n_apa, x_apa(lrt[[1]][1]), ", *p* ", 
                             p_apa(lrt[[3]][1]), "\nX2 -> &chi;^2^(", lrt[[2]][2], n_apa, x_apa(lrt[[1]][2]), ", *p* ", 
                             p_apa(lrt[[3]][2]), "\nX1:X2 -> &chi;^2^(", lrt[[2]][3], n_apa, x_apa(lrt[[1]][3]), ", *p* ", 
                             p_apa(lrt[[3]][3]), sep = "")
            
          }
          
          ### Displaying results
          
          vali3 = paste("Likelihood Ratio Test on Logistic Regression model", n, "\nDeviance = ", 
                        r(dev[[4]][2]), ", p-value = ", ifelse(is.na(dev[[5]][2]), 1, r(dev[[5]][2])), 
                        vali3_type, "\nNote: Deviance between additive (Y ~ X1 + X2) & multiplicative model (Y ~ X1 * X2)", sep = "")
          
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
