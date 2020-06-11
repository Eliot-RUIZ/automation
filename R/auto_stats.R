auto_stats = function(data, y, x1 = NULL, x2 = NULL, paired = "none", id = NULL, digits = 3) {
  
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
    stop("The ID of the subject is constant while it must have at least two levels.")
  
  if(!(is.character(Y) || is.factor(Y) || is.integer(Y) || is.numeric(Y)))
    stop("Y must be either a character, a factor, an integer or a number.
         Check it with this command: (is.character(y) || is.factor(y) || is.integer(y) || is.numeric(y))
         Change it using as.character(y) etc.")
  
  if(!is.null(x1) && !(is.character(X1) || is.factor(X1) || is.integer(X1) || is.numeric(X1)))
    stop("X1 must be either a character, a factor, an integer or a number.
         Check it with this command: (is.character(x1) || is.factor(x1) || is.integer(x1) || is.numeric(x1))
         Change it using as.character(x1) etc.")
  
  if(!is.null(x2) && !(is.character(X2) || is.factor(X2) || is.integer(X2) || is.numeric(X2)))
    stop("X2 must be either of type character, factor, integer or numeric.
         Check it with this command: (is.character(x2) || is.factor(x2) || is.integer(x2) || is.numeric(x2))
         Change it using as.character(x2) etc.")
  
  if(!is.null(id) && !(is.character(ID) || is.factor(ID) || is.integer(ID) || is.numeric(ID)))
    stop("ID must be either of type character, factor, integer or numeric.
         Check it with this command: (is.character(id) || is.factor(id) || is.integer(id) || is.numeric(id))
         Change it using as.character(id) etc.")
  
  if(!(paired == "none" || paired == "first" || paired == "second" || paired == "both"))
    stop('The "paired" argument only takes those values: "none", "first", "second", "both".')
  
  ## Qualitative Y
  
  if(is.factor(y) || is.character(y))
    
    ### No X
    
    if(is.null(X1) && is.null(X2))
      
      # Independence
      
      if(paired == "none")
        
        tab_n = sort(table(Y), decreasing = T)
  tab_prop = sort(prop.table(table(Y)), decreasing = T)
  
  # Test
  
  # Violation of Cochran's Rule
  
  if(any(chisq.test(table(Y))$expected) <= 5)
    
    # One-sample Chi-squared Test with Yates' correction
    
    prop.test(tab_n)
  
  # Satisfied Cochran's Rule
  
  else
    
    # One-sample Chi-squared Test
    
    prop.test(tab_n, correct = F)
  
  # Effect size: Cohen's H
  
  abs(2 * asin(sqrt(tab_prop[[1]])) # 2 * asin(sqrt(tab_prop[[2]])))
      
      # Probability of the most represented category and its CI
      
      prop.test(tab_n)[[4]]
      
      prop.test(tab_n)[[6]][1:2]
      
      # Repeated measures: ERROR
      
      else
        
        stop("Y cannot be paired. Add a repeated factor in the formula.")
      
      ### One X
      
      else if(!is.null(X1) && is.null(X2))
        
        # Independence
        
        if(paired == "none")
          
          tab_n = table(X1, Y)
      print(tab_n)
      
      # X and Y have two levels each
      
      if(length(unique(X1)) == 2 && length(unique(Y)) == 2)
        
        # Exact Fisher's Test
        
        fisher.test(tab_n)
      
      # Effect size
      
      # Violation of Cochran's Rule
      
      cochran = table(chisq.test(tab_n)$expected > 5)
      
      if(length(cochran) == 1 && names(cochran) == "FALSE" ||
         length(cochran) == 1 && cochran[[1]] < round(0.8*nrow(tab_n) * ncol(tab_n)) ||
         length(cochran) == 2 && cochran[[2]] < round(0.8*nrow(tab_n) * ncol(tab_n)))
        
        # Effect size: Phi coefficient
        
        sqrt(prop.test(tab_n)[[1]][[1]]/sum(tab_n))
      
      warning("No effect size have been specifically designed for use after a Fisher's Exact Test to my knowlegde. 
                The Phi coefficient based on the Chi-squared with Yates correction (Cochran rule not respected) test 
                (approximation) was used instead.")
      
      # Satisfied Cochran's Rule 
      
      else
        
        # Effect size: Phi coefficient
        
        sqrt(prop.test(tab_n, correct = F)[[1]][[1]]/sum(tab_n))
      
      warning("No effect size have been specifically designed for use after a Fisher's Exact Test to my knowlegde. 
                The Phi coefficient based on the Chi-squared without Yates correction (Cochran's rule respected) test 
                (approximation) was used instead.")
      
      # Odds ratio and their CI
      
      # Sample size <= 1000
      
      if(sum(tab_n) <= 1000)
        
        # fisher.exact (more precise CI)
        
        fisher.exact(tab_n)[[3]][[1]]
      
      fisher.exact(tab_n)[[2]][1:2]
      
      # Sample size > 1000
      
      else
        
        # fisher.test (fastest computation)
        
        fisher.test(tab_n)[[3]][[1]]
      
      fisher.test(tab_n)[[2]][1:2]
      
      # X and/or Y have more than two levels
      
      else
        
        # Exact Fisher's Test
        
        fisher.test(tab_n)
      
      # Effect size
      
      # Violation of Cochran's Rule
      
      cochran = table(chisq.test(tab_n)$expected > 5)
      
      if(length(cochran) == 1 && names(cochran) == "FALSE" ||
         length(cochran) == 1 && cochran[[1]] < round(0.8*nrow(tab_n) * ncol(tab_n)) ||
         length(cochran) == 2 && cochran[[2]] < round(0.8*nrow(tab_n) * ncol(tab_n)))
        
        # Effect size: Cramer's V
        
        cramerV(tab_n, bias.correct = T)
      
      warning("No effect size have been specifically designed for use after a Fisher's Exact Test to my knowlegde. 
                The Cramer's V based on the Chi-squared with Yates correction (Cochran rule not respected) test 
                (approximation) was used instead.")
      
      # Satisfied Cochran's Rule 
      
      else
        
        # Effect size: Cramer's V
        
        cramerV(tab_n)
      
      warning("No effect size have been specifically designed for use after a Fisher's Exact Test to my knowlegde. 
                The Cramer's V based on the Chi-squared without Yates correction (Cochran's rule respected) test 
                (approximation) was used instead.")
      
      # Post-hoc
      
      # X has two levels
      
      if(length(unique(X1)) == 2)
        
        # Post-hoc: Pairwise Fisher's Exact Test
        
        fisher.multcomp(tab_n)
      
      # X has multiple levels
      
      if(length(unique(X1)) == 2)
        
        # Post-hoc: Pairwise Fisher's Exact Test and Cramer's V
        
        pairwiseNominalIndependence(tab_n, compare = "row", gtest = F, chisq = F, cramer = T)
      
      # Repeated measures
      
      else if(paired == "second" || paired == "both")
        stop("Only one factor : the paired argument must be either "none" or "first.")

else if(paired == "first" && ID == NULL)
  stop("Please provide the ID of the subjects in the ID argument, or change the paired argument to "none".)
      
      else
        
        data_long = group_by(ID, .data = data.frame(Y, X1, ID)) %>% mutate(n_measure = n(), n_levels = length(unique(X1))) %>% arrange(ID, X1)
      data_long$ID = factor(data_long$ID, ordered = T)
      
      if(length(unique(data_long$n_measure)) != 1 || length(unique(data_long$n_levels)) != 1)
        print(data_long)
      stop("All the individuals haven't been measured during all sessions. Revise your data or the paired argument.")
      
      else if(length(unique(data_long$X1)) != data_long$n_levels[1])
        print(data_long)
      stop("The levels of the factor change accross subjects. Please check the spelling of the factor's levels.")
      
      data_long = data_long[ , -which(names(data_long) %in% c("n_measure", "n_levels"))]
      
      data_wide = data_long %>% spread(X1, Y)
      tab_n = table(data_wide[,-1])
      tab_f = ftable(data_wide[,-1])
      
      # X has two levels
      
      if(length(unique(X1)) == 2)
        
        print(tab_n)
      
      # Y has two levels (2 x 2)
      
      if(length(unique(Y)) == 2)
        
        # Exact McNemar's Test
        
        mcnemar.exact(tab_n)
      
      # Effect size: Cohen's G
      
      (tab_n[1, 2]/(tab_n[1, 2]+tab_n[2, 1])) # 0.5
      
      # Odds ratio and their CI
      
      mcnemar.exact(tab_n)[[5]][[1]]
      
      mcnemar.exact(tab_n)[[4]][1:2]
      
      # Y has more than two levels (n x n)
      
      else if(length(unique(Y)) > 2)
        
        # Asymptotic General Symmetry Test
        
        symmetry_test(Y ~ as.factor(X1) | ID, data_long)
      
      # Stuart-Maxwell Marginal Homogeneity Test
      
      mh_test(tab_n)
      
      # Effect size: Cohen's G & Odds ratio (global and pairwise)
      
      cohenG(tab_n)
      
      # Post-hoc: Pairwise Symmetry Test
      
      nominalSymmetryTest(tab_n)[[2]]
      
      # X has more than two levels
      
      else if(length(unique(X1)) > 2)
        
        print(tab_f)
      
      # Y has two levels (2 x 2 x k)
      
      if(length(unique(Y)) == 2)
        
        # Asymptotic General Symmetry Test
        
        symmetry_test(Y ~ as.factor(X1) | ID, data_long)
      
      # Cochran's Q Test and Pairwise Wilcoxon Sign Test (post-hoc)
      
      X1 = as.factor(X1)
      cochran.qtest(Y ~ X1 | ID, data_long)
      
      # Y has more than two levels (n x n x k)
      
      else if(length(unique(Y)) > 2)
        
        # Asymptotic General Symmetry Test
        
        symmetry_test(Y ~ as.factor(X1) | ID, data_long)
      
      # Post-hoc: Pairwise Perumutation Symmetry Test
      
      pairwisePermutationSymmetry(Y ~ X1 | ID, data_long)
      
      ### Two X
      
      else if(!is.null(X1) && !is.null(X2))
        
        # Both X are independent measures
        
        if(paired == "none")
          
          tab_n = table(data$X1, data$Y, data$X2)
      
      ftable(tab_n)
      
      # Woolf's Test: Homogeneity of odds ratios across strata
      
      if(woolf(tab_n)[[3]] > 0.05)
        
        # Cochran-Mantel-Haenszel Test
        
        mantelhaen.test(tab_n)
      
      warning("This test has been designed to know is there is an association between the first factor and the 
                dependant variable. As the 2nd factor is used as an adjustement, you should change the position in the formula if 
                this is the variable of main interest.")
      
      # Global odds ratio (only for 2 x 2 x k)
      
      if(dim(tab_n)[1] > 2 || dim(tab_n)[2] > 2)
        
        mantelhaen.test(tab_n)[[5]][[1]]
      
      mantelhaen.test(tab_n)[[4]][1:2]
      
      # Pairwise odds ratio 
      
      oddsratio(tab_n)
      
      # Post-hoc: Groupewise Exact Fisher's Test
      
      groupwiseCMH(tab_n, group = 3, fisher = T)
      
      # Woolf's Test: Heterogeneity of odds ratio across strata
      
      else
        
        mod_simple = glm(Y ~ X1 + X2, family = binomial, data)
      
      mod_interaction = glm(Y ~ X1 * X2, family = binomial, data)
      
      anova(mod_simple, mod_interaction, test = "Chisq")
      
      # Wald's Test on a multiplicative model (logistic regression) 
      
      if(anova(mod_simple, mod_interaction, test = "Chisq")[[5]][2] <= 0.05)
        
        summary(mod_interaction)[[12]]
      
      if(Anova(mod_interaction, type = 3)[[3]][3] > 0.05)
        Anova(mod_interaction, type = 2)
      
      else
        
        Anova(mod_interaction, type = 3)
      
      # Wald's Test on an additive model (logistic regression) 
      
      else
        
        summary(mod_simple)[[12]]
      
      Anova(mod_simple, type = 2)
      
      # One or two X are repeated measures: NOT SUPPORTED
      
      else
        stop("This function does not support design with a qualitative Y and one or both paired factors. However, 
               you can test the effect of the factors (repeated or not) on Y one after the other using this function.")
      
      ## Quantitative Y
      
      else if(is.numeric(y) || is.integer(y)) 
        
        else 
          
          stop("Y variable must belong to the class numeric, integer, character or factor. 
                 Check it with is.[...] and change it with as.[...]).")
      
      ### No X
      
      if(is.null(X1) && is.null(X2) && is.null(MU))
        stop("No analysis of Y could be conducted if Y is quantitative and X1, X2, ID or MU have not been filled.")
      
      
      else if(is.null(X1) && is.null(X2) && !is.null(MU))
        
        # Independence
        
        if(paired == "none")
          
          if(is.null(theoric_mean))
            theoric_mean = 0
      warning("No theoric mean have been provided to compare it with the mean of Y. 
                Therefore, the theoric mean was automatically set to 0.")
      
      
      else
        
        # Non-normality of Y (Shapiro-Wilk Test) and/or outliers (Boxplot Method)
        
        if(shapiro.test(Y)[2] > 0.05 || identify_outliers(nrow(as.data.frame(Y)) != 0)
           
           if(identify_outliers(nrow(as.data.frame(Y)) != 0)
              warning("There was outliers : please check for measurement and/or experimental errors. If so, remove those values 
                        from the dataframe. Otherwise, a non-parametric method (more robust) was used to deal with those outliers.")
              
              # One-sample Wilcoxon's Test 
              
              wilcox.test(Y, mu = theoric_mean)
              
              # Effect size : r
              
              wilcox_effsize(Y ~ 1, mu = theoric_mean, data = as.data.frame(Y))
              
              # Pseudo-median of Y and its CI
              
              wilcox.test(Y, mu = theoric_mean, conf.int = T)[[9]][[1]]
              wilcox.test(Y, mu = theoric_mean, conf.int = T)[[8]][1:2]
              
              # Normality of Y (Shapiro-Wilk Test) and no outliers (Boxplot Method)
              
              else
                
                # One-sample Student's T-Test
                
                t.test(Y, mu = theoric_mean)
              
              # Effect size: Cohen's D
              
              cohens_d(Y ~ 1, mu = theoric_mean, data = as.data.frame(Y))
              
              # Mean of Y and its CI
              
              t.test(Y, mu = theoric_mean)[[5]][[1]]
              t.test(Y, mu = theoric_mean)[[4]][1:2]
              
              # Repeated measures: ERROR
              
              else
                
                stop("Y cannot be paired. Add a repeated factor in the formula.")
              
              ### Un X
              
              else if(!is.null(X1) && is.null(X2))
                
                # Qualitative X
                
                if(is.factor(x) ||
                   is.character(x))
                  
                  # X has two levels
                  
                  else if(length(unique(X1)) == 2)
                    
                    mod.lm = lm(Y ~ X1, data)
              
              #  Independence
              
              if(paired == "none")
                
                # Normality of Y (Shapiro-Wilk Test)
                
                if(shapiro.test(Y)[2] > 0.05)
                  
                  # Outliers (Boxplot Method)
                  
                  if(nrow(group_by(X1, .data = data) %>% identify_outliers(Y)) != 0) 
                    
                    warning("There was outliers : please check for measurement and/or experimental errors. If so, remove those values 
                              from the dataframe. Otherwise, a robust method was used to deal with those outliers.")
              
              # Yuen's Test (20% trimmed means)
              
              yuen(Y ~ X1, data = data)
              
              # Effect size: Robust Cohen's D
              
              yuen(Y ~ X1, data = data)$effsize
              
              # Difference between the means and its CI
              
              yuen(Y ~ X1, data = data)$diff
              
              yuen(Y ~ X1, data = data)$conf.int
              
              # No outliers (Boxplot Method)
              
              else
                
                # Homoscedasticity (Fisher-Snedecor Test)
                
                if(var.test(Y ~ X1)[3] > 0.05)
                  
                  # Two-sample Student's T-Test
                  
                  t.test(Y ~ X1, var.equal = T)
              
              # Effect size: Cohen's D
              
              cohens_d(Y~X1, data = data, var.equal = T)
              
              # Difference between the means and its CI
              
              t.test(Y ~ X1, var.equal = T)$estimate[1] - t.test(Y ~ X1, var.equal = T)$estimate[2]
              
              t.test(Y ~ X1, var.equal = T)$conf.int
              
              # Sous-sujet 4
              
              # Heteroscedasticity (Fisher-Snedecor Test)
              
              else
                
                # Two-sample Student's T-Test with Welch correction
                
                t.test(Y ~ X1)
              
              # Effect size: Cohen's D
              
              cohens_d(Y~X1, data = data)
              
              # Difference between the means and its CI
              
              t.test(Y ~ X1)$estimate[1] - t.test(Y ~ X1)$estimate[2]
              
              t.test(Y ~ X1)$conf.int
              
              # Non-normality of Y (Shapiro-Wilk Test)
              
              else
                
                # Two-sample Wilcoxon's Test
                
                wilcox.test(Y ~ X1)
              
              # Effect size: r
              
              wilcox_effsize(Y~X1, data = data)
              
              # Difference between the means and its CI
              
              wilcox.test(Y ~ X1)$estimate
              
              wilcox.test(Y ~ X1, conf.int=T)$conf.int
              
              # Repeated measures
              
              else if(paired == "both" || paired == "second") 
                stop('There is only one factor: please state paired == "first".')
              
              else if(paired == "first") 
                
                if(ID == NULL)
                  warning("IMPORTANT: Please, make sure that the dataframe is ordered so that the 1st observation of 
                            Group 1 is the same subject as the 1st observation of Group 2 and so on.")
              
              else
                group_by(X1, .data = data) %>% arrange(X1, ID)
              
              if(var(table(data$X1) != 0)
                 stop("There is not two observations per subject. Verify the paired argument or your data.")
                 
                 data.long = reshape(data, timevar = "X1", direction = "wide") %>% select(starts_with("Y"))
                 
                 diff = c(data.long[1] # data.long[2])[[1]]
                          
                          data.diff = cbind(data.long, diff)
                          
                          # Normality of the difference (Shapiro-Wilk Test)
                          
                          if(shapiro.test(diff) > 0.05)
                            
                            # Outliers (Boxplot Method)
                            
                            if(nrow(identify_outliers(diff, data = data.diff)) != 0)
                              
                              warning("There was outliers : please check for measurement and/or experimental errors. 
                                        If so, remove those values from the dataframe. Otherwise, a robust method was used to deal 
                                        with those outliers.")
                          
                          # Paired Yuen's Test (20% trimmed means)
                          
                          yuend(data.long[1], data.long[2])
                          
                          # Taille d'effet : Robust Cohen's d
                          
                          yuend(data.long[1], data.long[2])$effsize
                          
                          # Difference between means and its CI
                          
                          yuend(data.long[1], data.long[2])$diff
                          
                          yuend(data.long[1], data.long[2])$conf.int
                          
                          # No outliers (Boxplot Method)
                          
                          else
                            
                            # Paired Two-sample Student's T-Test
                            
                            t.test(Y ~ X1, paired = T)
                          
                          # Effect size: Cohen's D
                          
                          cohens_d(Y~X1, data = data, paired = T)
                          
                          # Difference between means and its CI
                          
                          t.test(Y ~ X1, paired = T)$estimate
                          
                          t.test(Y ~ X1, paired = T)$conf.int
                          
                          # Non-normality of the difference (Shapiro-Wilk Test)
                          
                          else if(shapiro.test(diff) <= 0.05)
                            
                            # Paired Two-sample Wilcoxon's Test
                            
                            wilcox.test(Y ~ X1, paired = T)
                          
                          # Effect size: r
                          
                          wilcox_effsize(Y ~ X1, data = data, paired = T)
                          
                          # Difference between means and its CI
                          
                          wilcox.test(Y ~ X1, paired = T)$estimate
                          
                          wilcox.test(Y ~ X1, paired = T, conf.int=T)$conf.int
                          
                          # X has more than two levels
                          
                          else if(length(unique(X1)) > 2)
                            
                            mod.lm = lm(Y ~ X1, data)
                          mod.aov = aov(Y ~ X1, data)
                          resids = summary(mod.lm)$residuals
                          
                          # Independence
                          
                          if(paired == "none")
                            
                            # Non-normality of the residuals (Shapiro-Wilk Test)
                            
                            if(shapiro.test(resids)$p.value <= 0.05)
                              
                              # Kruskal-Wallis Test
                              
                              kruskal.test(Y ~ X1, data)
                          
                          # Effect size: Eta-squared (based on H-statistic)
                          
                          kruskal_effsize(Y ~ X1, data = data)
                          
                          # Post-hoc: Dunn's Test
                          
                          dunn_test(Y ~ X1, p.adjust.method = "holm", data = data, detailed = T)
                          
                          # Normality of the residuals (Shapiro-Wilk Test)
                          
                          else
                            
                            # Outliers (Boxplot Method)
                            
                            if( nrow(group_by(X1, .data = data) %>% identify_outliers(Y)) != 0 )
                              warning("There was outliers : please check for measurement and/or experimental errors. 
                                        If so, remove those values from the dataframe. Otherwise, a robust method was used to 
                                        deal with those outliers.")
                          
                          # Robust One-way ANOVA (20% trimmed means)
                          
                          robust_ANOVA = t1way(Y ~ X1, data)
                          
                          # Robust effect size
                          
                          robust_ANOVA[5]
                          
                          # Post-hoc: Pairwise comparison
                          
                          lincon(Y ~ X1, data)
                          
                          # No outliers (Boxplot Method)
                          
                          else
                            
                            # Heteroscedasticity (Levene's Test)
                            
                            if( levene_test(Y ~ X1, data = data)[[4]]  <= 0.05 )
                              
                              # Welch ANOVA
                              
                              welch_anova_test(Y ~ X1, data = data)
                          
                          # Effect size: Global Eta-squared
                          
                          anova_test(Y ~ X1, data = data, effect.size = "ges")[[7]]
                          
                          warning("The effect size might be slightly inacurrate since it was calculated with an homoscedastic model. 
                                    Indeed, the effect size to use after a Welch ANOVA is still being discussed and has not been coded 
                                    in R yet (2020) to my knowledge.")
                          
                          #  Post-hoc: Games-Howell's Test
                          
                          games_howell_test(Y ~ X1, data = data, detailed = T)
                          
                          # Homoscedasticity (Levene's Test)
                          
                          else
                            
                            # One-way ANOVA
                            
                            anova_test(Y ~ X1, data = data)
                          
                          # Effect size: Global Eta-squared
                          
                          anova_test(Y ~ X1, data = data, effect.size = "ges")[[7]]
                          
                          # Post-hoc: Tukey's Test
                          
                          tukey = TukeyHSD(model.aov, ordered = T)[[1]]
                          tukey[order(-tukey[,1]),]
                          
                          # Repeated measures
                          
                          else if(paired == "both")
                            
                            if(ID == NULL)
                              stop("Please provide the name of the column containing the blocking variable (i.e. id of the subjects) 
                                     in the ID argument.")
                          
                          # Non-normality of Y by groups (Shapiro-Wilk Test)
                          
                          grouped_normality = group_by(X1, .data = data) %>%
                            shapiro_test(Y)
                          
                          if( any(grouped_normality[5] <= 0.05)
                              
                              # Friedman's Test
                              
                              friedman.test(Y ~ X1 |ID, data = selfesteem)
                              
                              # Effect-size: Kendall's W
                              
                              friedman_effsize(Y ~ X1 |ID)
                              
                              # Pairwise comparison (Wilcoxon's Tests)
                              
                              wilcox_test(Y ~ X1, paired = T, p.adjust.method = "holm", data = data)
                              
                              warning("IMPORTANT: Please, make sure that dataframe is ordered so that the 1st observation of 
                                        Group 1 is the same subject as the 1st observation of Group 2 and so on. The post-hoc analysis 
                                        would otherwise be completely wrong.")
                              
                              # Normality of Y by groups (Shapiro-Wilk Test)
                              
                              else
                                
                                # Outliers (Boxplot Method)
                                
                                if(nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0)
                                  
                                  warning("There was outliers : please check for measurement and/or experimental errors. 
                                            If so, remove those values from the dataframe. Otherwise, a robust method have been used 
                                            to deal with those outliers.")
                              
                              # Robust paired One-way ANOVA
                              
                              rmanova(Y, X1, ID)
                              
                              # Effect size: Global Eta-squared
                              
                              anova_test(dv = Y, wid = ID, within = X1, data = data, effect.size = "ges")[[1]][7]
                              
                              warning("No packages offering functions specifically designed to measure effect size of a 
                                        robust one-way repeated model are implemented in R to my knowledge. The effect size given 
                                        here is a partial eta squared of a parametric one-way repeated model.")
                              
                              # Pairwise comparison
                              
                              rmmcp(Y, X1, ID)
                              
                              # No outliers (Boxplot Method)
                              
                              # One-way paired ANOVA
                              
                              paired_anova = anova_test(dv = Y, wid = ID, within = X1, data = data, effect.size = "ges") 
                              
                              mauchly_test = paired_anova[2]
                              
                              corrected_anova = get_anova_table(paired_anova, correction = "auto")
                              
                              # Effect size: Global Eta-squared
                              
                              corrected_anova[[7]]
                              
                              # Pairwise comparison (Student's T-Tests)
                              
                              pairwise_t_test(Y ~ X1, data = data, paired = T, p.adjust.method = "holm")
                              
                              # Quantitative X
                              
                              else
                                
                                # Linear relationship between the mean of Y and the residuals of the model (Linearity Test)
                                
                                if(gvlma(mod.lm)$GlobalTest$GlobalStat4$pvalue <= 0.05)
                                { warning("Non-linear relationship between the factor and Y, which may decrease accuracy. Therefore, 
                                            you should consider transforming the variables (e.g. log, sqrt, polynomial regression) or 
                                            using a GAM (i.e performing a non-linear regression).")
                                  
                                  # Absence of residuals' autocorrelation (Durbin-Watson Test)
                                  
                                  if(dwtest(mod.lm) <= 0.05)
                                  { warning("Residuals are autocorelated, which may decrease accuracy. This is mainly problematic 
                                              for Time Series Data but it can be fixed by adding a factor containing lagged residuals to 
                                              the original model.") }
                                  
                                  # Independence
                                  
                                  if(paired == "none")
                                    
                                    # Heteroscedasticity (Breusch-Pagan Test) and/or non-normality of the residuals (Shapiro-Wilk Test)
                                    
                                    if( bptest(mod.lm)$p.value > 0.05 || shapiro.test(resids)$p.value > 0.05)
                                      
                                      # Linear Robust Regression
                                      
                                      anova(lmRob(Y ~ X1, data))
                                  
                                  # Kendall's correlation
                                  
                                  cor(X1, Y, method = "kendall")
                                  
                                  # Homoscedasticity (Breusch-Pagan Test) and normality of the residuals (Shapiro-Wilk Test)
                                  
                                  else
                                    
                                    # Outliers (Bonferroni Outlier Test)
                                    
                                    if(outlierTest(mod.lm)[3] <= 0.05)
                                      
                                      warning("There was outliers : please check for measurement and/or experimental errors. 
                                                If so, remove those values from the dataframe. Otherwise, a robust method was used to 
                                                deal with those outliers.")
                                  
                                  # Winsorized correlation (20% winsorization)
                                  
                                  wincor(X1, Y)
                                  
                                  # No outliers (Bonferroni Outlier Test)
                                  
                                  else
                                    
                                    # Simple Linear Regression
                                    
                                    anova(mod.lm)
                                  
                                  # Pearson's correlation
                                  
                                  cor(X1, Y)
                                  
                                  # Repeated measures: ERROR
                                  
                                  else
                                    
                                    stop("The factor cannot be continuous and paired.")
                                  
                                  ### Two X
                                  
                                  else if(!is.null(X1) && !is.null(X2))
                                    
                                    mod.lm = lm(Y ~ X1*X2, data)
                                  mod.aov = aov(Y ~ X1*X2, data)
                                  resids = summary(mod.lm)$residuals
                                  
                                  # Both X are qualitative
                                  
                                  if(length(unique(X1)) == 1)
                                    
                                    stop("The 1st factor is constant while it must have at least two modalities.")
                                  
                                  else if(length(unique(X2)) == 1)
                                    
                                    stop("The 2nd factor is constant while it must have at least two modalities.")
                                  
                                  else
                                    
                                    # Both X are independent measures
                                    
                                    if(paired == "none")
                                      
                                      # Non-normality of the residuals (Shapiro-Wilk Test)
                                      
                                      if(shapiro.test(resids)$p.value <= 0.05)
                                        
                                        # Presence of missing values
                                        
                                        if(anyNA(Y) || anyNA(X1) || anyNA(X2))
                                          
                                          warning("No packages offering functions specifically designed for post-hoc analysis 
                                                    after a permutation test have been coded in R to my knowledge. A pairwise Wilcoxon Test 
                                                    was used instead of it.")
                                  
                                  # Monte-Carlo Test (permutation)
                                  
                                  permutation = ezPerm(dv = Y, wid = ID, between = c(X1, X2), data)
                                  
                                  # Effect size: Partial Eta-squared
                                  
                                  if(anova(mod.lm)[5][[1]][3] > 0.05 {
                                    
                                    ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 2, data = data, effect.size = "pes") }
                                    
                                    else ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 1, data = data, effect.size = "pes")
                                    
                                    ANOVA[[7]]
                                    
                                    warning("The standardized effect size was not calculated for a specific use after a 
                                              permutation test. The result might be slightly inaccurate since it uses a parametric model.")
                                    
                                    # Post-hoc
                                    
                                    # Significant interaction effect
                                    
                                    if(permutation[[2]][3] <= 0.05)
                                      
                                      # Interaction plot
                                      
                                      plot(allEffects(mod.lm), multiline=T, ci.style="bars")
                                    
                                    # Grouped (2nd X) pairwise comparison of the 1st X (Wilcoxon's Tests)
                                    
                                    group_by(X2, .data = data) %>%
                                      pairwise_wilcox_test(Y ~ X1,
                                                           p.adjust.method = "holm"
                                      )
                                    
                                    # Grouped (1st X) pairwise comparison of the 2nd X (Wilcoxon's Tests)
                                    
                                    group_by(X1, .data = data) %>%
                                      pairwise_wilcox_test(Y ~ X2,
                                                           p.adjust.method = "holm"
                                      )
                                    
                                    # Significant main effects and non-significant interaction effect
                                    
                                    else if(permutation[[2]][1] <= 0.05 ||
                                            permutation[[2]][2] <= 0.05)
                                      
                                      # Pairwise comparison of the 1st X (Wilcoxon's Tests)
                                      
                                      pairwise_wilcox_test(Y ~ X1, data = data, p.adjust.method = "holm")
                                    
                                    # Pairwise grouped comparison of the 2nd X (Wilcoxon's Tests)
                                    
                                    pairwise_wilcox_test(Y ~ X2, data = data, p.adjust.method = "holm")
                                    
                                    # Absence of missing values
                                    
                                    else
                                      
                                      # Two-way ANOVA on aligned rank model
                                      
                                      art_model = art(Y ~ X1*X2, data)
                                    
                                    if(anova(art_model)[[7]][3] > 0.05) {
                                      ANOVA = anova(art_model, type = 2) }
                                    
                                    else ANOVA = anova(art_model, type = 1)
                                    
                                    # Effect size: Partial Eta-squared
                                    
                                    with(anova(model), `Sum Sq`/(`Sum Sq` + `Sum Sq.res`))
                                    
                                    # Post-hoc
                                    
                                    # Significant interaction effect
                                    
                                    if(ANOVA[[7]][3] <= 0.05)
                                      
                                      # Interaction plot
                                      
                                      plot(allEffects(mod.lm), multiline=T, ci.style="bars")
                                    
                                    # Pairwise comparison of the interaction (Tukey's Test)
                                    
                                    art_lm = artlm(art_model, colnames(art_model$aligned)[3])
                                    
                                    marginal_X1_X2 = emmeans(art_lm, ~ X1:X2)
                                    
                                    contrast(marginal_X1_X2, method="pairwise", adjust="tukey")
                                    
                                    # Significant main effects and non-significant interaction effect
                                    
                                    else if(ANOVA[[7]][1] <= 0.05 ||
                                            ANOVA[[7]][2] <= 0.05)
                                      
                                      # Pairwise comparison of the 1st X (Tukey's Test)
                                      
                                      art_lm = artlm(art_model, colnames(art_model$aligned)[1])
                                    
                                    marginal_X1 = emmeans(model.lm, ~X1)
                                    
                                    pairs(marginal_X1, adjust = "tukey")
                                    
                                    # Pairwise comparison of the 2nd X (Tukey's Test)
                                    
                                    art_lm = artlm(art_model, colnames(art_model$aligned)[2])
                                    
                                    marginal_X2 = emmeans(model.lm, ~X2)
                                    
                                    pairs(marginal_X2, adjust = "tukey")
                                    
                                    # Normality of the residuals (Shapiro-Wilk Test)
                                    
                                    else
                                      
                                      # Outliers (Boxplot Method)
                                      
                                      if( nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0 )
                                        warning("There was outliers : please check for measurement and/or experimental errors. If so, 
                                                  remove those values from the dataframe. Otherwise, a robust method was used to deal with 
                                                  those outliers.")
                                    
                                    # Robust Two-way ANOVA (20% trimmed means)
                                    
                                    robust_ANOVA = t2way(Y ~ X1*X2, data)
                                    
                                    # Effect size: Partial Eta-squared
                                    
                                    if(anova(mod.lm)[5][[1]][3] > 0.05 {
                                      
                                      ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 2, data = data, effect.size = "pes") }
                                      
                                      ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 1, data = data, effect.size = "pes")
                                      
                                      ANOVA[[7]]
                                      
                                      warning("No packages offering functions specifically designed to measure effect size of a 
                                                two-way trimmed model are implemented in R to my knowledge. The effect size given here is a 
                                                partial eta squared of a parametric model without correction of homoscedasticity.")
                                      
                                      # Post-hoc
                                      
                                      # Pairwise comparison and legend matrix
                                      
                                      comp = mcp2atm(Y ~ X1*X2, data = data)
                                      comp
                                      model.matrix(comp)
                                      
                                      # Significant interaction effect
                                      
                                      if(robust_ANOVA[6] <= 0.05)
                                        
                                        # Interaction plot
                                        
                                        plot(allEffects(mod.lm), multiline=T, ci.style="bars") }
                              
                              # No outliers (Boxplot Method)
                              
                              else
                                
                                # Test
                                
                                # Heteroscedasticity (Levene's Test)
                                
                                if( levene_test(Y ~ X1*X2, data = data)[[4]]  <= 0.05 )
                                  
                                  # Two-way ANOVA with heteroscedasticity correction (Hubert-White covariance matrix) 
                                  
                                  if(anova(mod.lm)[5][[1]][3] > 0.05 {
                                    
                                    ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 2, white.adjust = T, data = data, effect.size = "pes") }
                                    
                                    else ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 1, white.adjust = T, data = data, effect.size = "pes")
                                    
                                    # Homoscedasticity (Levene's Test)
                                    
                                    else
                                      
                                      # Two-way ANOVA
                                      
                                      if(anova(mod.lm)[5][[1]][3] > 0.05 {
                                        
                                        ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 2, data = data, effect.size = "pes") }
                                        
                                        else ANOVA = anova_test(dv = Y, between = c(X1, X2), type = 1, data = data, effect.size = "pes")
                                        
                                        # Post-hoc
                                        
                                        # Effect size: Partial Eta-squared
                                        
                                        ANOVA[[7]]
                                        
                                        # Significant interaction effect
                                        
                                        if(ANOVA[[7]][3] <= 0.05)
                                          
                                          # Interaction plot
                                          
                                          plot(allEffects(mod.lm), multiline=T, ci.style="bars")
                                        
                                        # Grouped (2nd X) ANOVA models with 1st X only
                                        
                                        group_by(X2, .data = data) %>%
                                          anova_test(Y ~ X1, error = model)
                                        
                                        # Grouped (1st X) ANOVA models with 2nd X only
                                        
                                        group_by(X1, .data = data) %>%
                                          anova_test(Y ~ X2, error = model)
                                        
                                        # Tukey's Test
                                        
                                        tukey_X1.X2 = TukeyHSD(model.aov, ordered = T)[[3]]
                                        tukey_X1.X2[order(-tukey_X1.X2[,1]),]
                                        
                                        # Significant main effects and non-significant interaction effect
                                        
                                        else if(ANOVA[[7]][1] <= 0.05 ||
                                                ANOVA[[7]][2] <= 0.05)
                                          
                                          # Tukey's Test on the 1st X
                                          
                                          tukey_X1 = TukeyHSD(mod.aov, ordered = T)[[1]]
                                        {if(nrow(tukey_X1) != 1) { tukey_X1[order(-tukey_X1[,1]),] } else {tukey_X1} }
                                        
                                        # Tukey's Test on the 2nd X
                                        
                                        tukey_X2 = TukeyHSD(mod.aov, ordered = T)[[2]]
                                        {if(nrow(tukey_X2) != 1) { tukey_X2[order(-tukey_X2[,1]),] } else {tukey_X2} }
                                        
                                        # Both X are repeated measures
                                        
                                        else if(paired == "both")
                                          
                                          if(ID == NULL)
                                            stop("Please provide the name of the column containing the blocking variable 
                                                   (i.e. id of the subjects) in the ID argument.")
                                        
                                        # Non-normality of Y by groups (Shapiro-Wilk Test) and/or outliers (Boxplot Method)
                                        
                                        grouped_normality = group_by(X1, X2, .data = data) %>%
                                          shapiro_test(Y)
                                        
                                        if( any(grouped_normality[5] <= 0.05) ||
                                            nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0 )
                                          
                                          if(nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0)
                                            warning("There was outliers : please check for measurement and/or experimental errors. 
                                                      If so, remove those values from the dataframe. Otherwise, a robust method or a non-parametric method have been used to deal with those outliers.")
                                        
                                        # Paired Monte-Carlo Test (permutation)
                                        
                                        permutation = ezPerm(dv = Y, wid = ID, within = c(X1, X2), data)
                                        
                                        # Effect size: Partial Eta-squared
                                        
                                        paired_anova = anova_test(dv = Y, wid = ID, within = c(X1, X2), data = data, effect.size = "pes") }

get_anova_table(paired_anova, correction = "auto")[[7]]

warning("The standardized effect size was not calculated for a specific use after a permutation test. The result might be 
        slightly inaccurate since it uses a parametric model.")

# Post-hoc

# Significant interaction effect

if(permutation[[2]][3] <= 0.05)
  
  # Interaction plot
  
  plot(allEffects(mod.lm), multiline=T, ci.style="bars") }

# Grouped (2nd X) pairwise comparison of the 1st X (Paired Wilcoxon's Tests)

group_by(X1, .data = data) %>%
  pairwise_wilcox_test(Y ~ X2, paired = T,
                       p.adjust.method = "holm"
  )

# Grouped (1st X) pairwise comparison of the 2nd X (Paired Wilcoxon's Tests)

group_by(X2, .data = data) %>%
  pairwise_wilcox_test(Y ~ X1, paired = T,
                       p.adjust.method = "holm"
  )

# Significant main effects and non-significant interaction effect

else if(permutation[[2]][1] <= 0.05 ||
        permutation[[2]][2] <= 0.05)
  
  # Pairwise comparison of the 1st X (Paired Wilcoxon's Tests)
  
  pairwise_wilcox_test(Y ~ X1, data = data, paired = T, p.adjust.method = "holm")

# Pairwise comparison of the 1st X (Paired Wilcoxon's Tests)

pairwise_wilcox_test(Y ~ X2, data = data, paired = T, p.adjust.method = "holm")

# Normality of Y by groups (Shapiro-Wilk Test) and no outliers (Boxplot Method)

else
  
  # Two-way paired ANOVA
  
  paired_anova = anova_test(dv = Y, wid = ID, within = c(X1, X2), data = data, effect.size = "pes", type = 1) 

mauchly_test = paired_anova[2]

corrected_anova = get_anova_table(paired_anova, correction = "auto")

# Effect size: Partial Eta-squared

corrected_anova[[7]]

# Post-hoc

# Significant interaction effect

if(corrected_anova[[5]][3] <= 0.05)
  
  # Interaction plot
  
  plot(allEffects(mod.lm), multiline=T, ci.style="bars")

# Grouped (2nd X) pairwise comparison of the 1st X (Paired Student's T-Tests)

group_by(X2, .data = data) %>%
  pairwise_t_test(Y ~ X1, paired = T,
                  p.adjust.method = "holm")

# Grouped (1st X) pairwise comparison of the 2nd X (Paired Student's T-Tests)

group_by(X2, .data = data) %>%
  pairwise_t_test(Y ~ X1, paired = T,
                  p.adjust.method = "holm"
  )

# Significant main effects and non-significant interaction effect

else if(ANOVA[[7]][1] <= 0.05 ||
        ANOVA[[7]][2] <= 0.05)
  
  # Pairwise comparison of the 1st X (Paired Student's T-Tests)
  
  pairwise_t_test(Y ~ X1, data = data, paired = T, p.adjust.method = "holm")

# Pairwise comparison of the 2nd X (Paired Student's T-Tests)

pairwise_t_test(Y ~ X2, data = data, paired = T, p.adjust.method = "holm")

# Mixed (one X is independent measures and the other repeated measures)

else if(paired == "first" || paired == "second")
  
  if(ID == NULL)
    stop("Please provide the name of the column containing the blocking variable (i.e. id of the subjects) in the ID argument.")

# A ADAPTER : Homogénéité des covariances

if(paired == "first") {
  if(box_m(data[, "Y", drop = F], data$X2)[2] < 0.001)
    warning("The homogeneity of covariance assumption is not satisfied. This could be fixed by increasing the sampling effort and/or 
            transforming the data. Another strategy could be to perform repeated measure ANOVA for each group of the independent variable.  
            This problem is usually linked with non-normality: non-parametric can therefore resolve this problem.") }

if(paired == "second") {
  if(box_m(data[, "Y", drop = F], data$X1)[2] < 0.001)
    warning("The homogeneity of covariance assumption is not satisfied. This could be fixed by increasing 
            the sampling effort and/or transforming the data. Another strategy could be to perform repeated measure ANOVA 
            for each group of the independent variable.  This problem is usually linked with non-normality: non-parametric can 
            therefore resolve this problem.") }

# Non-normality of Y by groups (Shapiro-Wilk Test) and/or heteroscedasticity by groups (Levene's Test) and/or outliers (Boxplot Method)

grouped_normality = group_by(X1, X2, .data = data) %>%
  shapiro_test(Y)

grouped_homogeneity = group_by(X1, X2, .data = data) %>%
  levene_test(Y)

if(any(grouped_normality[5] <= 0.05) ||
   any(grouped_homogeneity[5] <= 0.05) ||
   nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0)
  
  warning("No packages offering functions specifically designed for post-hoc analysis after a permutation test or 
          a robust test (trimmed means) have been coded in R to my knowledge. A pairwise Wilcoxon Test was used instead of it.") }

if(nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0)
  warning("There was outliers : please check for measurement and/or experimental errors. If so, remove those values 
          from the dataframe. Otherwise, a robust method or a non-parametric method have been used to deal with those outliers.")

# Test

# Non-normality and/or heteroscedasticity

if(any(grouped_normality[5] <= 0.05) ||
   any(grouped_homogeneity[5] <= 0.05)
   
   # Mixed Monte-Carlo Test (permutation)
   
   if(paired == "first") {
     permutation = ezPerm(dv = Y, wid = ID, within = X1, between = X2, data) }
   
   else if(paired == "second") {
     permutation = ezPerm(dv = Y, wid = ID, within = X2, between = X1, data) }
   
   # Outliers
   
   else if(nrow(group_by(X1, X2, .data = data) %>% identify_outliers(Y)) != 0)
     
     # ANOVA on the trimmed means (20%)
     
     bwtrim(Y ~ X1*X2, id = ID, data) ]

# Effect size: Partial Eta-squared

if(paired == "first") {
  paired_anova = anova_test(dv = Y, wid = ID, within = X1, between = X2, data = data, effect.size = "pes") }

else if(paired == "second") {
  paired_anova = anova_test(dv = Y, wid = ID, within = X2, between = X1, data = data, effect.size = "pes") }

get_anova_table(paired_anova, correction = "auto")[[7]]

warning("The standardized effect size was not calculated for a specific use after a permutation test or a robust mixed ANOVA. 
        The result might be slightly inaccurate since it uses a parametric model.")

# Post-hoc

# Significant interaction effect

if(permutation[[2]][3] <= 0.05)
  
  # Interaction plot
  
  plot(allEffects(mod.lm), multiline=T, ci.style="bars")

# Grouped (1st X and/or 2nd X) pairwise comparison of the 1st X and/or 2nd X (Paired or not Wilcoxon's Tests)

if(paired == "first") {
  group_by(X2, .data = data) %>%
    pairwise_wilcox_test(Y ~ X1,
                         p.adjust.method = "holm") 
  
  group_by(X1, .data = data) %>%
    pairwise_wilcox_test(Y ~ X2, paired = T, p.adjust.method = "holm") }

else if(paired == "second") {
  group_by(X1, .data = data) %>%
    pairwise_wilcox_test(Y ~ X2,
                         p.adjust.method = "holm") 
  
  group_by(X2, .data = data) %>%
    pairwise_wilcox_test(Y ~ X1, paired = T, p.adjust.method = "holm") }

# Significant main effects and non-significant interaction effect

else if(permutation[[2]][1] <= 0.05 ||
        permutation[[2]][2] <= 0.05)
  
  # Pairwise comparison of the 1st X (Paired or not Wilcoxon's Tests)
  
  if(paired == "first") {
    pairwise_wilcox_test(Y ~ X1, data = data, paired = T, p.adjust.method = "holm") }

if(paired == "second") {
  pairwise_wilcox_test(Y ~ X1, data = data, p.adjust.method = "holm") }

# Pairwise comparison of the 2nd X (Paired or not Wilcoxon's Tests)

if(paired == "first") {
  pairwise_wilcox_test(Y ~ X2, data = data, p.adjust.method = "holm") }

if(paired == "second") {
  pairwise_wilcox_test(Y ~ X2, data = data, paired = T, p.adjust.method = "holm") }

# Normality of Y by groups (Shapiro-Wilk Test) and heteroscedasticity by groups (Levene's Test) and no outliers (Boxplot Method)

else
  
  # Mixed Two-way ANOVA
  
  if(paired == "first") {
    paired_anova = anova_test(dv = Y, wid = ID, within = X1, between = X2, data = data, effect.size = "pes") }

else if(paired == "second") {
  paired_anova = anova_test(dv = Y, wid = ID, within = X2, between = X1, data = data, effect.size = "pes") }

mauchly_test = paired_anova[2]

corrected_anova = get_anova_table(paired_anova, correction = "auto")

# Effect size: Partial Eta-squared

corrected_anova[[7]]

# Post-hoc

# Significant interaction effect

if(corrected_anova[[5]][3] <= 0.05)
  
  # Interaction plot
  
  plot(allEffects(mod.lm), multiline=T, ci.style="bars")

# Grouped (1st X and/or 2nd X) pairwise comparison of the 1st X and/or 2nd X (Paired or not Student's T-Tests)

if(paired == "first") {
  group_by(X2, .data = data) %>%
    pairwise_t_test(Y ~ X1,
                    p.adjust.method = "holm") 
  
  group_by(X1, .data = data) %>%
    pairwise_t_test(Y ~ X2, paired = T,
                    p.adjust.method = "holm") }

else if(paired == "second") {
  group_by(X1, .data = data) %>%
    pairwise_t_test(Y ~ X2,
                    p.adjust.method = "holm") 
  
  group_by(X2, .data = data) %>%
    pairwise_t_test(Y ~ X1, paired = T,
                    p.adjust.method = "holm") }

# Significant main effects and non-significant interaction effect

else if(ANOVA[[7]][1] <= 0.05 ||
        ANOVA[[7]][2] <= 0.05)
  
  # Pairwise comparison of the 1st X (Paired or not Student's T-Tests)
  
  if(paired == "first") {
    pairwise_t_test(Y ~ X1, data = data, paired = T, p.adjust.method = "holm") }

if(paired == "second") {
  pairwise_t_test(Y ~ X1, data = data, p.adjust.method = "holm") }

# Pairwise comparison of the 2nd X (Paired or not Student's T-Tests)

if(paired == "first") {
  pairwise_t_test(Y ~ X2, data = data, p.adjust.method = "holm") }

if(paired == "second") {
  pairwise_t_test(Y ~ X2, data = data, paired = T, p.adjust.method = "holm") }

# Both X are quantitative

# Linear relationship between the mean of Y and the residuals of the model (Linearity Test)

if(gvlma(mod.lm)$GlobalTest$GlobalStat4$pvalue <= 0.05)
  
{ warning("Non-linear relationship between the factor(s) and Y, which may decrease accuracy. 
          Therefore, you should consider transforming the variables (e.g. log, sqrt, polynomial regression) or using a 
          GAM (i.e performing a non-linear regression).")
  
  # Absence of multicolinearity (VIF)
  
  if(vif(mod.lm)[1]>4) {warning("Multicolinearity between factors. Please consider removing the 1st factor from the formula (highest VIF)") }
  
  else if(vif(mod.lm)[2]>4) {warning("Multicolinearity between factors. Please consider removing the 2nd factor from the formula (highest VIF)") }
  
  else
    
    # Absence of correlation between the residuals and one or both X (Kendall's correlation)
    
    if(cor.test(X1, resids, method= "kendall")$p.value <= 0.05 && cor.test(X2, resids, method= "kendall")$p.value <= 0.05) {
      { warning("Both factors are correlated with the residuals. OLS is biased so you should consider using a Model II Regression".) }
      
      
      else if(cor.test(X1, resids, method= "kendall")$p.value <= 0.05) { warning("The first predictor is correlated with the residuals. 
                                                                                 OLS is biased so you should consider using a Model II Regression".) } }
  
  else if(cor.test(X2, resids, method= "kendall")$p.value <= 0.05)
  { warning("Second factor correlated with the residuals. OLS is biased so you should consider using a Model II Regression".) }
  
  else
    
    # Absence of residuals' autocorrelation (Durbin-Watson Test)
    
    if(dwtest(mod.lm) <= 0.05)
    { warning("Residuals are autocorelated, which may decrease accuracy. This is mainly problematic for Time Series Data but it 
              can be fixed by adding a factor containing lagged residuals to the original model.") }
  
  # Independence
  
  if(paired == "none")
    
    # Non-normality of the residuals (Shapiro-Wilk Test) and/or heteroscedasticity (Breusch-Pagan Test) and/or outliers (Bonferroni Outlier Test) 
    
    if( bptest(mod.lm)$p.value > 0.05 ||
        shapiro.test(resids)$p.value > 0.05 ||
        outlierTest(mod.lm)[3] <= 0.05)
      
      if(outlierTest(mod.lm)[3] <= 0.05)
        warning("There was outliers : please check for measurement and/or experimental errors. If so, remove those values from the dataframe. 
                Otherwise, a robust method was used to deal with those outliers.")
  
  # Multivariate robust regression
  
  anova(lmRob(Y ~ X1*X2, data)
        
        # Effect size: Partial Cohen's f-squared
        
        if(anova(lmRob(Y ~ X1*X2, data))[[3]][2] <= 0.05) {
          R2_X1 = summary(lmRob(Y~X1))$r.squared
          R2_X1_X2 = summary(lmRob(Y~X1+X2))$r.squared
          p_cohen_f2_X1 = (R2_X1_X2 # R2_X1)/(1 # R2_X1_X2)
                           p_cohen_f2_X1 }
        
        else if(anova(lmRob(Y ~ X1*X2, data))[[3]][3] <= 0.05) {
          R2_X2 = summary(lm(Y~X2))$r.squared
          R2_X1_X2 = summary(lm(Y~X1+X2))$r.squared
          p_cohen_f2_X2 = (R2_X1_X2 # R2_X2)/(1 # R2_X1_X2)
                           p_cohen_f2_X2 }
        
        # Correlation matrix
        
        # Outliers (Bonferroni Outlier Test)
        
        if(outlierTest(mod.lm)[3] <= 0.05)
          
          # Winsorized correlation matrix (20% winsorization)
          
          winall(data.frame(Y, X1, X2))
        
        # No outliers (Bonferroni Outlier Test)
        
        else
          
          # Kendall's correlation matrix 
          
          cor(data.frame(Y, X1, X2), method = "kendall")
        
        # Significant interaction effect
        
        if(anova(lmRob(Y ~ X1*X2, data))[[3]][4] <= 0.05)
          
          # Interaction plot
          
          plot(allEffects(mod.lm), multiline=T, ci.style="bars")
        
        # Normality of the residuals (Shapiro-Wilk Test) and homoscedasticity (Breusch-Pagan Test) and no outliers (Bonferroni Outlier Test)
        
        else
          
          # Multivariate regression
          
          anova(mod.lm)
        
        # Effect size: Partial Cohen's f-squared
        
        if(anova(mod.lm)[[5]][1] <= 0.05) {
          R2_X1 = summary(lm(Y~X1))$r.squared
          R2_X1_X2 = summary(lm(Y~X1+X2))$r.squared
          p_cohen_f2_X1 = (R2_X1_X2 # R2_X1)/(1 # R2_X1_X2)
                           p_cohen_f2_X1 }
        
        else if(anova(mod.lm)[[5]][2] <= 0.05) {
          R2_X2 = summary(lm(Y~X2))$r.squared
          R2_X1_X2 = summary(mod.lm)$r.squared
          p_cohen_f2_X2 = (R2_X1_X2 # R2_X2)/(1 # R2_X1_X2)
                           p_cohen_f2_X2 }
        
        # Correlation matrix
        
        cor(data.frame(Y, X1, X2))
        
        # Significant interaction effect
        
        if(anova(mod.lm)[[5]][3]<= 0.05)
          
          # Interaction plot
          
          plot(allEffects(mod.lm), multiline=T, ci.style="bars")
        
        # Repeated measures: ERROR
        
        else if(paired != "none") {
          if(id != NULL)
            stop("Please, provide data in a long format instead of a wide format, if you wanted to perform a paired test.") }
        
        else 
          stop("The factor(s) cannot be continuous and paired.")
        
        # One X is qualitative and the other is quantitative: NOT SUPPORTED
        
        if(is.numeric(X1) || is.integer(X1))
          if(is.factor(X2) || is.character(X2)) {
            
            stop("The 1st X was a number/integer while the 2nd X was a character/factor. This is not supported by this function. 
                 You might want to convert the 1st X in a character or a factor (as.[...]) and run this function again. 
                 You could also want to perform a MANOVA, which are also not supported by this function.")
            
            else if(is.factor(X1) || is.character(X1))
              if(is.numeric(X2) || is.integer(X2)) {
                
                stop("The 1st X was a character/factor while the 2nd X was a number/integer. 
                     This is not supported by this function. You might want to convert the 2nd X in a character or a factor (as.[...]) 
                     and run this function again. You could also want to perform a MANOVA, which is not supported by this function.")
                
                
              }
            
