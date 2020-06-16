ci_nlm = function(formula, fct, data, method = "delta", level = 0.05, nb_boot = 200, expand_x = NA, keep_cols = NULL) {
  
  mf = match.call(expand.dots = FALSE)                                                     
  m = match(c("formula", "data"), names(mf), 0L)                                     
  mf = mf[c(1L, m)]                                   
  mf$drop.unused.levels = TRUE            
  mf[[1L]] = quote(stats::model.frame)                                                          ## Même chose que compare_nlm pour toute cette partie.  
  mf = eval(mf, parent.frame())        
  mt = attr(mf, "terms")                                                       
  x = model.matrix(mt, mf)[,2]                                        
  y = model.response(mf, "numeric")  
  options(error = NULL, max.print=100)                                                         
  
  model = tryCatch(drm(y ~ x, fct = fct, data = data), error = function(e) stop("Convergence failed")) ## Calcul du modèle et erreur si non adapté.
  residuals = residuals(model)                                                                  ## Calcul des résidus du modèle.
  
  ID_vars = match(names(mf), colnames(data))                                                    ## Pour connaitre la position de x et y dans le tableau
  New_Data = data[complete.cases(data[ , ID_vars]),]                            ## Pour retirer les lignes ou il y a des NA dans les colonnes X et/ou Y.
  
  if(anyNA(subset(data, select = names(mf[1]))) || anyNA(subset(data, select = names(mf[2])))) { ## Informer l'utilisateur que des lignes...
    warning("Rows containing NA in X and Y columns were silently removed.")  }                   ## ...retirés du tableau pour les calculs
  
  {if(!anyNA(expand_x)) {                                                                        ## Vérification des conditions si l'utilisateur ...
    ## ... souhaite utiliser la commande expand_x.
    if(!is.numeric(expand_x)) { stop("Expand_x must be a numeric vector.") }                     ## Il faut que le vecteur soit numérique.
    
    else if(length(expand_x) != 2) { stop("Expand_x must be a vector with two values (lower/upper limits).") } ## Il faut qu'il contienne deux valeurs.
    ## Et qu'elles englobent X.
    else if(min(expand_x) >= min(x)||max(expand_x) <= max(x)) { stop("The limits of expand_x have to incorporate those of x.") } ## Arrêt de la fonction
    ## ...et message d'erreur sinon.
    else {  size_new_x = max(expand_x) - min(expand_x)                                           ## Si tout est bon, calculer la taille du nouveau X.
    step_x = (max(x) - min(x)) / length(x)  ## Calculer le pas mayen entre les valeurs (pour ne pas gonfler artificiellement les X et donc réduire l'IC).
    below_x = expand.grid(New_X = seq(min(expand_x), min(x), by = step_x))                       ## Créer les nouveaux X inférieurs aux données.
    original_x = data.frame(New_X = x)                                                           ## Transformer les X existants en tableau.
    above_x = expand.grid(New_X = seq(max(x), max(expand_x), by = step_x))                       ## Créer les nouveaux X supérieurs aux données.
    New_X = rbind(below_x, original_x, above_x)  }  }  }                                         ## Ajouter la colonne des X entre les nouveaux X.
  
  {if(method == "delta") {                                                                       ## Si l'utilisateur veut calculer l'IC par bootstrap...
    ## Si il veut étirer les données, calculer les prédictions...
    {if(anyNA(expand_x)) { New_CI_tab = suppressWarnings(data.frame(predict(model, interval = "confidence"))) } ## et les IC avec predict du package drc.
      
      else { New_CI_tab = suppressWarnings(data.frame(predict(model, newdata = New_X, interval = "confidence"))) ## Et calculer l'IC avec la fonction predict.
      New_CI_tab = cbind(New_X, New_CI_tab) } }                                                 ## Ajouter le nouveau X au tableau.
    # Pour informer que calculer l'IC avec predict n'est possible qu'avec les S-S du package drc.
    {if(is.null(ncol(New_CI_tab)) || ncol(New_CI_tab) == 1 || ncol(New_CI_tab) == 2) stop('        
      Please consider using the equivalent bootstrap method (method = "boot").
      Delta method is not available for this Self-Starters (list on the Github page).
      ') 
      else if(ncol(New_CI_tab) == 3) names(New_CI_tab) = c("Predictions","Lower_CI","Upper_CI") ## Renommage des colonnes.
      else names(New_CI_tab) = c("New_X","Predictions","Lower_CI","Upper_CI")  }  }
    
    else if(method == "boot") {                                                                 ## Si l'utilisateur veut calculer l'IC par bootstrap... 
      
      if(anyNA(expand_x)) {                                                                     ## Si l'utilisateur ne veut pas étendre l'axe des X... 
        Predictions = suppressWarnings(predict(model))                                          ## Calcul des valeurs de Y prédites par le modèle.
        ci_matrix = matrix(0, nrow = nrow(New_Data), ncol = nb_boot)                            ## Construction d'une matrice vide pour le stockage.
        
        for(i in 1:nb_boot){                                                                    ## Calcul de nouveaux Y en ajoutant des résidus choisis
          Y = Predictions + sample(residuals, size = nrow(New_Data), replace = T)               ## aléatoirement aux valeurs prédites. Puis calcul si...
          model_boot = suppressWarnings(tryCatch(drm(Y ~ x, fct = fct), error = function(e) stop("Convergence failed"))) ## possible du modèle X fois...
          ci_matrix[,i] = suppressWarnings(predict(model_boot))  }                              ## ...et stockage des valeurs prédites dans la matrice.
        
        New_CI_tab = data.frame(t(apply(ci_matrix, 1, function(x) quantile(x, c(level/2,1-level/2))))) ## Calcul des bornes de l'IC selon le seuil voulu
        New_CI_tab = cbind(Predictions, New_CI_tab)                                             ## Ajout des valeurs prédites initialement au tableau.
        names(New_CI_tab) = c("Predictions","Lower_CI","Upper_CI")  }                           ## Nommage des colonnes
      
      else {  warning("Bootstrap method is less reliable than Delta method when X are expanded.") ## La méthode Delta est mieux dans ce cas après tests.
        
        New_CI_tab = data.frame(New_X = New_X)                                                  ## Renommage du tableau New_X pour le modifier ensuite.
        Predictions = suppressWarnings(predict(model, newdata = New_CI_tab))                    ## Calcul des prédictions à partir des nouveaux X.
        New_CI_tab$Predictions = Predictions                                                    ## Ajout des prédictions au nouveau tableau de données.
        ci_matrix = matrix(0, nrow = nrow(New_CI_tab), ncol = nb_boot)                          ## Création d'une matrice vide de taille adaptée.                                             
        
        for(i in 1:nb_boot){                                                                         
          Y = Predictions + sample(residuals, size = nrow(New_CI_tab), replace = T)             ## Même chose que plus haut, mais adapté au nouveau...
          model_boot = drm(Y ~ New_CI_tab$New_X, fct = fct)                                     ## ...jeu de données.
          ci_matrix[,i] = suppressWarnings(predict(model_boot, newdata = New_CI_tab))  }                                                                                            
        
        New_CI_tab[c("Lower_CI","Upper_CI")] = t(apply(ci_matrix, 1, function(x) quantile(x, c(level/2,1-level/2))))  }  } ## Ajout de l'IC au tableau.
    
    else stop('Method must be either "delta" or "boot"')  }
  
  if(!is.null(keep_cols)) {                                                                     ## Si l'utilisateur veut ajouter une/des colonne(s)...
    
    supplementary_cols = match(keep_cols, colnames(data))                                       ## D'abord otenir leur numéro dans le tableau.
    previous_names = names(New_CI_tab)                                                          ## Stockage des noms des colonnes de New_CI_tab.
    new_names = vector(length = 0)                                                              ## Vecteur vide pour mettre le nom des colonnes à ajouter
    
    if(length(keep_cols) == 1) {                                                                ## Si un seul argument est entré dans keep_cols...
      
      if(keep_cols == "all") {                                                                  ## Si cet argument est tout le tableau précédents...
        
        if(!anyNA(expand_x)) { rep_dataframe = New_Data[rep(seq_len(nrow(New_Data)), length.out = nrow(New_CI_tab)), ] ## Sinon, répliquer le tableau.
        New_CI_tab = cbind(rep_dataframe, New_CI_tab)  }                                        ## Et l'ajouter au nouveau tableau avec les IC.
        
        else{ New_CI_tab = cbind(New_Data, New_CI_tab)  }  }            ## Si l'utilisateur ne veut pas étirer les X, combiner les tableaux directement.
      
      else if(!anyNA(supplementary_cols)) { rep_col = rep(t(New_Data[,supplementary_cols]), length.out = nrow(New_CI_tab)) ## Si l'utilisateur ne veut
      New_CI_tab = cbind(rep_col, New_CI_tab)                           ## ...ajouter qu'une seule colonne, augmenter sa taille et l'ajouter au tableau.
      names(New_CI_tab) = c(keep_cols, previous_names)  }                                                         ## Renommage des colonnes du tableau.
      
      else stop('Syntax error - Must be a vector of one or more column names or "all".')  }     ## Message d'erreur si il y a une erreur de syntaxe. 
    
    else { if(anyNA(supplementary_cols)) stop('Syntax error - Must be a vector of one or more column names or "all".') ## De même si plusieurs colonnes.
      
      else {rep_cols = as.data.frame(matrix(nrow = nrow(New_CI_tab), ncol = max(supplementary_cols)))  ## Création d'un tableau vide de longueur adaptée.
      
      for(j in supplementary_cols) { new_names[j] = names(tab_ab_c[j])                              ## Cherche et stocke les noms des colonnes à ajouter.
      rep_cols[j] = as.matrix(rep(t(subset(data, select = j)), length.out = nrow(New_CI_tab))) } }  ## Augmentation du nombre de lignes des colonnes.
      
      rep_cols = rep_cols[, colSums(is.na(rep_cols)) < nrow(rep_cols)]                              ## Retirage des colonnes vides ou remplies de NA.
      New_CI_tab = cbind(rep_cols, New_CI_tab)                                                      ## Ajout des colonnes répétés à New_CI_tab.
      names(New_CI_tab) = c(new_names[!is.na(new_names)], previous_names)  }  }                     ## Renommage des colonnes (en enlevant les NA).
  
  print(New_CI_tab)                                                                                 ## Affichage du tableau final
}                                                                         
