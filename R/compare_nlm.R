compare_nlm = function(formula, data, digits = 2, arrange = c("AIC", "RMSE", "BIC"), increase = T, plot_model = NA, package = F, ...) {
  ## Ci-dessus : Nommage de la fonction et définition de ses arguments et des valeurs par défaut + acceptation d'éventuels arguments extérieurs (...).
  
  mf = match.call(expand.dots = FALSE)                                     ## Pour ne pas stopper la fonction si l'un des arguments n'est pas spécifié.
  m = match(c("formula", "data"), names(mf), 0L)                        ## Création d'un vecteur rescensant si formula et data sont présents (0 sinon).  
  mf = mf[c(1L, m)]                                   ## Réduit l'expression donnée par l'utilisateur à la formule (1L) et/ou les données si présentes.
  mf$drop.unused.levels = TRUE            ## Ajout d'une spécification dans la formule (pour utiliser directement des vecteurs ou data$y ~ data$x içi).
  mf[[1L]] = quote(stats::model.frame)                ## Transformation de compare_nlm(formula, data, drop.unused.levels=T) en model.frame(même chose).                           
  mf = eval(mf, parent.frame())        ## Evaluation de l'expression dans l'environnement global où se trouve data pour extraire que x et y du tableau.
  
  mt = attr(mf, "terms")                                                              ## Extraction des informations sur les termes du model.frame.
  x = model.matrix(mt, mf)[,2]                                              ## Extraction et stockage de x avec pour nom le numéro de ligne dans data.
  y = model.response(mf, "numeric")                                                   ## De même en plus court pour y avec la fonction model.response.
  
  fct_env = new.env()                                                     ## Définition d'un environnement propre à la fonction où stocker les modèles.
  
  functions_aomisc = c(c(16:32),c(34,35),52)                                          ## Self-starters des packages aomisc et drc fonctionnels 
  functions_drc = c(3,4,c(7:12),c(21:27),c(38:39),c(43:46),c(48:49),54,55,c(59:66),70,c(72:89),c(97:104),108,c(110:112),131,c(133:144),c(147:162))
  func = c(ls("package:drc")[functions_drc], ls("package:aomisc")[functions_aomisc])  ## Assemblage des Self-starters dans une liste
  number_fct = length(func)                                                           ## Comptage du nombre de fonctions
  
  reset_par=par(oma=c(0,0,0,0),mfrow=c(1L,1L),mar=c(5.1,4.1,4.1,2.1))                 ## Stockage des valeurs graphiques par défaut avant modification.
  if(!anyNA(plot_model)) {                                                            ## Si l'utilisateur veut ajouter un graphique (par défaut non)...
    num_rows = as.list(c(1, 1, 1, 2, 2, 2, 3, 3, 3))                                  ## Listes du nombre de lignes et de colonnes nécessaires... 
    num_cols = as.list(c(1, 2, 3, 2, 3, 3, 3, 3, 3))                                  ## ...en fonction du nombre de graphiques à plotter.
    num_plot = as.numeric(length(plot_model))                                         ## Comptage du nombre de graphiques demandé par l'utilisateur.
    id_plot = as.list(as.vector(plot_model))                                          ## Liste des ID's des graphs à plotter.
    for(j in num_plot) {                                                              ## Vérification de la validité du nombre et de l'ID des graphs...
      { if(num_plot >= 10) {stop("Only 1 to 9 plots please.")}                        ## ...au début pour vite interrompre la fonction en cas d'erreur.
        else if (as.numeric(id_plot[[j]]) < 1) {stop("Plot 0 does not exist !")}      ## Sinon, messages d'erreurs pour expliquer où est l'erreur...
        else if (as.numeric(id_plot[[j]]) > number_fct) {stop("This plot does not exist !")} }  }   ## ...afin de la régler facilement. 
    par(oma=c(2,2,0.5,0),mar=c(2.5,3.5,2,2),mfrow=c(as.numeric(num_rows)[num_plot], as.numeric(num_cols)[num_plot]))  
  }        ## Ci-dessus : Division adaptée de la fenêtre graphique à la demande, et définition de l'emplacement du texte et de l'épaisseur des marges.
  on.exit(par(reset_par))                                                             ## Restauration des paramètres graphiques non modifiés.
  
  model = sprintf("fit%s",seq(number_fct)+1)                                          ## Vecteurs contenant les mêmes noms que les modèles, ... 
  residuals = sprintf("residuals%s",seq(number_fct)+1)                                ## ... les erreurs et les coefficients calculés plus tard et ...  
  coeffs = sprintf("coeff%s",seq(number_fct))                                         ## stockés dans l'environnement, afin de pouvoir les appeler.
  rmse = vector(length = number_fct)                                                  ## Définitions de vecteurs vides et d'un tableau vide pour y ...
  number_coeff = vector(length = number_fct)                                          ## ... stocker les coefficients, leur nombre et les RMSE.
  coeff_tab = data.frame(coeff1 = rep(0, number_fct), coeff2 = rep(0, number_fct), coeff3 = rep(0, number_fct), coeff4 = rep(0, number_fct), 
                         coeff5 = rep(0, number_fct), coeff6 = rep(0, number_fct), coeff7 = rep(0, number_fct))
  
  for(i in 1:number_fct) {                                                    ## Exécute chacun des Self-Starters de la liste les uns après les autres.
    suppressWarnings(tryCatch( { assign(paste("fit",(i+1),sep=""), drm(formula = y ~ x, fct = get(func[i])()), envir = fct_env)
      number_coeff[i] = unlist(paste(length(coef(get(model[i], fct_env))), "coeffs", sep = " ")) ## Puis stocke les modèles dans l'environnement et ...
      coeff_tab[i,] = as.list(paste0(names(coef(get(model[i], fct_env))),             ## ...en extrait leur nombre de coefficients et leurs valeurs pour
                                     round(coef(get(model[i], fct_env)), digits = digits))) } , ## les stocker dans un vecteur vide ou le tableau vide.  
      error = function(e) print(sprintf("Convergence failed - Self-Starter ID : %d", i))))          ##  Message indiquant l'ID du/des S-S déficient(s).
    
    suppressWarnings(tryCatch( { assign(paste("residuals",i+1,sep=""), y - predict(get(model[i], fct_env)))
      rmse[i] = round(sqrt(mean(get(residuals[i], fct_env)^2)), digits = digits) },  ## Calcul et stockage (environnement) des résidus de chaque modèle.
      error = function(e) NA))    
    
    if(package == TRUE)  {                                                            ## Changement des noms pour y ajouter le nom du package d'origine
      { if(!all(is.na(match(ls("package:aomisc"), func[i]))))  {  func[i] = sprintf("aomisc::%s", func[i])  } ## ...si l'option est actvée.
        else  {  func[i] = sprintf("drc::%s", func[i])  }  }  }  }                       ## Puis calcul des RMSE comme précédemment pour les coefficients.
  
  if(!anyNA(plot_model)) {                                                            ## Si l'utilisateur veut ajouter un graphique ...
    for(k in id_plot) {                                                               ## Plotter chacun des modèles demandés en affichant son nom et le
      plot(get(model[k], fct_env), log="", xlab = "", ylab = "", main = paste(func[k], " (RMSE:", rmse[k], ")")) }  ## RMSE, mais pas le nom des axes.
    mtext(text=names(mf)[1],side=2,line=0,outer=TRUE)                                 ## Affichage du nom des variables de la formule entrée par ... 
    mtext(text=names(mf)[2],side=1,line=0,outer=TRUE) }                               ## ... l'utilisateur autour de la fenêtre graphique.
  
  aic = cbind.data.frame(round(unlist(eapply(fct_env, FUN = AIC)), digits = digits))  ## Calcul de l'AIC de chacun des modèles par rapport aux autres.
  rownames(aic) = sub("fit", "", basename(rownames(aic)))                             ## Conversion du nom des modèles (nom des lignes) en chiffre.
  aic$ID = as.numeric(rownames(aic))                                                  ## Duplication du nom des lignes pour les "protéger" dans une ...
  aic = aic[order(aic$ID),]                                                           ## ... colonne puis tri de cette colonne par ordre croissant.
  
  bic = cbind.data.frame(round(unlist(eapply(fct_env, FUN = BIC)), digits = digits))  ## De même pour le BIC.
  rownames(bic) = sub("fit", "", basename(rownames(bic)))                             
  bic$ID = as.numeric(rownames(bic))
  bic = bic[order(bic$ID),]
  
  insertRow = function(data, newrow, r) {                                             ## Ajout d'une fonction permettant d'insérer une ligne dans ...
    data[seq(r+1,nrow(data)+1),] = data[seq(r,nrow(data)),]                           ## ... un data.frame à la position requise.
    data[r,] = newrow
    data  }
  
  missing_model = setdiff(1:length(func), aic$ID)                                     ## Stockage du nom des modèles manquant par comparaison avec ...
  for(j in 1:length(missing_model)) {                                                 ## ... l'identifiant attendu pour l'ensemble des modèles.
    aic = insertRow(data = aic, c(NA, missing_model[j]), missing_model[j])            ## Puis insertion de lignes indiquant que le modèle est manquant,
    bic = insertRow(data = bic, c(NA, missing_model[j]), missing_model[j])  }         ## ... à la position où il aurait dû être.
  
  coeff_tab = as.data.frame(t(apply(coeff_tab,1, function(x) {x[duplicated(x)] = ''; x}))) ## Remplacement des coefficients dupliqués en remplissant le
  coeff_tab = data.frame(lapply(coeff_tab, function(x) {sub("\\(Intercept)", " ", x)})) ## tableau par un espace, et raccourcissement du nom du coeff.
  
  result = data.frame(func, aic[-1,1], bic[-1,1], rmse, number_coeff, coeff_tab)      ## Premier assemblage en tableau des valeurs calculées plus haut.
  id_models = rownames(result)                                                        ## Pour avoir l'identifiant (ligne) de chacun des modèles.
  result = data.frame(id_models, func, aic[-1,1], bic[-1,1], rmse, number_coeff, coeff_tab)          ## Assemblage à nouveau avec l'identifiant.
  names(result) = c("ID", "Function", "AIC", "BIC", "RMSE", "Nb_coeffs", "Coeff_1", "Coeff_2",       ## Changement des noms de chaque colonne pour ...
                    "Coeff_3", "Coeff_4", "Coeff_5", "Coeff_6", "Coeff_7")                           ## ...qu'ils soient suffisament courts et visibles.
  result[result == 0] = NA                                                            ## Pour transformer les résultats divers donné par les différents
  result[result == FALSE] = NA                                                        ## ...en NA, afin que toutes les cases aient les mêmes.
  
  id_order_cols = match(arrange, colnames(result))                                    ## Passage en numéro de colonne des noms entrés par l'utilisateur.
  {if(anyNA(id_order_cols)) {stop("Dataframe can only be ordered using those columns: RMSE, AIC, BIC & Nb_coeffs")} ## Si non valide -> message d'erreur
    else if(length(arrange) == 1) {result = result[with(result, order(result[,id_order_cols[1]], decreasing = !increase)),]}
    else if(length(arrange) == 2) {result = result[with(result, order(result[,id_order_cols[1]], result[,id_order_cols[2]], decreasing = !increase)),]}
    else if(length(arrange) == 3) {result = result[with(result, order(result[,id_order_cols[1]], result[,id_order_cols[2]], 
                                                                      result[,id_order_cols[3]], decreasing = !increase)),]}
    else if(length(arrange) == 4) {result = result[with(result, order(result[,id_order_cols[1]], result[,id_order_cols[2]],
                                                                      result[,id_order_cols[3]], result[,id_order_cols[4]], decreasing = !increase)),]} 
  }  ## Sinon, triage du tableau en fonction des colonnes demandées (il faut faire 4 cas, en fonction de si il y a 1, 2, 3 ou 4 colonnes demandées)
  
  Rank = c(1:number_fct)                                                              ## Ajout d'une colonne rang pour montrer à quelle place par ...
  result = cbind(Rank, result)                                                        ## ... rapport aux autres se situent le modèle.
  
  result[] = lapply(result, as.character)                                             ## Transformation en charactère de tout le tableau pour pouvoir... 
  result = format(result, justify = "centre")                                         ## ...centrer les valeurs de chaque colonne et effectuer la suite.
  sep_line = vector(length = ncol(result))                                            ## Création de vecteurs vides pour faire une ligne de séparation 
  title_name = vector(length = ncol(result))                                          ## ...entre le titre et les valeurs, et pour centrer les titres.
  for(l in 1:length(result)) {                                                        ## Pour chacune des colonnes...
    x = nchar(colnames(result))[l]                                                    ## ...compter le nombre de caractères de son titre et du...
    y = max(nchar(result[,l]))                                                        ## ...maximum de charactères atteint par les valeur de la colonne. 
    {if(x > y) {sep_line[l] = strrep("=", x+1)                                        ## Ensuite, créer un vecteur constitué d'une suite de =, d'une 
    title_name[l] = colnames(result)[l] }                                           ## ...longueur égale à celle du plus grande nombre de charactères.
      else {sep_line[l] = strrep("=", y+1)                                            ## De même avec des espaces, autour des titres pour les centrer.
      title_name[l] = paste(strrep(" ", (y-x)/2), colnames(result)[l], strrep(" ", ((y-x)/2+1)), sep = "")}} }
  names(result) = title_name                                                          ## Renommage des colonnes avec les titres centrés
  result = rbind(sep_line,result)                                                     ## Ajout de la ligne de séparation en haut du tableau (ligne 1)
  
  options(error = NULL, max.print=300)              ## Définition du nombre de lignes à montrer + éviter d'aller en mode débogage si il y a une erreur.
  print(result, row.names = FALSE)                                              ## Commande pour afficher le tableau final, sans les numéros de lignes.
}