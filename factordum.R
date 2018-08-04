# factodum <- function(df, factornames) {
# 
#   for (i in seq_along(factornames)) {
# 
#     help.df <- df[ , c(factornames[i], "MERCHANDISE")]
#     model <- model_matrix(df, ~ help.df[ ,1] - 1)
#     name <- factornames[i]
# 
#     if (any(is.na(help.df))) {
# 
#       rows_missing <- dim(df)[1] - dim(model)[1]
#       lvls <- nlevels(help.df[ ,1])
#       vec <- rep(NA, lvls)
#       vecs <- rep(vec, rows_missing)
#       vec_list <- split(vecs, 1:rows_missing)
# 
# 
#       model <- model_matrix(df, ~ help.df[ ,1] - 1)
# 
#       for (i in 1:rows_missing) {
#         model <- rbind(model,
#                        vec_list[[i]])
#       }
# 
#     }
#     names(model) <- paste0(name, "_", levels(help.df[ ,1]))
#     df <- cbind(df,
#                 model)
# 
#   }
#   return(df)
# }

factodum<-function(variable,dataset){
  help.df<-dataset[,c(variable,"MERCHANDISE")]
  numboflev<-nlevels(help.df[,1])
  help.df<-model_matrix(dataset,~help.df[,1]-1)
  numbofcol<-ncol(dataset)
  dataset<-cbind(dataset,help.df)
  
  
  #rename variables
  for(i in 1:numboflev){
    colnames(dataset)[numbofcol+i] <- paste(variable,i)
  }
  dataset[,variable]<-NULL
  
  assign('dataset',dataset, envir=.GlobalEnv)
}
