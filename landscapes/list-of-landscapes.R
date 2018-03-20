# Generate a large number of landscapes across parameter values
source('header.R')

landscape_size_vec <- c(64, 128)
env_vec <- c(-Inf, 0:6)
frag_vec <- c(-Inf, 0:6)
prop_matrix_vec <- c(0, 0.25, 0.5)
risk_quantile_vec <- c(0.5, 0.75, 1)


list_of_landscapes <- vector(mode = "list", length = (length(landscape_size_vec) *
                                                          length(env_vec) *
                                                          length(frag_vec) *
                                                          length(prop_matrix_vec) *
                                                          length(risk_quantile_vec)))


meta_landscape_list <- array(dim = c((length(landscape_size_vec) *
                       length(env_vec) *
                       length(frag_vec) *
                       length(prop_matrix_vec) *
                       length(risk_quantile_vec)),
                       6))
                   

colnames(meta_landscape_list) <- c('id',
                                   'landscape_size', 
                                   'env', 
                                   'frag', 
                                   'prop_matrix', 
                                   'risk_quantile') 

count <- 0

for ( eye in seq_along(landscape_size_vec) ){
  for ( jay in seq_along(env_vec) ){
    for ( kay in seq_along(frag_vec) ){
      for ( ell in seq_along(prop_matrix_vec) ){
        for ( emm in seq_along(risk_quantile_vec) ){

count <- count + 1
                    
tmp_params <- list(LANDSCAPE.SIZE = landscape_size_vec[ eye ],
                   ENV = env_vec[ jay ],
                   FRAG = frag_vec[ kay ],
                   PROP.MATRIX = prop_matrix_vec[ ell ],
                   RISK.QUANTILE = risk_quantile_vec[ emm ])


x <- do.call(generateWavelet_landscape, 
             tmp_params)


meta_landscape_list[count,] <- c(count,
                                landscape_size_vec[ eye ],
                                env_vec[ jay ],
                                frag_vec[ kay ],
                                prop_matrix_vec[ ell ],
                                risk_quantile_vec[ emm ])
list_of_landscapes[[count]] <- x

        }
      }
    }
  }
}
  

# write.csv(meta_landscape_list, str_c(path_landscapes, "/meta_landscape_list.RData"))
# write.csv(list_of_landscapes, str_c(path_landscapes, "/list_of_landscapes.RData"))
