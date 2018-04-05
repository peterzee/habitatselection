## Run replicates of population simulations on wavelet landscapes

source('header.R')


landscape_size_vec <- c(64)
env_vec <- c(0, 2, 4)
frag_vec <- c(0)
prop_matrix_vec <- c(0)
risk_quantile_vec <- c(0.75, 1)

popsize_vec <- c(250)
landscape_vec <- c(NA)
risk_mag_vec <- c(0, 0.4)
perception_vec <- c(0.1, 0.5)
mvt_vec <- c(0.1, 0.5)
mvt_mod_vec <- c(0.0)
mem_depth_vec <- c(10)
mem_weight_vec <- c(0.0, 0.1)
random_start_vec <- c(TRUE)

moore_depth <- 2

##################
replicates <- 2

number_rows <- (replicates * 
               (length(landscape_size_vec) *     ## landscape parameters
                  length(env_vec) * 
                  length(frag_vec) * 
                  length(prop_matrix_vec) * 
                  length(risk_quantile_vec)) * 
               (length(popsize_vec) *            ## population parameters
                  length(landscape_vec) *
                  length(risk_mag_vec) *
                  length(perception_vec) *
                  length(mvt_vec) *
                  length(mvt_mod_vec) *
                  length(mem_depth_vec) *
                  length(mem_weight_vec) *
                  length(random_start_vec)))

number_cols <- 1 + 15 + (2 * moore_depth)   ## ncolumns = id + parameters + outputs (est and p.val for each moore)
results <- array(dim = c(number_rows, number_cols)) 

colnames(results) <- c('id',
                       'replicate',
                       'landscape_size',
                       'env',
                       'frag',
                       'prop_matrix',
                       'risk_quantile',
                       'popsize',
                       'landscape',
                       'risk_mag',
                       'perception',
                       'mvt',
                       'mvt_mod',
                       'mem_depth',
                       'mem_weight',
                       'random_start',
                       'm1.coef',
                       'm1.pval',
                       'm2.coef',
                       'm2.pval')

count <- 0
##################

## loop structure (landscape features)
for ( ae in seq_along(landscape_size_vec)) {
  for ( be in seq_along(env_vec)) {
    for ( ce in seq_along(frag_vec)) {
      for ( de in seq_along(prop_matrix_vec)) {
        for ( ee in seq_along(risk_quantile_vec)) {


## generate landscape
tmp_params_landscape <- list(LANDSCAPE.SIZE = landscape_size_vec[ ae ],
                             ENV = env_vec[ be ],
                             FRAG = frag_vec[ ce ],
                             PROP.MATRIX = prop_matrix_vec[ de ],
                             RISK.QUANTILE = risk_quantile_vec[ ee ])

landscape <- do.call(generateWavelet_landscape, 
                     tmp_params_landscape)

## internal loop structure (population parameters)
for ( ab in seq_along(popsize_vec)){
  for ( bb in seq_along(landscape_vec)){
    for ( cb in seq_along(risk_mag_vec)){
      for ( db in seq_along(perception_vec)){
        for ( eb in seq_along(mvt_vec)){
          for ( fb in seq_along(mvt_mod_vec)){
            for ( gb in seq_along(mem_depth_vec)){
              for ( hb in seq_along(mem_weight_vec)){
                for ( ib in seq_along(random_start_vec)){
                  
              for ( eye in 1:replicates){
              
                count <- count + 1
  
tmp_params_landscape <- list(POP.SIZE = popsize_vec[ ab ],
                             LANDSCAPE = landscape_vec[ bb ],
                             RISK.MAG = risk_mag_vec[ cb ],
                             PERCEPTION = perception_vec[ db ],
                             MVT = mvt_vec[ eb ],
                             MVT.MOD = mvt_mod_vec[ fb ],
                             MEM.DEPTH = mem_depth_vec[ gb ],
                             MEM.WEIGHT = mem_weight_vec[ hb ],
                             RANDOM.START = random_start_vec[ ib ])

tmp_params_landscape$LANDSCAPE <- landscape
sim <- do.call(pop.habitatselection, 
               tmp_params_landscape)

## calculate moore metrics
tmp_params_moore <- list(LANDSCAPE = landscape,
                         EGG.LANDSCAPE = sim$egg.landscape,
                         MOORE.RANGE = moore_depth)

moore.data <- do.call(moore.summary,
                      tmp_params_moore)

## fit models for local and regional
tmp_outs <- rep(NA, 4)
  
  for (i in 1:moore_depth){
      qq <- as_data_frame(moore.data$big.table[,,i])
      
      blah <- qq %>% 
        filter(patch.type == 1) %>% 
        select(risk.score, n.eggs) %>% 
        lm(n.eggs ~ risk.score, data = .) %>% 
        summary()
      
        if (nrow(blah$coefficients) > 1){
          tmp_outs[1:2] <- blah$coefficients[2,1]
          tmp_outs[3:4] <- blah$coefficients[2,4]
        }
        
      }

results[count,] <- c(count,
                     eye,
                     landscape_size_vec[ ae ],
                     env_vec[ be ],
                     frag_vec[ ce ],
                     prop_matrix_vec[ de ],
                     risk_quantile_vec[ ee ],
                     popsize_vec[ ab ],
                     landscape_vec[ bb ],
                     risk_mag_vec[ cb ],
                     perception_vec[ db ],
                     mvt_vec[ eb ],
                     mvt_mod_vec[ fb ],
                     mem_depth_vec[ gb ],
                     mem_weight_vec[ hb ],
                     random_start_vec[ ib ],
                     tmp_outs)

                    ## replicates
                    print(count)
                    } 
## close loops (internal; pop params)
                }
              }
            }
          }
        }
      }
    }
  }
}
## close loops (landscape params)
        }
      }
    }
  }
}



##################

x <- as_data_frame(results)
