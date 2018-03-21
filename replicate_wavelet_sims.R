## Run replicates of population simulations on wavelet landscapes

source('header.R')


landscape_size_vec <- c(64)
env_vec <- c(0, 2, 4)
frag_vec <- c(0)
prop_matrix_vec <- c(0)
risk_quantile_vec <- c(0.75, 1)

popsize_vec <- c(500)
landscape_vec <- c(NA)
risk_mag_vec <- c(0, 0.4)
perception_vec <- c(0.1, 0.5)
mvt_vec <- c(0.1, 0.5)
mvt_mod_vec <- c(0.0)
mem_depth_vec <- c(10)
mem_weight_vec <- c(0.0, 0.1)
random_start_vec <- c(TRUE)

##################
replicates <- 10

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

number_cols <- 1 + 15 + 5   ## ncolumns = id + parameters + outputs
results <- array(dim = c(number_rows, number_cols)) 

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

## simulate (first, update 'landscape' parameter)

tmp_params_landscape <- list()

sim <- do.call(pop.habitatselection, 
               tmp_params_landscape)

## calculate moore metrics

## fit models for local and regional

## extract fits

## close loops (internal; pop params)
## close loops (landscape params)
}}}}}


##################