# source('generateLandscape_function.R')
# 
# world <- generateLandscape(20, 200, 50)
# A <- world$landscape

## egg landscape
A
E <- egg.landscape


    ROW <- 5
    COL <- 6
    
    range <- 2
    
    row.range <- (ROW - range):(ROW + range)
    row.range[row.range <= 0] <- row.range[row.range <= 0] - 1
    row.range[row.range > (nrow(A))] <- row.range[row.range > (nrow(A))] + 1
    
    col.range <- (COL - range):(COL + range)
    col.range[col.range <= 0] <- col.range[col.range <= 0] - 1
    col.range[col.range > (ncol(A))] <- col.range[col.range > (ncol(A))] + 1
    
    v.range <- row.range %% (nrow(A)+1)
    h.range <- col.range %% (ncol(A)+1)
    
    moore <- A[v.range, h.range]
    moore[range+1, range+1] <- NA
    
    moore.empty <- sum(moore == 0, na.rm = TRUE)
    moore.safe <- sum(moore == 1, na.rm = TRUE)
    moore.risk <- sum(moore == 2, na.rm = TRUE)
    
    risk.score <- moore.risk / (length(moore)-1)
    
    #####
    egg.moore <- E[v.range, h.range]
    
    
    egg.moore.empty <- egg.moore[moore == 0]
    egg.moore.safe <- egg.moore[moore == 1]
    egg.moore.risk <- egg.moore[moore == 2]
    
    sum(egg.moore.safe)
    sum(egg.moore.risk)
    