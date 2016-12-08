# funcs.R

to_compare <- function(network1, network2, combine = T){
    
    # to_compare() adds structural zeroes for two-mode networks based on what is not included in the two networks being compared
    # takes two arguments, network1 and network2, both two-mode network matrices, i.e. to_compare(network1, network2)
    
    print(paste0("Processing network1 with ", nrow(network1), " rows", " and ", ncol(network1), " columns."))
    print(paste0("Processing network2 with ", nrow(network2), " rows", " and ", ncol(network2), " columns."))
    
    print(paste0("### PROCESSING ###"))
    
    # for mode 1 (orgs)
    
    add_to_row_network1 <- network2[, 1][!(network2[, 1] %in% network1[, 1])]
    add_to_row_network2 <- network1[, 1][!(network1[, 1] %in% network2[, 1])]
    
    row_vector_network1 <- rep(0, ncol(network1))
    row_vector_network2 <- rep(0, ncol(network2))
    
    first_row_network1 <- nrow(network1)
    first_row_network2 <- nrow(network2)
    
    for (i in 1:length(add_to_row_network1)){
        network1 <- rbind(network1, row_vector_network1)
        network1[first_row_network1 + i, 1] <- add_to_row_network1[i]
    }
    
    
    for (i in 1:length(add_to_row_network2)){
        network2 <- rbind(network2, row_vector_network2)
        network2[first_row_network2 + i, 1] <- add_to_row_network2[i]
    }
    
    # for mode 2 (prefs)
    
    add_to_col_network1 <- names(network2)[!(names(network2) %in% names(network1))]
    add_to_col_network2 <- names(network1)[!(names(network1) %in% names(network2))]
    
    col_vector_network1 <- rep(0, nrow(network1))
    col_vector_network2 <- rep(0, nrow(network2))
    
    first_col_network1 <- ncol(network1)
    first_col_network2 <- ncol(network2)
    
    for (i in 1:length(add_to_col_network1)){
        network1 <- cbind(network1, col_vector_network1)
        names(network1)[first_col_network1 + i] <- add_to_col_network1[i]
    }
    
    for (i in 1:length(add_to_col_network2)){
        network2 <- cbind(network2, col_vector_network2)
        names(network2)[first_col_network2 + i] <- add_to_col_network2[i]
    }
    
    row_target_network2 <- network1[, 1]
    network2 <- network2[match(row_target_network2, network2[, 1]),]
    
    col_order_network1 <- c(names(network1)[1], sort(names(network1)[2:length(names(network1))]))
    network1 <- network1[, col_order_network1]
    row.names(network1) <- network1[, 1]
    network1 <- network1[, -1]
    
    col_order_network2 <- c(names(network2)[1], sort(names(network2)[2:length(names(network2))]))
    network2 <- network2[, col_order_network2]
    row.names(network2) <- network2[, 1]
    network2 <- network2[, -1]
    

    print(paste0("Processed network1 with ", nrow(network1), " rows", " and ", ncol(network1), " columns."))
    print(paste0("Processed network2 with ", nrow(network2), " rows", " and ", ncol(network2), " columns."))
    
    print(paste0("For the object the output is saved to, use [[1]] and [[2]] to index processed networks."))
    
    if (combine == T){
        print(paste0("For combined networks, for the object the output is saved to use [[3]]"))
        out[[3]] <- network1 + network2
        out <- list(network1, network2, combined_network)
    } else{
        out <- list(network1, network2)
    }
    return(out)
    
}