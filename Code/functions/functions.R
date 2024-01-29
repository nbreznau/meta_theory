# function to identify and then subset models in set_matrix by their variables
subset_models <- function(set_matrix,
                          cmp_matrix,
                          keep_only = c("X1","X2","X3","X4")){
    cmp_matrix_a <- as.data.frame(cmp_matrix) %>%
        mutate(X1 = ifelse(grepl("X1", unq_nodes), 1, 0),
               X2 = ifelse(grepl("X2", unq_nodes), 1, 0),
               X3 = ifelse(grepl("X3", unq_nodes), 1, 0),
               X4 = ifelse(grepl("X4", unq_nodes), 1, 0)) %>%
        subset(select = c(model, X1, X2, X3, X4))
    X1 <- "X1"
    X2 <- "X2"
    X3 <- "X3"
    X4 <- "X4"
    if(X1 %in% keep_only){
        cmp_matrix_a <- subset(cmp_matrix_a, X1 == 1)
    }
    if(X2 %in% keep_only){
        cmp_matrix_a <- subset(cmp_matrix_a, X2 == 1)
    }
    if(X3 %in% keep_only){
        cmp_matrix_a <- subset(cmp_matrix_a, X3 == 1)
    }
    if(X4 %in% keep_only){
        cmp_matrix_a <- subset(cmp_matrix_a, X4 == 1)
    }
    if(!(X1 %in% keep_only)){
        cmp_matrix_a <- subset(cmp_matrix_a, X1 == 0)
        set_matrix <- set_matrix[,!grepl("X1", colnames(set_matrix))]
    }
    if(!(X2 %in% keep_only)){
        cmp_matrix_a <- subset(cmp_matrix_a, X2 == 0)
        set_matrix <- set_matrix[,!grepl("X2", colnames(set_matrix))]
    }
    if(!(X3 %in% keep_only)){
        cmp_matrix_a <- subset(cmp_matrix_a, X3 == 0)
        set_matrix <- set_matrix[,!grepl("X3", colnames(set_matrix))]
    }
    if(!(X4 %in% keep_only)){
        cmp_matrix_a <- subset(cmp_matrix_a, X4 == 0)
        set_matrix <- set_matrix[,!grepl("X4", colnames(set_matrix))]
    }

    sub_set <- cmp_matrix_a$model

    set_matrix <- subset(set_matrix, model %in% sub_set)

    return(set_matrix)
}
