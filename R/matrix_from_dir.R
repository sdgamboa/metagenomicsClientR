#' Title
#'
#' @param dir_path Character vector indicating path of the directory with tsv files.
#'
#' @return matrix
#' @importFrom readr read_tsv
#' @importFrom dplyr full_join
#' @importFrom purrr reduce
#' @export
#'
#' @examples
#' 
#' 
#' 
matrix_from_dir <- function(dir_path) {
    
    list_of_files <- list.files(dir_path)
    names_no_tsv <- gsub(".tsv$", "", list_of_files)
    list_of_tsvs <- lapply(list_of_files, function(x) readr::read_tsv(paste0(path, "/", x)))
    names(list_of_tsvs) <- names_no_tsv
    df <- purrr::reduce(list_of_tsvs, dplyr::full_join)
    mat <- as.matrix(df[,-1])
    row_names <- df[[1]]
    rownames(mat) <- row_names
    colnames(mat) <- names_no_tsv
    return(mat)
    
}