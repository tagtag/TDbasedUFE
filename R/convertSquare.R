#' Generate squared tensor from multiomics data
#'
#' @param Multi A list that include multiomics data
#'
#' @return A tensor computed from multiomics data
#' @export
#'
#' @examples
#' omics1 <- matrix(runif(100),10)
#' dimnames(omics1) <- list(1:10,1:10)
#' omics2 <- matrix(runif(100),10)
#' dimnames(omics2) <- dimnames(omics1)
#' Multi <- list(omics1,omics2)
#' Z <- convertSquare(Multi)
convertSquare<-function(Multi){
    Multi <- lapply(Multi,function(x){x[is.na(x)]<-0;return(x)})
    Multi <- lapply(Multi,function(x){t(x)%*%x})
    Multi <- array(unlist(Multi),unlist(c(lapply(Multi,dim)[1],length(Multi))))
    return(Multi)
}
