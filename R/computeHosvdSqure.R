#' Title
#' Compute higher order singular value decomposition from the
#'  tensor generated from squared matrix
#' @param Z A tensor including sample names, feature values,
#'  associated with featureRange and sample properties
#' @param dims dimensions to be compued by HOSVD
#' @param scale  If value is scaled
#'
#' @return List that includes output from HOSVD
#' @export
#'
#' @examples
#' omics1 <- matrix(runif(100),10)
#' dimnames(omics1) <- list(seq_len(10),seq_len(10))
#' omics2 <- matrix(runif(100),10)
#' dimnames(omics2) <- dimnames(omics1)
#' Multi <- list(omics1,omics2)
#' Z <- PrepareSummarizedExperimentTensorSquare(
#'             sample=matrix(colnames(omics1),1),
#'             feature=list(omics1=rownames(omics1),
#'             omics2=rownames(omics2)),
#'             value=convertSquare(Multi),
#'             sampleData=list(NA))
#'             HOSVD <- computeHosvdSqure(Z)
computeHosvdSqure <-function(Z,dims=unlist(lapply(dim(Z@value),
    function(x){min(10,x)})),scale=TRUE)
{
    if (scale)
    {
        mean <- apply(Z@value,length(dim(Z@value)),mean)
        Z@value <- aperm(aperm(Z@value,c(length(dim(Z@value)),
            c(seq_len(length(dim(Z@value))-1))))/mean,
            c(c(2:length(dim(Z@value)),1)))
    }
    hosvd(as.tensor(Z@value),dims)
}
