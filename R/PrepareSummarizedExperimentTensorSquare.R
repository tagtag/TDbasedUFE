#' Title
#' Generate feature values formatted as a tensor format from Squared matrix
#' @param sample Sample names
#' @param feature Feature id names
#' @param value Squared Feature values
#' @param featureRange Genomic coordinate attributed to feature id (if any)
#' @param sampleData Sample property (labels etc)
#'
#' @return A tensor including sample names, feature values, associated with featureRange and sample properties
#' @export
#'
#' @examples
#' omics1 <- matrix(runif(100),10)
#' dimnames(omics1) <- list(1:10,1:10)
#' omics2 <- matrix(runif(100),10)
#' dimnames(omics2) <- dimnames(omics1)
#' Multi <- list(omics1,omics2)
#' Z <- PrepareSummarizedExperimentTensorSquare(sample=matrix(colnames(omics1),1),
#'             feature=list(omics1=rownames(omics1),
#'             omics2=rownames(omics2)),
#'             value=convertSquare(Multi),
#'             sampleData=list(NA))
PrepareSummarizedExperimentTensorSquare <- function(sample=list(NULL),feature,value,featureRange=GRanges(NULL),sampleData=list(NULL))
{
    new("SummarizedExperimentTensorSquare",sample=sample,feature=feature,value=value,
        featureRange=featureRange,sampleData=sampleData)
}

