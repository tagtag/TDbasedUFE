#' Title
#' Show selecte featues as Table
#' @param Z Tensor of features
#' @param index List that includes selected features and P-values
#'
#' @return Table list of selected features
#' @export
#'
#' @examples
#' set.seed(2)
#' HOSVD <- hosvd(as.tensor(array(runif(10000*3*3),c(10000,3,3))),c(10,3,3))
#' input_all <- c(2,2)
#' index <- selectFeature(HOSVD,input_all,de=0.01,p0=0.01)
#' index$index[1:100] <- T
#' Z <- PrepareSummarizedExperimentTensor(sample=matrix(as.character(1:9),c(3,3)),
#'                                       feature=as.character(1:10000),
#'                                       value=array(runif(10000*3*3),c(10,3,3)))
#' head(tableFeatures(Z,index))
tableFeatures <- function(Z,index){
    TABLE <- data.frame(Z@feature[index$index],index$p.value[index$index],
                        p.adjust(index$p.value,"BH")[index$index])
    colnames(TABLE) <- c("Feature","p value","adjusted p value")
    TABLE <- TABLE[order(TABLE[,2]),]
    return(TABLE)
}
