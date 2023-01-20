#' Title
#'Select singular value vectors from HOSVD (boxplot version)
#' @param HOSVD output from HOSVD
#' @param cond Labels fo select singlar value vector number
#'
#' @return Selected singular value vector IDs
#' @export
#'
#' @examples
#'Z <- PrepareSummarizedExperimentTensor(sample=matrix(as.character(1:6),c(3,2)),
#'                                       feature=as.character(1:10),
#'                                       value=array(runif(10*3*2),c(10,3,2)))
#'                                       HOSVD <- computeHosvd(Z)
#'                                       cond <- list(0,c("A","B","C"),c("A","B"))
#'               input_all <- selectSingularValueVectorLarge(HOSVD,cond)
selectSingularValueVectorLarge <- function(HOSVD,cond,input_all=NULL){
    if (!is.null(input_all))
    {
        return(input_all)
    } else {
    for (i in 2:length(HOSVD$U))
    {
        j<-1
        while(j %in% 1:dim(HOSVD$U[[i]])[2]){
            boxplot(HOSVD$U[[i]][,j]~cond[[i]],main=j)
            abline(0,0,col=2,lty=2)
            input <- menu(c("NEXT","PREV","SELCT"))
            if (input==2){
                if (j!=1){j<-j-1}
            } else if (input==3){
                break
            } else {
                if (j<dim(HOSVD$U[[i]])[2])j<-j+1
            }
        }
        input_all <- c(input_all,j)
    }
    return(input_all)
    }
}
