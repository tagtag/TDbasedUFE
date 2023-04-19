#' Title
#'Select singular value vectors from HOSVD (boxplot version)
#' @param HOSVD output from HOSVD
#' @param cond Labels to select singular value vector number
#' @param input_all if list is not null, no interactive mode is
#' activated but provided values are used.
#'
#' @return Selected singular value vector IDs
#' @export
#'
#' @examples
#'Z <- PrepareSummarizedExperimentTensor(
#'sample=matrix(as.character(seq_len(6)),c(3,2)),
#' feature=as.character(seq_len(10)),
#'  value=array(runif(10*3*2),c(10,3,2)))
#' HOSVD <- computeHosvd(Z)
#'  cond <- list(0,c("A","B","C"),c("A","B"))
#'  input_all <- selectSingularValueVectorLarge(HOSVD,cond,input_all=c(1,1))
selectSingularValueVectorLarge <- function(HOSVD,cond,input_all=NULL){
    interact<-FALSE
    if (is.null(input_all))interact <- TRUE
    for (i in 2:length(HOSVD$U))
    {
        j<-1
        if (interact)
        {
            ui <- fluidPage(
                sidebarLayout(
                    sidebarPanel(
                        actionButton(inputId="action", label="Next"),
                        actionButton(inputId="prev",  label="Prev"), 
                        actionButton(inputId="select", label="Select")),
                    mainPanel(
                        plotOutput("plot")
                    )
                )
            )
            server <- function(input, output){
                observeEvent(input$action, {
                    if (j<dim(HOSVD$U[[i]])[2]) j<<-j+1
                })
                observeEvent(input$prev, {
                    if (j!=1){j<<-j-1}
                })  
                observeEvent(input$select, {
                     stopApp()
                })  
                output$plot <- renderPlot({
                    input$action
                    input$prev
                    par(mfrow=c(length(cond),1))
                    par(mai=c(0.3,0.2,0.2,0.2))
                    boxplot(HOSVD$U[[i]][,j]~cond[[i]],main=j)
                    abline(0,0,col=2,lty=2)
                    par(mfrow=c(1,1))
                })
            }
            app<- shinyApp(ui, server)
            runApp(app)
            input_all <- c(input_all,j)
        } else {
            boxplot(HOSVD$U[[i]][,input_all[i-1]]~cond[[i]],main=input_all[i-1])
            abline(0,0,col=2,lty=2)
        }
    }
    return(input_all)
}
