#' Title
#' Select singular value vectors from HOSVD
#' @param HOSVD output from HOSVD
#' @param input_all if ist is no null, no interactive mode is
#'  activated but provided values are used.
#'
#' @return Selected singular value vector IDs
#' @export
#'
#' @examples
#'Z <- PrepareSummarizedExperimentTensor(
#'sample=matrix(as.character(seq_len(6)),c(3,2)),
#' feature=as.character(seq_len(10)),
#'  value=array(runif(10*3*2),c(10,3,2)))
#'  HOSVD <- computeHosvd(Z)
#' input_all <- selectSingularValueVectorSmall(HOSVD,input_all=c(1,1))
selectSingularValueVectorSmall <- function(HOSVD,input_all=NULL){
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
                ; stopApp()
                })  
                output$plot <- renderPlot({
                    input$action
                    input$prev
                    if(length(unique(sign(HOSVD$U[[i]][,j])))==1)
                    {
                        RANGE <- range(c(0,range(HOSVD$U[[i]][,j])))
                    } else
                    {
                        RANGE<-range(HOSVD$U[[i]][,j])
                    }
                    plot(HOSVD$U[[i]][,j],type="h",ylim=RANGE,main=j)
                    abline(0,0,col=2,lty=2)
                })
            }
            app<- shinyApp(ui, server)
            runApp(app)
            input_all <- c(input_all,j)
        } else
        {
            if(length(unique(sign(HOSVD$U[[i]][,input_all[i-1]])))==1)
            {
                RANGE <- range(c(0,range(HOSVD$U[[i]][,input_all[i-1]])))
            } else
            {
                RANGE<-range(HOSVD$U[[i]][,input_all[i-1]])
            }
            plot(HOSVD$U[[i]][,input_all[i-1]],type="h",
                ylim=RANGE,main=input_all[i-1])
            abline(0,0,col=2,lty=2)
        }
    }
    return(input_all)
}

