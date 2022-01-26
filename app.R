#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#set input widths
wd=75
# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
    tags$style(
      HTML("
           .inline div {
                display: inline-block;
           }
           ")
    )),
    # Application title
    titlePanel("YT_Start_Stop"),
    h4("Convert YouTube share links to links with a start AND stop time"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("link","Paste YouTube Link here"),
            h4("Start Time"),
            div(class="inline",
            numericInput("start_min","(min)",value=0,width=wd),
            numericInput("start_sec","(sec)",value=0,width=wd)
            ),
            h4("End Time"),
            div(class="inline",
            numericInput("end_min","(min)",value=NA,width=wd),
            numericInput("end_sec","(sec)",value=NA,width=wd)
            )),


        # Show a plot of the generated distribution
        mainPanel(
           h2("New Link"),
            textOutput("newlink")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$newlink <- renderText({
        # browser()
        st <- input$start_min*60+input$start_sec


        endString <-
            if (is.na(input$end_min)&is.na(input$end_sec)) {
            } else{
                e_min<-if(is.na(input$end_min)){0}else{input$end_min}
                paste0("&end=",e_min*60+input$end_sec)
            }
        #extract youtube ID pattern
        base <-
            gsub("^.*(?<=\\.be/|\\.com/)([^\\?]*)",
                 "\\1",
                 input$link,
                 perl = TRUE)

        if (base == "") {
            "Paste a YouTube link at left"
        } else{
            paste0("https://www.youtube.com/embed/",
                   base,
                   "?start=",
                   st,
                   endString)
        }

    })
}

# Run the application
shinyApp(ui = ui, server = server)
