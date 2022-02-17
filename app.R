#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny); library(rclipboard)

#set input widths
wd=75
# Define UI for application that draws a histogram
ui <- fluidPage(
    rclipboardSetup(),
    tags$head(
    tags$style(
      HTML("
           .inline div {
                display: inline-block;
           }
           .results *{
                margin-top: 1rem;
                margin-bottom: 1rem;
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
            numericInput("start_min","(min)",value=0,width=wd,min=0,step=1),
            numericInput("start_sec","(sec)",value=0,width=wd,min=0,step=1)
            ),
            h4("End Time"),
            div(class="inline",
            numericInput("end_min","(min)",value=NA,width=wd,min=0,step=1),
            numericInput("end_sec","(sec)",value=NA,width=wd,min=0,step=1)
            )),


        # Show a plot of the generated distribution
        mainPanel(
           h2("New Link"),
            textOutput("newlink"),
           uiOutput("clip")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  vals<-reactiveValues()
  observe({
    if (!is.na(input$end_min) & is.na(input$end_sec)) {
    shiny::updateNumericInput(session, "end_sec", value = 0)
    }
    }
  )

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
        base_sans_watch <-gsub("watch?v=","",input$link,fixed=TRUE)
        base<-
            gsub("^.*(?<=\\.be\\/|\\.com\\/|embed\\/)([^\\?]*)(\\?.*)?$",
                 "\\1",
                 base_sans_watch,
                 perl = TRUE)

        if (base == "") {
            out<-"Paste a YouTube link at left"
        } else{
           out<- paste0("https://www.youtube.com/embed/",
                   base,
                   "?start=",
                   st,
                   endString)
        }
        vals$LINK<-out
        out

    })

    # Add clipboard buttons
  output$clip <- renderUI({
    if(!grepl("www",vals$LINK,fixed=TRUE)){

    }else{
    tagList(
    div(class="results",
    rclipButton("clipbtn", "Copy Link", vals$LINK, icon=icon("clipboard")),
    a(div(icon("link"),"Test Link"),href=vals$LINK,target="_blank"),
    ))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
