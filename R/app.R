#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Population growth: births and deaths"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          sliderInput("N0", label = "Initial Population size:",
                      min = 10, max = 100, value = 50, step = 10),
          
          sliderInput("S", label = "Survivorship:",
                      min = 0.2, max = 0.9, value = .8, step = 0.1),
          
          sliderInput("b", label = "Birth rate:",
                      min = 0.2, max = 0.9, value = .2, step = 0.1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
       nSteps<-50
       time<-seq(from=1, to=nSteps, by=1)
       
       Nvec<-c(input$N0, rep(NA, times=nSteps-1))
       
       for (i in 2:nSteps){
           Nvec[i]<-Nvec[i-1]*(input$S+input$b)
       }
       
       #   plot(Nvec~time, ylab="Number of individuals (N)", xlab="Time (t)",
       #        ylim=c(input$N0,max(Nvec)), type="o")
       plot_df <- data.frame(N=Nvec,Time=time)
       ggplot(plot_df, aes(x=Time, y=N))+
           geom_line(color="#BB5635")+
           geom_point(color="#BB5635", size=3)+
           ylab("Population size (N)")
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

