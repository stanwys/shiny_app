library(shiny)
library(caret)
library(stats)

genders <- c("female","male")
class_tickets <- c(1,2,3)
survive_model <- readRDS("./rf.rds")
fare_model <- readRDS("./cart.rds")
imageNames <- c("100.png","90.png","70.png","50.png","40.png","30.png")
img_folder <- "./img/"

ui <- fluidPage(
   
   titlePanel("Titanic prediction"),
   
   sidebarLayout(
      sidebarPanel(
         textInput("Fname", "First Name", value = "", 
                   width = NULL, placeholder = NULL),
         textInput("Lname", "Last name", value = "", 
                   width = NULL, placeholder = NULL ),
         numericInput("Age", "Age", value = 0, min = 0,width = NULL),
         
         selectInput(inputId = "Pclass",
                     label = "Choose ticket class",
                     choices = class_tickets,
                     selected = 3),
      
         selectInput(inputId = "Sex",
                    label = "Choose gender",
                    choices = genders,
                    selected = "female"),
         actionButton("predictButton", "Predict fare and survival chances", width = '100%')
      ),
      
      mainPanel(
        htmlOutput("survival"),
        htmlOutput("ticket"),
        htmlOutput("survival_description"),
        imageOutput("ship_image")
      )
      
   )
)

server <- function(input, output, session) {
   
  output$survival <- renderText({
    paste("<h4 style=\"display: inline\">Predicted survival chance: </h4>")
  })
  
  output$ticket <- renderText({
    paste("<h4 style=\"display: inline\">Predicted ticket price: </h4>")
  })
  
  
  observeEvent(input$predictButton,{
    
    if (is.numeric(input$Age) && input$Age >= 0){
    
      if (as.character(input$Sex)=="male"){
        is_male <- c(1)
        is_female <- c(0)
      }
      else{
        is_male <- c(0)
        is_female <- c(1)
      }
      Sex <- c(input$Sex)
      Age <- c(input$Age)
      Pclass <- c(as.numeric(input$Pclass))
      
      prob_data <- data.frame(Sex, Age, Pclass)
      reg_data <- data.frame(Age, Pclass, is_male, is_female)
      
      surival_chance <- stats::predict(survive_model, newdata = prob_data, type = "prob")
      predicted_fare <- stats::predict(fare_model, newdata = reg_data)
      
      survData <- calcSurvivalData(surival_chance$X1)
      greeting <- calcGreeting(input$Sex, input$Age, input$Fname, input$Lname)
      
      output$survival <- renderText({
        paste("<h4 style=\"display: inline\">Predicted survival chance: </h4><b style=\"color:", survData[1] ,"\">", 
              surival_chance$X1 * 100, "% </b>", sep="")
      })

      output$ticket <- renderText({
        p1 <- paste("<h4 style=\"display: inline\">Predicted ticket price: </h4>")
        p2 <- paste("<b>", format( round(predicted_fare,2) , nsmall = 2)," pounds","</b>",sep="")
        paste(p1,p2)
      })
    
      output$survival_description <- renderText({
        paste("<p>",greeting, survData[2], "</p>",sep=" ")
      })
      
      output$ship_image <- renderImage({
        list(
          src = paste( img_folder, survData[3], sep=""),
          width = 400,
          height = 300,
          alt = paste("")
        )
        
      }, deleteFile = FALSE)
      
    }
    else{
      
      showModal(modalDialog(
        title = "Age error",
        "Age must be a non-negative number!"
      ))
      
    }
    
  })
  
  calcGreeting <- function(gender, age, Fname, Lname){
    
    if (age >= 18){
      if(as.character(gender)=="male"){
        greet <- paste("Mr. ",Lname,",",sep="")
      }
      else{
        greet <- paste("Ms. ",Lname,",",sep="")
      }
    } 
    else{
      greet <- paste(Fname, ",", sep = "") 
    }
    
    greet  
  }

  calcSurvivalData <- function(survival_chance){
    color <- "#FF0000"
    description <- "it might have been a one way ticket for you."
    img_name <- imageNames[length(imageNames)]
    
    if(survival_chance == 1.0){
      color <- "#00FF00"
      description <- "you would have been sooo lucky ;)"
      img_name <- imageNames[1]
    }
    else if(survival_chance >= 0.9){
      color <- "#80FA00"
      description <- "there might have been some inconveniences during voyage."
      img_name <- imageNames[2]
    }
    else if(survival_chance >= 0.7){
      color <- "#CCFF00"
      description <- "no risk, no fun :)"
      img_name <- imageNames[3]
    }
    else if(survival_chance >= 0.5){
      color <- "#FFFF00"
      description <- "you might have considered tossing a coin..."
      img_name <- imageNames[4]
    }
    else if(survival_chance >= 0.4){
      color <- "#FFCC00"
      description <- "would you have been sure about getting on board?"
      img_name <- imageNames[5]
    }
    return(list(color,description,img_name))
    
  }
 
}


# Run the application 
shinyApp(ui = ui, server = server)

