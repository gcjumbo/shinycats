
shinyServer(function(input, output) {
    # Input for Shiny to take in variables of intereset to the user
    currentModel <- reactive({
        input$response %>% paste("~", 
                                 input$base,
                                 paste(c("", input$predictor), collapse = " + ")) %>%
            as.formula %>% glm(family = binomial(link = 'logit'), data = train)
    })
    
    # Output for Logistic Regression Plot
    
    # Output for ROC Curve
    
    # Output for Model Calculations
    output$modelOutput <- renderPrint({
        summary(currentModel())
    })
})