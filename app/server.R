
shinyServer(function(input, output) {
    # Input for logistic regression model to take in variables of interest to the user
    # Allows for model to change as user inputs variables
    currentModel <- reactive({
        input$response %>% paste("~", 
                                 input$base,
                                 paste(c("", input$predictor), collapse = " + ")) %>%
            as.formula %>% glm(family = binomial(link = 'logit'), data = train)
    })
    
    # Input for dataset to take in variables of interest to the user
    # Allows for probabilities and links to change as user changes model
    currentData <- reactive({
        cD <- test
        cD$prob1 <- predict(currentModel(), test, type = "response")
        cD$link1 <- predict(currentModel(), test, type = "link")
        return(cD)
    })
    
    # Output for Logistic Regression Plot
    # Because there is a major gap in probabilities less than 0.1, I subsetted them out to produce
    #   a better looking logistic regression model plot
    output$logplot <- renderPlot({
        cD <- currentData()
        cD <- cD[cD$prob1 > 0.1, ]
        ggplot(data = cD, 
               aes(x = cD$link1, 
                   y = cD$prob1, 
                   col = as.factor(cD$adopted))) +
            scale_color_manual(values = c("red", "black")) + 
            geom_point() +
            geom_rug()
    })
    
    # Output for ROC Curve w/ added accessories
    output$rocplot <- renderPlot({
        plot(roc(currentData()$adopted, currentData()$prob1, direction = "<"),
             xlab = "False Positive Rate",
             ylab = "True Positive Rate",
             col = "black", 
             lwd = 3
        )
        roc1 <- simple_roc(currentData()$adopted == "1", currentData()$link1)
        with(roc1, points(1 - FPR,
                          TPR,
                          col = 1 + labels,
                          pch = 2))
        legend("topright", 
               title = "Adopted",
               legend = c("No", "Yes"),
               pch = 2,
               col = c("red", "black"),
               bty='n',
               cex=.75)
    })
    
    # Output for AUC Calculation
    output$auc <- renderPrint({
        a <- roc(response = currentData()$adopted, predictor = currentData()$prob1, direction = "auto")
        a$auc
    })
    
    # Output for Model Calculations
    output$modelOutput <- renderPrint({
        summary(currentModel())
    })
})