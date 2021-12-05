---
# Documentation: https://wowchemy.com/docs/managing-content/

title: "Creating Shiny apps in Rstudio"
subtitle: ""
summary: ""
authors:
- brunomesqu
tags: []
categories: []
date: 2021-11-24T20:47:51-05:00
lastmod: 2021-11-24T20:47:51-05:00
featured: false
output:
  blogdown::html_page:
    toc: false
draft: false
runtime: shiny

# Featured image
# To use, add an image named `featured.jpg/png` to your page's folder.
# Focal points: Smart, Center, TopLeft, Top, TopRight, Left, Right, BottomLeft, Bottom, BottomRight.
image:
  caption: ""
  focal_point: ""
  preview_only: false

# Projects (optional).
#   Associate this post with one or more of your projects.
#   Simply enter your project's folder or file name without extension.
#   E.g. `projects = ["internal-project"]` references `content/project/deep-learning/index.md`.
#   Otherwise, set `projects = []`.
projects: []
---

Shiny is an exciting R package that allows us to create a variety of interactive widgets and visualizations straight from Rstudio! This is without a doubt a growing environment that in the front line of a shift in the scientific community on the storytelling approaches to data visualization.

Throughout this brief tutorial, we will go over the process of creating one simple widget with Shiny. There is a gargantuan amount of content and possibilities when it comes to the Shiny package, but this tutorial will focus on the main points you need to know to get a shiny app up and running. I encourage everyone who wishes to dive deeper into the capabilities and under workings of Shiny to do so, just understand it is far beyond the scope of this introductory tutorial.

Before we look into what code goes behind a Shiny app, let's first take a look at an example of an app.

The following app aims to plot the process of fitting a regression line to your data on a simple linear regression modelling approach. The user is able to adjust the value of the intercept for the line as well as the coefficient for the slope of the single predictor used in the model, in return, the widget displays a plot corresponding to the line according to these parameters, and calculates the $R^2$ vale that corresponds to this line. The purpose of this widget is to help students visualize how a line is fitted by the least squares method, and toy around with the model by fitting it manually.


<iframe height="800" width="100%" frameborder="no" src="https://h84by1-bruno-mesquita.shinyapps.io/AppTest/"> </iframe>


Okay, now that we have seen a nice example of a Shiny app, let's look at some code! I like to use this app as an example because although it is a fairly simple app, there is still quite a bit to dissect to help understand how Shiny apps work.

There are quite a few ways to create an app in R, which can also be comprised of a single file or multiple files. For the purposes of this tutorial let's stick with single file apps in the interest of simplicity. To create a shiny app you can do so directly from Rstudio by clicking *File>New File>Shiny Web App...* Then choose the name of your app, if it is a single/multiple file app, and which directory to save in. Pretty simple so far!

Now let's move on to actually going over how to build our Shiny app.

First of all, these are the packages we will be using in this tutorial, so let's call them.

```{r}
library(shiny) # Can't make a shiny app without it!
library(shinythemes) # This gives us some nicer looking presets
library(modelr) # This is a package that aids with modeling, but it also comes with some data sets, which we will be using here in our widget.
library(tidyverse) 
library(ggplot2) 
library(ggthemes)
```

Alright, so now let's get into the app itself. 

Any Shiny app is comprised of two crucial code components. An 'UI', and a 'Server' component. When running the app, these two must be called in order to build a properly functioning application. In case you are curious, these are also related to single/multiple file apps, as in a single file you have both components on the same script, while in a multiple file app you have a script for the UI, and one for the server. Let's briefly go over what each of these components are actually doing in an app.

The User Interface, abbreviated to UI: It is the forefront of your app. It controls what is presented to the user, how it looks, and the avenues through which the app will capture user input. This is pretty much a very fancy object in R.

The server function: This controls how your app works. Things in the server function are not directly shown to the user unless specified in the UI, but this is where the user input will be converted into something to be displayed back. As the name implies, this is a function.

It is important to understand that a Shiny app works due to the interplay of these two components. A server function is usually 'reactive', and will be updated when the contents of the 'input' object (manipulated by the app user) change.

Let's go through the UI object in that linear regression app we have seen before, pay attention to the comments in the code.

```
# Define UI for application
ui <- fluidPage(theme = shinytheme("sandstone"), # This is the function for creating our layout, with a theme preset from ::shinythemes

    # Application title
    titlePanel("Fitting a Line to Your Data"), # This is pretty self-explanatory, but notice how we are building our app's visuals 'block by block'

    # Sidebar with a slider input for the coefficients. 
    sidebarLayout(
        sidebarPanel(
            sliderInput("slope",   # This is where we will capture the user input. Luckily, Shiny already comes with robust functions for sliders
                        "Slope:",  # The previous 'slope' was the name of the input object, this one, is the title of the slider on the UI
                        min = 0,
                        max = 5,
                        step = 0.01,
                        value = 4), # And then we have the numerical values for the sldier such as the range and step of selection, this final value is the default one that appears when the user first starts the app.
            
            sliderInput("intercept","Intercept:", # We do pretty much the same thing for our second input slider, for the intercept.
                        min=0,
                        max=8,
                        step=0.01,
                        value = 2)
        ),

        # Show the plot we will generate later on in the server function
        mainPanel(
           plotOutput("plot1")
        )
    )
)
```

As you can see, this portion of the code is mainly setting up the visuals of our app, as well as providing the means through which we will collect user input. There are many different functions in the shiny package, it would be unreasonable to cover all of them here, but the Shiny website is very robust, with many tutorials and examples of apps in a diverse gallery.

The avenues for collection of the input that we have here in our UI will then allow the user information to be properly collected and manipulated in the server function, which we will look at now: 


```
# Define server logic required to plot our regression model
server <- function(input, output) {  # It is a function that, like any other, will take an input, and return an output
    
    idf <- reactive({ # We make a data frame that will be used to store the input values from the sliders. Since this data frame is set as 'reactive' it will automatically update itself once the user input changes, neat!
        data.frame(
            slope = as.numeric(input$slope), # We can call object from the user input much like we do for lists. (from the object input, we get 'slope')
            intercept = as.numeric(input$intercept) # And here again we get the values for 'slope' in the input
        )
    })
    
# Now let's prepare our output!
    output$plot1 <- renderPlot({ # Our final result will be a plot
        
        df <- as.data.frame(idf()) # We make sure our input is being read as a traditional data frame for easier subsetting 
        
        dataplot <- as.data.frame(sim1) # This will be the data we will be using for our plot, it comes from the ::modelr package
        
        yhat <- df$intercept + (df$slope*dataplot$x) # This is the GLM equation, we use the variables from the input here to calculate our predicted values of the regression line
        
        dataplot <- cbind(dataplot,yhat) # We add the predicted values for y for any given value of x to our data plot
      
      # Here we have some further calculations that are the bread and butter of the GLM, so that we can achieve an Rsqrd value
        
        diff <- dataplot$y - yhat 
        Varfit <- (mean(diff^2))
        Varmean <- mean((sim1$y-mean(sim1$y))^2)
        Rsqrd <- as.numeric((Varmean-Varfit)/Varmean)
        
      # The subtitle for our plot
        subtitle <- sprintf("Rsqrd: %g",round(Rsqrd, digits=3))
        
      # A data frame witht the values used to plot the regression line
        lineplot <- as.data.frame(cbind(df,Rsqrd,subtitle))
     
      # And here we have a very straightforward approach to plotting using ggplot2 as usual
      
        ggplot(dataplot, aes(x = x,y = y)) + 
            geom_point(size = 2) +
            geom_abline(data = lineplot,size = 1, aes(intercept = intercept, slope = slope)) +
            geom_segment(aes(xend=x),yend=yhat,color="darkred")+
            scale_color_manual(values = c(resid = "darkred"), labels = c(resid = "residuals"))+
            theme_bw(base_size=16)+
            labs(title = "Regression Line on Scatterplot"
                 ,subtitle = subtitle,
                 x = 'Predictor',
                 y = 'Variable')
            
       
    })
}
```
As you can see, after the initial scripts for setting up the server function and capturing the user input, it's really just a matter of doing what we usually do in order to make plots in R, with he exception of a bit more busywork in setting up the linear modelling by hand, but that is just a matter of using the tried and true formulas of this model.

Once we have both the UI object, and the server function, it is just a matter of running the application with the shinyApp function:

```{r}
# Run the application 
shinyApp(ui = ui, server = server)
```
Do note that doing so will simply run the application *locally*!Your app is still not online and can't be easily shared. To do so, you will have to host it somewhere. A very accessible solution to this problem is to publish your app through shinyapps.io, Rstudio's very own hosting service for shiny apps. This can be done directly through Rstudio by clicking the *publish* icon on the top right corner of the pop-up screen of your app.

And that's all! I understand this was a fairly brief introduction to Shiny apps, but I wanted to keep it simple. Hopefully throughout this tutorial you were able to get a general understanding of building your very own widgets in R, maybe you already have some ideas of what you'd like to build!

{{< figure library="true" src="tamatoa.jpg" >}}
