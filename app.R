library("ggplot2")
library("ggpubr")
library("heyshiny")
library("httr")
library("jpeg")

ui <- fluidPage(
  useHeyshiny("en"),
  titlePanel("Random Sampling"),
  wellPanel(
    h4("This app works correctly on Google Chrome browser (enable microphone)"),
    h4("Voice commands, say:"),
    HTML(
      '&ensp; * <i>Distribution + n</i>: shows the density plot of sampling n values from the chosen distribution. For example, say <i>binomial 100</i> <br/>',
      '&ensp;&ensp; <i>Distribution</i> can be one of Hypergeometric, Logistic, Beta, Uniform, Chi-Squared, Multinomial, Normal, Weibull, Negative Binomial, Binomial, Log Normal, Cauchy, Signed Wilcoxon, Wilcoxon, F, Student t, Poisson, Gamma, Geometric, Exponential <br/>',
      '&ensp; * <i>Color</i>: changes the color of the points. <br/>',
      '&ensp; * If you feel bored, say it! <i>boring!</i> <br/>',
      '&ensp; * Or try the <i>mustache</i> feature! <br/>'
    ),
    
    # distribution inputs
    speechInput("Hypergeometric", "Hypergeometric :n_samp"),
    speechInput("Logistic", "Logistic :n_samp"),
    speechInput("Beta", "Beta :n_samp"),
    speechInput("Uniform", "Uniform :n_samp"),
    speechInput("Chi_Squared", "Chi-Squared :n_samp"),
    speechInput("Multinomial", "Multinomial :n_samp"),
    speechInput("Normal", "Normal :n_samp"),
    speechInput("Weibull", "Weibull :n_samp"),
    speechInput("Negative_Binomial", "Negative Binomial :n_samp"),
    speechInput("Binomial", "Binomial :n_samp"),
    speechInput("Log_Normal", "Log Normal :n_samp"),
    speechInput("Cauchy", "Cauchy :n_samp"),
    speechInput("Signed_Wilcoxon", "Signed Wilcoxon :n_samp"),
    speechInput("Wilcoxon", "Wilcoxon :n_samp"),
    speechInput("F", "F :n_samp"),
    speechInput("Student_t", "Student t :n_samp"),
    speechInput("Poisson", "Poisson :n_samp"),
    speechInput("Gamma", "Gamma :n_samp"),
    speechInput("Geometric", "Geometric :n_samp"),
    speechInput("Exponential", "Exponential :n_samp"),

    speechInput("color", "color"),
    speechInput("boring", "boring"),
    speechInput("mustache", "mustache"),
    
    plotOutput("plot"),
    
    HTML(
      'Do you also want voice recognition in you `shiny` apps? Check out the <a href="https://github.com/jcrodriguez1989/heyshiny" target="_blank">`heyshiny`</a> R package. <br/>',
      'This `shiny` app was inspired by <a href="https://yihui.shinyapps.io/voice/" target="_blank"> Yihui</a>, whom I greatly admire.'
    )
  )
)

server <- function(input, output, session) {
  # needed values for some random functions
  m <- n <- k <- shape1 <- shape2 <- df <- size <- shape <- df1 <- df2 <-
    ncp <- lambda <- Sigma <- .8818
  size <- 8
  prob <- .5

  # current selected random function
  rand_fun <- reactiveVal(function() rnorm(30))

  # string to number
  str_to_num <- function(n_char) {
    res <- suppressWarnings(as.numeric(n_char))
    # not recognized or invalid number
    if (is.na(res)) {
      res <- 30 # default when not recognized
    }
    res
  }

  # select function and number of samples
  observeEvent(input$Hypergeometric, {
    rand_fun(function() rhyper(str_to_num(input$Hypergeometric), m, n, k))
  })
  observeEvent(input$Logistic, {
    rand_fun(function() rlogis(str_to_num(input$Logistic)))
  })
  observeEvent(input$Beta, {
    rand_fun(function() rbeta(str_to_num(input$Beta), shape1, shape2))
  })
  observeEvent(input$Uniform, {
    rand_fun(function() runif(str_to_num(input$Uniform)))
  })
  observeEvent(input$Chi_Squared, {
    rand_fun(function() rchisq(str_to_num(input$Chi_Squared), df))
  })
  observeEvent(input$Multinomial, {
    rand_fun(function() rmultinom(str_to_num(input$Multinomial), 20, c(1, 3, 6, 10))[1, ])
  })
  observeEvent(input$Normal, {
    rand_fun(function() rnorm(str_to_num(input$Normal)))
  })
  observeEvent(input$Weibull, {
    rand_fun(function() rweibull(str_to_num(input$Weibull), shape))
  })
  observeEvent(input$Negative_Binomial, {
    rand_fun(function() rnbinom(str_to_num(input$Negative_Binomial), size, prob))
  })
  observeEvent(input$Binomial, {
    rand_fun(function() rbinom(str_to_num(input$Binomial), size, prob))
  })
  observeEvent(input$Log_Normal, {
    rand_fun(function() rlnorm(str_to_num(input$Log_Normal)))
  })
  observeEvent(input$Cauchy, {
    rand_fun(function() rcauchy(str_to_num(input$Cauchy)))
  })
  observeEvent(input$Signed_Wilcoxon, {
    rand_fun(function() rsignrank(str_to_num(input$Signed_Wilcoxon), n))
  })
  observeEvent(input$Wilcoxon, {
    rand_fun(function() rwilcox(str_to_num(input$Wilcoxon), m, n))
  })
  observeEvent(input$F, {
    rand_fun(function() rf(str_to_num(input$F), df1, df2, ncp))
  })
  observeEvent(input$Student_t, {
    rand_fun(function() rt(str_to_num(input$Student_t), df, ncp))
  })
  observeEvent(input$Poisson, {
    rand_fun(function() rpois(str_to_num(input$Poisson), lambda))
  })
  observeEvent(input$Gamma, {
    rand_fun(function() rgamma(str_to_num(input$Gamma), shape))
  })
  observeEvent(input$Geometric, {
    rand_fun(function() rgeom(str_to_num(input$Geometric), prob))
  })
  observeEvent(input$Exponential, {
    rand_fun(function() rexp(str_to_num(input$Exponential)))
  })

  # select points random color
  color <- reactive({
    input$color
    sample(rainbow(8818), 1)
  })

  # calculate random points
  points <- reactive({
    data.frame(
      values = rand_fun()(),
      y = 0, # to plot points
      color = NA
    )
  })

  # set Karl image
  karl <- reactiveVal(FALSE)
  observeEvent(input$mustache, karl(!karl()))
  karl_img <- readJPEG("moustache.jpg")
  
  output$plot <- renderPlot({
    act_points <- points()
    act_points$color <- color()
    ggplot(act_points, aes(color = color)) +
      (if (karl()) background_image(karl_img)) +
      geom_point(aes(x = values, y = y)) +
      geom_density(aes(x = values)) +
      scale_colour_identity() + # use the color name
      theme(legend.position = "none") +
      ggtitle(paste0("n = ", nrow(act_points)))
  })

  observeEvent(input$boring, {
    search_raw <- try({
      GET(
        url = "http://api.giphy.com/",
        path = "/v1/gifs/trending",
        query = list(api_key = "GAUeYalixQovJJGRACGEaKRpNunOoH1q")
      )
    })

    gif <- ""
    if (!inherits(search_raw, "try-error") && search_raw$status_code == 200) {
      meme <- sample(content(search_raw)$data, 1)[[1]]
      gif <- paste0(
        '<center><img src="',
        meme$images$fixed_height$url,
        '"></center>'
      )
    }
    showModal(modalDialog(
      HTML(gif),
      title = "Not boring!",
      size = "m",
      easyClose = TRUE
    ))
  })
}

shinyApp(ui, server)
