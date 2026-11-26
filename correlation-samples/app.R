library(shiny)
library(dplyr)
library(ggplot2)
library(glue)
library(stringr)

format.decimal <- function(x, digits = 2) {
  x %>%
    round(digits = digits) %>%
    format(nsmall = digits) %>%
    str_remove('0(?=\\.)') %>%
    str_trim
}

apa.pval <- function(p) {
  ifelse(
    p < .001, '< .001',
    glue ('= {format.decimal(p, digits = 3)}')
  )
}

apa.report <- function(x) {
  glue(
    'r({x$parameter}) = {format.decimal(x$estimate)}, p {apa.pval(x$p.value)}, 95% CI [{x$conf.int[1] %>% format.decimal}, {x$conf.int[2] %>% format.decimal}]'
  )
}

standardise <- function(x) {
  x <- x - mean(x)
  x / sd(x)
}

correlated.variables <- function(r, n) {

  desired.correlations <- matrix(
    c(
      1.00, r,
      r, 1.00
    ), nrow = 2, byrow = TRUE
  )
  
  d <- matrix(
    rnorm(n = n * 2), ncol = 2
  ) %*% chol(             # Multiply by Cholesky
    desired.correlations  # decomposition of desired r
  ) %>%
    as.data.frame %>%
    setNames(c('x', 'y')) %>%
    mutate(
      across(
        .cols = c(x,y),
        .fns = standardise
      )
    )
}

cor.plot <- function(r, n) {

  d <- correlated.variables(r, n)

  results <- d %>%
    cor.test(~ x + y, .)
  
  d %>%
    ggplot(
      aes(x = x, y = y)
    ) +
    geom_point() +
    theme_bw() +
    ggtitle(results %>% apa.report)
}

ui <- fluidPage(

    titlePanel('Samples of correlated random variables'),

    sidebarLayout(
        sidebarPanel(
            sliderInput('population.r',
                        'Population r:',
                        min = -.90,
                        max = .90,
                        step = .1,
                        value = 0),
            sliderInput('sample.n',
                        'Sample N:',
                        min = 0,
                        max = 1000,
                        step = 10,
                        value = 10)
        ),

        mainPanel(
           plotOutput('corPlot')
        )
    )
)

server <- function(input, output) {

    output$corPlot <- renderPlot({
      cor.plot(input$population.r, input$sample.n)
    })
}

shinyApp(ui = ui, server = server)
