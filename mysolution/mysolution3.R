library(shiny)
library(bslib)
library(igraph)


run_simulation <- function(g, seeds, prob_multiplier, n_iter) {
  V(g)$activated <- FALSE
  V(g)$newly_activated <- FALSE
  
  V(g)[seeds]$activated <- TRUE
  V(g)[seeds]$newly_activated <- TRUE
  
  results <- numeric(n_iter)
  results[1] <- length(seeds)
  
  for(k in 2:n_iter) {
    spreaders <- V(g)[V(g)$newly_activated == TRUE]
    V(g)$newly_activated <- FALSE # reset
    
    if(length(spreaders) == 0) {
      results[k:n_iter] <- sum(V(g)$activated)
      break
    }
    
    ie_list <- incident_edges(g, spreaders, mode="out")
    incident_edges <- do.call(c, ie_list)
    
    if(length(incident_edges) > 0) {
      targets <- head_of(g, incident_edges)
      weights <- incident_edges$weight
      
      probs <- weights * prob_multiplier
      probs[probs > 1] <- 1
      
      success <- runif(length(probs)) < probs
      candidates <- unique(targets[success])
      
      real_new_indices <- candidates[V(g)[candidates]$activated == FALSE]
      
      if(length(real_new_indices) > 0) {
        V(g)[real_new_indices]$activated <- TRUE
        V(g)[real_new_indices]$newly_activated <- TRUE
      }
    }
    results[k] <- sum(V(g)$activated)
  }
  return(results)
}

dfGraph <- read.table("https://bergplace.org/share/out.radoslaw_email_email", skip=2)
dfGraph <- dfGraph[, 1:2]

g_raw <- graph.data.frame(dfGraph, directed = TRUE)
cnt_i <- degree(g_raw, mode="out")

E(g_raw)$weight <- 1
g <- simplify(g_raw, remove.multiple = TRUE, remove.loops = TRUE, 
              edge.attr.comb = list(weight="sum"))

edge_list <- get.edgelist(g, names=TRUE)
source_nodes <- edge_list[,1]
total_counts <- cnt_i[source_nodes]
E(g)$weight <- E(g)$weight / total_counts

ui <- page_sidebar(
  title = "Rozprzestrzenianie się informacji w sieci",
  
  sidebar = sidebar(
    sliderInput("multiplier",
                "Mnożnik prawdopodobieństwa (wij):",
                min = 0.1,
                max = 2.0,
                value = 1.0,
                step = 0.1),
    
    sliderInput("iterations",
                "Liczba iteracji:",
                min = 1,
                max = 50,
                value = 10)
  ),
  
  plotOutput("distPlot")
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    n_seeds <- round(0.05 * vcount(g))
    n_sims <- 100
    steps <- input$iterations
    mult <- input$multiplier
    
    s_deg <- order(degree(g, mode="out"), decreasing=TRUE)[1:n_seeds]
    s_bet <- order(betweenness(g), decreasing=TRUE)[1:n_seeds]
    s_clo <- order(closeness(g, mode="out"), decreasing=TRUE)[1:n_seeds]
    
    s_pr  <- order(page.rank(g)$vector, decreasing=TRUE)[1:n_seeds]
    
    res_mat <- matrix(0, nrow=steps, ncol=5)
    
    # Degree
    tmp <- numeric(steps)
    for(i in 1:n_sims) tmp <- tmp + run_simulation(g, s_deg, mult, steps)
    res_mat[,1] <- tmp / n_sims
    
    # Betweenness
    tmp <- numeric(steps)
    for(i in 1:n_sims) tmp <- tmp + run_simulation(g, s_bet, mult, steps)
    res_mat[,2] <- tmp / n_sims
    
    # Closeness
    tmp <- numeric(steps)
    for(i in 1:n_sims) tmp <- tmp + run_simulation(g, s_clo, mult, steps)
    res_mat[,3] <- tmp / n_sims
    
    # Random
    tmp <- numeric(steps)
    for(i in 1:n_sims) {
      s_rnd <- sample(V(g), n_seeds)
      tmp <- tmp + run_simulation(g, s_rnd, mult, steps)
    }
    res_mat[,4] <- tmp / n_sims
    
    # PageRank
    tmp <- numeric(steps)
    for(i in 1:n_sims) tmp <- tmp + run_simulation(g, s_pr, mult, steps)
    res_mat[,5] <- tmp / n_sims
    
    matplot(res_mat, type = "l", lty = 1, lwd = 2,
            xlab = "Iteracja", ylab = "Liczba aktywowanych węzłów",
            col = c("red", "blue", "green", "black", "orange"))
    
    legend("bottomright", 
           legend = c("Out-degree", "Betweenness", "Closeness", "Random", "PageRank"),
           col = c("red", "blue", "green", "black", "orange"),
           lty = 1, lwd = 2)
  })
}

shinyApp(ui = ui, server = server)