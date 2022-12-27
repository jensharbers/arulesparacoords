
library(arules)
library(arulesViz)
library(reshape2)
library(ggpubr)
library(plotly)
library(plyr)
library(ggplot2)
library(igraph)


# more than half of the code is taken from https://github.com/mhahsler/arulesViz/blob/master/R/ruleExplorer.R, copied at 12.11.2022 15:19


example("eclat")
rules <- rules[2412:2438]
rules@quality$ChiSquaredp <-  interestMeasure(rules,measure = "ChiSquared", significance=TRUE)
rules@quality$ChiSquaredp_holm <- p.adjust(rules@quality$ChiSquaredp, method="holm")

# plot(rules, shading = "lift",measure="support", method="paracoord", n=50)


  #' A parallel coordinates diagram for ggplot2 and plotly
  #'
  #' @param ruleset an object of class "rules"
  #' @param shading measure of interestingness used for the color of the points/arrows/nodes (e.g., "support", "confidence", "lift"). The default is "lift".
  #' @param measure measure(s) of interestingness
  #'  (e.g., "support", "confidence", "lift", "order") used in the visualization.
  #' @param method use "ggplot2" for a ggplot object, or "plotly" for an interactive plot
  #' @param start indicates first row number within a slice of a ruleset (inclusive), default = 1
  #' @param end indicates last row number within a slice of a ruleset (inclusive), default is NA (all rules included)
  #' @param decreasing should the shading factor be sorted in decreasing order?
  #'
  #' @return a "ggplot" object, or in case a "plotly" object for further modifications
  #' @export 
  #'
  #' @examples
  #' example("eclat")
  #' rules <- rules[2392:2428]
  #' rulesparacoord(rules)
  #' rulesparacoord(rules, decreasing = TRUE, end=27) # plot top 27 rules by lift in decreasing order
  #' rulesparacoord(rules, method="plotly")
  #' 
  #' rulesparacoord(rules)
rulesparacoord <- function(ruleset,method="ggplot2", shading = "lift",measure="support", start=1, end=NA, decreasing = FALSE){
  
    if (length(ruleset) < 1)
      stop("No rules of length 2 or longer.")
  if(is.na(end)){
    end = length(ruleset)
  }
  ruleset <- ruleset[size(ruleset) > 1]
  ruleset <- sort(ruleset, by = shading,  decreasing = decreasing)
  ruleset <- ruleset[c(start:end),]
  
  rsdf <- DATAFRAME(ruleset)
  rsdf$ind <- rownames(rsdf)
  rl <- strsplit(as.character(rsdf[,c("LHS")]),",")
  df <- plyr::ldply(rl, rbind)
  df$ind <- rownames(rsdf)
  df <- reshape2::melt(df, id.vars = "ind")
  df <- df[complete.cases(df),]
  df$value <- sub("{","",df$value, fixed=TRUE)
  df$value <- as.factor(sub("}","",df$value, fixed=TRUE))
  df <- df[order(df$ind,df$variable),]
  df$variable <- as.numeric(df$variable)
  
  RHS <- as.data.frame(as.character(rsdf$RHS))
  colnames(RHS) <- "value"
  RHS$ind <- rownames(rsdf)
  RHS$variable <- 'RHS'
  RHS$value <- sub("{","",RHS$value, fixed=TRUE)
  RHS$value <- sub("}","",RHS$value, fixed=TRUE)
  
  df2 <- rbind(df,RHS[,c(3,1,2)])
  df2 <- df2[order(df2$ind, df2$variable),]
  
  df2$variable <- factor(df2$variable, levels=c(sort(setdiff(unique(df2$variable), 'RHS'),decreasing=TRUE), 'RHS'))
  df2$ind <- as.factor(df2$ind)
  df2 <- merge(df2,rsdf[,-c(1:2)], by.x="ind",by.y="ind")
  plt <- ggplot(df2, aes_string(x="variable", y="value", group="ind",  color=measure, size=shading))+
    geom_line(arrow = arrow())+ ylab("   ")+xlab("Position")+
    geom_vline(xintercept=c(unique(df2$variable)), linetype="dotted")+
    theme_classic()+
    ggtitle(paste("Parallel coordinates plot for",length(unique(df2$ind)),"rules"))
    
  
  plt <- plt + theme(legend.position='none',
          axis.text = element_text(color="black", size=12))+
    theme(plot.title = element_text(hjust = -0))+
    theme(panel.border = element_rect(colour = "black",fill=NA))+
    scale_color_gradient(
      low = "#EE0000FF",
      high = "#EEEEEEFF",
      guide = "colourbar"
    )
  if(method=="ggplot2"){
  return(plt)
  } 
  else if(method=="plotly"){
    return(ggplotly(plt + 
                      geom_vline(xintercept=c(unique(df2$variable)), linetype="dotted")))
  }
  }
      
#plt <- rulesparacoord(rules, method="plotly",shading = "lift",measure="support")
#plt 


#' A Frequency diagram for ggplot2 and plotly
#'
#' @param ruleset an object of class "rules"
#' @param shading measure of interestingness used for the color of the points/arrows/nodes (e.g., "support", "confidence", "lift"). The default is "lift".
#' @param measure measure(s) of interestingness
#'  (e.g., "support", "confidence", "lift", "order") used in the visualization.
#' @param method use "ggplot2" for a ggplot object, or "plotly" for an interactive plot
#' @param start indicates first row number within a slice of a ruleset (inclusive), default = 1
#' @param end indicates last row number within a slice of a ruleset (inclusive), default is NA (all rules included)
#' @param decreasing should the shading factor be sorted in decreasing order?
#'
#' @return a "ggplot" object, or in case a "plotly" object for further modifications
#' @export 
#'
#' @examples
#' example("eclat")
#' rules@quality$size <- size(rules)
#' rules <- rules[2412:2428]
#' plotfreqmatrix(rules, method = "ggplot2", measure = "size")
#' plotfreqmatrix(rules, method = "ggplot2", measure = "size", end=12)
#' plotfreqmatrix(rules, method = "plotly", measure = "size")
plotfreqmatrix <- function(ruleset,method="ggplot2",measure="size", start=1, end=NA){
  
  if(is.na(end)){
    end = length(ruleset)
  }
  
  ruleset <- ruleset[c(start:end),]
  df <- DATAFRAME(ruleset)
  df$ct <- 1
  df <- df[,c(measure,"RHS","ct")]
  df$measure <- as.factor(df[,1])
  df <- aggregate(ct~measure+RHS,data=df,sum)
  
  plt <- ggplot(df,aes(x=measure,y=RHS,fill=ct))+geom_raster()+theme_classic()+
    theme(legend.position='none',
          axis.text = element_text(color="black", size=12))+
    theme(plot.title = element_text(hjust = -0))+
    scale_fill_gradient(
      low = "#EEEEEEFF",
      high = "#EE0000FF",
      guide = "colourbar")+xlab(measure)
  
  if(method=="ggplot2"){
    return(plt)
  } 
  else if(method=="plotly"){
    return(ggplotly(plt))}
  
}

#' Frequency of Items (rule based)
#' 
#' Plots a bar plot indicating the frequency of the top n items in a data set,
#' if rule meet minimum support or all other specified caps
#'
#' @param ruleset an object of class "rules"
#' @param method use "ggplot2" for a ggplot object, or "plotly" for an interactive plot
#' @param start indicates first row number within a slice of a rule set (inclusive), default = 1
#' @param end indicates last row number within a slice of a rule set (inclusive), default is NA (all rules included)
#' @param decreasing should the shading factor be sorted in decreasing order?
#' @param min_threshold should the minimum count value be indicated (black horizontal line)?, default=TRUE
#'
#' @return a "ggplot" object, or in case a "plotly" object for further modifications
#' @export 
#'
#' @examples
#' data(Adult)
#' rules <- apriori(Adult, parameter = list(supp = 0.05, conf = 0.09, target = "rules"))
#' freqbarplot(rules, method="ggplot2")
#' freqbarplot(rules, method="plotly", end=22) # plot top 22 items
#' freqbarplot(rules, method="ggplot2", min_threshold = FALSE)
freqbarplot <- function(ruleset,method="plotly",  start=1, end=NA,min_threshold=TRUE){
  
  min_count <- ruleset@info$ntransactions * ruleset@info$support
  rl <- DATAFRAME(ruleset)
  rl <- rl[rl$LHS=="{}",c("RHS","support","count")]
  
  if(is.na(end)){
    end = length(rl$RHS)
  }
  
  rl <- rl[c(start:end),]
  
  
  plt <- ggplot(rl, aes(y=RHS,x=count,fill=count))+geom_bar(stat="identity")+theme_classic()+
    theme(legend.position='none',
          axis.text = element_text(color="black", size=12))+
    theme(plot.title = element_text(hjust = -0))+
    scale_fill_gradient(
      low = "#EEEEEEFF",
      high = "#EE0000FF",
      guide = "colourbar")
  
  if(min_threshold){
    plt <- plt + geom_vline(xintercept=min_count)
  }
  
  if(method=="ggplot2"){
    return(plt)
  } 
  if(method=="plotly"){
    return(ggplotly(plt))
    }
  
}

ruleExplorerEXT <-
  function(x,
           sidebarWidth = 2,
           graphHeight = '600px') {
    
    rlang::check_installed(c("shiny", "shinythemes"))
    
    # shinyTheme <- shinythemes::shinytheme("yeti")
    
    message("ruleExplorer started.")
    
    # show warnings immediately
    o <- options(warn = 1)
    on.exit(options(o))
    
    ### rounding helpers
    roundUp <- function(x, digits = 3)
      round(x + .5 * 10 ^ -digits, digits)
    roundDown <- function(x, digits = 3)
      round(x - .5 * 10 ^ -digits, digits)
    
    ### dataset can be rules or transactions
    
    ### make sure we have transactions or rules
    if (!is(x, "rules") && !is(x, "transactions")) {
      message("Converting dataset into transactions.")
      x <- transactions(x)
    }
    
    ### default measures to use
    xIndexCached <- "support"
    yIndexCached <- "confidence"
    zIndexCached <- "lift"
    
    ### javascript cannot handle very large arrays
    itemLabels <- itemLabels(x)
    if (length(itemLabels) > 10000)
      itemLabels <-
      list('Disabled because of excessive number of items (>10,000)' = c(""))
    
    if (is(x, "rules")) {
      if (length(x) < 1)
        stop("Zero rules provided!")
      
      minSupp <- roundDown(min(quality(x)$support), 5)
      maxSupp <- roundUp(max(quality(x)$support), 5)
      minConf <- roundDown(min(quality(x)$confidence), 3)
      maxConf <- roundUp(max(quality(x)$confidence), 3)
      minLift <- floor(min(quality(x)$lift))
      maxLift <- ceiling(max(quality(x)$lift))
      
      supp <- minSupp
      conf <- minConf
      lift <- minLift
    } else {
      ### transactions
      minSupp <- 0
      maxSupp <- 1
      minConf <- 0
      maxConf <- 1
      minLift <- 0
      maxLift <- 25
      lift <- 0
      
      defaultParam <- new('APparameter')
      supp <- defaultParam@support
      conf <- defaultParam@confidence
    }
    
    ## create Shiny UI and server code
    shiny::shinyApp(
      ui = shiny::shinyUI(
        shiny::fluidPage(
          theme = shinythemes::themeSelector(),
          
          shiny::titlePanel("Association Rule Explorer"),
          
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::htmlOutput('numRulesOutput'),
              shiny::br(),
              
              shiny::sliderInput(
                "supp",
                "Minimum Support:",
                min = minSupp,
                max = maxSupp,
                value = supp ,
                step = (maxSupp - minSupp) / 10000,
                sep = ""
              ),
              shiny::sliderInput(
                "conf",
                "Minimum Confidence:",
                min = minConf,
                max = maxConf,
                value = conf ,
                step =  (maxConf - minConf) / 1000,
                sep = ""
              ),
              shiny::sliderInput(
                "lift",
                "Minimum Lift:",
                min = minLift,
                max = maxLift,
                value = lift ,
                step =  (maxLift - minLift) / 1000,
                sep = ""
              ),
              shiny::sliderInput(
                "length",
                "Rule length (from-to):",
                min = 1,
                max = 20,
                value = c(1, 10) ,
                step =  1,
                sep = ""
              ),
              
              shiny::em(shiny::HTML('Filter rules by items:')),
              shiny::selectInput(
                'colsType',
                NULL,
                c('Exclude items:' = 'rem', 'Require items:' = 'req')
              ),
              shiny::uiOutput("choose_columns"),
              shiny::selectInput(
                'colsLHSType',
                NULL,
                c(
                  'Exclude items from LHS:' = 'rem',
                  'Require items in LHS:' = 'req'
                )
              ),
              shiny::uiOutput("choose_lhs"),
              shiny::selectInput(
                'colsRHSType',
                NULL,
                c(
                  'Exclude items from RHS:' = 'rem',
                  'Require items in RHS:' = 'req'
                )
              ),
              shiny::uiOutput("choose_rhs"),
              width = sidebarWidth
            ),
            
            shiny::mainPanel(
              shiny::tabsetPanel(
                id = 'tabs',
                
                shiny::tabPanel(
                  'Data Table',
                  value = 'datatable',
                  shiny::br(),
                  DT::dataTableOutput("rulesDataTable")
                ),
                
                #####
                shiny::tabPanel(
                  'Parallel Coordinates Diagramm',
                  value = 'pcd',
                  shiny::wellPanel(
                    shiny::fluidRow(
                      shiny::column(2, shiny::uiOutput("measure_pcm")),
                      shiny::column(2, shiny::uiOutput("shading_pcm")),
                      shiny::column(3, shiny::uiOutput("topRules_pcm"))
                    )
                  ),
                  plotly::plotlyOutput("scatterPlot2", width = '100%', height =
                                         graphHeight)
                ),
                
                shiny::tabPanel(
                  'Boxplot Diagramm',
                  value = 'boxplot',
                  shiny::wellPanel(
                    shiny::fluidRow(
                      shiny::column(2, shiny::uiOutput("measure_box")),
                      shiny::column(2, shiny::uiOutput("group_box"))
                    )
                  ),
                  plotly::plotlyOutput("boxplot", width = '100%', height =
                                         graphHeight)
                ),
                
                shiny::tabPanel(
                  'Matrix Frequency Diagramm',
                  value = 'mfd',
                  shiny::wellPanel(
                    shiny::fluidRow(
                      shiny::column(2, shiny::uiOutput("measure_mfd"))
                    )
                  ),
                  plotly::plotlyOutput("mfd_plot", width = '100%', height =
                                         graphHeight)
                ),
                #####
                
                
                shiny::tabPanel(
                  'Scatter',
                  value = 'scatter',
                  shiny::wellPanel(
                    shiny::fluidRow(
                      shiny::column(2, shiny::uiOutput("xAxisSelectInput")),
                      shiny::column(2, shiny::uiOutput("yAxisSelectInput")),
                      shiny::column(2, shiny::uiOutput("cAxisSelectInput")),
                      shiny::column(
                        3,
                        shiny::sliderInput(
                          "jitter_scatter",
                          "Jitter",
                          min = 0,
                          max = 1,
                          value = 0,
                          step = 1 / 100,
                          sep = ""
                        )
                      ),
                      shiny::column(3, shiny::uiOutput("topRules_scatter"))
                    )
                  ),
                  plotly::plotlyOutput("scatterPlot", width = '100%', height =
                                         graphHeight)
                ),
                
                shiny::tabPanel(
                  'Matrix',
                  value = 'matrix',
                  shiny::wellPanel(shiny::fluidRow(
                    shiny::column(6, shiny::uiOutput("cAxisSelectInput_matrix")),
                    shiny::column(6, shiny::uiOutput("topRules_matrix"))
                  )),
                  plotly::plotlyOutput("matrixPlot", width = '100%', height =
                                         graphHeight)
                ),
                
                shiny::tabPanel(
                  'Grouped Matrix',
                  value = 'grouped',
                  shiny::wellPanel(shiny::fluidRow(
                    shiny::column(6, shiny::uiOutput("cAxisSelectInput_grouped")),
                    shiny::column(6, shiny::uiOutput("kSelectInput"))
                  )),
                  shiny::plotOutput("groupedPlot", width = '100%', height =
                                      graphHeight)
                ),
                
                shiny::tabPanel(
                  'Freq Bar Plot',
                  value = 'fbp',
                  shiny::wellPanel(shiny::fluidRow()),
                  plotly::plotlyOutput("freqBarPlot", width = '100%', height =
                                      graphHeight)
                ),
                
                shiny::tabPanel(
                  'Graph',
                  value = 'graph',
                  shiny::wellPanel(shiny::fluidRow(
                    shiny::column(6, shiny::uiOutput("cAxisSelectInput_graph")),
                    shiny::column(6, shiny::uiOutput("topRules_graph"))
                  )),
                  
                  visNetwork::visNetworkOutput("graphPlot", width = '100%', height =
                                                 graphHeight)
                ),
                
                shiny::tabPanel(
                  'Export',
                  value = 'export',
                  shiny::br(),
                  shiny::downloadButton('rules.csv', 'Export rules (CSV)')
                )
              ),
              width = 12 - sidebarWidth
            )
          )
        )
      ),
      server = function(input, output, session) {
        output$numRulesOutput <- shiny::renderUI({
          if (is(x, "rules"))
            shiny::em(shiny::HTML(paste(
              'Selected rules: ', length(rules()),
              ' of ', length(x)
            )))
          else
            shiny::em(shiny::HTML(paste('Rules: ', length(rules(
              
            )))))
        })
        
        output$kSelectInput <- shiny::renderUI({
          shiny::sliderInput(
            'k',
            label = 'Choose # of rule clusters',
            min = 1,
            max = 50,
            step = 1,
            value = 15
          )
        })
        
        output$xAxisSelectInput <- shiny::renderUI({
          shiny::selectInput("xAxis", "X Axis:", colnames(quality(rules())), selected =
                               xIndexCached)
        })
        
        output$measure_pcm <- shiny::renderUI({
          shiny::selectInput("measure", "Measure:", colnames(quality(rules())), selected =
                               xIndexCached)
        })
        
        output$measure_box <- shiny::renderUI({
          shiny::selectInput("measure_box", "Measure:", colnames(quality(rules())), selected =
                               xIndexCached)
        })
        
        output$measure_mfd <- shiny::renderUI({
          shiny::selectInput("measure_mfd", "Measure:", colnames(quality(rules())[!(colnames(quality(rules())) %in% c("support","lift","confidence"))]) , selected =
                               xIndexCached)
        })
        
        output$yAxisSelectInput <- shiny::renderUI({
          shiny::selectInput("yAxis", "Y Axis:", colnames(quality(rules())), selected =
                               yIndexCached)
        })
        
         
        
        output$cAxisSelectInput <- shiny::renderUI({
          shiny::selectInput("cAxis", "Shading:", colnames(quality(rules())), selected =
                               zIndexCached)
        })
        
        output$shading_pcm <- shiny::renderUI({
          shiny::selectInput("shading", "Shading:", colnames(quality(rules())), selected =
                               zIndexCached)
        })
        
        output$group_box <- shiny::renderUI({
          shiny::selectInput("group", "Group:", colnames(quality(rules())), selected =
                               zIndexCached)
        })
        
        output$cAxisSelectInput_matrix <- shiny::renderUI({
          shiny::selectInput("cAxis_matrix",
                             "Shading:",
                             colnames(quality(rules())),
                             selected = zIndexCached)
        })
        
        output$cAxisSelectInput_grouped <- shiny::renderUI({
          shiny::selectInput("cAxis_grouped",
                             "Shading:",
                             colnames(quality(rules())),
                             selected = zIndexCached)
        })
        
        output$cAxisSelectInput_graph <- shiny::renderUI({
          shiny::selectInput("cAxis_graph",
                             "Shading:",
                             colnames(quality(rules())),
                             selected = zIndexCached)
        })
        
        output$topRules_matrix <- shiny::renderUI({
          shiny::sliderInput(
            "topRules_matrix",
            "Top rules shown (keep below 500):",
            min = 1,
            max = length(rules()),
            value = min(100, length(rules())),
            step = 1,
            sep = ""
          )
        })
        
 
        
        output$topRules_scatter <- shiny::renderUI({
          shiny::sliderInput(
            "topRules_scatter",
            "Top rules shown (keep below 500):",
            min = 1,
            max = length(rules()),
            value = min(100, length(rules())),
            step = 1,
            sep = ""
          )
        })
        
        output$topRules_pcm <- shiny::renderUI({
          shiny::sliderInput(
            "topRules_pcm",
            "Top rules shown (keep below 500):",
            min = 1,
            max = length(rules()),
            value = min(100, length(rules())),
            step = 1,
            sep = ""
          )
        })
        
        
        
        output$topRules_graph <- shiny::renderUI({
          shiny::sliderInput(
            "topRules_graph",
            "Top rules shown (keep below 500):",
            min = 1,
            max = length(rules()),
            value = min(100, length(rules())),
            step = 1,
            sep = ""
          )
        })
        
        output$choose_columns <- shiny::renderUI({
          shiny::selectizeInput('cols', NULL, itemLabels, multiple = TRUE)
        })
        
        output$choose_lhs <- shiny::renderUI({
          shiny::selectizeInput('colsLHS', NULL, itemLabels, multiple = TRUE)
        })
        
        output$choose_rhs <- shiny::renderUI({
          shiny::selectizeInput('colsRHS', NULL, itemLabels, multiple = TRUE)
        })
        
        ## caching data
        cachedRules <- NULL
        cachedSupp <- supp
        cachedConf <- conf
        cachedLift <- lift
        cachedMinL <- minLift
        cachedMaxL <- maxLift
        
        if (is(x, "rules")) {
          cachedRules <- x
          cachedSupp <<- info(x)$support
          cachedConf <<- info(x)$confidence
          cachedLift <<- min(quality(x)$lift)
          cachedMinL <<- min(size(x))
          cachedMaxL <<- max(size(x))
        }
        
        # re-mine rules if necessary dataset is transactions!
        remineRules <- shiny::reactive({
          # use a minimum of 1 absolute support!
          supp <- input$supp
          if (supp == 0)
            supp <- 1 / length(x)
          
          message("Remining rules...")
          
          rules <- apriori(
            x,
            parameter = list(
              support = as.numeric(supp),
              confidence = as.numeric(input$conf),
              minlen = input$length[1],
              maxlen = input$length[2]
            ),
            control = list(verbose = FALSE)
          )
          quality(rules) <- interestMeasure(rules, transactions = x)
          
          message("Remined ", length(rules), " rules.")
          
          cachedRules <<- rules
          cachedSupp <<- input$supp
          cachedConf <<- input$conf
          cachedLift <<- input$lift
          cachedMinL <<- input$length[1]
          cachedMaxL <<- input$length[2]
        })
        
        # handle warning for too low support
        override <- shiny::reactiveVal(FALSE)
        
        shiny::observeEvent(input$cancel, {
          shiny::removeModal()
          # reset the slider (Note this does not change input$supp!)
          shiny::updateSliderInput(session, "supp", value = cachedSupp)
        })
        
        shiny::observeEvent(input$continue, {
          shiny::removeModal()
          override(TRUE)
        })
        
        rules <- shiny::reactive({
          # recalculate rules?
          
          if (is(x, 'transactions')) {
            # check for low minimum support first
            if (input$supp * length(x) > 10 || override()) {
              if (is.null(cachedRules))
                remineRules()
              if ((input$supp < cachedSupp) ||
                  input$conf < cachedConf)
                remineRules()
              if (input$length[1] < cachedMinL ||
                  input$length[1] > cachedMaxL)
                remineRules()
              
            } else {
              shiny::showModal(
                shiny::modalDialog(
                  title = 'Warning',
                  'Very low minimum support! Too low values can result in long wait times and memory issues.',
                  footer = shiny::tagList(
                    shiny::actionButton('cancel', 'cancel'),
                    shiny::actionButton('continue', 'proceed')
                  )
                )
              )
            }
          }
          
          ar <- cachedRules
          
          # filter rules
          if (input$supp > cachedSupp) {
            ar <- subset(ar, subset = support > input$supp)
          }
          
          if (input$conf > cachedConf) {
            ar <- subset(ar, subset = quality(ar)$confidence > input$conf)
          }
          
          if (input$lift > cachedLift) {
            ar <- subset(ar, subset = lift > input$lift)
          }
          
          if (input$length[1] > cachedMinL) {
            ar <- ar[size(ar) >= input$length[1]]
          }
          
          if (input$length[2] < cachedMaxL) {
            ar <- ar[size(ar) <= input$length[2]]
          }
          
          if (input$colsType == 'rem' && length(input$cols) > 0) {
            ar <- subset(ar, subset = !(items %in% input$cols))
          }
          
          if (input$colsType == 'req' && length(input$cols) > 0) {
            ar <- subset(ar, subset = items %in% input$cols)
          }
          
          if (input$colsLHSType == 'rem' &&
              length(input$colsLHS) > 0) {
            ar <- subset(ar, subset = !(lhs %in% input$colsLHS))
          }
          
          if (input$colsLHSType == 'req' &&
              length(input$colsLHS) > 0) {
            ar <- subset(ar, subset = lhs %in% input$colsLHS)
          }
          
          if (input$colsRHSType == 'rem' &&
              length(input$colsRHS) > 0) {
            ar <- subset(ar, subset = !(rhs %in% input$colsRHS))
          }
          
          if (input$colsRHSType == 'req' &&
              length(input$colsRHS) > 0) {
            ar <- subset(ar, subset = rhs %in% input$colsRHS)
          }
          
          shiny::validate()
          
          ar
        })
        
        # remember settings for other plots
        shiny::observe({
          shiny::req(input$xAxis)
          xIndexCached <<- input$xAxis
        })
        shiny::observe({
          shiny::req(input$yAxis)
          yIndexCached <<- input$yAxis
        })
        shiny::observe({
          shiny::req(input$cAxis)
          zIndexCached <<- input$cAxis
        })
        shiny::observe({
          shiny::req(input$cAxis_matrix)
          zIndexCached <<- input$cAxis_matrix
        })
        shiny::observe({
          shiny::req(input$cAxis_grouped)
          zIndexCached <<- input$cAxis_grouped
        })
        shiny::observe({
          shiny::req(input$cAxis_graph)
          zIndexCached <<- input$cAxis_graph
        })
        
        # Present errors nicely to the user
        handleErrors <- shiny::reactive({
          shiny::validate(
            shiny::need(
              length(rules()) > 0,
              'No rules to visualize! Decrease support, confidence or lift.'
            )
          )
        })
        
        ## Data Table ##########################
        output$rulesDataTable <- DT::renderDT({
          handleErrors()
          inspectDT(rules())
        })
        
        output$scatterPlot2 <- plotly::renderPlotly({
          shiny::req(
            input$measure,
            input$shading,
            input$topRules_pcm
            
          )
          handleErrors()
          suppressWarnings(
            
            rulesparacoord(rules(), method = 'plotly',
                           measure = input$measure,
                           shading = input$shading,
                           end = input$topRules_pcm 
            )
          )
          
          
          
        })
        
        output$boxplot <- plotly::renderPlotly({
          shiny::req(
            input$measure_box,
            input$group
          )
          handleErrors()
          suppressWarnings(
            ggplotly(ggplot(DATAFRAME(rules()), aes_string(x=input$group, y=input$measure_box)) + 
              geom_boxplot(outlier.colour="red", outlier.shape=8,
                           outlier.size=4) + theme_classic())
            
            )
        })
        
        output$mfd_plot <- plotly::renderPlotly({
          shiny::req(
            input$measure_mfd
          )
          handleErrors()
          suppressWarnings(
            plotfreqmatrix(rules(), method = "plotly", measure = input$measure_mfd)
            
          )
        })
        
        ## Scatter Plot ##########################
        output$scatterPlot <- plotly::renderPlotly({
          shiny::req(
            input$xAxis,
            input$yAxis,
            input$cAxis,
            input$topRules_scatter,
            input$jitter_scatter
          )
          handleErrors()
          suppressWarnings(
            plot(
              rules(),
              method = 'scatterplot',
              measure = c(input$xAxis, input$yAxis),
              shading = input$cAxis,
              engine = 'htmlwidget',
              control = list(
                max = input$topRules_scatter,
                jitter = input$jitter_scatter
              )
            )
          )
        })
        
        ## Matrix Plot ###################
        output$matrixPlot <- plotly::renderPlotly({
          shiny::req(input$cAxis_matrix, input$topRules_matrix)
          handleErrors()
          suppressWarnings(
            plot(
              rules(),
              method = 'matrix',
              shading = input$cAxis_matrix,
              engine = 'htmlwidget',
              control = list(max = input$topRules_matrix)
            )
          )
        })
        
        output$freqBarPlot <- plotly::renderPlotly({
          handleErrors()
          suppressWarnings(
            freqbarplot(
              rules(),
              method = 'plotly',
              start=1,
              end=23
            )
          )
        })
        
        output$groupedPlot <- shiny::renderPlot({
          shiny::req(input$cAxis_grouped, input$k)
          handleErrors()
          
          plot(
            rules(),
            method = 'grouped',
            shading = input$cAxis_grouped,
            engine = 'ggplot2',
            control = list(k = input$k)
          ) + theme(text = element_text(size = 14))
        })
        
        
        
        ## Graph Plot ##########################
        output$graphPlot <- visNetwork::renderVisNetwork({
          shiny::req(input$cAxis_graph, input$topRules_graph)
          handleErrors()
          
          suppressWarnings(
            plt <- plot(
              rules(),
              method = 'graph',
              shading = input$cAxis_graph,
              engine = 'htmlwidget',
              control = list(max = input$topRules_graph)
            )
          )
 
          plt
        })
        
        ## Export ########################
        output$rules.csv <- shiny::downloadHandler(
          filename = 'rules.csv',
          content = function(file) {
            utils::write.csv(as(rules(), "data.frame"), file)
          }
        )
        
        
      }
    )
  }


example("eclat")
rules <- rules[2412:2438]
rules@quality$ChiSquaredp <-  interestMeasure(rules,measure = "ChiSquared", significance=TRUE)
rules@quality$ChiSquaredp_holm <- p.adjust(rules@quality$ChiSquaredp, method="holm")
rules@quality$size <- size(rules)

G <- graph.data.frame(DATAFRAME(rules)[,c("LHS","RHS")], directed = FALSE)
cl <- cluster_louvain(G)
#plot(G, vertex.color=rainbow(3, alpha=0.6)[cl$membership],vertex.label=NA)
#plot(cl,G, vertex.color=rainbow(3, alpha=0.6)[cl$membership],vertex.label=NA)

helper <- cbind(vertex=V(G), louvain_ante=cl$memberships[1,], louvain_cons=cl$memberships[1,])
helper <- data.frame(helper)
helper$vertex_name <- rownames(helper)
mgd <- merge(DATAFRAME(rules),helper, by.x=c("LHS"),by.y=c("vertex_name"))

rules@quality$louvain_ante <- mgd$louvain_ante
rules@quality$louvain_cons <-  mgd$louvain_cons

#plotfreqmatrix(rules, method = "ggplot2", measure = "louvain_ante")
#plotfreqmatrix(rules, method = "plotly", measure = "louvain_ante")

# example("Apriori")
ruleExplorerEXT(rules)
