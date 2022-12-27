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
#' library(arules)
#' library(plotly)
#' library(ggplot2)
#' data(Adult)
#' rules <- apriori(Adult, parameter = list(supp = 0.05, conf = 0.09, target = "rules"))
#' freqbarplot(rules, method="ggplot2")
#' freqbarplot(rules, method="plotly", end=22) # plot top 22 items
#' freqbarplot(rules, method="ggplot2", min_threshold = FALSE)

library(arules)
library(plotly)
library(ggplot2)

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
