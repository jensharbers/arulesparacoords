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
