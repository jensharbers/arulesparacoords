library(arules)
library(arulesViz)
library(reshape2)
library(ggpubr)
library(plotly)
library(plyr)
library(ggplot2)
example("eclat")

plot(rules, shading = "lift",measure="support", method="paracoord", n=50)

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
  #' rulesparacoord(rules)
  #' rulesparacoord(rules, decreasing = TRUE, end=30) # plot top 30 rules by lift in decreasing order
  #' rulesparacoord(rules, method="plotly")
  #' 
  #' rules <- rules[2412:2438]
  #' rulesparacoord(rules)
  rulesparacoord <- function(ruleset,method="ggplot2", shading = "lift",measure="support", start=1, end=NA, decreasing = FALSE){
  
    if (length(rules) < 1)
      stop("No rules of length 2 or longer.")
  if(is.na(end)){
    end = length(rules)
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
    geom_vline(xintercept=c(unique(df2$variable)), linetype="dotted")
  
  plt <- plt +theme_classic()+
    ggtitle(paste("Parallel coordinates plot for",length(rules),"rules"))+
    theme(legend.position='none',
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
      
    
