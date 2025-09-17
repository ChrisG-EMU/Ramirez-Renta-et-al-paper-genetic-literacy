## Decisions on Figures:
#   Use scatterplot for main plot and density for margins for continuous; densities for categorical
#   Separate figure for each variable (y-axis = variable of interest, x-axis = genetic literacy score)
#   Do not filter on education
#   Add n= annotations

# For updated fig, want to show the raw data; do not care about including education level
mycol <- c("SPARK" = "#1F78B4", "General Population" = "#B2DF8A")

# continuous variable case
# GL Score by population and continuous variable
fig_cont <- function(var, ylab, outfile=NULL, yscale=NULL, u=unit(rep(0, 4), units="npc")){
  plotdat <-  combined %>%
    # GL1, GL2, and GL3 should all be non-missing
    filter(if_all(.cols = c(GL1, GL2, GL3, {{ var }} ), .fns = ~ !is.na(.x)))
  
  n.gp <- sum(plotdat$popl == "General Population")
  n.spark <- sum(plotdat$popl == "SPARK")

  # Dummy figure just to generate appropriate legend
  leg.fig <- plotdat %>%
    ggplot(aes(y = GL1, col=popl, fill=popl)) +
    geom_density(alpha=0.5) +
    scale_discrete_manual(values = mycol,
                          breaks = c("General Population", "SPARK"),
                          labels = c(paste0("General Population\n(n = ", n.gp, ")"), 
                                     paste0("SPARK\n(n = ", n.spark, ")")),
                          aesthetics = c("fill", "color"), 
                          name=NULL) +
    theme(legend.text.align = 0.5,
          legend.direction = "horizontal")
  
  s1 <- plotdat %>%
    ggplot(aes(y = {{ var }} , x = GL1, col=popl, fill=popl)) +
    geom_point(alpha=0.1) +
    scale_discrete_manual(values = mycol, aesthetics = c("fill", "color")) +
    labs(x="Familiarity Score",  y=ylab) +
    scale_x_continuous(breaks = 1:7) +
    theme_classic() +
    theme(plot.margin = u)
  
  s2 <- plotdat %>%
    ggplot(aes(y = {{ var }} , x = GL2, col=popl, fill=popl)) +
    geom_point(alpha=0.1) +
    scale_discrete_manual(values = mycol, aesthetics = c("fill", "color")) +
    scale_x_continuous(breaks = seq(1,17,2)) +
    labs(x="Knowledge Score",  y=ylab) +
    theme_classic() +
    theme(axis.title.y = element_blank(),      # remove extra y-axis label
          plot.margin = u) 
  
  s3 <- plotdat %>%
    ggplot(aes(y = {{ var }} , x = GL3, col=popl, fill=popl)) +
    geom_point(alpha=0.1) +
    scale_discrete_manual(values = mycol, aesthetics = c("fill", "color")) +
    scale_x_continuous(breaks = 0:6) +
    labs(x="Skills Score",  y=ylab) +
    theme_classic()+
    theme(axis.title.y = element_blank(),     # remove extra y-axis label
          plot.margin = u) 
  
  hy <- plotdat %>%
    ggplot(aes(y = {{ var }}, col=popl, fill=popl)) +
    geom_density(alpha=0.5) +
    scale_discrete_manual(values = mycol, aesthetics = c("fill", "color")) +
    clean_theme() +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.margin = u)
  
  if(!is.null(yscale)){
    s1 <- s1 +
      scale_y_continuous(breaks = yscale)
    s2 <- s2 +
      scale_y_continuous(breaks = yscale)
    s3 <- s3 +
      scale_y_continuous(breaks = yscale)
    hy <- hy +
      scale_y_continuous(breaks = yscale)
  }
  
  hx1 <- plotdat %>%
    ggplot(aes(x = GL1 , col=popl, fill=popl)) +
    geom_density(alpha=0.5) +
    scale_fill_manual(values = mycol, aesthetics = c("fill", "color")) +
    clean_theme() +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.margin = u)
  
  hx2 <- plotdat %>%
    ggplot(aes(x = GL2 , col=popl, fill=popl)) +
    geom_density(alpha=0.5) +
    scale_fill_manual(values = mycol, aesthetics = c("fill", "color")) +
    clean_theme() +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.margin = u)
  
  hx3 <- plotdat %>%
    ggplot(aes(x = GL3 , col=popl, fill=popl)) +
    geom_density(alpha=0.5) +
    scale_fill_manual(values = mycol, aesthetics = c("fill", "color")) +
    clean_theme() +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.margin = u)
  
  g <- ggarrange(hx1, hx2, hx3, NULL, 
                 s1, s2, s3, hy,
                 nrow=2, ncol=4, align="hv", widths=c(2, 2, 2, 1), heights=c(1,2),
                 legend = "bottom", common.legend=T, legend.grob = get_legend(leg.fig))
  
  if(!is.null(outfile)){
    ggsave(paste0(out_dir, outfile, ".png"), width=7, height=4, units="in")
  }
  return(g)
}

# binary/categorical variable case
fig_cat <- function(var, outfile=NULL, u=unit(rep(0.03, 4), units="npc"), plot="density"){
  
  plotdat <-  combined %>%
    # GL1, GL2, and GL3 should all be non-missing
    filter(if_all(.cols = c(GL1, GL2, GL3, {{ var }} ), .fns = ~ !is.na(.x)))
  
  n.gp <- sum(plotdat$popl == "General Population")
  n.spark <- sum(plotdat$popl == "SPARK")

  
  g1 <- ggplot(plotdat, aes(x=GL1, fill=popl, col=popl)) +
    facet_grid(cols=vars( {{ var }} )) +
    scale_discrete_manual(values = mycol,
                          breaks = c("General Population", "SPARK"),
                          labels = c(paste0("General Population\n(n = ", n.gp, ")"), 
                                     paste0("SPARK\n(n = ", n.spark, ")")),
                          aesthetics = c("fill", "color"), 
                          name=NULL) +
    scale_x_continuous(breaks = 1:7) +
    labs(x="Familiarity Score") +
    theme_classic() +
    theme(legend.text.align = 0.5,
          legend.direction = "horizontal", 
          plot.margin = u)
  
  g2 <- ggplot(plotdat, aes(x=GL2, fill=popl, col=popl)) +
    facet_grid(cols=vars( {{ var }} )) +
    scale_discrete_manual(values = mycol, aesthetics = c("fill", "color"), name=NULL) +
    scale_x_continuous(breaks = seq(1,17,2))+
    labs(x="Knowledge Score") + 
    theme_classic() +
    theme(plot.margin = u)
  
  g3 <- ggplot(plotdat, aes(x=GL3, fill=popl, col=popl)) +
    facet_grid(cols=vars( {{ var }} )) +
    scale_discrete_manual(values = mycol, aesthetics = c("fill", "color"), name=NULL) +
    scale_x_continuous(breaks = 0:6) +
    labs(x="Skills Score") + 
    theme_classic() +
    theme(plot.margin = u)
  
  if(plot == "density"){
    g1 <- g1 +
      geom_density(alpha=0.5) +
      labs(y = "Density")
    g2 <- g2 +
      geom_density(alpha=0.5) +
      labs(y = "Density")
    g3 <- g3 +
      geom_density(alpha=0.5) +
      labs(y = "Density")
  }else if(plot == "histogram"){
    g1 <- g1 +
      geom_histogram(alpha=0.5, binwidth = 1, position = position_dodge()) +
      labs(y = "Count")
    g2 <- g2 +
      geom_histogram(alpha=0.5, binwidth=1, position = position_dodge()) +
      labs(y = "Count")
    g3 <- g3 +
      geom_histogram(alpha=0.5, binwidth=1, position = position_dodge()) +
      labs(y = "Count")
  }
  
  g <- ggarrange(g1, g2, g3, nrow=3, ncol=1, align = "hv", 
                 common.legend = T, legend = "top", legend.grob = get_legend(g1))
  
  if(!is.null(outfile)){
    ggsave(paste0(out_dir, outfile, ".png"), width=6, height=6, units="in")
  }
  
  return(g)
}