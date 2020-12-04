library(ggplot2)

# Function to plot EACF

plot_eacf <- function(input_data, process ='') {
  eacf_data <- eacf(input_data)$symbol
  
  data <- expand.grid(AR = as.factor(seq(from = 0, to = nrow(eacf_data)-1, by = 1)),
                      MA = as.factor(seq(from = 0, to = ncol(eacf_data)-1, by = 1)))
  
  data$symbols <- as.vector(eacf_data)
  
  data$values <- as.factor(ifelse(as.vector(eacf_data) == 'x', 1, 0))
  
  ggplot(data, aes(x = MA, y = AR, fill = values)) +
    geom_tile() +
    geom_text(aes(label = symbols)) +
    scale_x_discrete(position = 'top') +
    scale_y_discrete(limits = rev(levels(data$AR))) +
    theme(legend.position = 'none', 
          panel.background = element_blank(), 
          plot.title = element_text(hjust = 0.5, size = 18),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12)) +
    scale_fill_manual(values = c('#FFFFFF', '#808080')) +
    labs(title = paste0('EACF for ', process, ' with n = ', length(input_data)))
}
