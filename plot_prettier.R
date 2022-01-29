library(directlabels)
library(grid)
library(tidyr)

plot_nrmse_pretty <-
  function(nrmse,
           method_name,
           missing_rate,
           ytitle = "",
           subtitle,
           panel,
           label = TRUE,
           legend = FALSE) {
    
    len <- dim(nrmse)[2]
    mean <- rowMeans(nrmse, na.rm = TRUE)
    sd <- apply(nrmse, 1, function(x)
      sd(x, na.rm = TRUE))
    # se <- sd/sqrt(len)
    
    number_of_missing_rate <- length(missing_rate)
    number_of_method <- length(method_name)
    
    methods = factor(method_name)
    methods = rep(methods, number_of_missing_rate)
    
    missingRate = missing_rate
    missingRate = rep(missingRate, each = number_of_method)
    
    df <- data.frame(missingRate, methods, mean, sd)
    
    res <- ggplot(df,
                  aes(
                    x = missingRate,
                    y = mean,
                    group = methods,
                    color = methods
                  )) +
      geom_line(size = 1, alpha =0.8, 
                 aes(linetype = methods)) +
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
                    width=.1, alpha= 0.8, size = 1) +
      #geom_point(shape = 0, size = 1) +
     geom_point(aes(shape = methods), size = 2.5) +
     scale_shape_manual(values = c(0,1,2,4,5,6,11, 14, 23, 24, 25)) +
      # xlab("Total Missing Rate") +
      xlab("") +
      ylab(ytitle) +
      ggtitle(subtitle) +
    scale_x_continuous(expand=c(
        0.15, 0
    )) +
      ylim(0.9, 1.7) + 
      facet_grid(factor(
        mean < panel,
        level = c(FALSE, TRUE),
        labels = c("A", "B")
      ) ~ .,
      scales = "free_y")
    
    if(!legend){
      res <- res + theme(legend.position = "none")
    }
    
     
    
    if (label) {
      return(
        res +
          geom_dl(aes(label = methods),
                  method =list(dl.trans(x = x+.3), "last.bumpup")) +
          geom_dl(aes(label = methods),
                  method =list(dl.trans(x = x-.3), "first.bumpup")) +
          theme(
            title = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            strip.text.y = element_blank()
          )
      )
    } else {
      return(
        res + theme(
          title = element_text(
            size = 12,
            face = "bold",
            colour = "white"
          ),
          axis.title = element_text(
            size = 12,
            face = "bold",
            colour = "white"
          ),
          axis.text.x = element_text(color = "white"),
          axis.text.y = element_text(color = "white"),
          legend.text = element_text(size = 12, colour = "white"),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_blank()
        )
      )
    }
  }




plot_SOR_pretty <-
  function(sor_matrix,
           method_name,
           missing_rate,
           subtitle,
           label = TRUE,
           legend = FALSE) {
    len <- dim(sor_matrix)[2]
    sd <- apply(log10(sor_matrix), 1, function(x)
      sd(x, na.rm = TRUE))
    se <- sd/sqrt(len)
    
    
    sor <- matrix(log10(t(sor_matrix)),
                  nrow = ncol(sor_matrix) * nrow(sor_matrix),
                  ncol = 1)
    
    
    number_of_missing_rate <- length(missing_rate)
    number_of_method <- length(method_name)
    
    methods = factor(method_name)
    methods = rep(methods, number_of_missing_rate)
    
    missingRate = missing_rate
    missingRate = rep(missingRate, each = number_of_method)
    
    df <- data.frame(missingRate, methods, sor, sd)
    
    res <- ggplot(df,
                  aes(
                    x = missingRate,
                    y = sor,
                    group = methods,
                    color = methods
                  )) +
      geom_line(size = 1, alpha = 0.8,
                aes(linetype = methods)) +
      geom_errorbar(aes(ymin = sor-sd, ymax = sor+sd),
                    width=.4, alpha= 0.8, size = 1) +
      geom_point(aes(shape = methods), size = 3) +
      scale_shape_manual(values = c(0, 1, 2, 15, 17, 5, 6, 16)) +
      xlab("Total Missing Rate (Batch A)") +
      ylab("log10 SOR") +
      ggtitle(subtitle) +
      scale_x_continuous(expand=c(
        0.15, 0
      )) 
    
      # coord_cartesian(xlim = c(
      #   missing_rate[1] - 1.5 * (missing_rate[2] - missing_rate[1]),
      #   missing_rate[number_of_missing_rate] + 1.5 * (missing_rate[2] -
      #                                                   missing_rate[1])
      # ))
    
    if(!legend){
      res <- res + theme(legend.position = "none")
    }
    
    if (label) {
      return(
        res + geom_dl(aes(label = methods),
                method =list(dl.trans(x = x+.3), "last.bumpup")) +
          geom_dl(aes(label = methods),
                  method =list(dl.trans(x = x-.3), "first.bumpup")) +
          theme(
            title = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12)
          )
      )
    } else{
      return(
        res + theme(
          title = element_text(
            size = 12,
            face = "bold",
            colour = "white"
          ),
          axis.title = element_text(
            size = 12,
            face = "bold",
            colour = "white"
          ),
          axis.text.x = element_text(color = "white"),
          axis.text.y = element_text(color = "white"),
          legend.text = element_text(size = 12, colour = "white")
        )
      )
    }
  }
