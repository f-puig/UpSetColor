UpSetColor <- function(data, mode = c('union','intersect','distinct')[3],
                       result = c('plot','list')[1],
                       set_order = c('decreasing','increasing','as.given', 'as.given.reverse')[2],
                       comb_order = NULL,
                       min_set_size = NULL, top_n_sets = NULL, min_comb_degree = NULL, max_comb_degree = NULL,
                       min_comb_size = NULL, max_comb_size = NULL, top_comb = NULL,
                       fill.1 = 'black', pch = 21,
                       fill.0='#cfcfcf', color.line.shape = '#A9A9A9',
                       comb_highlight = NULL, color.highlight = 'red',
                       numbers.size= NULL, size.line = 0.5, size.dot = 3,
                       color.bar.sets = 'black', color.bar.comb = 'black',
                       plot.title.size = 14,
                       size.comb.axis.y = 8, size.matrix.axis.y = 8, size.Set.axis.x = 8,
                       title.plot = 'Upset Plot',
                       title.comb.axis.y = 'Number of elements',
                       title.Set.axis.x = 'Set size',
                       title.matrix.y = 'Sets',
                       scale.Set.y.log10 = FALSE, title.hjust = 0.5,
                       heights = c(0.35, 0.65), widths = c(0.7,0.3),
                       color.stripes = c("#FFFFFF", "#F0F0F0"),
                       verbose = FALSE)
{
  
  #' UpSetColor - Visualization of intersecting sets with colored highlights
  #'
  #' Make the UpSet plot.
  #' @param data A list of sets (set of the list of sets is a vector), or a binary matrix or data frame (where rows are elements and columns are sets).
  #' @param mode The mode used to calculate the set combinations. Possible options are "distinct" (default), "intersect", and "union". In distinct mode, 1 means in that set and 0 means not in that set; in intersect mode: 1 means in that set and 0 is not taken into account; in union mode, as in the intersect mode, 1 means in that set and 0 is not taken into account. When there are multiple 1 in union mode, the relationship is OR. Under the union mode, the seven combination sets can overlap.
  #' @param result Possible options are "plot" (default), "list.plot" and "list.data". With "list.plot", a list with the 3 ggplot objects that compose the UpSet plot is returned. With "list.data", a list with the data used as input of the plots is returned.
  #' @param set_order Possible options are "increasing" (default), "decreasing", "as.given" and "as.given.reverse". It controls the order of the sets in the Set bar plot (bottom right) and the matrix plot (bottom left). "increasing" and "decreasing" sort the bars by their size, "as.given" produces no sorting. "as.given.reverse" reverses the set order.
  #' @param comb_order Vector of integers to customize manually the order of the combination bar plot (top).
  #' @param min_set_size The minimum number of elements needed in a set. If a set does not have more than this number, it will not be included in the UpSet plot. If NULL, no set filtering is performed according to this argument.
  #' @param top_n_sets Integer denoting that only the n-th largest sets will be used in the UpSet plot. If NULL, no set filtering is performed according to this argument.
  #' @param min_comb_degree The minimum number of sets compared. Set combinations involving less than this number of sets will not be shown in the UpSet plot.
  #' @param max_comb_degree The maximum number of sets compared. Set combinations involving more than this number of sets will not be shown in the UpSet plot.
  #' @param min_comb_size The minimum number of elements in the set combination. Set combinations below this number will not be shown in the UpSet plot.
  #' @param max_comb_size The maximum number of elements in the set combination. Set combinations above this number will not be shown in the UpSet plot.
  #' @param top_comb The number of element combinations to display in the combination bar plot (top) and the matrix plot (bottom left). The default is 20. To display all combinations, this argument must be set at 2^length(data)-1 (if data is a list) or 2^ncol(data)-1 (if data is a binary matrix or data frame).
  #' @param fill.1 String with the colors used to color the dots involved in every set combination. It must be of the same size as the number of displayed combinations in the upper plot, or be of length 1. If NULL, dots will be colored in black.
  #' @param pch Integer with the dot shape. To display a dot outline, use a value between 21 and 25.
  #' @param color.line.shape Color used for the dot online. It must be of length 1.
  #' @param fill.0 String with the color used to color the dots not involved in the set combinations. It is of length 1. Default is "#cfcfcf".
  #' @param comb_highlight The bar positions in the combination bar plot (top) that will be highlighted. The involved Sets will also be highlighted in the other plots. If NULL, none of the set combinations is highlighted.
  #' @param color.highlight Optional. Color used to highlight the set combinations. The default is red.
  #' @param numbers.size The font size for the bar labels in the combination plot (top). If NULL (default), the bar labels will not be displayed.
  #' @param size.line The width of the line connecting the "1" dots in the matrix plot. The default is 0.5.
  #' @param size.dot The dot size in the matrix plot. The default is 3.
  #' @param color.bar.sets String with the colors used to color the bars in the bottom-right plot. It must be of the same size as the total number of sets, or be of length 1. If NULL, the bars will be colored in black.
  #' @param color.bar.comb String with the colors used to color the bars in the top plot. It must be of the same size than the total number of displayed combinations, or be of length 1. If NULL, the bars will be colored in black.
  #' @param plot.title.size Title font size. Default is 14.
  #' @param size.comb.axis.y Y-axis title font size in the top plot. Default is 8.
  #' @param size.matrix.axis.y Y-axis title font size in the matrix plot. Default is 8.
  #' @param size.Set.axis.x X-axis title font size in the bottom-right plot. Default is 8.
  #' @param title.plot String with the plot title. Default is "UpSet plot".
  #' @param title.comb.axis.y String with the title used in the y-axis of the top plot. The default is "Number of elements".
  #' @param title.Set.axis.x String with the title used in the y-axis of the bottom-right plot. The default is "Set size".
  #' @param title.matrix.y String with the title used in the y-axis of the matrix plot. The default is "Sets".
  #' @param scale.Set.y.log10 If FALSE, the y-axis of the top plot is decimal. If TRUE, the scale is log10.
  #' @param title.hjust Parameter for horizontal adjustment of the main title. The default is 0.5.
  #' @param heights Plot heights in the layout. The default is c(0.35, 0.65).
  #' @param widths Plot widths in the layout. The default is c(0.7,0.3).
  #' @param color.stripes The colors used to fill the rectangles in the background. The default is c("#FFFFFF", "#F0F0F0")
  #' @param verbose If TRUE, a progress bar and the spent time are shown.
  #' @return A plot (if results = 'plot'), a list of ggplot objects (if results = 'list.plot') or a list of data.frames (if result = 'list.data').
  #' @examples
  #' set.seed(123)
  #' lt = list(a = sample(letters, 5),
  #'          b = sample(letters, 10),
  #'          c = sample(letters, 15))
  #' UpSetColor(data = lt, top_comb=7)
  #' @export
  #'
  #'
  
  if(verbose){
    start_ini_time <- Sys.time()
    progress_bar = utils::txtProgressBar(min=0, max=80, style = 3, char = "=")
  }
  
  is_integer <- function(x){
    return(min(abs(c(x%%1, x%%1-1))) < 1e-5)
  }
  
  dec2bin <- function(n) {
    bin <- vector()
    if(n > 1) {
      bin <- dec2bin(as.integer(n/2))
    }
    bin <- append(n %% 2, bin)
    return(bin)
  }
  
  # Check input data
  if(is.matrix(data) || is.data.frame(data)){
    
    if(!(all(unique(unlist(data)) %in% c(0,1)))){
      stop("'data' provided is neither a binary matrix nor a binary data frame. 'data' should contain only 0 and 1 values.")
    }
    
    if(is.null(ncol(data))){
      colnames(data) <- 1:ncol(data)
    }
    
    data_list <- list()
    for(i in 1:ncol(data)){
      data_list[[colnames(data)[i] ]] <- which(data[,i] != 0)
    }
    data <- data_list
    rm(data_list)
  }
  
  if(!is.list(data)){
    stop("'data' provided is neither a binary matrix, nor a binary data frame, nor a list.")
  }
  
  if(!(mode %in% c('union','intersect','distinct'))){
    stop("'mode' must be 'union','intersect', or 'distinct'.")
  }
  
  if(!(result %in% c('plot','list.plot', 'list.data'))){
    stop("'result' must be 'plot', 'list.plot', or 'list.data'.")
  }
  
  # Extract original set names
  if(is.null(names(data))){
    original_names <- 1:length(data)
  } else {
    original_names <- names(data)
  }
  
  # Filter lists if desired
  if(!is.null(min_set_size)){
    lengths_data <- lengths(data)
    pos <- which(lengths_data < min_set_size)
    pos <- rev(pos)
    for(i in pos){
      data[[i]] <- NULL
    }
    rm(lengths_data,pos)
  }
  
  if(!is.null(top_n_sets)){
    
    if(!is_integer(top_n_sets) || top_n_sets < 1){
      stop("'top_n_sets' should be a positive integer.")
    }
    
    lengths_data <- lengths(data)
    pos <- order(lengths_data, decreasing = TRUE)[1:top_n_sets]
    pos <- rev(setdiff(1:length(data),pos))
    for(i in pos){
      data[[i]] <- NULL
    }
    rm(lengths_data,pos)
  }
  
  if(is.null(comb_order) && is.null(top_comb)){
    if(length(data) < 4) {
      top_comb <- 2^length(data)-1
    } else {
      top_comb <- 20
    }
    comb_order <- 1:top_comb
  } else if(is.null(top_comb) && !is.null(comb_order)){
    top_comb <- max(comb_order, na.rm = TRUE)
  } else if (!is.null(top_comb) && is.null(comb_order)) {
    comb_order <- 1:top_comb
  }
  
  if(length(comb_order) != top_comb){
    stop("'comb_order' should be NULL or should have the same length as top_comb.")
  }
  
  if(!(set_order %in% c('decreasing','increasing','as.given', 'as.given.reverse'))){
    stop("'set_order' must be 'decreasing','increasing', 'as.given' or 'as.given.reverse'.")
  }
  
  iter <- 2^(length(data))-1
  
  if(is.null(names(data))){
    names_data <- 1:length(data)
  } else {
    names_data <- names(data)
  }
  
  if(verbose){
    pieceBar <- floor(iter/80)
  }
  
  
  if(!is.null(min_comb_degree) && !is_integer(min_comb_degree)){
    if(min_comb_degree < 1){
      stop("'min_comb_degree' must be NULL or a positive integer.")
    }
    stop("'min_comb_degree' must be NULL or a positive integer.")
  } else if(is.null(min_comb_degree)){
    min_comb_degree <- 0
  }
  
  if(!is.null(max_comb_degree) && !is_integer(max_comb_degree)){
    if(max_comb_degree < 1){
      stop("'max_comb_degree' must be NULL or a positive integer.")
    }
    stop("'max_comb_degree' must be NULL or a positive integer.")
  } else if(is.null(max_comb_degree)){
    max_comb_degree <- length(data)
  }
  
  if(!is.null(min_comb_size) && !is_integer(min_comb_size)){
    if(min_comb_size < 1){
      stop("'min_comb_size' must be NULL or a positive integer.")
    }
    stop("'min_comb_size' must be NULL or a positive integer.")
  }
  
  if(!is.null(max_comb_size) && !is_integer(max_comb_size)){
    if(max_comb_size < 1){
      stop("'max_comb_size' must be NULL or a positive integer.")
    } else if(max_comb_size > length(unique(unlist(data)))){
      stop("'max_comb_size' cannot exceed the number of unique elements across all sets.")
    }
    stop("'max_comb_size' must be NULL or a positive integer.")
  }
  
  # Calculate the combinations
  comb_size <- vector()
  comb_degree <- vector()
  it <- sets <- combination <- vector()
  for(i in 1:iter){
    
    # To check that the code progresses
    if(verbose){
      
      if(i %% 2*pieceBar == 0){
        utils::setTxtProgressBar(progress_bar, value = i/pieceBar)
      }
      
    }
    
    ibin <- which(dec2bin(i) == 1) # Find which are 1
    if(length(ibin) < min_comb_degree || length(ibin) > max_comb_degree){
      next
    }
    
    it <- append(it,i) # The iteration number
    comb_degree <- append(comb_degree,length(ibin))
    sets <- append(sets, names_data[ibin])
    combination <- append(combination, rep(i, length(ibin)))
    
    if(mode == 'union'){
      x_list <- vector()
      for(j in 1:length(ibin)){
        x_list <- append(x_list, data[[ ibin[j] ]])
      }
      comb_size <- append(comb_size, length(unique(x_list)))
    } else if(mode == 'intersect'){
      for(j in 1:length(ibin)){
        if(j == 1){
          x_list <- data[[ ibin[j] ]]
        } else {
          x_list <- intersect(x_list, data[[ ibin[j] ]])
        }
      }
      comb_size <- append(comb_size, length(unique(x_list)))
    } else { # Distinct: everything that is in common between 1 and different from what is common in 0.
      x_list <- vector()
      # First determine everything that is common
      for(j in 1:length(ibin)){
        if(j == 1){
          x_list <- data[[ ibin[j] ]]
        } else {
          x_list <- intersect(x_list, data[[ ibin[j] ]])
        }
      }
      # Then everything that is missing
      ibin0 <- setdiff(1:length(data),ibin) # Find which are 0
      if(length(ibin0) != 0){
        x_list0 <- vector()
        # First determine everything that is common
        for(j in 1:length(ibin0)){
          if(j == 1){
            x_list0 <- data[[ ibin0[j] ]]
          } else {
            x_list0 <- union(x_list0, data[[ ibin0[j] ]])
          }
        }
        comb_size <- append(comb_size, length(setdiff(x_list,x_list0)))
      } else {
        comb_size <- append(comb_size, length(x_list))
      }
      
    }
  }
  df_comb_size <- data.frame(combination = it,
                             comb_degree = comb_degree,
                             comb_size = comb_size)
  df_combination <- data.frame(combination = combination,
                               sets = sets)
  
  # Now filter combinations with min_comb_size and max_comb_size
  if(!is.null(min_comb_size)){
    df_comb_size <- df_comb_size[df_comb_size$comb_size >= min_comb_size,]
  }
  if(!is.null(max_comb_size)){
    df_comb_size <- df_comb_size[df_comb_size$comb_size <= max_comb_size,]
  }
  
  df_comb_size <- df_comb_size[order(df_comb_size$comb_size, decreasing = TRUE),]
  if(nrow(df_comb_size) < top_comb){
    warning("There are less set combinations than 'top_comb'. All set combinations are shown")
    if(!is.null(comb_order)){
      comb_order <- 1:nrow(df_comb_size)
    }
    top_comb <- nrow(df_comb_size)
  } else {
    df_comb_size <- df_comb_size[1:top_comb,]
  }
  
  if(!is.null(comb_highlight ) && max(comb_highlight) > top_comb){
    if(all(comb_highlight) > max(comb_highlight)){
      comb_highlight <- NULL
    } else {
      comb_highlight <- comb_highlight[comb_highlight <= top_comb]
    }
  }
  
  if(is.null(comb_order)){
    df_comb_size <- df_comb_size[order(df_comb_size$comb_degree, df_comb_size$combination),]
    df_comb_size$combination <- factor(as.character(df_comb_size$combination),
                                       levels = unique(as.character(df_comb_size$combination)))
  } else {
    df_comb_size$combination <- factor(as.character(df_comb_size$combination),
                                       levels = unique(as.character(df_comb_size$combination))[comb_order])
  }
  
  df_combination <- df_combination[df_combination$combination %in% levels(df_comb_size$combination), ]
  df_combination$combination <- factor(df_combination$combination, levels = levels(df_comb_size$combination))
  
  if(set_order == "as.given"){
    df_combination$sets <- factor(df_combination$sets, levels = unique(df_combination$sets))
    df_sets <- data.frame(sets = unique(df_combination$sets),
                          sizes = lengths(data)[match(unique(df_combination$sets), names_data)])
  } else if (set_order == 'as.given.reverse'){
    df_combination$sets <- factor(df_combination$sets, levels = rev(unique(df_combination$sets)))
    df_sets <- data.frame(sets = unique(df_combination$sets),
                          sizes = lengths(data)[match(unique(df_combination$sets), names_data)])
  } else if (set_order == 'decreasing'){
    sets_kept <- unique(df_combination$sets)
    sizes <- lengths(data)[match(sets_kept, names_data)]
    df_combination$sets <- factor(df_combination$sets, levels = sets_kept[order(sizes, decreasing = TRUE)])
    df_sets <- data.frame(sets = factor(sets_kept, levels = levels(df_combination$sets)),
                          sizes = sizes)
  } else {
    sets_kept <- unique(df_combination$sets)
    sizes <- lengths(data)[match(sets_kept, names_data)]
    df_combination$sets <- factor(df_combination$sets, levels = sets_kept[order(sizes, decreasing = FALSE)])
    df_sets <- data.frame(sets = factor(sets_kept, levels = levels(df_combination$sets)),
                          sizes = sizes)
  }
  
  # Make rectangle stripes
  nr <- nrow(df_sets)
  df_sets$ymin <- c(1:nr)-0.5
  df_sets$ymax <- c(1:nr)+0.5
  df_sets$color <- color.stripes[(c(1:nr %% length(color.stripes)) + 1)]
  
  # Make lines
  perms <- as.character(df_comb_size$combination)
  
  start <- end <- vector()
  for(i in 1:length(perms)){
    start[i] <- min(as.numeric(df_combination$sets)[df_combination$combination == perms[i]])
    end[i] <- max(as.numeric(df_combination$sets)[df_combination$combination == perms[i]])
  }
  
  df_comb_size$start <- factor(levels(df_sets$sets)[start], levels = levels(df_sets$sets))
  df_comb_size$end <- factor(levels(df_sets$sets)[end], levels = levels(df_sets$sets))
  
  if(length(fill.1) == 1){
    fill.1 <- rep(fill.1, top_comb)
  } else if (is.null(fill.1)){
    fill.1 <- rep('black', top_comb)
  } else if(length(fill.1) != top_comb){
    stop("'fill.1' should be of length 1 or equal to top_comb.")
  }
  
  if(!is.null(color.highlight) && !is.null(comb_highlight)){
    to_high <- as.character(df_comb_size$combination[comb_highlight])
    fill.1[df_comb_size$combination %in% to_high] <- rep(color.highlight,
                                                         length(comb_highlight))
    to_high_set <- df_combination$sets[which(df_combination$combination %in% to_high)]
  }
  
  df_comb_size$fill.1 <- factor(fill.1) # Color line in matrix plot
  df_combination$fill.1 <- fill.1[match(df_combination$combination, df_comb_size$combination)]
  
  # Make empty points
  df_empty <- data.frame(combination = rep(levels(df_combination$combination),
                                           each = length(levels(df_combination$sets))),
                         sets = rep(levels(df_combination$sets),
                                    length(levels(df_combination$combination))))
  df_combination <- merge(df_empty, df_combination, all = TRUE)
  df_combination$fill.1[is.na(df_combination$fill.1)] <-fill.0
  df_combination$sets <- factor(df_combination$sets, levels = levels(df_sets$sets))
  df_combination$combination <- factor(df_combination$combination, levels = levels(df_comb_size$combination))
  df_combination$fill.1 <- factor(df_combination$fill.1,
                                  levels = c(levels(df_comb_size$fill.1),fill.0))
  
  # Make plots
  fig_bottom <- ggplot2::ggplot()
  
  for(i in 1:nrow(df_sets)){
    
    fig_bottom <-  fig_bottom + ggplot2::geom_rect(data = df_sets[i,],
                                                   ggplot2::aes(xmin = -Inf, xmax = +Inf,
                                                                ymin = ymin, ymax = ymax),
                                                   fill = df_sets$color[i])
  }
  
  fig_bottom <- fig_bottom +
    ggplot2::coord_cartesian(xlim = c(0, nrow(df_comb_size)+1))
  
  fig_bottom <- fig_bottom +
    ggplot2::geom_point(data = df_combination,
                        ggplot2::aes(x = as.numeric(combination),
                                     y = as.numeric(sets),
                                     fill = fill.1),
                        size = size.dot,
                        color = color.line.shape,
                        pch = pch,
                        show.legend = FALSE) +
    ggplot2::scale_fill_manual(breaks = levels(df_combination$fill.1),
                               values = levels(df_combination$fill.1))
  
  for(i in 1:nrow(df_comb_size)){
    
    fig_bottom <- fig_bottom + ggplot2::geom_segment(data = df_comb_size[i,],
                                                     ggplot2::aes(x = as.numeric(combination),
                                                                  xend = as.numeric(combination),
                                                                  y = as.numeric(start),
                                                                  yend = as.numeric(end)),
                                                     color = as.character(df_comb_size$fill.1[i]),
                                                     size = size.line, show.legend = FALSE)
  }
  
  fig_bottom <- fig_bottom +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = title.matrix.y) +
    ggplot2::scale_y_continuous(labels = levels(df_combination$sets),
                                breaks = 1:length(levels(df_combination$sets)),
                                limits = c(0.5,(nrow(df_sets)+0.5)),
                                expand = c(0,0)) +
    ggplot2::scale_x_continuous(limits = c(0,(nrow(df_combination)+1)), expand = c(0,0)) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = size.matrix.axis.y),
      plot.margin= ggplot2::margin(0, 0, 5.5, 5.5),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.title.x= ggplot2::element_blank(),
      axis.text.x= ggplot2::element_blank(),
      axis.ticks.x= ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
    )
  
  
  if(length(color.bar.comb) == 1){
    color.bar.comb <- rep(color.bar.comb, top_comb)
  } else if (is.null(color.bar.comb)){
    color.bar.comb <- rep('grey80', top_comb)
  } else if(length(color.bar.comb) != top_comb){
    stop("'color.bar.comb' should be of length 1 or equal to top_comb.")
  }
  if(!is.null(color.highlight) && !is.null(comb_highlight)){
    color.bar.comb[comb_highlight]<- rep(color.highlight, length(comb_highlight))
  }
  df_comb_size$color.bar <- factor(color.bar.comb)
  
  fig_top <- ggplot2::ggplot(data = df_comb_size, ggplot2::aes(x = as.numeric(combination), y = comb_size)) +
    ggplot2::geom_bar(stat = "identity", width = 0.7, ggplot2::aes(fill = color.bar), show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = levels(df_comb_size$color.bar)) +
    ggplot2::scale_x_continuous(limits = c(0,(nrow(df_comb_size)+1 )),
                                expand = c(0,0),
                                breaks = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = title.comb.axis.y,
                  title = if(!is.null(title.plot)){title.plot} else {NULL} ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = title.hjust,
                                         size = plot.title.size),
      axis.title.y = ggplot2::element_text(size = size.comb.axis.y),
      axis.text.y = ggplot2::element_text(size = size.comb.axis.y-2),
      axis.line =  ggplot2::element_line(colour = "gray0",
                                         size = 0.5, linetype = "solid"),
      plot.margin = ggplot2::margin(5.5, 0, 0, 5.5),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.title.x= ggplot2::element_blank(),
      axis.text.x= ggplot2::element_blank(),
      axis.ticks.x= ggplot2::element_blank(),
      axis.ticks.y= ggplot2::element_line(size = 0.5),
      panel.border = ggplot2::element_blank()
    )
  
  if(!is.null(numbers.size)){
    fig_top <- fig_top +
      ggplot2::geom_text(ggplot2::aes(label=comb_size), vjust=-0.25, size = numbers.size)
  }
  
  if(scale.Set.y.log10){
    fig_top <- fig_top + ggplot2::scale_y_continuous(trans='log10', expand = c(0,0),
                                                     limits = c(1, max(df_comb_size$comb_size*1.5, na.rm = TRUE)))
  } else {
    fig_top <- fig_top + ggplot2::scale_y_continuous(expand = c(0,0),
                                                     limits =  c(0, max(df_comb_size$comb_size*1.2, na.rm = TRUE)))
  }
  
  if(length(color.bar.sets) == 1){
    color.bar.sets <- rep(color.bar.sets, length(original_names))
  } else if (is.null(color.bar.sets)){
    color.bar.sets <- rep('grey80', length(original_names))
  } else if(length(color.bar.sets) != length(original_names)){
    stop("'color.bar.sets' should be of length 1 or equal to the original number of sets.")
  }
  
  df_sets$color_sets <- color.bar.sets[match(df_sets$sets, original_names)]
  
  if(!is.null(color.highlight) && !is.null(comb_highlight)){
    df_sets$color_sets[df_sets$sets %in% to_high_set] <- color.highlight
  }
  
  df_sets$color_sets <- factor(df_sets$color_sets)
  
  fig_right <- ggplot2::ggplot(data = df_sets, ggplot2::aes(x = as.numeric(sets),
                                                            y = sizes, fill = color_sets)) +
    ggplot2::geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = levels(df_sets$color_sets)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = title.Set.axis.x) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::scale_x_continuous(limits = c(0.5,(nrow(df_sets)+0.5)),
                                expand = c(0,0),
                                breaks = NULL) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = size.Set.axis.x),
      axis.text.x = ggplot2::element_text(size = size.Set.axis.x-2),
      axis.line =  ggplot2::element_line(size = 0.5, color = 'gray0'),
      plot.margin= ggplot2::margin(0, 5.5, 5.5, 0),
      axis.title.y= ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.text.y= ggplot2::element_blank(),
      axis.ticks.y= ggplot2::element_blank(),
      axis.ticks.x= ggplot2::element_line(size = 0.5),
      panel.border = ggplot2::element_blank()
    ) +
    ggplot2::coord_flip()
  
  void <-  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 1, y = 1, label = "") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin= ggplot2::margin(5.5, 5.5, 0, 0))
  
  if(result == 'plot'){
    
    egg::ggarrange(fig_top, void, fig_bottom, fig_right,
                   ncol = 2, nrow = 2,
                   heights = heights, widths = widths)
    
    if(verbose){
      start_end_time <- Sys.time()
      print(start_end_time -start_ini_time)
    }
    
  } else if (result == "list.plot") { # Return a list with all the plots
    xx <- list()
    xx[['Comb']] <- fig_top
    xx[['Matrix']] <- fig_bottom
    xx[['Set']] <- fig_right
    xx[['Blank']] <- void
    
    if(verbose){
      start_end_time <- Sys.time()
      print(sprintf("Spent time: %s seconds", (start_ini_time -start_end_time)/1000))
    }
    
    return(xx)
  } else if (result == 'list.data'){
    xx <- list()
    xx[['Comb']] <- df_comb_size
    xx[['Matrix']] <- df_combination
    xx[['Set']] <- df_sets
    return(xx)
  }
}
