
process_image <- function(image_file_name, k_list) {
  #   
  # #   process_image computes the k-means clustering for the pixels of an image by RGB values and matches the nearest DMC thread colour
  # # 
  # # Input:
  # # - image_file_name: a PNG or JPEG image.
  # # - k_list: the number of centres in the clustering
  # # 
  # # Output:
  # # - cluster_info: a tibble where k_list is the k number of centers in the clustering, kclust is the output of the original k-means clustering, tidied is a summary of the k-means clustering on a per-cluster level, glanced is the single row summary, augmented is the classifications added to the original data set, and colour is the RGB with its corresponding matched DMC thread colour
  # # 
  # # Example:
  # # image = "image_location.jpg"
  # # k = c(2,6,14)
  # # my_cluster <- process_image(image, k)
  # 
  #   
  set.seed(314)
  if(!require(imager)) {
    stop("The imager packages must be installed. Run install.packages(\"imager\") and then try again.")
  }
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  if(!require(dmc)) {
    stop("The dmc packages must be installed. Run install.packages(\"dmc\") and then try again.")
  }
  
  #load image
  im = load.image(image_file_name)
  
  #tidy the data
  tidy_dat = as.data.frame(im, wide="c") %>%
    rename(R=c.1, G=c.2, B=c.3)
  
  #only working with colours so remove the pixel coordinates for k-means clustering
  im_dat = select(tidy_dat, c(-x,-y))
  
  #k-means clustering and RGB 
  kclusts = 
    tibble(k_list) %>%
    mutate(
      kclust = map(k_list, ~kmeans(x = im_dat, centers = .x, nstart = 4)),
      tidied = map(kclust, tidy),
      glanced = map(kclust, glance),
      augmented = map(kclust, augment, tidy_dat),
      hex_colour = map(tidied , ~rgb(.x))
    )
  
  #matched DMC colours
  cluster_info = kclusts %>%
    unnest(cols = c(hex_colour)) %>%
    mutate(dmc_col = map(hex_colour, ~dmc(.x))) %>%
    unnest(dmc_col) %>%
    group_by(k_list) %>%
    nest(colour = c(hex_colour, dmc, name, hex, red, green, blue))
  
  return(cluster_info)
  
}







scree_plot <- function(cluster_info) {
  
  # 
  # # scree_plot produces a plot of the k clusters versus the total within sum of squares
  # # 
  # # Input:
  # # - cluster_info: a tibble where k_list is the k number of clusters to use for k-means clustering, kclust is the output of the original k-means 
  # # clustering, tidied is a summary of the k-means clustering on a per-cluster level, glanced is the single row summary, augmented is the 
  # # classifications added # # to the original data set, and colour is the RGB with its corresponding matched DMC thread colour
  # # 
  # # Output:
  # # - a ggplot object of the k clusters versus the total within sum of squares; select the optimal number of clusters by the "elbow rule", that is, # # the bend on the plot which indicates that additional clusters beyond this kth cluster have little value
  # # 
  # # Example:
  # # image = "image_location.jpg"
  # # k = c(2,6,14)
  # # my_cluster <- process_image(image, k)
  # # scree_plot(my_clusters)
  #
  
  
  clusterings = cluster_info %>%
    unnest(cols=c(glanced))
  
  return(ggplot(clusterings, aes(k_list, tot.withinss)) +
           geom_line() + 
           geom_point() + labs(title = "Optimal Number of Clusters", x = "Number of Clusters k", y="Total Within-Cluster Sum of Squares", caption = "Scree Plot for Different Cluster Sizes") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                                                                                                                                                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))
}







colour_strips <- function(cluster_info) {
  
  #   
  # # colour_strips produces colour strips with the DMC colour closest to the cluster center colour for each k number of clusters
  # # 
  # # Input:
  # # - cluster_info: a tibble where k_list is the k number of centres in the clustering, kclust is the output of the original k-means clustering, 
  # # tidied is a summary of the k-means clustering on a per-cluster level, glanced is the single row summary, augmented is the classifications added # # to the original data set, and colour is the RGB with its corresponding matched DMC thread colour
  # # 
  # # Output:
  # # - a ggplot object of colour strips with the DMC colour closest to the cluster center colour for each k number of centers
  # # 
  # # Example:
  # # image = "image_location.jpg"
  # # k = c(2,6,14)
  # # my_cluster <- process_image(image, k)
  # # my_colours <- colour_strips(my_cluster)
  #
  
  colourings = cluster_info %>%
    unnest(cols = c(colour))
  
  n_col = length(colourings$hex)
  
  rect_dat <- tibble(x1 = c(0:(n_col-1)), x2 = c(1:n_col), y1 = rep(0,n_col),
                     y2 =rep(1,n_col), colour = colourings$hex)
  
  return(rect_dat %>% ggplot()  +
           labs(caption = "Colour Strips of Matched DMC Thread") + 
           geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=colour), colour ="black")  + 
           scale_fill_manual(values = rect_dat$colour, labels=rect_dat$colour)+ theme_void() + theme(legend.position = "bottom") +
           facet_wrap(~colourings$k_list, scales = "free", strip.position = "bottom", nrow=length(unique(colourings$k_list)))) 
  
}




change_resolution <- function(image_df, x_size)
{
  ## change_resolution(image_df, x_size) subsamples an image to produce
  ## a lower resolution image. Any non-coordinate columns in the data
  ## frame are summarized with their most common value in the larger
  ## grid cell.
  ##
  ## Input:
  ## - image_df: A data frame in wide format. The x-coordinate column MUST
  ##             be named 'x' and the y-coordinate column MUST be named 'y'.
  ##             Further columns have no naming restrictions.
  ## - x_size:   The number of cells in the x-direction. The number of cells
  ##             in the vertical direction will be computed to maintain the 
  ##             perspective. There is no guarantee that the exact number
  ##             of cells in the x-direction is x_size
  ##
  ## Output:
  ## - A data frame with the same column names as image_df, but with fewer 
  ##   entries that corresponds to the reduced resolution image.
  ##
  ## Example:
  ##   library(imager)
  ##   library(dplyr)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager') 
  ##   im <- load.image(fpath)
  # im_dat<- as.data.frame(im,wide = "c") %>% rename(R = c.1, G = c.2, B = c.3) %>%
  #          select(x,y,R,G,B)
  # agg_image <- change_resolution(im_dat, 50)
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  if(!require(dplyr)) {
    stop("The dplyr packages must be installed. Run install.packages(\"dplyr\") and then try again.")
  }
  set.seed=(314)
  sp_dat <- image_df 
  gridded(sp_dat) = ~x+y
  
  persp = (gridparameters(sp_dat)$cells.dim[2]/gridparameters(sp_dat)$cells.dim[1])
  y_size = floor(x_size*persp)
  orig_x_size = gridparameters(sp_dat)$cells.dim[1]
  orig_y_size = gridparameters(sp_dat)$cells.dim[2]
  
  x_res = ceiling(orig_x_size/x_size)
  y_res = ceiling(orig_y_size/y_size)
  
  gt = GridTopology(c(0.5,0.5), c(x_res, y_res),
                    c(floor(orig_x_size/x_res), floor(orig_y_size/y_res)))
  SG = SpatialGrid(gt)
  agg = aggregate(sp_dat, SG, function(x) names(which.max(table(x)))[1] )
  agg@grid@cellsize <- c(1,1)
  df <- agg %>% as.data.frame %>% rename(x = s1, y = s2)  %>% select(colnames(image_df))
  
  return(df)
  
}




make_pattern <- function(cluster_info, k, x_size, black_white, background_colour) {
  
  #   
  # #   make_pattern plots the cross stitched pattern with the associated thread colour and a guide grid for an image of a lowered resolution. The default parameters for black_white = FALSE and background_colour=NULL returns all the colours of the k-means clustered image of lowered resolution.
  # # 
  # # Input:
  # # - cluster_info: the output of process_image
  # # - k: the chosen cluster size
  # # - x_size: the (approximate) total number of possible stitches in the horizontal direction
  # # - black_white: (logical) print the pattern in black and white (TRUE) or colour (FALSE, default)
  # # - background_colour: the colour of the background, which should not be stitched in the pattern (default is to not have a colour)
  # # 
  # # Output:
  # # - a ggplot object of the cross stitched pattern with the associated thread colour and a guide grid for an image of a lowered resolution. The default parameters for black_white = FALSE and background_colour=NULL returns all the colours of the k-means clustered image of lowered resolution.
  # # 
  # # Example:
  # # image = "image_location.jpg"
  # # k = c(2,6,14)
  # # my_cluster <- process_image(image, k)
  # # my_pattern <- make_pattern(my_cluster, k=6, x_size=50, black_white = FALSE, background_colour = NULL)
  # # my_pattern_nobkgrnd <- make_pattern(my_cluster, k=6, x_size=50, black_white = FALSE, background_colour = "#FCB0B9")
  # # my_pattern_bw <- make_pattern(my_cluster, k=6, x_size=50, black_white = TRUE, background_colour = NULL)  
  # # my_pattern_nobkgrnd_bw <- make_pattern(my_cluster, k=6, x_size=50, black_white = TRUE, background_colour = "#FCB0B9")  
  set.seed=(314)
  
  selected_cluster = cluster_info %>%
    filter(k_list == k)
  
  assignments = selected_cluster %>%
    unnest(augmented) %>%
    rename(cluster = .cluster)
  
  lower_res = change_resolution(select(assignments, c(x,y,cluster)), x_size)
  
  cluster_to_colour = selected_cluster %>%
    unnest(col=c(tidied,colour))  
  
  im_frame <- tibble(k = cluster_to_colour$cluster, name = paste(cluster_to_colour$name, cluster_to_colour$dmc, sep="-"),
                     col = cluster_to_colour$hex)
  
  #conditional colour
  if(black_white == FALSE) cond_col = im_frame$col else cond_col = rep("black", length(im_frame$k))
  
  return(lower_res %>% ggplot(aes(x, y)) + aes(col=cluster, shape=cluster) + geom_point() +
           scale_y_reverse() +
           scale_colour_manual(values = cond_col,
                               label =  im_frame %>% select(name) %>% deframe) +
           scale_shape_manual(values = im_frame %>% select(k) %>% deframe,
                              label = im_frame %>% select(name) %>% deframe) +
           labs(x="", y="", color = "DMC Colour", shape="DMC Colour", caption="Cross-Stitch with DMC Thread Colour") +
           theme_bw() +
           theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "black", size=1), panel.grid.minor = element_blank(), axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())
  )
  
}
