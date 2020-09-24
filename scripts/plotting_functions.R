# Plotting helper functions -------

#' Figure Theme
#' 
#' Theme for figures with frequently used formatting instructions.
#' @param legend whether to show the legend
#' @param grid whether to show the grid
#' @param plot_margin margins for c(top, right, bottom, left) in mm
#' @param text_size font size for all text on plot 
#' @param axis_text_size font size for the axes' text (define only if different from text_size)
#' @param axis_x_rotated whether to rotate the x axis labels
theme_figure <- function(legend = TRUE, grid = TRUE, plot_margin = c(1, 1, 1, 1), 
                         text_size = 20, axis_text_size = NULL, axis_x_rotate = 0) {
  the_theme <- theme_bw() + 
    theme(text = element_text(size = text_size),
          plot.background = element_blank(), panel.background = element_blank(),
          panel.border = element_rect(color="black", size=1), 
          strip.background = element_rect(color="black", linetype = 1),
          plot.margin = unit(plot_margin, "mm")
    )
  # adjust grid
  if(!grid)
    the_theme <- the_theme + theme(panel.grid = element_blank())
  else
    the_theme <- the_theme + theme(panel.grid.minor = element_blank())
  # adjust legend
  if (!legend)
    the_theme <- the_theme + theme(legend.position = "none")
  # overwrite axis text size if provided
  if (!is.null(axis_text_size))
    the_theme <- the_theme + 
      theme(axis.text = element_text(size = axis_text_size)) 
  # axis rotation
  if (axis_x_rotate != 0) {
    the_theme <- the_theme + 
      theme(axis.text.x = element_text(angle = axis_x_rotate, vjust = 0.5, hjust = 1))
  }
  return(the_theme)
}

#' Latex labeller
#' 
#' Latex labeller for ggplot that will interpret latex equations correctly (i.e. anything between $$). 
#' Works for both the \code{labels} parameter of discrete ggplot2 scales as well as the \code{labeller} of facets.
latex_labeller <- function(labels, ...) {
  
  require("dplyr")
  require("tidyr")
  require("purrr")
  require("latex2exp")
  
  # figure out if we're in a scale or facet labeller
  facet_labels <- is(labels, "data.frame")
  if (!facet_labels) labels <- tibble(..x.. = as.character(labels))
  
  # gather labels
  labels <- labels %>% 
    # add position info
    mutate(pos = row_number()) %>% 
    # gather labels
    mutate_if(is.factor, as.character) %>% 
    gather(var, val, -pos) %>% as_tibble() 
  
  # convert latex to expression
  labels <- labels %>% 
    mutate(
      val = map(val, ~latex2exp::TeX(.x))
    )
  
  # spread data frame again
  labels <- labels %>% 
    filter(!is.na(pos)) %>% 
    spread(var, val) %>% 
    select(-pos)
  
  # return appropriate value for labeller
  if (facet_labels) return(labels)
  else return(labels$..x..)
}
class(latex_labeller) <- c("function", "labeller")




# Edits to egg package to keep strips

#' tag_facet
#'
#' @description Adds a dummy text layer to a ggplot to label facets and sets facet strips to blank. 
#' This is the typical formatting for some journals that consider facets as subfigures 
#' and want to minimise margins around figures.
#' @param p ggplot
#' @param open opening character, default: (
#' @param close  closing character, default: )
#' @param tag_pool character vector to pick tags from
#' @param x x position within panel, default: -Inf
#' @param y y position within panel, default: Inf
#' @param hjust hjust
#' @param vjust vjust
#' @param fontface fontface
#' @param family font family
#' @param ... further arguments passed to geom_text layer
#'
#' @return plot with facet strips removed and replaced by in-panel tags 
#' @importFrom ggplot2 geom_text ggplot_build theme element_blank aes_string
#' @export
#' @examples
#' library(ggplot2)
#' mydf = data.frame(
#'   x = 1:90,
#'   y = rnorm(90),
#'   red = rep(letters[1:3], 30),
#'   blue = c(rep(1, 30), rep(2, 30), rep(3, 30)))
#' 
#' p <- ggplot(mydf) +
#'   geom_point(aes(x = x, y = y)) +
#'   facet_wrap(
#'     ~ red + blue)
#' tag_facet(p)

tag_facet_keep_strip <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE)
}


#' tag_facet_outside
#'
#' @description Adds a dummy text layer to a ggplot to label facets and sets facet strips to blank. 
#' This is the typical formatting for some journals that consider facets as subfigures 
#' and want to minimise margins around figures.
#' @param p ggplot
#' @param open opening character, default: (
#' @param close  closing character, default: )
#' @param tag_fun_top labelling function 
#' @param tag_fun_right labelling function
#' @param x x position within cell
#' @param y y position within cell
#' @param hjust hjust
#' @param vjust vjust
#' @param fontface fontface
#' @param family font family
#' @param draw logical: draw the resulting gtable
#' @param ... further arguments passed to geom_text layer
#' @return plot with facet strips removed and replaced by in-panel tags 
#' @importFrom ggplot2 ggplot_gtable geom_text ggplot_build theme element_blank 
#' @importFrom utils as.roman
#' @export
#' @examples
#' library(ggplot2)
#' d = data.frame(
#'   x = 1:90,
#'   y = rnorm(90),
#'   red = rep(letters[1:3], 30),
#'   blue = c(rep(1, 30), rep(2, 30), rep(3, 30)))
#' 
#' p <- ggplot(d) +
#'   geom_point(aes(x = x, y = y)) +
#'   facet_grid(red ~ blue)
#'   
#' tag_facet_outside(p)
#' 
tag_facet_outside_keep_strip <-  function(p, open=c("(",""), close = c(")","."),
                               tag_fun_top = function(i) letters[i],
                               tag_fun_right = utils::as.roman,
                               x = c(0,0), y = c(0.5, 1),
                               hjust = c(0,0), vjust = c(0.5,1), 
                               fontface = c(2,2), family="", draw = TRUE, ...){
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  
  tags_top <- paste0(open[1],tag_fun_top(unique(lay$COL)),close[1])
  tags_right <- paste0(open[2],tag_fun_right(unique(lay$ROW)),close[2])
  
  tl <- lapply(tags_top, grid::textGrob, x=x[1], y=y[1],
               hjust=hjust[1], vjust=vjust[1], 
               gp=grid::gpar(fontface=fontface[1], fontfamily = family, ...))
  rl <- lapply(tags_right, grid::textGrob, x=x[2], y=y[2],
               hjust=hjust[2], vjust=vjust[2], 
               gp=grid::gpar(fontface=fontface[2], fontfamily = family, ...))
  
  
  g <- ggplot_gtable(gb)
  g <- gtable::gtable_add_rows(g, grid::unit(1,"line"), pos = 0)
  l <- unique(g$layout[grepl("panel",g$layout$name), "l"])
  g <- gtable::gtable_add_grob(g, grobs = tl, t=1, l=l)
  
  wm <- do.call(grid::unit.pmax, lapply(rl, grid::grobWidth))
  g <- gtable::gtable_add_cols(g, wm, pos = max(l))
  t <- unique(g$layout[grepl("panel",g$layout$name), "t"])
  g <- gtable::gtable_add_grob(g, grobs = rl, t=t, l=max(l) + 1)
  g <- gtable::gtable_add_cols(g, unit(2,"mm"), pos = max(l))
  
  if(draw){
    grid::grid.newpage()
    grid::grid.draw(g)
  }
  invisible(g)
}



#' tag_facet_outside
#'
#' @description Adds a dummy text layer to a ggplot to label facets and sets facet strips to blank. 
#' This is the typical formatting for some journals that consider facets as subfigures 
#' and want to minimise margins around figures.
#' @param p ggplot
#' @param open opening character, default: (
#' @param close  closing character, default: )
#' @param tag_fun_top labelling function 
#' @param tag_fun_right labelling function
#' @param x x position within cell
#' @param y y position within cell
#' @param hjust hjust
#' @param vjust vjust
#' @param fontface fontface
#' @param family font family
#' @param draw logical: draw the resulting gtable
#' @param ... further arguments passed to geom_text layer
#' @return plot with facet strips removed and replaced by in-panel tags 
#' @importFrom ggplot2 ggplot_gtable geom_text ggplot_build theme element_blank 
#' @importFrom utils as.roman
#' @export
#' @examples
#' library(ggplot2)
#' d = data.frame(
#'   x = 1:90,
#'   y = rnorm(90),
#'   red = rep(letters[1:3], 30),
#'   blue = c(rep(1, 30), rep(2, 30), rep(3, 30)))
#' 
#' p <- ggplot(d) +
#'   geom_point(aes(x = x, y = y)) +
#'   facet_grid(red ~ blue)
#'   
#' tag_facet_outside(p)
#' 
tag_facet_outside_keep_strip_no_side_tags <-  function(p, open=c("(",""), close = c(")","."),
                                          tag_fun_top = function(i) letters[i],
                                          # tag_fun_right = utils::as.roman,
                                          x = c(0,0), y = c(0.5, 1),
                                          hjust = c(0,0), vjust = c(0.5,1), 
                                          fontface = c(2,2), family="", draw = TRUE, ...){
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  
  tags_top <- paste0(open[1],tag_fun_top(unique(lay$COL)),close[1])
  # tags_right <- paste0(open[2],tag_fun_right(unique(lay$ROW)),close[2])
  
  tl <- lapply(tags_top, grid::textGrob, x=x[1], y=y[1],
               hjust=hjust[1], vjust=vjust[1], 
               gp=grid::gpar(fontface=fontface[1], fontfamily = family, ...))
  # rl <- lapply(tags_right, grid::textGrob, x=x[2], y=y[2],
  #              hjust=hjust[2], vjust=vjust[2], 
  #              gp=grid::gpar(fontface=fontface[2], fontfamily = family, ...))
  
  
  g <- ggplot_gtable(gb)
  g <- gtable::gtable_add_rows(g, grid::unit(1,"line"), pos = 0)
  l <- unique(g$layout[grepl("panel",g$layout$name), "l"])
  g <- gtable::gtable_add_grob(g, grobs = tl, t=1, l=l)
  
  # wm <- do.call(grid::unit.pmax, lapply(rl, grid::grobWidth))
  # g <- gtable::gtable_add_cols(g, wm, pos = max(l))
  # t <- unique(g$layout[grepl("panel",g$layout$name), "t"])
  # g <- gtable::gtable_add_grob(g, grobs = rl, t=t, l=max(l) + 1)
  # g <- gtable::gtable_add_cols(g, unit(2,"mm"), pos = max(l))
  
  if(draw){
    grid::grid.newpage()
    grid::grid.draw(g)
  }
  invisible(g)
}



tag_facet2 <-  function(p, open="(", close = ")",
                        tag_pool = letters,
                        x = 0, y = 0.5,
                        hjust = 0, vjust = 0.5, 
                        fontface = 2, ...){
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  nm <- names(gb$layout$facet$params$rows)
  
  tags <- paste0(open,tag_pool[unique(lay$COL)],close)
  
  tl <- lapply(tags, grid::textGrob, x=x, y=y,
               hjust=hjust, vjust=vjust, gp=grid::gpar(fontface=fontface))
  
  g <- ggplot_gtable(gb)
  g <- gtable::gtable_add_rows(g, grid::unit(1,"line"), pos = 0)
  lm <- unique(g$layout[grepl("panel",g$layout$name), "l"])
  g <- gtable::gtable_add_grob(g, grobs = tl, t=1, l=lm)
  grid::grid.newpage()
  grid::grid.draw(g)
}
