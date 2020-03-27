#' @title theme_twg
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin plot title family, face, size and margin
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size facet label font family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font family, face and size
#' @param axis_title_just axis title font justificationk one of `[blmcrt]`
#' @param plot_margin plot margin (specify with [ggplot2::margin])
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks

theme_twg <- function(base_family="Myriad Pro Condensed", base_size = 11.5,
                      plot_title_family=base_family, plot_title_size = 18,
                      plot_title_face="bold", plot_title_margin = 4,
                      subtitle_family="Myriad Pro Condensed", subtitle_size = 12,
                      subtitle_face = "plain", subtitle_margin = 15,
                      strip_text_family = base_family, strip_text_size = 12,
                      strip_text_face = "plain",
                      caption_family = "Myriad Pro Condensed", caption_size = 9,
                      caption_face = "italic", caption_margin = 10,
                      axis_title_family = base_family, axis_title_size = 13,
                      axis_title_face = "plain", axis_title_just = "rt",
                      plot_margin = ggplot2::margin(10,10,10,10),
                      panel_spacing = ggplot2::unit(0.5, "lines"),
                      grid_col = "#cccccc", grid = TRUE, 
                      axis_col = "#cccccc",axis = FALSE, 
                      ticks = FALSE) {
  
  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)
  
  ret <- ret + ggplot2::theme(legend.background=ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.key=ggplot2::element_blank())
  
  if (inherits(grid, "character") | grid == TRUE) {
    
    ret <- ret + ggplot2::theme(panel.grid=ggplot2::element_line(color=grid_col, size=0.10))
    ret <- ret + ggplot2::theme(panel.grid.major=ggplot2::element_line(color=grid_col, size=0.1))
    ret <- ret + ggplot2::theme(panel.grid.minor=ggplot2::element_line(color=grid_col, size=0.1))
    
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.x=ggplot2::element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.y=ggplot2::element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.x=ggplot2::element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.y=ggplot2::element_blank())
    }
    
  } else {
    ret <- ret + ggplot2::theme(panel.grid=ggplot2::element_blank())
  }
  
  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + ggplot2::theme(axis.line=ggplot2::element_line(color="#2b2b2b", size=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x=ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x=ggplot2::element_line(color=axis_col, size=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y=ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y=ggplot2::element_line(color=axis_col, size=0.15))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x=ggplot2::element_line(color=axis_col, size=0.15))
      ret <- ret + ggplot2::theme(axis.line.y=ggplot2::element_line(color=axis_col, size=0.15))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line=ggplot2::element_blank())
  }
  
  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  } else {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_line(size=0.15))
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_line(size=0.15))
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_line(size=0.15))
    ret <- ret + ggplot2::theme(axis.ticks.length = grid::unit(5, "pt"))
  }
  
  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  
  ret <- ret + ggplot2::theme(axis.text.x=ggplot2::element_text(margin=ggplot2::margin(t=0)))
  ret <- ret + ggplot2::theme(axis.text.y=ggplot2::element_text(margin=ggplot2::margin(r=0)))
  ret <- ret + ggplot2::theme(axis.title=ggplot2::element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.x=ggplot2::element_text(hjust=xj, size=axis_title_size,
                                                                 family=axis_title_family, face=axis_title_face))
  ret <- ret + ggplot2::theme(axis.title.y=ggplot2::element_text(hjust=yj, size=axis_title_size,
                                                                 family=axis_title_family, face=axis_title_face))
  ret <- ret + ggplot2::theme(strip.text=ggplot2::element_text(hjust=0, size=strip_text_size,
                                                               face=strip_text_face, family=strip_text_family))
  ret <- ret + ggplot2::theme(panel.spacing.x=grid::unit(.5, "lines"))
  ret <- ret + ggplot2::theme(panel.spacing.y=grid::unit(.5, "lines"))
  ret <- ret + ggplot2::theme(plot.title=ggplot2::element_text(hjust=0, size=plot_title_size,
                                                               margin=ggplot2::margin(b=plot_title_margin),
                                                               family=plot_title_family, face=plot_title_face))
  ret <- ret + ggplot2::theme(plot.subtitle=ggplot2::element_text(hjust=0, size=subtitle_size,
                                                                  margin=ggplot2::margin(b=subtitle_margin),
                                                                  family=subtitle_family, face=subtitle_face))
  ret <- ret + ggplot2::theme(plot.caption=ggplot2::element_text(hjust=1, size=caption_size,
                                                                 margin=ggplot2::margin(t=caption_margin),
                                                                 family=caption_family, face=caption_face))
  ret <- ret + ggplot2::theme(plot.margin=plot_margin)
  
  ret <-  ret + ggplot2::theme(panel.spacing=panel_spacing)
  
  ret
  
}

