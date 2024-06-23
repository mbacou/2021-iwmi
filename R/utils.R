#' `mblabs` 12-color palette
#'
#' Default named qualitative color palette with 12 hues derived from `mblabs`
#' Bootstrap theme.
#'
#' @keywords datasets
#' @name pal
#' @format A named character vector of hex color codes
#' @export
pal = c(
  cyan   = "#789494",
  teal   = "#445e64",
  dark   = "#222e32",
  pink   = "#aea39a",
  purple = "#98796f",
  indigo = "#9b59b6",
  blue   = "#3daee9",
  green  = "#27ae60",
  yellow = "#edcd37",
  orange = "#f67400",
  red    = "#da4453",
  light  = "#f9f9f9"
)


#' Color ramp based on `mblabs` theme colors
#'
#' Default qualitative color ramp derived from `mblabs` Bootstrap theme.
#'
#' @param x number of colors to interpolate
#' @inheritParams grDevices::colorRamp
#' @return A function to interpolate colors
#' @examples
#'
#' x <- rchisq(100, df=4)
#' hist(x, freq=FALSE, ylim=c(0, 0.2), col=pal(20), border=12)
#' hist(x, freq=FALSE, ylim=c(0, 0.2), col=pal(8), border=12)
#'
#' @export
mblabs.colors <- function(x, ...) colorRampPalette(unname(pal), ...)(x)


#' Apply graphic color palettes
#'
#' Sets `mblabs` default values to `thematic::thematic_on`, which modify
#' `lattice` and `ggplot` color palettes and fonts.
#'
#' @inheritParams thematic::thematic_on
#' @inheritDotParams thematic::thematic_on
#' @return A global graphic theme
#' @importFrom thematic thematic_on font_spec sequential_gradient
#' @examples
#' theme_labs_on()
#' lattice::show.settings()
#'
#' @export
theme_labs_on <- function(
  bg = "transparent",
  fg = "#090909",
  accent = pal[["teal"]],
  font = "Roboto Condensed",
  sequential = sequential_gradient(fg_weight=.5, bg_weight=.5, fg_low=FALSE),
  qualitative = mblabs.colors(12, alpha=.9),
  ...) thematic_on(
    bg, fg, accent, font=font_spec(font), sequential, qualitative, ...)


#' Themed `ggplot`
#'
#' A themed `ggplot` for `mblabs` website.
#'
#' @inheritParams ggplot2::ggplot
#' @param pos_x Position of x-axis (bottom or top)
#' @param pos_y Position of y-axis (right or left)
#' @param base_size Base font size in points
#' @param base_family Base font family
#' @param base_bg Plot, panel, legend background
#' @param base_color Color for text and line elements
#' @inheritDotParams ggplot2::theme
#'
#' @return A `ggplot2` object with new theme elements applied
#' @importFrom ggthemes theme_foundation
#' @import ggplot2
#' @examples
#' require(ggplot2)
#' theme_labs_on()
#'
#' gglabs(mtcars, aes(factor(carb), mpg, fill=factor(carb))) + geom_col() +
#'   guides(y=guide_none(), y.sec=guide_axis()) +
#'   labs(
#'     title = "My Long and Descriptive Plot Title",
#'     subtitle = "My very long subtitle with many units",
#'     caption = "My very long plot caption with many references.")
#'
#' gglabs(mtcars, aes(wt, mpg, color=carb)) +
#'   geom_point() + geom_smooth() +
#'   labs(title="My Title", subtitle="My Subtitle", caption="My plot caption")
#'
#' gglabs(mtcars, aes(wt, mpg, color=factor(cyl))) +
#'   geom_point() +
#'   labs(title="My Title", subtitle="My Subtitle", caption="My plot caption")
#'
#' @export
gglabs <- function(
  data = NULL,
  mapping = aes(),
  pos_x = c("bottom", 'top'),
  pos_y = c("right", "left"),
  base_size = 12,
  base_family = NULL, # use theme,
  base_bg = NULL, # use theme
  base_color = NULL, # use theme
  ...) {

  pos_x = match.arg(pos_x)
  pos_y = match.arg(pos_y)

  ggplot(data, mapping

  ) + guides(
    x = if(pos_x=="top") guide_none() else guide_axis(),
    x.sec = if(pos_x=="bottom") guide_none() else guide_axis(),
    y = if(pos_y=="right") guide_none() else guide_axis(),
    y.sec = if(pos_y=="left") guide_none() else guide_axis()

  ) + theme_foundation(
    base_size = base_size, base_family = base_family
  ) + theme(

    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    text = element_text(color=base_color, lineheight=.9),
    line = element_line(linetype=1, color=base_color),
    rect = element_rect(fill=NA, linetype=0, color=NA),
    plot.background = element_rect(fill=base_bg, color=NA),
    panel.background = element_rect(fill=base_bg, color=NA),
    panel.grid = element_line(color=NULL, linetype=3),
    panel.grid.major = element_line(color=base_color),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(),

    plot.title = element_text(
      face="plain", hjust=0, size=base_size*1.25),
    plot.subtitle = element_text(margin=margin(0,0,1,0, "lines"),
      face="plain", size=base_size, hjust=0),
    strip.text = element_text(
      face="bold", hjust=0, size=base_size),
    plot.caption = element_text(margin=margin(0,0,0,0, "lines"),
      size=base_size*0.8, hjust=0),

    axis.text = element_text(size=base_size, face="bold"),
    axis.text.y = element_text(color=NULL),
    axis.title.x = element_text(color=base_color, size=base_size, face="bold", hjust=1),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color=NULL),
    axis.ticks.length = unit(0.25, "lines"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color=NULL),
    axis.line = element_line(),
    axis.line.y = element_blank(),

    legend.background = element_rect(fill=NA, color=NA),
    legend.box.background = element_rect(fill=NA, color=NA),
    legend.title = element_text(size=base_size*0.9, hjust=0),
    legend.margin = margin(0,0,0,0.5, "lines"),
    legend.key.size = unit(.8, "lines"),
    legend.text = element_text(base_size*0.85),
    legend.position="right", legend.justification="left",
    legend.direction="vertical",
    ...
  )
}


#' Format percentages
#'
#' Fix scales::percent() for NA values
#'
#' @param x A numeric vector to format
#' @inheritDotParams scales::number_format
#' @return formatted character vector
#' @importFrom scales percent
#' @importFrom data.table fifelse
#' @export
pct <- function(x, ...) fifelse(is.na(x) | !is.numeric(x), "--", percent(x, ...))


#' Format currencies
#'
#' Fix scales::comma() for NA values
#'
#' @param x A numeric vector to format
#' @inheritDotParams scales::number_format
#' @return formatted character vector
#' @importFrom scales comma
#' @importFrom data.table fifelse
#' @export
curr <- function(x, ...) fifelse(is.na(x) | !is.numeric(x), "--", comma(x, ...))

#' Format yday dates
#'
#' @param x A numeric vector to format
#' @inheritParams base::format.Date
#' @return formatted character vector
#' @export
yday_to_date <- function(x, format="%m-%d") format(as.Date(as.character(x), format="%j"), format)

