## ----include = FALSE, message = FALSE-----------------------------------------
library("here")
library("tidyverse"); theme_set(theme_bw(9));
theme_update(panel.grid.minor = element_blank())
library("patchwork")
library("ggdensity")

knitr::opts_chunk$set(
  fig.align = 'center',
  cache = TRUE,
  fig.height = 3,
  fig.width = 5.5,
  fig.env = "widefigure", # defined in RJounrnal.sty
  fig.pos = "h!",
  echo = FALSE,
  comment = "##",
  # want raster images!
  dev = "png",
  dpi = 300,
  # for RJournal
  highlight = FALSE,
  background = "#FFFFFF"
)


knitr::knit_hooks$set(inline = function(x) {

  # deal with pipe
  if(x == "%>%") return("\\verb|%>%|")
  if(x == "%@%") return("\\verb|%@%|")
  if(x == "%*%") return("\\verb|%*%|")
  if(length(grep("%@%", x)) > 0) {
    strings <- strsplit(x, " %@% ", fixed = TRUE)[[1]]
    knitted_strings <- vapply(strings, inline, character(1))
    return(paste(knitted_strings, collapse = " \\verb|%@%| "))
  }
  if(length(grep("%>%", x)) > 0) {
    strings <- strsplit(x, " %>% ", fixed = TRUE)[[1]]
    knitted_strings <- vapply(strings, inline, character(1))
    return(paste(knitted_strings, collapse = " \\verb|%>%| "))
  }

  # work
  out <- paste0("{\\tt ", highr:::hi_latex(x), "}")
  out <- gsub("_", "\\\\_", out)
  out <- gsub("\\^", "\\\\string^", out)
  out <- gsub("\\$", "\\\\$", out)
  out <- gsub("&", "\\\\&", out)
  out

})

function_placeholder <- function() {
  f <- function(...) invisible()
  invisible(f)
}

object_placeholder <- function() invisible(NULL)

# For bibtex
system(paste("biber", sub("\\.Rnw$", "", knitr::current_input())))


## ----comparing_plots, fig.height=4.25, fig.width=6, warning = FALSE, fig.cap = r"(Comparing various geoms on a bivariate standard normal sample of size $n = 2500$)"----
set.seed(1)

n <- 2500

df <- tibble( "x" = rnorm(n), "y" = rnorm(n) )

p11 <- ggplot(df, aes(x, y)) +
  geom_point(alpha = 1, size = .3) +
  labs(title = "geom_point()")

p12 <- ggplot(df, aes(x, y)) +
  geom_density_2d_filled() +
  scale_x_continuous(lim = c(-3.5, 3.5)) +
  scale_y_continuous(lim = c(-3.5, 3.5)) +
  guides(
    fill = guide_legend(ncol = 1, title.position = "top", byrow = FALSE)
  ) +
  labs(title = "geom_density_2d_filled()")

p21 <- ggplot(df, aes(x, y)) +
    geom_density_2d() +
  labs(title = "geom_density_2d()")

p22 <- ggplot(df, aes(x, y)) +
    geom_hdr() +
  labs(title = "geom_hdr()")

library("patchwork")


(p11 + p12) / (p21 + p22) &
  coord_fixed(xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5), expand = FALSE) &
  theme(
    axis.title = element_blank(),
    # plot.title = element_text(family = "monospace", size = 7),
    plot.title = element_text(size = 7),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 5),
    legend.justification = c(0,.5),
    legend.margin = margin(-5, 0, 0, 0),
    legend.key.size = unit(.4, "cm")
  )


## ----comparing_plots-2, fig.height=2.5, warning=FALSE, fig.cap = "\\code{geom\\_density\\_2d\\_filled()} and \\code{geom\\_hdr()} (white) from Figure~\\ref{fig:comparing_plots}"----
#  superimposed with the true contours (pink)
f <- function(x, y) dnorm(x)*dnorm(y)
ggplot(df, aes(x, y)) +
  geom_density2d_filled() +
  # geom_hdr_lines(color = "red", alpha = 1, size = .5) +
  # geomtextpath::geom_textdensity2d(
  #   aes(level = after_stat(probs)),
  #   stat = "hdr_lines_fun", fun = f,
  #   alpha = 1, color = "pink", size = 1.5, linetype = 2
  # ) +
  # geom_hdr_lines_fun(fun = f,
  #   alpha = 1, color = "pink", size = .1, linetype = 1
  # ) +
  geomtextpath::geom_textdensity2d(
    aes(level = after_stat(probs)),
    stat = "hdr_lines", alpha = 1, color = "white", size = 2.2
  ) +
  scale_x_continuous(lim = c(-3.5, 3.5)) +
  scale_y_continuous(lim = c(-3.5, 3.5)) +
  coord_fixed(xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5), expand = FALSE) +
  scale_fill_viridis_d(guide = "none") +
  theme(
    axis.title = element_blank()
  )


## ----hdr-1d-1, fig.height=4.5, fig.width=7, fig.cap = r"($p_{f}(c)$ and $\mc{R}_{f}(c)$ are computed by discretizing the density $f(x)$, determining the probabilities with densities above $c$, and constructing HDRs as unions of intervals)", position = "h!"----
N <- 17
c <- .23
f <- dnorm

grid <- tibble(
  x = seq(-3.5, 3.6, length.out = N),
  f = f(x),
  p = f / sum(f)
) |>
  arrange(desc(f)) |>
  mutate(a = cumsum(p)) |>
  arrange(x)

grid <- tibble(
  x = seq(-3.5, 3.6, length.out = N),
  f = dnorm(x),
  p = f / sum(f)
) |>
  arrange(desc(f)) |>
  mutate(a = cumsum(p)) |>
  arrange(x)

de <- grid$x |> diff() |> mean() # these will be the same up to numerical error

p1 <- ggplot(grid, aes(x, f)) +
  # geom_hline(yintercept = 0, color = "gray5") +
  geom_function(fun = f, xlim = c(-3.5, 3.5), color = "gray20") +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = -.03, ymax = -.01, fill = f > c),
    data = grid, color = NA, size = .15, alpha = 0
  ) +
  scale_y_continuous(
    sec.axis = sec_axis("p", trans = ~ .x / sum(grid$f), labels = scales::label_percent())
  ) +
   scale_fill_manual(
     values = c("white", "gray75"),
     breaks = c(FALSE, TRUE),
     guide = "none"
   ) +
  # labs(title = "I") +
  theme_minimal(11) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y.left = element_text(angle = 0, vjust = .5),
    axis.text.y.right = element_blank(),
    axis.title.y.right = element_blank()
  )

p2 <- ggplot(grid, aes(x, f)) +
  # geom_hline(yintercept = 0, color = "gray5") +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = 0, ymax = f),
    data = grid, fill = "gray75", alpha = 1, color = "gray50"
  ) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = f), data = grid, linetype = 2, size = .25) +
  geom_function(fun = f, xlim = c(-3.5, 3.5), color = "gray20") +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = -.03, ymax = -.01, fill = f > c),
    data = grid, color = NA, size = .15, alpha = 0
  ) +
  geom_point() +
  scale_y_continuous(
    sec.axis = sec_axis("p", trans = ~ .x / sum(grid$f), labels = scales::label_percent())
  ) +
   scale_fill_manual(
     values = c("white", "gray75"),
     breaks = c(FALSE, TRUE),
     guide = "none"
   ) +
  # labs(title = "II") +
  theme_minimal(11) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y.right = element_text(angle = 0, vjust = .5),
    axis.text.y.left = element_blank(),
    axis.title.y.left = element_blank()
  )

p3 <- ggplot(grid, aes(x, f)) +
  # geom_hline(yintercept = 0, color = "gray5") +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = 0, ymax = f),
    data = grid, fill = "gray75", alpha = 1, color = "gray50"
  ) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = f), data = grid, linetype = 2, size = .25) +
  geom_hline(yintercept = c, color = "red") + # color = "gray40"
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = -.03, ymax = -.01, fill = f > c),
    data = grid, color = "white", size = .15, alpha = 0
  ) +
  geom_point() +
  scale_y_continuous(
    breaks = c(.0, .1, .2, c, .3, .4),
    labels = c(.0, .1, .2, "c", .3, .4),
    sec.axis = sec_axis("p", trans = ~ .x / sum(grid$f), labels = scales::label_percent())
  ) +
   scale_fill_manual(
     values = c("white", "gray75"),
     breaks = c(FALSE, TRUE),
     guide = "none"
   ) +
  # labs(title = "III") +
  theme_minimal(11) +
  theme(
    axis.title.y.left = element_text(angle = 0, vjust = .5),
    axis.text.y.right = element_blank(),
    axis.title.y.right = element_blank()
  )

p4 <- ggplot(grid, aes(x, f)) +
  # geom_hline(yintercept = 0, color = "gray5") +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = 0, ymax = f, , fill = f > c),
    data = grid |> filter(f > c), alpha = 1, color = "gray50"
  ) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = f), data = grid |> filter(f > c), linetype = 2, size = .25) +
  geom_hline(yintercept = c, color = "red") + # color = "gray40"
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = -.03, ymax = -.01, fill = f > c),
    data = grid, color = "gray5", size = .15
  ) +
  geom_point(data = grid |> filter(f > c)) +
  scale_y_continuous(
    breaks = c(.0, .1, .2, c, .3, .4),
    labels = c(.0, .1, .2, "c", .3, .4),
    sec.axis = sec_axis("p", trans = ~ .x / sum(grid$f), labels = scales::label_percent())
  ) +
   scale_fill_manual(
     values = c("white", "gray75"),
     breaks = c(FALSE, TRUE),
     guide = "none"
   ) +
  # labs(title = "IV") +
  theme_minimal(11) +
  theme(
    axis.title.y.right = element_text(angle = 0, vjust = .5),
    axis.text.y.left = element_blank(),
    axis.title.y.left = element_blank()
  )

(p1 + p2) / (p3 + p4) &
  theme(
    panel.grid.minor = element_blank()
  )


## ----hdr-1d-2, fig.height=2.75, fig.width=7, fig.cap = r"(As the mesh size $N$ grows, the HDR approximation improves for any $f(x)$. Here $N = 100$ and the 90\% HDR is illustrated.)", position = "h!"----
N <- 100
f <- function(x) {
  .90*dnorm(x, -.5, 1) + .10*dnorm(x, 2, .15)
}

alpha <- .10

grid <- tibble(
  x = seq(-3.5, 3.6, length.out = N),
  f = f(x),
  p = f / sum(f)
) |>
  arrange(desc(f)) |>
  mutate(
    a = cumsum(p),
    hdr = (a <= 1 - alpha),
    hdr = hdr | lag(hdr) # get one more, the first that overshoots
  ) |>
  arrange(x)

f_alpha <- grid |> filter(hdr) |> pull(f) |> min()
c <- f_alpha

de <- grid$x |> diff() |> mean() # these will be the same up to numerical error

p1 <- ggplot(grid, aes(x, f)) +
  geom_function(fun = f, xlim = c(-3.5, 3.5), color = "gray20", n = 501) +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = -.03, ymax = -.01, fill = hdr),
    data = grid, color = NA, size = .15, alpha = 0
  ) +
  geom_hline(yintercept = c, color = "gray40") +
  scale_y_continuous("f",
    breaks = c(.0, .1, .2, c, .3, .4),
    labels = c(.0, "", .2, expression(f[.10]), .3, .4),
    sec.axis = sec_axis("p", trans = ~ .x / sum(grid$f), labels = scales::label_percent())
  ) +
   scale_fill_manual(
     values = c("white", "gray75"),
     breaks = c(FALSE, TRUE),
     guide = "none"
   ) +
  theme_minimal(11) +
  theme(
    axis.title.y.left = element_text(angle = 0, vjust = .5),
    axis.text.y.right = element_blank(),
    axis.title.y.right = element_blank()
  )

p2 <- ggplot(grid, aes(x, f)) +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = 0, ymax = f),
    data = grid, fill = "gray75", alpha = 1, color = "gray50", size = .15
  ) +
  geom_hline(yintercept = c, color = "gray40") +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = -.03, ymax = -.01, fill = hdr),
    data = grid, color = "white", size = .15, alpha = 0
  ) +
  geom_point(size = .25) +
  scale_y_continuous(
    breaks = c(.0, .1, .2, c, .3, .4),
    labels = c(.0, .1, .2, expression(f[alpha]), .3, .4),
    sec.axis = sec_axis("p", trans = ~ .x / sum(grid$f), labels = scales::label_percent())
  ) +
   scale_fill_manual(
     values = c("white", "gray75"),
     breaks = c(FALSE, TRUE),
     guide = "none"
   ) +
  # labs(title = "III") +
  theme_minimal(11) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

p3 <- ggplot(grid, aes(x, f)) +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = 0, ymax = f, , fill = hdr),
    data = grid |> filter(hdr), alpha = 1, color = "gray50", size = .15
  ) +
  geom_hline(yintercept = c, color = "gray40") +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = -.03, ymax = -.01, fill = hdr),
    data = grid, color = "gray5", size = .15
  ) +
  geom_point(data = grid |> filter(hdr), size = .25) +
  scale_y_continuous(
    breaks = c(.0, .1, .2, c, .3, .4),
    labels = c(.0, .1, .2, expression(f[alpha]), .3, .4),
    sec.axis = sec_axis("p", trans = ~ .x / sum(grid$f), labels = scales::label_percent())
  ) +
   scale_fill_manual(
     values = c("white", "gray75"),
     breaks = c(FALSE, TRUE),
     guide = "none"
   ) +
  # labs(title = "IV") +
  theme_minimal(11) +
  theme(
    axis.title.y.right = element_text(angle = 0, vjust = .5),
    axis.text.y.left = element_blank(),
    axis.title.y.left = element_blank()
  )

(p1 + p2 + p3) &
  theme(
    panel.grid.minor = element_blank()
  )


## ----hdr-1d-3, fig.height=2.75, fig.width=7, fig.cap = r"(Computing several HDRs can be done with no added computational complexity, since the regions are nested. Here the 50\%, 80\%, and 90\% HDRs $\widehat{\mc{R}}_{\al}$ are illustrated using an estimated density based on $n = 1000$ draws.)", position = "h!", warning=FALSE----
N <- 100
f <- function(x) {
  .90*dnorm(x, -.5, 1) + .10*dnorm(x, 2, .15)
}


# sample n = 100 draws from f using rejection sampling.
set.seed(4)
n <- 1000
x_data <- vector(length = n)
count <- 0
M <- optimize(f, c(-3.5, 3.6), maximum = TRUE)$objective
repeat {
  x_prop <- runif(1, -3.5, 3.6)
  if (runif(1, 0, M) <= f(x_prop)) {
    x_data[count] <- x_prop
    count <- count + 1
  }
  if (count >= n) break
}
# hist(x_data, breaks = 100)

alpha <- .10

grid <- tibble(
  x = seq(-3.5, 3.6, length.out = N),
  f = f(x),
  fhat = density(x_data, from = -3.5, to = 3.6, n = N)$y,
  p = fhat / sum(fhat)
) |>
  arrange(desc(fhat)) |>
  mutate(
    a = cumsum(p),
    hdr50 = (a <= 1 - .50),
    hdr50 = hdr50 | lag(hdr50),
    hdr80 = (a <= 1 - .20),
    hdr80 = hdr80 | lag(hdr80) & !hdr50,
    hdr90 = (a <= 1 - .10),
    hdr90 = hdr90 | lag(hdr90) & !hdr50 & !hdr80,
    hdr = case_when(
      hdr50 ~ "hdr50",
      hdr80 ~ "hdr80",
      hdr90 ~ "hdr90",
      TRUE ~  "hdr100",
    ) |> factor(levels = c("hdr100", "hdr90", "hdr80", "hdr50"), ordered = TRUE)
  ) |>
  arrange(x)

f_50 <- grid |> filter(hdr50) |> pull(fhat) |> min()
f_80 <- grid |> filter(hdr80) |> pull(fhat) |> min()
f_90 <- grid |> filter(hdr90) |> pull(fhat) |> min()

de <- grid$x |> diff() |> mean() # these will be the same up to numerical error

p1 <- ggplot(grid, aes(x, fhat)) +
  geom_function(fun = f, xlim = c(-3.5, 3.5), color = "red", alpha = .25, n = 501) +
  geom_line(color = "gray20") +
  geom_point(
    aes(x, y),
    data = tibble(x = x_data, y = runif(n, -.03, -.01)),
    shape = 4, alpha = .25,
    inherit.aes = FALSE
  ) +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = -.03, ymax = -.01, fill = hdr),
    data = grid, color = NA, size = .15, alpha = 0
  ) +
  geom_hline(yintercept = f_50, color = "gray40", size = .25) +
  geom_hline(yintercept = f_80, color = "gray40", size = .25) +
  geom_hline(yintercept = f_90, color = "gray40", size = .25) +
  scale_y_continuous("f",
    breaks = c(.0, .1, .2, .3, .4, f_50, f_80, f_90),
    labels = c("", "", ".2", ".3", "", expression(hat(f)[.50]), expression(hat(f)[.20]), expression(hat(f)[.10])),
    sec.axis = sec_axis("p", trans = ~ .x / sum(grid$fhat), labels = scales::label_percent())
  ) +
  theme_minimal(11) +
  theme(
    axis.title.y.left = element_text(angle = 0, vjust = .5),
    axis.title.y.right = element_blank(),
    axis.text.y.right = element_blank(),
  )

p2 <- ggplot(grid, aes(x, fhat)) +
  geom_function(fun = f, xlim = c(-3.5, 3.5), color = NA, n = 501) +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = 0, ymax = fhat),
    data = grid, fill = "gray75", alpha = 1, color = "gray50", size = .15
  ) +
  geom_hline(yintercept = f_50, color = "gray40", size = .25) +
  geom_hline(yintercept = f_80, color = "gray40", size = .25) +
  geom_hline(yintercept = f_90, color = "gray40", size = .25) +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = -.03, ymax = -.01, fill = hdr),
    data = grid, color = "white", size = .15, alpha = 0
  ) +
  geom_point(size = .25) +
  theme_minimal(11) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

p3 <- ggplot(grid, aes(x, fhat)) +
  geom_function(fun = f, xlim = c(-3.5, 3.5), color = NA, n = 501) +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = 0, ymax = fhat, fill = hdr),
    data = grid |> filter(hdr != "hdr100"), alpha = .5, color = "gray50", size = .15
  ) +
  geom_hline(yintercept = f_50, color = "gray40", size = .25) +
  geom_hline(yintercept = f_80, color = "gray40", size = .25) +
  geom_hline(yintercept = f_90, color = "gray40", size = .25) +
  geom_rect(
    aes(xmin = x - de/2, xmax = x + de/2, ymin = -.03, ymax = -.01, fill = hdr),
    data = grid |> filter(hdr != "hdr100"), color = NA, size = .15
  ) +
  geom_point(data = grid |> filter(hdr != "hdr100"), size = .25) +
  scale_y_continuous(
    sec.axis = sec_axis("p", trans = ~ .x / sum(grid$fhat), labels = scales::label_percent())
  ) +
  theme_minimal(11) +
  theme(
    axis.title.y.right = element_text(angle = 0, vjust = .5),
    axis.text.y.left = element_blank(),
    axis.title.y.left = element_blank()
  )

(p1 + p2 + p3) &
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


## ----hdr-2d-2, fig.height=2, fig.cap = r"(Figure~\ref{fig:hdr-2d-1} from the plane perspective. \CRANpkg{ggdensity} constructs HDRS by applying the marching squares contouring algorithm to binary grid where $f_{ij} \geq f_{\al}$ provided by \code{isolines()} and \code{isobands()}.)", position = "h!"----
Nx <- Ny <- 11
f <- function(x,y) dnorm(x)*dnorm(y)

df <- expand_grid(
  x = seq(-4, 4, length.out = Nx),
  y = seq(-4, 4, length.out = Ny)
) |>
  rowwise() |>
  mutate( fxy = f(x,y) ) |>
  ungroup() |>
  mutate( p = fxy / sum(fxy) ) |>
  arrange(desc(p)) |>
  mutate(
    a = cumsum(p),
    i = (a <= .95), # this is the basic idea, but it's off by one
    i = i | lag(i), # this fixes it, i.e. captures first over .95
    f_alpha = min(fxy[i]),
    i_expanded = (fxy >= .999*f_alpha)
  )

p1 <- ggplot(df, aes(x, y)) +
  geom_point(aes(fill = i_expanded), shape = 21) +
  scale_fill_manual(values = c("white", "black")) +
  coord_equal() +
  theme(
    legend.position = "none"
  )

p2 <- ggplot(df, aes(x, y)) +
  geom_contour(aes(z = +i_expanded), breaks = .5, color = "red") +
  geom_point(aes(fill = i_expanded), shape = 21) +
  scale_fill_manual(values = c("white", "black")) +
  coord_equal() +
  theme(
    legend.position = "none"
  )

p3 <- ggplot(df, aes(x, y)) +
  geom_contour_filled(aes(z = +i_expanded), breaks = c(-1, .5, 2)) +
  geom_point(fill = "black", shape = 21, data = df |> filter(i_expanded)) +
  geom_point(fill = "white", shape = 21, data = df |> filter(!i_expanded)) +
  scale_fill_manual(values = c("gray90", "gray50")) +
  coord_equal() +
  theme(
    legend.position = "none"
  )

(p1 + p2 + p3) &
  theme(
    axis.title = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = NA, color = "gray50"),
    panel.grid = element_blank()
  )


## ----eval=FALSE, results='hide'-----------------------------------------------
## df_mm_data <- readr::read_csv("Paper/mathematica-data.csv",
##   col_names = c("x", "y")
## )
## df_mm_hist <- readr::read_csv("Paper/mathematica-fhat-hist.csv",
##   col_names = c("x", "y", "fhat", "p", "a", "i_expanded")
## )
## df_mm_kde <- readr::read_csv("Paper/mathematica-fhat-kde.csv",
##   col_names = c("x", "y", "fhat", "p", "a", "i_expanded"),
## )
##
##
## s <- seq(-4, 4, length.out = 26)
## s <- s[-26] + diff(s)[1]/2
## s <- c(-4 - .16, s, 4 + .16)
##
## (p1 <- ggplot() +
##   geom_contour_filled(aes(x, y, z = +i_expanded), data = df_mm_hist, breaks = c(-1, .5, 2)) +
##   geom_segment(aes(x, y, xend = xend, yend = yend), data = tibble(x = s, xend = s, y = -4-.16, yend = 4+.16), color = "gray70", size = .25) +
##   geom_segment(aes(x, y, xend = xend, yend = yend), data = tibble(y = s, yend = s, x = -4-.16, xend = 4+.16), color = "gray70", size = .25) +
##   # geom_point(aes(x, y),
##   #   fill = "black", shape = 21, size = .5,
##   #   data = df_mm_hist |> filter(i_expanded == 1)
##   # ) +
##   # geom_point(aes(x, y),
##   #   fill = "white", shape = 21, size = .5,
##   #   data = df_mm_hist |> filter(i_expanded == 0)
##   # ) +
##   # geom_point(aes(x, y),
##   #   color = "black", shape = 16, size = 1.75,
##   #   data = df_mm_hist |> filter(i_expanded == 1)
##   # ) +
##   # geom_point(aes(x, y),
##   #   color = "white", shape = 16, size = 1.75,
##   #   data = df_mm_hist |> filter(i_expanded == 0)
##   # ) +
##   geom_point(aes(x, y), data = df_mm_data, size = 2.5, shape = 21, fill = "cyan") +
##   # geom_contour(aes(x, y, z = +i_expanded), data = df_mm_hist, breaks = .5, color = "red") +
##   scale_x_continuous(expand = expansion(0)) +
##   scale_y_continuous(expand = expansion(0)) +
##   scale_fill_manual(values = c("gray90", "gray50")) +
##   coord_equal() +
##   labs("x" = NULL, "y" = NULL) +
##   theme(
##     text = element_text(size = 12),
##     axis.title = element_blank(),
##     legend.position = "none",
##     panel.background = element_rect(fill = NA, color = "gray50"),
##     panel.grid = element_blank()
##   ))
## ggsave(filename = "Paper/figures/fhat-hist-5.pdf", plot = p1, width = 4, height = 4)
##
##
## (p2 <- ggplot() +
##   geom_contour_filled(aes(x, y, z = +i_expanded), data = df_mm_kde, breaks = c(-1, .5, 2)) +
##   geom_segment(aes(x, y, xend = xend, yend = yend), data = tibble(x = s, xend = s, y = -4-.16, yend = 4+.16), color = "gray70", size = .25) +
##   geom_segment(aes(x, y, xend = xend, yend = yend), data = tibble(y = s, yend = s, x = -4-.16, xend = 4+.16), color = "gray70", size = .25) +
##   # geom_point(aes(x, y),
##   #   fill = "black", shape = 21, size = 3,
##   #   data = df_mm_kde |> filter(i_expanded == 1)
##   # ) +
##   # geom_point(aes(x, y),
##   #   fill = "white", shape = 21, size = 3,
##   #   data = df_mm_kde |> filter(i_expanded == 0)
##   # ) +
##   # geom_point(aes(x, y),
##   #   color = "black", shape = 16, size = 3,
##   #   data = df_mm_kde |> filter(i_expanded == 1)
##   # ) +
##   # geom_point(aes(x, y),
##   #   color = "white", shape = 16, size = 3,
##   #   data = df_mm_kde |> filter(i_expanded == 0)
##   # ) +
##   geom_point(aes(x, y), data = df_mm_data, size = 2.5, shape = 21, fill = "cyan") +
##   # geom_contour(aes(x, y, z = +i_expanded), data = df_mm_kde, breaks = .5, color = "red") +
##   scale_x_continuous(expand = expansion(0)) +
##   scale_y_continuous(expand = expansion(0)) +
##   scale_fill_manual(values = c("gray90", "gray50")) +
##   coord_equal() +
##   labs("x" = NULL, "y" = NULL) +
##   theme(
##     text = element_text(size = 12),
##     axis.title = element_blank(),
##     legend.position = "none",
##     panel.background = element_rect(fill = NA, color = "gray50"),
##     panel.grid = element_blank()
##   ))
## ggsave(filename = "Paper/figures/fhat-kde-5.pdf", plot = p2, width = 4, height = 4)
##
## # ggplot(df_mm_data, aes(x, y)) +
## #   ggdensity::geom_hdr(probs = .95, method = "histogram")
##
##
## # (p1 + p2)


## ----ex-methods, fig.pos = "p", out.width = "6.5in", out.height = "9.5in", fig.asp = 1.5, warning = FALSE, fig.cap = r"(Comparing HDRs obtained with different {\tt method} arguments to \code{geom\_hdr()}.)"----
library("purrr")
library("patchwork")

df_norm <- data.frame("x" = rnorm(5000), "y" = rnorm(5000))

df_norm_mix <- data.frame(
  "x" = rnorm(5000) + c(-1.5, 1.5),
  "y" = rnorm(5000) + c(1.5, -1.5)
)

df_exp <- data.frame("x" = rexp(5000, 1), "y" = rexp(5000, 1))

p_df <- function(df) {
  ggplot(df, aes(x, y)) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      axis.title = element_blank()
    )
}

p_row <- function(layer, title, ylabs = FALSE) {
  # p_title <- grid::textGrob(title, rot = 90, gp = grid::gpar(fontsize = 8, fontfamily = "monospace"))
  # p_title <- grid::textGrob(title, just = "right", gp = grid::gpar(fontsize = 9))
  # p_title <- grid::textGrob(title, gp = grid::gpar(fontsize = 7, fontfamily = "monospace"))
  p_title <- grid::textGrob(title, gp = grid::gpar(fontsize = 7))

  p_norm <- p_df(df_norm) +
    layer +
    coord_fixed(xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5))

  p_norm_mix <- p_df(df_norm_mix) +
    layer +
    coord_fixed(xlim = c(-4.5, 4.5), ylim = c(-4.5, 4.5))

  p_norm_exp <- p_df(df_exp) +
    layer + coord_fixed(xlim = c(-.25, 6), ylim = c(-.25, 6))

  list(p_title, p_norm, p_norm_mix, p_norm_exp)
}


geoms <- list(
  geom_point(size = .15, alpha = .6),
  # geom_density_2d_filled(),
  # extreme xlim, ylim ensure that HDRs aren't clipped
  geom_hdr(method = "kde", xlim = c(-10, 10), ylim = c(-10, 10)),
  geom_hdr(method = "mvnorm", xlim = c(-10, 10), ylim = c(-10, 10)),
  geom_hdr(method = "histogram"),
  geom_hdr(method = "freqpoly", xlim = c(-10, 10), ylim = c(-10, 10))
)

titles <- c(
  "",
  '"kde"',
  '"mvnorm"',
  '"histogram"',
  '"freqpoly"'
)

map2(geoms, titles, p_row) |>
  unlist(recursive = FALSE) |>
  wrap_plots(ncol = 4, widths = c(.35, 1, 1, 1), heights = 1)
  # wrap_plots(ncol = 4, widths = c(.05, 1, 1, 1), heights = 1)



## ----echo-hdr-2d-known_X, echo = TRUE, eval = FALSE---------------------------
## f_X <- function(x1, x2) dnorm(x1) * dgamma(x2, 5, 3)
## ggplot() + geom_hdr_fun(fun = f_X, xlim = c(-4, 4), ylim = c(0, 5))

## ----echo-hdr-2d-known-Y, echo = TRUE, eval = FALSE---------------------------
## f_Y <- function(y1, y2) exp(-1/(2 * .20^2) * (y1^2 + y2^2 - 1)^2)
## ggplot() +
##   geom_hdr_fun(fun = f_Y, normalized = FALSE, xlim = c(-4, 4), ylim = c(-4, 4)) +
##   coord_equal()


## ----eval-hdr-2d-known, fig.height=2.75, fig.width=7, fig.cap = r"(\code{geom\_hdr\_fun()} can be used to plot HDRs of normalized and un-normalized known PDFs.)"----
f_X <- function(x1, x2) dnorm(x1) * dgamma(x2, 5, 3)
p1 <- ggplot() + geom_hdr_fun(fun = f_X, xlim = c(-4, 4), ylim = c(0, 5))

f_Y <- function(y1, y2) exp(-1/(2 * .20^2) * (y1^2 + y2^2 - 1)^2)
p2 <- ggplot() +
  geom_hdr_fun(fun = f_Y, normalized = FALSE, xlim = c(-4, 4), ylim = c(-4, 4)) +
  coord_equal()

(p1 + p2) &
  theme(legend.position = "none")


## ----palmer_penguins_code, echo = TRUE, eval = FALSE--------------------------
## ggplot(penguins, aes(flipper_length_mm, bill_length_mm, fill = species)) +
##   geom_hdr_lines(aes(color = species), method = "mvnorm") +
##   geom_jitter(shape = 21)

## ----palmer_penguins, fig.height = 2.5, warning = FALSE, fig.cap = r"(Plotting HDRs for species in the \code{penguins} dataset using \code{geom\_hdr\_lines()})"----
library("palmerpenguins")

ggplot(penguins, aes(flipper_length_mm, bill_length_mm, fill = species)) +
  geom_hdr_lines(aes(color = species), method = "mvnorm", xlim = c(150, 250), ylim = c(25, 65)) +
  geom_point(position = "jitter", shape = 21, size  = 1, stroke  = .25, show.legend = FALSE) +
  scale_fill_brewer(palette = 7, type = "qual") +
  scale_color_brewer(palette = 7, type = "qual") +
  theme(legend.key.size = unit(.4, "cm"),
        legend.text = element_text(size = 9),
        panel.grid.major = element_blank())


## ----normality-penguins, eval = FALSE, echo = TRUE----------------------------
## penguins |>
##   filter(species == "Chinstrap") |>
##   ggplot(aes(flipper_length_mm, bill_length_mm)) +
##   geom_hdr() +
##   geom_hdr_lines(color = "red", method = "mvnorm") +
##   geom_jitter(color = "red")


## ----normality-wines, eval = FALSE, echo = TRUE-------------------------------
## ggplot(wines, aes(uronic, malic)) +
##   geom_hdr() +
##   geom_hdr_lines(method = "mvnorm", color = "red") +
##   geom_jitter(color = "red")


## ----normality_create_vis, fig.height = 2.25, fig.width = 5.7, echo = FALSE, fig.cap="Checking approximate normality of palmer penguin data (left) and wines data (right)"----
data(wines, package = "sn")
data(penguins, package = "palmerpenguins")

p1 <- penguins |>
  filter(species == "Chinstrap") |>
  ggplot(aes(flipper_length_mm, bill_length_mm)) +
  geom_hdr(xlim = c(160, 230), ylim = c(35, 65)) +
  geom_hdr_lines(xlim = c(160, 230), ylim = c(35, 65), color = "red", size = .5, method = "mvnorm") +
  geom_jitter(color = "red", size = .5)

p2 <- ggplot(wines, aes(uronic, malic)) +
  geom_hdr(xlim = c(-1, 2), ylim = c(-2, 8)) +
  geom_hdr_lines(xlim = c(-1, 2), ylim = c(-2, 8), method = "mvnorm", color = "red", size = .8) +
  geom_jitter(color = "red", size = .5)

(p1 + p2) &
  theme(legend.position = "none")


## ----code_misc_vis, echo = TRUE, eval = FALSE---------------------------------
## p <- ggplot(faithful, aes(eruptions, waiting))
##
## p + geom_hdr()
## p + geom_hdr_points()
## p + geom_hdr_rug()


## ----eval_misc_vis, echo = FALSE, fig.height = 2.25, fig.width = 5.7, fig.cap= "Alternative methods of visualizing HDRs with faithful data"----
p1 <- ggplot(faithful, aes(eruptions, waiting)) +
  geom_hdr(xlim = c(0, 7), ylim = c(30, 110)) +
  theme_bw(9)

p2 <- ggplot(faithful, aes(eruptions, waiting)) +
  geom_hdr_points(xlim = c(0, 7), ylim = c(30, 110), size = .5) +
  theme_bw(9) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

p3 <- ggplot(faithful, aes(eruptions, waiting)) +
  geom_point(size = .5) +
  geom_hdr_rug(length = unit(.3, "cm")) +
  theme_bw(9) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

(p1 + p2 + p3) &
  theme(legend.position = "none") &
  coord_cartesian(
    xlim = c(1, 6),
    ylim = c(40, 100)
  )


## ----code_misc_vis_2, echo = TRUE, eval = FALSE-------------------------------
## ggplot(faithful, aes(eruptions)) +
##   geom_density() +
##   geom_hdr_rug(aes(fill = after_stat(probs)), length = unit(.05, "npc"), alpha = 1) +
##   scale_fill_viridis_d(option = "magma", begin = .8, end = 0)


## ----eval_misc_vis_2, echo = FALSE, fig.height = 2.25, fig.cap= "Visualizing univariate HDRs"----
ggplot(faithful, aes(eruptions)) +
  geom_density() +
  geom_hdr_rug(aes(fill = after_stat(probs)), length = unit(.07, "npc"), alpha = 1) +
  scale_fill_viridis_d(option = "magma", begin = .8, end = 0) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(-1, 8)) +
  coord_cartesian(xlim = c(1, 6))


## ----exp_example-code, eval = FALSE, echo = TRUE------------------------------
## set.seed(1)
##
## df <- data.frame(x = rexp(100, 1), y = rexp(100, 1))
##
## # pdf for parametric density estimate
## f <- function(x, y, lambda) dexp(x, lambda[1]) * dexp(y, lambda[2])
##
## # estimate parameters governing joint pdf
## lambda_hat <- apply(df, 2, mean)
##
## ggplot(df, aes(x, y)) +
##   geom_hdr_fun(fun = f, args = list(lambda = lambda_hat)) +
##   geom_point(fill = "lightgreen", shape = 21) +
##   coord_fixed() +
##   scale_x_continuous(limits = c(0, 7)) +
##   scale_y_continuous(limits = c(0, 7))

## ----exp_example, fig.height=2.5, echo = FALSE, fig.cap = r"(Plotting HDRs of specified distributions with \code{geom\_hdr\_fun()})"----
set.seed(1)

df <- data.frame(x = rexp(100, 1), y = rexp(100, 1))

# pdf for parametric density estimate
f <- function(x, y, lambda) dexp(x, lambda[1]) * dexp(y, lambda[2])

# estimate parameters governing joint pdf
lambda_hat <- apply(df, 2, mean)

ggplot(df, aes(x, y)) +
  geom_hdr_fun(fun = f, args = list(lambda = lambda_hat)) +
  geom_point(size = 1, fill = "lightgreen", shape = 21) +
  coord_fixed() +
  scale_x_continuous(limits = c(0, 7)) +
  scale_y_continuous(limits = c(0, 7)) +
  theme_bw(9) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title = element_blank()
  )

