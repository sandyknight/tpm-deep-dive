# Build the TPM odds-ratio slide deck, styled to match the retention deck:
# per exposure, a headline slide (forest plot left, results table right) and
# an annual trend slide (faceted chart, diamonds coloured by 95%
# significance). Charts are embedded as PNGs on the OHID master/layout from
# templates/ohid_theme.pptx so the branding carries over. Chart/table
# geometry is hardcoded below, lifted from the retention deck's slide XML
# (EMU / 914400 = inches).

library(ggplot2)

# Blank deck carrying the OHID master/theme/layouts (originally extracted
# from the retention odds-ratio deck by deleting its slides).
TEMPLATE <- "./templates/ohid_theme.pptx"
RESULTS <- "./fit_summaries/tpm_odds_ratios.parquet"
OUT <- "./slides/tpm_odds_ratio_slides.pptx"

# Arial on Windows; Liberation Sans (metric-compatible) on Linux boxes
# that lack it. Template charts use a plain sans.
FONT <- if ("Arial" %in% systemfonts::system_fonts()$family) {
  "Arial"
} else {
  "Liberation Sans"
}
TABLE_FONT <- "Arial"
SIG_FILL <- c(No = "#F8766D", Yes = "#00BFC4") # template uses ggplot defaults

IMG_LOC <- officer::ph_location(
  left = 0.3937,
  top = 1.0977,
  width = 5.9867,
  height = 5.6848
)
TAB_LOC <- officer::ph_location(left = 6.5399, top = 1.0976, width = 6.4699)
# Full-slide chart area (16:9 slide is 13.333in wide; margins match IMG_LOC's
# left margin) — used for annual plots flagged annual_full_slide.
FULL_IMG_LOC <- officer::ph_location(
  left = 0.3937,
  top = 1.0977,
  width = 12.5459,
  height = 5.6848
)
LAYOUT <- "2_content_large"

HEADLINE_PERIOD <- "2026-03-31"

# Slide order follows the retention deck (substance, age, housing,
# referral, previous journeys), with region appended.
ANALYSES <- list(
  substance_group = list(
    title = "Substance group treatment progress odds ratios",
    comparator = "Comparator: Alcohol only",
    column_label = "Substance group"
  ),
  age = list(
    title = "Age group treatment progress odds ratios",
    comparator = "Comparator: 18-24",
    column_label = "Age group"
  ),
  housing_start = list(
    title = "Housing need at treatment start odds ratios",
    comparator = "Comparator: No housing problem",
    column_label = "Housing need at start"
  ),
  referral_source = list(
    title = "Referral source treatment progress odds ratios",
    comparator = "Comparator: Self, family & friends",
    column_label = "Referral source"
  ),
  previous_journeys = list(
    title = "Previous treatment journeys odds ratios",
    comparator = "Comparator: 2-3 previous journeys",
    column_label = "Previous journeys"
  ),
  region = list(
    title = "Regional treatment progress odds ratios",
    comparator = "Comparator: London",
    column_label = "Region",
    # Nine facetted regions don't fit the half-slide chart area
    annual_full_slide = TRUE
  )
)

fmt <- function(x) sprintf("%.2f", x)

fmt_p <- function(p) {
  dplyr::case_when(
    p < 0.01 ~ "<0.01",
    p < 0.05 ~ "<0.05",
    p < 0.10 ~ "<0.10",
    .default = ">0.10"
  )
}

theme_tpm <- function() {
  theme_minimal(base_family = FONT, base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 17, colour = "black"),
      plot.title.position = "plot",
      plot.subtitle = element_text(
        size = 14,
        colour = "black",
        margin = margin(t = 8, b = 16)
      ),
      plot.caption = element_text(
        size = 12,
        colour = "black",
        hjust = 1,
        margin = margin(t = 10)
      ),
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      axis.ticks = element_blank(),
      axis.text = element_text(colour = "grey40", size = 12),
      axis.title.y = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

forest_plot <- function(d, spec) {
  d$level <- factor(d$level, levels = sort(unique(d$level)))
  nudge <- 0.05 * diff(range(c(d$conf.low, d$conf.high)))
  ggplot(d, aes(x = estimate, y = level)) +
    geom_vline(xintercept = 1, linewidth = 0.5) +
    geom_errorbar(
      aes(xmin = conf.low, xmax = conf.high),
      orientation = "y",
      width = 0.12,
      linewidth = 0.4
    ) +
    geom_point(size = 1.8) +
    geom_text(
      aes(label = fmt(estimate)),
      nudge_y = 0.22,
      size = 4.6,
      family = FONT
    ) +
    geom_text(
      aes(x = conf.low, label = fmt(conf.low)),
      nudge_x = -nudge,
      hjust = 1,
      fontface = "italic",
      size = 4.6,
      family = FONT
    ) +
    geom_text(
      aes(x = conf.high, label = fmt(conf.high)),
      nudge_x = nudge,
      hjust = 0,
      fontface = "italic",
      size = 4.6,
      family = FONT
    ) +
    scale_x_continuous(expand = expansion(mult = 0.18)) +
    labs(
      title = spec$title,
      subtitle = spec$comparator,
      x = "Odds ratios (95% confidence intervals)",
      caption = sprintf("Data: %s only", substr(HEADLINE_PERIOD, 1, 4))
    ) +
    theme_tpm() +
    theme(
      axis.title.x = element_text(size = 13, hjust = 1, margin = margin(t = 6))
    )
}

annual_plot <- function(d, spec) {
  # Full-slide plots get slightly smaller text: the panels are roomier, so
  # the labels don't need to fight for space.
  sz <- if (isTRUE(spec$annual_full_slide)) {
    list(axis = 10, axis_x = 8, strip = 11, legend = 11)
  } else {
    list(axis = 12, axis_x = 9, strip = 12, legend = 12)
  }
  d$year <- as.integer(substr(d$data_period, 1, 4))
  d$significant <- factor(
    ifelse(d$conf.low > 1 | d$conf.high < 1, "Yes", "No"),
    levels = c("No", "Yes")
  )
  years <- sort(unique(d$year))
  ggplot(d, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.4) +
    geom_line(linetype = "dotted", linewidth = 0.4) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.25,
      linewidth = 0.4
    ) +
    geom_point(
      aes(fill = significant),
      shape = 23,
      size = 3.2,
      colour = "black"
    ) +
    scale_fill_manual(
      values = SIG_FILL,
      drop = FALSE,
      name = "Statistically significant at 95% level"
    ) +
    scale_x_continuous(breaks = years, labels = years) +
    facet_wrap(
      ~level,
      ncol = if (isTRUE(spec$annual_full_slide)) 4 else 3,
      labeller = label_wrap_gen(16)
    ) +
    labs(
      title = spec$title,
      subtitle = spec$comparator,
      caption = sprintf("Data: %s to %s", min(years), max(years))
    ) +
    theme_tpm() +
    theme(
      axis.title.x = element_blank(),
      axis.text = element_text(size = sz$axis),
      axis.text.x = element_text(size = sz$axis_x, angle = 70),
      strip.text = element_text(
        face = "bold",
        hjust = 0,
        size = sz$strip,
        colour = "black"
      ),
      panel.spacing = unit(1.2, "lines"),
      legend.position = "bottom",
      legend.justification = "left",
      legend.title = element_text(size = sz$legend),
      legend.text = element_text(size = sz$legend)
    )
}

or_table <- function(d, spec) {
  tab <- d |>
    dplyr::arrange(level) |>
    dplyr::transmute(
      level,
      or = fmt(estimate),
      lo = fmt(conf.low),
      hi = fmt(conf.high),
      p = fmt_p(p.value)
    )
  flextable::flextable(tab) |>
    flextable::set_header_labels(
      level = spec$column_label,
      or = "Odds ratio",
      lo = "CI lower",
      hi = "CI upper",
      p = "p-value"
    ) |>
    flextable::add_header_lines(spec$comparator) |>
    flextable::add_header_lines(spec$title) |>
    flextable::theme_booktabs() |>
    flextable::bold(part = "header", i = 1) |>
    flextable::font(part = "all", fontname = TABLE_FONT) |>
    flextable::fontsize(part = "all", size = 12) |>
    flextable::fontsize(part = "header", i = 1, size = 13) |>
    flextable::width(j = 1, width = 2.8) |>
    flextable::width(j = 2, width = 1.0) |>
    flextable::width(j = 3:5, width = 0.87)
}

save_png <- function(plot, loc = IMG_LOC) {
  path <- tempfile(fileext = ".png")
  ggsave(path, plot, width = loc$width, height = loc$height, dpi = 300)
  path
}

results <- nanoparquet::read_parquet(RESULTS) |>
  dplyr::filter(!reference, !grepl(" to ", data_period))

pres <- officer::read_pptx(TEMPLATE)
master <- officer::layout_summary(pres) |>
  dplyr::filter(layout == LAYOUT) |>
  dplyr::pull(master)
while (length(pres) > 0) {
  pres <- officer::remove_slide(pres, 1)
}

for (name in names(ANALYSES)) {
  spec <- ANALYSES[[name]]
  d <- dplyr::filter(results, analysis == name)
  headline <- dplyr::filter(d, data_period == HEADLINE_PERIOD)

  pres <- officer::add_slide(pres, layout = LAYOUT, master = master) |>
    officer::ph_with(
      officer::external_img(
        save_png(forest_plot(headline, spec)),
        width = IMG_LOC$width,
        height = IMG_LOC$height
      ),
      location = IMG_LOC
    ) |>
    officer::ph_with(or_table(headline, spec), location = TAB_LOC)

  annual_loc <- if (isTRUE(spec$annual_full_slide)) FULL_IMG_LOC else IMG_LOC
  pres <- officer::add_slide(pres, layout = LAYOUT, master = master) |>
    officer::ph_with(
      officer::external_img(
        save_png(annual_plot(d, spec), annual_loc),
        width = annual_loc$width,
        height = annual_loc$height
      ),
      location = annual_loc
    )
}

dir.create("./slides", showWarnings = FALSE)
print(pres, target = OUT)
cat("written:", OUT, "\n")
