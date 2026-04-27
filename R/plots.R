library (ggplot2)

theme_linkedin <- function(base_size = 24) {

  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = base_size * 1.2,
        face = "plain",
        hjust = 0.5
      ),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = base_size * 0.7),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "none",
      strip.text = ggplot2::element_text(size = base_size * 0.8)
    )
}

scale_color_linkedin <- function() {
  ggplot2::scale_color_manual(
    values = c(
      "Low"  = "#4C78A8",
      "Med"  = "#F58518",
      "High" = "#54A24B",
      "Confounded" = "#E45756",
      "Balanced"   = "#72B7B2"
    )
  )
}

scale_fill_linkedin <- function() {
  ggplot2::scale_fill_manual(
    values = c(
      "Confounded" = "#E45756",
      "Balanced"   = "#72B7B2"
    )
  )
}

plot_base <- function(p, title = NULL) {

  p +
    ggplot2::labs(title = title) +
    theme_linkedin()
}

plot_structure <- function(dt_conf, dt_bal, annotate = TRUE) {

  dt_conf[, Experiment := "Confounded"]
  dt_bal[,  Experiment := "Balanced"]

  dt_all <- data.table::rbindlist(list(dt_conf, dt_bal))

  p <- ggplot2::ggplot(
    dt_all,
    ggplot2::aes(x = Soil, y = Temp, color = Temp)
  ) +
    ggplot2::geom_jitter(width = 0.15, height = 0.15, size = 3, alpha = 0.8) +
    ggplot2::facet_wrap(~Experiment)

  p <- plot_base(p, "Same data. Different structure") +
    scale_color_linkedin()

  if (annotate) {
    p <- p + annotate_linkedin("structure")
  }

  p
}

plot_confounding <- function(dt, annotate = TRUE) {

  p <- ggplot2::ggplot(
    dt,
    ggplot2::aes(x = Soil, y = Temp, color = Temp)
  ) +
    ggplot2::geom_jitter(width = 0.15, height = 0.15, size = 3, alpha = 0.8)

  p <- plot_base(p, "Each soil at one temperature") +
    scale_color_linkedin()

  if (annotate) {
    p <- p + annotate_linkedin("confounding")
  }

  p
}

plot_clustering <- function(dt, annotate = TRUE) {

  p <- ggplot2::ggplot(
    dt,
    ggplot2::aes(x = Soil, y = Yield)
  ) +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.6, size = 2.5)

  p <- plot_base(p, "Measurements cluster within soils")

  if (annotate) {
    p <- p + annotate_linkedin("clustering")
  }

  p
}

plot_within_between <- function(dt) {

  means <- dt[, .(mean_yield = mean(Yield)), by = Soil]

  p <- ggplot2::ggplot(
    dt,
    ggplot2::aes(x = Soil, y = Yield)
  ) +
    ggplot2::geom_point(alpha = 0.4) +
    ggplot2::geom_point(
      data = means,
      ggplot2::aes(y = mean_yield),
      size = 4
    )

  plot_base(p, "Within vs between soil variability")
}

plot_centered <- function(dt) {

  dt_copy <- data.table::copy(dt)
  dt_copy[, centered := Yield - mean(Yield), by = Soil]

  p <- ggplot2::ggplot(
    dt_copy,
    ggplot2::aes(x = Soil, y = centered)
  ) +
    ggplot2::geom_point(alpha = 0.6)

  plot_base(p, "Within-soil variability")
}

plot_model_comparison <- function(dt_conf, dt_bal) {

  dt_conf[, Experiment := "Confounded"]
  dt_bal[,  Experiment := "Balanced"]

  dt_all <- data.table::rbindlist(list(dt_conf, dt_bal))

  p <- ggplot2::ggplot(
    dt_all,
    ggplot2::aes(x = Soil, y = Yield, color = Temp)
  ) +
    ggplot2::geom_point(alpha = 0.7, size = 2.5) +
    ggplot2::facet_wrap(~Experiment)

  plot_base(p, "What the model can learn depends on design") +
    scale_color_linkedin()
}

plot_se <- function(m_conf, m_bal, annotate = TRUE) {

  se_conf <- coef(summary(m_conf))
  se_bal  <- coef(summary(m_bal))

  dt <- data.table::rbindlist(list(
    data.table::data.table(
      term = rownames(se_conf),
      se = se_conf[, "Std. Error"],
      Model = "Confounded"
    ),
    data.table::data.table(
      term = rownames(se_bal),
      se = se_bal[, "Std. Error"],
      Model = "Balanced"
    )
  ))

  dt <- dt[grep("Temp", term)]

  p <- ggplot2::ggplot(
    dt,
    ggplot2::aes(x = term, y = se, fill = Model)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.6), width = 0.5)

  p <- plot_base(p, "Same data size. Different uncertainty") +
    scale_fill_linkedin()

  if (annotate) {
    p <- p + annotate_linkedin("se")
  }

  p
}

make_plot <- function(type, ...) {

  switch(type,

    "structure"      = plot_structure(...),
    "confounding"    = plot_confounding(...),
    "clustering"     = plot_clustering(...),
    "within_between" = plot_within_between(...),
    "centered"       = plot_centered(...),
    "comparison"     = plot_model_comparison(...),
    "se"             = plot_se(...),

    stop("Unknown plot type")
  )
}

annotate_linkedin <- function(type) {

  switch(type,

    "confounding" =
      ggplot2::annotate(
        "text",
        x = Inf, y = Inf,
        label = "confounded",
        hjust = 1.1, vjust = 1.5,
        size = 6,
        color = "#444444"
      ),

    "structure" =
      ggplot2::annotate(
        "text",
        x = Inf, y = Inf,
        label = "same data\n different structure",
        hjust = 1.1, vjust = 1.5,
        size = 5.5,
        color = "#444444"
      ),

    "se" =
      ggplot2::annotate(
        "text",
        x = 1.5, y = Inf,
        label = "more independent information\n→ lower uncertainty",
        vjust = 1.5,
        size = 5.5,
        color = "#444444"
      ),

    "clustering" =
      ggplot2::annotate(
        "text",
        x = Inf, y = Inf,
        label = "within soil similarity",
        hjust = 1.1, vjust = 1.5,
        size = 6,
        color = "#444444"
      ),

    NULL
  )
}

export_linkedin <- function(plot,
                           filename,
                           path = "figures",
                           size = 1080,
                           dpi = 300) {

  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  full_path <- file.path(path, filename)

  ggplot2::ggsave(
    filename = full_path,
    plot = plot,
    width = size,
    height = size,
    units = "px",
    dpi = dpi,
    bg = "white",
    limitsize = FALSE
  )

  invisible(full_path)
}

make_plot_export <- function(type,
                             filename,
                             annotate = TRUE,
                             ...) {

  p <- switch(type,

    "structure"      = plot_structure(...),
    "confounding"    = plot_confounding(..., annotate = annotate),
    "clustering"     = plot_clustering(..., annotate = annotate),
    "within_between" = plot_within_between(...),
    "centered"       = plot_centered(...),
    "comparison"     = plot_model_comparison(...),
    "se"             = plot_se(..., annotate = annotate),

    stop("Unknown plot type")
  )

  export_linkedin(p, filename)
}

generate_all_plots <- function(dataset,
                               dt_bal,
                               m_lmer_conf,
                               m_lmer_bal,
                               path = "figures",
                               annotate = TRUE) {

  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  message("Generating LinkedIn plots...")

  # ---- Post 1: clustering
  make_plot_export(
    "clustering",
    filename = "01_post1_clustering.png",
    dt = dataset,
    annotate = annotate
  )

  # ---- Post 2: within vs between
  p2 <- plot_within_between(dataset)
  export_linkedin(p2, "02_post2_within_between.png", path)

  # ---- Post 3: centered
  p3 <- plot_centered(dataset)
  export_linkedin(p3, "03_post3_centered.png", path)

  # ---- Post 4: confounding intro
  make_plot_export(
    "confounding",
    filename = "04_post4_confounding_intro.png",
    dt = dataset,
    annotate = annotate
  )

  # ---- Post 5: model intuition
  make_plot_export(
    "comparison",
    filename = "05_post5_model_view.png",
    dt_conf = dataset,
    dt_bal = dt_bal
  )

  # ---- Post 6: confounding reveal
  make_plot_export(
    "confounding",
    filename = "06_post6_confounding_focus.png",
    dt = dataset,
    annotate = TRUE
  )

  # ---- Post 7: structure comparison
  make_plot_export(
    "structure",
    filename = "07_post7_structure.png",
    dt_conf = dataset,
    dt_bal = dt_bal,
    annotate = annotate
  )

  # ---- Post 8: standard errors
  make_plot_export(
    "se",
    filename = "08_post8_standard_errors.png",
    m_conf = m_lmer_conf,
    m_bal  = m_lmer_bal,
    annotate = annotate
  )

  # ---- Post 9: final comparison
  make_plot_export(
    "comparison",
    filename = "09_post9_final_comparison.png",
    dt_conf = dataset,
    dt_bal = dt_bal
  )

  message("All plots generated in: ", normalizePath(path))
}
