library(ggplot2)

theme_linkedin <- function(base_size = 24) {
  # Abbassiamo il base_size standard
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = rel(1.2),
        face = "bold",
        hjust = 0, # Allineato a sinistra è più "LinkedIn style"
        margin = margin(b = 15),
        lineheight = 1.1 # Spazio tra righe del titolo
      ),
      plot.margin = margin(20, 20, 20, 20), # Respira!
      axis.title = ggplot2::element_text(size = rel(0.8), color = "grey30"),
      axis.text = ggplot2::element_text(size = rel(0.7)),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = NA),
      strip.text = ggplot2::element_text(
        size = rel(0.8),
        face = "bold",
        margin = margin(5, 5, 5, 5)
      )
    )
}

scale_color_linkedin <- function() {
  ggplot2::scale_color_manual(
    values = c(
      "Low" = "#4C78A8",
      "Med" = "#F58518",
      "High" = "#54A24B",
      "Confounded" = "#E45756",
      "Balanced" = "#72B7B2"
    )
  )
}

scale_fill_linkedin <- function() {
  ggplot2::scale_fill_manual(
    values = c(
      "Confounded" = "#E45756",
      "Balanced" = "#72B7B2",
      "lm" = "#E45756",
      "lmer" = "#72B7B2"
    )
  )
}

plot_base <- function(p, title = NULL) {
  p +
    ggplot2::labs(title = title) +
    theme_linkedin()
}

plot_structure <- function(dt_conf, dt_bal, title = TRUE, annotate = FALSE) {
  dt_conf[, Experiment := "Confounded"]
  dt_bal[, Experiment := "Balanced"]

  dt_all <- data.table::rbindlist(list(dt_conf, dt_bal))

  p <- ggplot2::ggplot(
    dt_all,
    ggplot2::aes(x = Soil, y = Temp, color = Temp)
  ) +
    ggplot2::geom_jitter(width = 0.15, height = 0.15, size = 3, alpha = 0.8) +
    ggplot2::facet_wrap(~Experiment)

  if (title) {
    p <- plot_base(p, "Same data.\nDifferent structure")
  }

  p <- p +
    scale_color_linkedin() +
    theme_linkedin()

  if (annotate) {
    p <- p + annotate_linkedin("structure")
  }

  p
}

plot_confounding <- function(dt, title = TRUE, annotate = FALSE) {
  p <- ggplot2::ggplot(
    dt,
    ggplot2::aes(x = Soil, y = Temp, color = Temp)
  ) +
    ggplot2::geom_jitter(width = 0.15, height = 0.15, size = 3, alpha = 0.8)

  if (title) {
    p <- plot_base(p, "Each soil at one temperature")
  }

  p <- p +
    scale_color_linkedin() +
    theme_linkedin()

  if (annotate) {
    p <- p + annotate_linkedin("confounding")
  }

  p
}

plot_clustering <- function(dt, title = TRUE, annotate = FALSE) {
  p <- ggplot2::ggplot(
    dt,
    ggplot2::aes(x = Soil, y = Yield)
  ) +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.6, size = 2.5)

  if (title) {
    p <- plot_base(p, "Measurements cluster\nwithin soils")
  }

  p <- p +
    scale_color_linkedin() +
    theme_linkedin()

  if (annotate) {
    p <- p + annotate_linkedin("clustering")
  }

  p
}

plot_within_between <- function(dt, title = TRUE) {
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

  if (title) {
    p <- plot_base(p, "Within vs between\nsoil variability")
  }

  p <- p +
    scale_color_linkedin() +
    theme_linkedin()

  p
}

plot_centered <- function(dt, title = TRUE) {
  dt_copy <- data.table::copy(dt)
  dt_copy[, centered := Yield - mean(Yield), by = Soil]

  p <- ggplot2::ggplot(
    dt_copy,
    ggplot2::aes(x = Soil, y = centered)
  ) +
    ggplot2::geom_point(alpha = 0.6)

  if (title) {
    p <- plot_base(p, "Within-soil variability")
  }

  p <- p +
    scale_color_linkedin() +
    labs(y = "Mean centered yield") +
    theme_linkedin()

  p
}

plot_model_comparison <- function(dt_conf, dt_bal, title = TRUE) {
  dt_conf[, Experiment := "Confounded"]
  dt_bal[, Experiment := "Balanced"]

  dt_all <- data.table::rbindlist(list(dt_conf, dt_bal))

  p <- ggplot2::ggplot(
    dt_all,
    ggplot2::aes(x = Soil, y = Yield, color = Temp)
  ) +
    ggplot2::geom_point(alpha = 0.7, size = 2.5) +
    ggplot2::facet_wrap(~Experiment)

  if (title) {
    p <- plot_base(p, "What the model can learn\ndepends on design")
  }

  p +
    scale_color_linkedin() +
    theme_linkedin()
}

plot_se <- function(m_conf, m_bal, title = TRUE, annotate = FALSE) {
  se_conf <- coef(summary(m_conf))
  se_bal <- coef(summary(m_bal))

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
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.6),
      width = 0.5
    )

  if (title) {
    p <- plot_base(p, "Same data size.\nDifferent uncertainty")
  }

  p <- p +
    labs(y = "Coefficient standard error", x = "Term") +
    scale_fill_linkedin() +
    theme_linkedin()

  if (annotate) {
    p <- p + annotate_linkedin("se")
  }

  p
}

plot_lm_lmer <- function(m_naive, m_lmer, title = TRUE, annotate = FALSE) {
  # 1. Estrazione Standard Errors
  se_naive <- coef(summary(m_naive))[, "Std. Error"]
  se_mixed <- coef(summary(m_lmer))[, "Std. Error"]

  # 2. Creazione data.table (molto più veloce di data.frame)
  se_dt <- data.table(
    term = names(se_naive),
    lm = se_naive,
    lmer = se_mixed
  )

  # 3. Trasformazione da formato wide a long (come pivot_longer)
  se_long <- melt(
    se_dt,
    id.vars = "term",
    variable.name = "model",
    value.name = "se"
  )

  # 4. Plotting
  p <- ggplot(se_long, aes(x = term, y = se, fill = model)) +
    geom_col(position = "dodge")

  if (title) {
    p <- plot_base(p, "Standard errors\nchange unevenly")
  }

  p <- p +
    labs(y = "Coefficient standard error", x = NULL) +
    scale_fill_linkedin() +
    scale_fill_discrete(
      labels = c("linear", "mixed"),
      limits = c("lm", "lmer")
    ) +
    theme_linkedin() +
    theme(
      legend.position = c(0, 1),
      legend.justification = c(0, 1),
      legend.direction = "horizontal",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  if (annotate) {
    p <- p + annotate_linkedin("se")
  }

  p
}

# Esempio di utilizzo:
# compare_se_dt(m_naive, m_lmer)

make_plot <- function(type, ...) {
  switch(
    type,

    "structure" = plot_structure(...),
    "confounding" = plot_confounding(...),
    "clustering" = plot_clustering(...),
    "within_between" = plot_within_between(...),
    "centered" = plot_centered(...),
    "comparison" = plot_model_comparison(...),
    "models" = plot_lm_lmer(...),
    "se" = plot_se(...),

    stop("Unknown plot type")
  )
}

annotate_linkedin <- function(type) {
  # Usiamo size in mm per ggplot2 (size 5 in ggplot è circa 14pt)
  common_label <- function(label, ...) {
    ggplot2::annotate(
      "text", # 'label' aggiunge uno sfondo bianco che aiuta la leggibilità
      x = Inf,
      y = Inf,
      label = label,
      hjust = 1.05,
      vjust = 1.5,
      size = 4.5,
      color = "#444444",
      ...
    )
  }

  switch(
    type,
    "confounding" = common_label("Confounded design"),
    "structure" = common_label("Same data,\ndifferent structure"),
    "se" = common_label("More information\nlower uncertainty"),
    "clustering" = common_label("Within-soil similarity"),
    NULL
  )
}

export_linkedin <- function(
  plot,
  filename,
  path = "figures",
  size = 7,
  dpi = 300
) {
  # Usiamo pollici (size = 7) per avere un controllo tipografico reale
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  ggplot2::ggsave(
    filename = file.path(path, filename),
    plot = plot,
    width = size,
    height = size,
    units = "in", # Passiamo a pollici
    dpi = dpi,
    bg = "white"
  )
}

make_plot_export <- function(type, filename, annotate = FALSE, ...) {
  p <- switch(
    type,

    "structure" = plot_structure(...),
    "confounding" = plot_confounding(..., annotate = annotate),
    "clustering" = plot_clustering(..., annotate = annotate),
    "within_between" = plot_within_between(...),
    "centered" = plot_centered(...),
    "comparison" = plot_model_comparison(...),
    "se" = plot_se(..., annotate = annotate),
    "models" = plot_lm_lmer(..., annotate = annotate),

    stop("Unknown plot type")
  )

  export_linkedin(p, filename)
}

generate_all_plots <- function(
  dataset,
  dt_bal,
  m_lm_conf,
  m_lmer_conf,
  m_lmer_bal,
  path = "figures",
  annotate = FALSE
) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

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
    "models",
    filename = "05_post5_model_view.png",
    m_naive = m_lm_conf,
    m_lmer = m_lmer_conf
  )

  # ---- Post 6: confounding reveal
  make_plot_export(
    "confounding",
    filename = "06_post6_confounding_focus.png",
    dt = dataset,
    annotate = annotate
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
    m_bal = m_lmer_bal,
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
