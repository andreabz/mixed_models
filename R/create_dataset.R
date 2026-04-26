library(data.table)

create_dataset <- function() {
  dt <- data.table(
    Soil = rep(paste0("S", 1:6), each = 8),
    Temp = rep(c("Low", "Low", "Med", "Med", "High", "High"), each = 8),
    Solvent = rep(rep(c("A", "B", "C", "D"), each = 2), times = 6),
    Rep = rep(c(1, 2), times = 24),
    Yield = c(
      52,
      50,
      55,
      54,
      57,
      56,
      53,
      52,
      48,
      47,
      51,
      50,
      52,
      51,
      49,
      48,
      60,
      59,
      63,
      62,
      65,
      64,
      61,
      60,
      57,
      56,
      59,
      58,
      61,
      60,
      58,
      57,
      68,
      67,
      71,
      70,
      73,
      72,
      69,
      68,
      64,
      63,
      66,
      65,
      68,
      67,
      65,
      64
    )
  )
}

create_dataset_balanced <- function() {
  soils <- paste0("S", 1:6)
  temps <- c("Low", "Med", "High")
  solvents <- c("A", "B", "C", "D")
  reps <- 1:2

  dt <- data.table::CJ(
    Soil = soils,
    Temp = temps,
    Solvent = solvents,
    Rep = reps
  )

  set.seed(123)

  # random soil effect
  soil_effects <- rnorm(length(soils), 0, 3)
  names(soil_effects) <- soils

  # fixed effects
  temp_effects <- c(Low = -15, Med = -7, High = 0)
  solvent_effects <- c(A = 0, B = 3, C = 5, D = 1)

  dt[,
    Yield := 65 +
      soil_effects[Soil] +
      temp_effects[Temp] +
      solvent_effects[Solvent] +
      rnorm(.N, 0, 0.7)
  ]

  dt
}
