install.packages("devtools")
library(devtools)
install_github("juanjovazpar/OncoSimul/OncoSimulR")

test_that("oncosimul v.2 objects and genotype plotting", {
    data(examplesFitnessEffects)
    p1 <-  oncoSimulIndiv(examplesFitnessEffects[["o3"]],
        model = "McFL", 
        mu = 5e-5,
        detectionSize = 1e8, 
        detectionDrivers = 3,
        sampleEvery = 0.025,
        max.num.tries = 10,
        keepEvery = 5,
        initSize = 2000,
        finalTime = 3000,
        onlyCancer = FALSE,
        keepPhylog = TRUE)
  class(p1)
  plot(p1, type = "fishplot")
})

test_that("require Phylog and require data class", {
  data(examplesFitnessEffects)
  simulWithoutPhyLog<-  oncoSimulIndiv(examplesFitnessEffects[["o3"]],
    model = "McFL", 
    mu = 5e-5,
    detectionSize = 1e8, 
    detectionDrivers = 3,
    sampleEvery = 0.025,
    max.num.tries = 10,
    keepEvery = 5,
    initSize = 2000,
    finalTime = 3000,
    onlyCancer = FALSE,
    keepPhylog = FALSE)

  expect_error(plot(simulWithoutPhyLog, type = "fish"),
    "Simulation must include phylog", fixed = TRUE)

  data(examplePosets)
  p705 <- examplePosets[["p705"]
  simulClassOncosimul1 <- oncoSimulIndiv(p705, model = "McFL",
    mu = 5e-6,
    sampleEvery = 0.02,
    keepEvery = 10,
    initSize = 2000,
    finalTime = 3000,
    max.num.tries = 100,
    onlyCancer = FALSE)

  expect_error(plot(simulClassOncosimul1, type = "fish"),
    "Simulation class must be oncosimul2", fixed = TRUE)
})
