test_that("modelo possui classe correta", {

  set.seed(123)

  p <- mlbench::mlbench.spirals(1e3,1.5, 0.09)
  # plot(p)
  p <- as.data.frame(p)

  modelo <- randomMachinesClassifier(
    formula = 'classes ~ x.1 + x.2',
    data = p,
    B = 50,
    method = 'WSVM',
    worst_prop = 0.6
  )

  # ndata <- as.data.frame(mlbench::mlbench.spirals(1e4,1.5,0.05))
  #
  # ndata$predito <- predict.randomMachinesClassifierClass(rmc_model = modelo, newdata = ndata)
  #
  # ndata <- ndata %>% mutate(acerto = case_when(classes == predito ~ TRUE, TRUE ~ FALSE))
  # mean(ndata$acerto)
  # plot(ndata$x.1, ndata$x.2)

  expect_true(
    is(modelo, "randomMachinesClassifierClass")
  )

})
