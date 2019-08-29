
FLAGS <- flags(
  flag_numeric("dropout1", 0.10),
  flag_numeric("dropout2", 0.10),
  flag_numeric("dense1", 16),
  flag_numeric("dense2", 16),
  flag_integer("batchsize", 50),
  flag_integer("epochs", 35),
  flag_integer("patience", 15),
  flag_numeric("validationsplit", 0.20)
)

runs <- tuning_run("traintensor.R", sample = 0.01, flags = list(
  dropout1 = c(0.1, 0.2, 0.3),
  dropout2 = c(0.1, 0.2, 0.3),
  dense1 = c(4, 16, 64),
  dense2 = c(4, 16, 64),
  batchsize = c(10, 50, 100),
  epochs = 25,
  patience = 10,
  validationsplit = c(0.10, 0.20)
))

runs[order(runs$eval_acc, decreasing = TRUE), ]
