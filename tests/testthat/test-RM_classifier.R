require(tidyverse)
require(mlbench)
set.seed(123)

devtools::load_all()

p <- mlbench::mlbench.spirals(1e3,1.5,0.05)
plot(p)
p <- as.data.frame(p)

modelo <- randomMachinesClassifier(formula = 'classes ~ x.1 + x.2', data = p, B = 50, method = 'original')
