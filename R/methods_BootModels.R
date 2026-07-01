source('stratified_kfold.R')
source('accuracy.R')

# Treinamento dos k modelos
kernlab::ksvm()
kernlab::lssvm()
kernlab::predict()

# Classificação
yardstick::accuracy_vec()
yardstick::brier_class_vec()
# Regressão
yardstick::rmse_vec()
yardstick::mse_vec()

# Outras possíveis métricas do yardstick
