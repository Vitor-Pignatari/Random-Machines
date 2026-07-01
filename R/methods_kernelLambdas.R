source('accuracy.R') # Calculo da acurácia de cada modelo kernel
source('log_normalizing.R') # para atribuir probabilidades aos kernels

# Classificação
yardstick::accuracy_vec()
yardstick::brier_class_vec()
# Regressão
yardstick::rmse_vec()
yardstick::mse_vec()

# Outras possíveis métricas do yardstick
