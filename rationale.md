# Rationale



## Steps enumeration
The package fits models through a four-stage pipeline. The user supplies training data and set of arguments. Formula, "Kernels", "prob = "True?", B.

Stage 1. Defines the kernel sampling probabilities (lambda_{r}). In: kernels, data, sampling.strat. Out: [\lambda_{1}:\lambda_{R}] 

Stage 2. Generates B bootstrap samples and their associated OOB samples. In: data, sampling.strat, B. Out: [bag_{1}:bag_{B}]

Stage 3. Samples B models using stage 1's probabilities and fit them to each b dataset. In: lambda, kernels, [bag_{1}:bag{B}]. Out: [g_{1}:g_{B}].

Stage 4. Compute each b prediction to get oob error. In: [g_{1}:g_{B}], [oob_{1}:oob_{B}]. Out: {[\hat{y}_{1}:hat_{y}_{B}], [\L2_{1}:\L2{B}]}.

Stage 5. Assign weights based on the errors.
Out: [\omega_{1}:\omega_{B}].

### Thoughts

Invariants: Kernels, regression task, "prob = True?", B, loss functions.

Polymorphism: Prediction output (probabilities vs class), regression task.

The result is a fitted model that supports print() and predict() and can be inspected for diagnostics through a diagnostics() (maybe?) function.

## S4 Class rationale

Class candidates: 
- Fitted model. Absolutely. Will store information about the object. Class FittedRM.
- Stage. Well-defined input and output shape, clear validation logic.
- Methods: One for each action in each Stage.
  - Stage 1 (KernelProb) :
    - fitSVM() (might include the sampling.strat and so on, tuning parameters also. have to thinkg about it)
    - computeLambda()
  - Stage 2 (BootSamples):
    - genBSamples()
  - Stage 3 (BModels):
    - sampleBModels()
    - fitBModels()
  - Stage 4 (OOBLoss):
    - predictOOB()
  - Stage 5 (OmegaPredict):
    - weightCalc()
    
## S3 interface:

- fit() method
- predict() method
- print() method
