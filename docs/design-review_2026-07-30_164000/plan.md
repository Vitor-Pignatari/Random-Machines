# randomMachines — design review & cleanup plan - Lambda/Omega calculation functions

> Review of a few points I need to make clear before proceeding with package development
> write the response to this into a plan-response.md with my logged decisions below each question you ask me.

---


## 1. Package review and test coverage for `R/weights.R`

The file has been changed and the function headers contain information about where they should be used by default.
It must be clear KernelLambdas calculates probabilities from performance metrics and restrictions over functions that can be used to do so must follow that fact.
It must be clear BootOmegas calculates weights from performance metrics and restrictions over functions that can be used to do so must follow that fact.
For both, the sum of all elements should be 1 so that weights are also directly interpretable in a given scale.

- [] **`.metric_direction() and .uses_minimize()`** — Review the need for those functions given the updates.
- [] Standardize the names using the dot prefix, even suggest a convention that would make it easier to understand what they're being used for.
- [] Study the possibility of turning the function into lambdaCalc and omegaCalc generic's methods, dispatching over 
[ArgSpecs subclasses] + Pipeline step class where it's being executed (Either KernelLambdas or BootOmegas) +
numeric vector of CV/Bootstrap metrics and having proper restrictions on each step. In case a user supplies a function to be used through the interface for either step, 
a method is created using that probability assignment/weight calculation and testing for necessary validity before allowing the calculations to happen.
- [] Research and plan the best practice adopted to allow users to pass functions with a set of the arguments pre-defined (like beta for `exp_score`) 

## 2. Aftermath of the work after you prompt me for decisions 
- [] Since function names and structuring have changed, make sure the scripts relying on it adapted to it. `devtools::document()` and ``devtools::test` to find and correct any bugs.
- [] Update the mermaid diagram once all is done. I'd like it to be in a bookdown file with pagination for each chapter since it's already split in a similar way.