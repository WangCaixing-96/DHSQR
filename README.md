# DHSQR

R code for "Distributed High-Dimensional Quantile Regression: Estimation Efficiency and Support Recovery", Forty-first International Conference on Machine Learning (ICML24).


# Simulation Description File

## The Manual of Each R file

**'Data_Start.R': **

In the 'Data_Start.R' file, the following tasks are performed:

- Data generation preparation.
- Calculation of evaluation-related indicators to estimate their values.(F1-score, precision, and recall)

**'CSQR.R'**: Implementation of DHSQR and Pooled DHSQR methods.

**'SmoothingQuantileRegression.R'**: Implementation of DPQR methods.

**'DREL.R'**: Implementation of DREL methods.

**'AVG-DC.R'**: Implementation of  Avg-DC methods.

**'Main_Simulation.R'**:  

In the 'Main_Simulation.R' file, five different methods are applied to estimate a single simulation. Furthermore, a wide array of indicators (l2-error, F1-score, precision, and recall) is collected to enable comprehensive comparisons and analyses.

**'Repeat_simulation.R'**: 

In the 'Repeat_simulation.R' file, the simulation is executed multiple times, and during each run, the l2-error, F1-score, precision, and recall indicators are recorded. The collected data is then prepared for subsequent visualization in graphs and tables.

**'Sensitivity_simulation_CSQR.R'**: The sensitivity experiment of bandwidth compared with DHSQR method.

**'Repeat_Time_simulation.R'**: Compare the time of different methods.

## Important Parameter Description

$N$ - Total Sample Size of $X$;

$n$ - Local Sample Size;

n_valid - Test Set Sample Size for Selecting the optimal lambda, default is 1000;

I - Number of iteration, default is 10;

num_repetitions - Number of repeats;

$M$ - Number of Local Machines，default is $M=\frac{N}{n}$;

$p$ - Dimension of $X$, default is 500;

lambda_list - Alternative $\lambda$ sets,  where $\lambda$  is Regularization Parameter of $\ell_1$-penalized least square problem;

h - The global bandwidth of DHSQR and DPQR , adjusted by hyperparameter c_h；

b - The local bandwidth of DHSQR ，adjusted by hyperparameter c_b；

h_REL -  The bandwidth of DREL；

b_SQR - The local bandwidth of DPQR;

$\tau$ - The Quantile Level.

## Figure 1, Figure 2 and Figure 3

### Figure 1

Make the following changes to 'Main_Simulation.R'：

- Seed Initialization: `set.seed(n_re)`
- Settings for **Pooled DHSQR **:
  - $c_h$ value: 2.8
- Settings for **DHSQR **:
  - $c_h$ value: 5
  - $c_b$ value: 0.53
- Settings for **DREL**:
  - $h_{REL}$ calculation: $h_{REL}=\sqrt{(s\log(N)/N)+s^{-0.5}\left(0.1(s^2)\log(N)/n\right)}$
- Settings for **DPQR**:
  - $b_{SQR}$ calculation: $b_{SQR} = 0.21\left(\frac{s\log(n)}{n}\right)^{1/3}$
- Alternative range for $\lambda$ with six values: $(10^{-0}, 10^{-0.5}, 10^{-1}, 10^{-1.5}, 10^{-2}, 10^{-2.5})$
- Distributions used in the above settings:
  - Normal Distribution
  - Student's t-Distribution with 3 degrees of freedom (t_3)

For the Cauchy distribution, there is a special setting for **DPQR**:

- Special setting for DPQR with Cauchy Distribution:
  - $b_{SQR} = 0.22\left(\frac{s\log(n)}{n}\right)^{1/3}$.

The setting of the homoscedastic error case and the heteroscedastic error case is the same.

### Figure 2 and 3

Make the following changes to 'Main_Simulation.R'：

- Seed Setting: Use `set.seed(n_re)` to ensure the reproducibility of experiment randomness.
- Error Distribution Setting: Choose the normal distribution as the error distribution.
- DREL Setting: Calculate the value of `h_REL` using the formula `h_REL = sqrt((s * log(N) / N) + s^(-0.5) * (0.1 * (s^2) * log(N) / n))`.
- Alternative Range for $\lambda$: Seven alternative $\lambda$ values are selected, ranging from `10^(-0.75)` to `10^(-2.25)`.
- Threshold for TP Calculation (F1 Score, Precision): Set to `0.015`.
- **Avg-DC** Threshold: Set to `0`.
- Settings for  **DHSQR **:
  - When `N = 5000`, `c_b = 0.5`.
  - When `N = 10000`, `c_b = 0.4`.
  - When `N = 20000`, `c_b = 0.4`.
  - In all cases, `c_h = 5`.
- Settings for **Pooled DHSQR**  :
  - When `N = 5000`, `N = 10000`, and `N = 20000`, `c_b = 0.53`.
  - In all cases, `c_h = 0.2`.
- Settings for  **DPQR** :
  - When `N = 5000`, `b_SQR = c_bsqr * (s * log(n) / n)^(1/3)`, where `c_bsqr = 3.2`.
  - When `N = 10000`, `b_SQR = c_bsqr * (s * log(n) / n)^(1/3)`, where `c_bsqr = 2.8`.
  - When `N = 20000`, `b_SQR = c_bsqr * (s * log(n) / n)^(1/3)`, where `c_bsqr = 2.5`.
- Settings for  **DPQR**  (Special Cases, Cauchy Distribution):
  - When `N = 5000`, `b_SQR = c_bsqr * (s * log(n) / n)^(1/3)`, where `c_bsqr = 5`.
  - When `N = 10000`, `b_SQR = c_bsqr * (s * log(n) / n)^(1/3)`, where `c_bsqr = 4.1`.
  - When `N = 20000`, `b_SQR = c_bsqr * (s * log(n) / n)^(1/3)`, where `c_bsqr = 3.6`.
- Settings for  **DPQR**  (homoscedastic error case ):
  - When `N = 10000`, `b_SQR = c_bsqr * (s * log(n) / n)^(1/3)`, where `c_bsqr = 2.9`.
  - When `N = 20000`, `b_SQR = c_bsqr * (s * log(n) / n)^(1/3)`, where `c_bsqr = 2.5`.

## Time and  Sensitivity Experiments

For time and sensitivity experiments, see related files **'Sensitivity_simulation_CSQR.R'** and **'Repeat_Time_simulation.R'**.

## Experiments for $\tau$

Make the following changes to 'Main_Simulation.R'：

- Settings for **Pooled DHSQR** :
  - `c_h`: Set to 0.2.
  - `c_b`: Set to 0.53.
  - `lambda_list`: Contains one value, `10^(-(3:9)/4)`.
- Settings for  **DHSQR**:
  - `c_h`: Set to 5.
  - `c_b`: Set to 0.6.
- Settings for **DREL**:
  - `h_REL`: Calculated using the formula `sqrt(s * log(N) / N) + (s^(-0.5)) * (0.1 * (s^2) * log(N) / n)`.
- Settings for **DPQR**:
  - `h_REL`: Calculated using the formula `sqrt(s * log(N) / N) + (s^(-0.5)) * (0.1 * (s^2) * log(N) / n)`.

- The table below provides the parameters for DPQR under different combinations of `n` and `N`. The values include `ch` and `cb` for various scenarios.

| $n$                                          | 200                  | 500                  | 1000                 |
| -------------------------------------------- | -------------------- | -------------------- | -------------------- |
| $N$                                          | **5000 10000 20000** | **5000 10000 20000** | **5000 10000 20000** |
| $c_h$($\tau=0.3$) homoscedastic error case   | 4.6                  | 4.3                  | 4.1                  |
| $c_b$                                        | 3                    | 3                    | 3                    |
| $c_h$($\tau=0.3$) heteroscedastic error case | 4.6                  | 4.3                  | 4.1                  |
| $c_b$                                        | 3                    | 3                    | 3                    |
| $c_h$($\tau=0.7$) homoscedastic error case   | 5                    | 4                    | 3.2                  |
| $c_b$                                        | 4                    | 2.5                  | 4                    |
| $c_h$($\tau=0.7$) heteroscedastic error case | 6.5                  | 6                    | 5                    |
| $c_b$                                        | 3                    | 3                    | 3                    |

## Experiments for $\beta^{*}$

Affter the rebuttal, we provide some additional experiment results using decaying sequence setting of nonzero parameters. 

In the case of Normal Heteroscedasticity, with $\tau = 0.7, n = 500, I = 10$, and we set the true parameter as 
$$ \boldsymbol{\beta}^{*} = (1,2^1,2^0,2^{-1},2^{-2},2^{-3},\boldsymbol{0}_{p-5})^\mathrm{T}. $$

Make the above changes to 'Repeat_Time_simulation' and 'Main_Simulation.R'.

- Seed Initialization: `set.seed(n_re)`

- Alternative range for $\lambda$ with six values: 

  ```
  lambda_list = c(10^(-(0:10)/4))
  ```

- Settings for **Pooled DHSQR **:

  - $c_h$ value: 0.2

- Settings for **DHSQR **:

  - $c_h$ value: 0.2
  - $c_b$ value: 0.53

- Settings for **DREL**:

  - $h_{REL}$ calculation: $h_{REL}=\sqrt{(s\log(N)/N)+s^{-0.5}\left(0.1(s^2)\log(N)/n\right)}$

- Settings for **DPQR**:

  - $b_{SQR}$ calculation: $b_{SQR} = 0.21\left(\frac{s\log(n)}{n}\right)^{1/3}$

| $n$   | 500      | 500       | 500       |
| ----- | -------- | --------- | --------- |
| $N$   | **5000** | **10000** | **20000** |
| $c_h$ | 4.3      | 5         | 6         |
| $c_b$ | 3        | 3         | 3         |

