# MBRM
## Mixed Regression Models with Generalized Log-Gamma Random Effects
### Authors
Lizandra C Fabio,
Vanessa Barros,
Cristian Lobos,
Jalmar M. F. Carrasco.
### Paper: Marginal multivariate approach: A novel strategy for handling correlated binary outcomes]{Marginal multivariate approach: A novel strategy for handling correlated binary outcomes
### Abstract
Modeling repeated binary data remains challenging, particularly in the presence of unbalanced responses and heterogeneous correlation structures. These complexities have motivated the development of more flexible regression frameworks. In this paper, we introduce a novel multivariate regression model tailored for correlated binary outcomes. The proposed multivariate distribution is derived from a Bernoulli mixed model under a marginal approach, incorporating a non-normal random intercept whose distribution is assumed to follow a generalized log-gamma (GLG) specification under a particular parameter setting. The complementary log-log link function is specified to induce the conjugacy between the Bernoulli response and the GLG-distributed random effect. This formulation leads to the Multivariate Bernoulli–GLG (MBerGLG) distribution, characterized by interpretable location and dispersion parameters. The associated MBerGLG regression model (MBerGLGR) offers a computationally tractable and analytically explicit alternative for modeling both balanced and unbalanced correlated binary data. Monte Carlo simulation studies demonstrate that the maximum likelihood estimators of the model parameters are asymptotically unbiased, efficient, and consistent. Moreover, the simulations reveal that the proportion of zeros and ones in the response variable is influenced by the variability of the shape parameter in the generalized log-gamma (GLG) distribution. Additionally, randomized quantile residuals are employed to assess model adequacy, detect potential model misspecification, and identify atypical subjects. Finally, the methodology is illustrated through two real data applications. 
### Install package
R

require(devtools)

devtools::install_github("carrascojalmar/MBRM")
