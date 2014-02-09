Intro
  - infinite mixture model
  - related to CRP (show graphical model)
  - applications:
    - as prior for object using potentially infinite array of features
    - for models involving bipartite graphs with size of >= 1 class of nodes unknown

Relation to CRP
  Dirichlet Process : CRP :: Beta Proces : IBP (Thibaux and Jordan)

Finite feature model (4.1)
  - Beta-binomial
  - Graphical model

Equivalence classes (4.2)

Indian Buffet Process basics:
  1. N customers enter a restaurant, in sequence
  2. First cusomer fills her plate with Poisson(alpha) number of dishes
  3. i^th customer samples dishes in proportion to their popularity, with prob m_k/i  (m_k = # of previous customers who sampled a dish)
  4. i^th customer samples Poisson (alpha/i) nuber of new dishes

Demo (Shiny app)

Properties (4.6)

Inference by Gibbs sampling (4.7)

Example

Further application

Extensions
  - faster sampling
    - through parallelization
    - or Gibbs sampling

Discussion
  - could we imagine an "Indian franchise process"?

Notes:
  do multiple demos to kill time
  present algorithm near beginning

