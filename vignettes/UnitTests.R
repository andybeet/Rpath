## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----Tolerance----------------------------------------------------------------
TOLERANCE <- 1e-5

## ----Create Baseline 1--------------------------------------------------------
CREATE_BASELINE_FILES <- TRUE

## ----Create Baseline 2--------------------------------------------------------
CREATE_BASELINE_FILES <- FALSE

## ----Plot Type 1--------------------------------------------------------------
PLOT_TYPE <- 1 

## ----ForcedBio, eval=FALSE----------------------------------------------------
#    REcosystem_scene <- rsim.scenario(REco, REco.params, 1:50)

## ----figs1, echo=FALSE, out.width="85%:", out.height="85%", fig.cap="Figure 1. No Difference in Baseline vs Current Models"----
knitr::include_graphics("img/plot_01_no_difference.png")

## ----figs2, echo=FALSE, out.width="85%:", out.height="85%", fig.cap="Figure 2. Difference in red of Baseline vs Current Models"----
knitr::include_graphics("img/plot_02_superimposed_with_difference.png")

## ----scale--------------------------------------------------------------------
YLIMIT_DIFFERENCE_PLOTS <- 0.05

## ----figs3, echo=FALSE, out.width="85%:", out.height="85%", fig.cap="Figure 3. Current-Baseline Result"----
knitr::include_graphics("img/plot_03_current-baseline.png")

