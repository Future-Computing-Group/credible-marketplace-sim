## Tests for the bang-bang rational ghost-bid adversary (Trilogy 2B Exps 8-9).
##
## Under the saturating-hazard SDS audit channel, the per-round expected
## operator profit
##   Pi(eps) = (1-eta)*lambda*eps - (1 - exp(-beta*eps)) * v_bar*n/tau^2
## is convex in eps (Pi'' > 0), so any interior critical point is a profit
## MINIMUM, not maximum. The rational adversary's optimal policy on the
## feasible amplitude interval [0, eps_max] is bang-bang:
##   play eps_max iff Pi(eps_max) > 0, else 0.
## Default eps_max = 1/beta (detector scale where linear and saturating
## regimes of the hazard cross).

library(testthat)

`%||%` <- function(a, b) if (is.null(a)) b else a

## Locate the repo root portably (see test-expL3-bilinear-surface.R).
.SIM_DIR <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."),
                          mustWork = FALSE)
if (!dir.exists(file.path(.SIM_DIR, "R"))) {
  .SIM_DIR <- if (requireNamespace("here", quietly = TRUE)) {
    here::here()
  } else {
    normalizePath(".", mustWork = TRUE)
  }
}

source(file.path(.SIM_DIR, "R", "sim_operator.R"))

test_that("expected_profit_per_round matches the closed-form value at a canonical point", {
  ## eps=0.5, lambda=0.1, eta=0, tau=12, beta=2, v_bar=1, n=40:
  ## gain   = (1-0)*0.1*0.5 = 0.05
  ## hazard = 1 - exp(-1) ~= 0.6321
  ## loss   = 0.6321 * 40 / 144 ~= 0.17559
  ## Pi     = 0.05 - 0.17559 ~= -0.12559
  pi_val <- expected_profit_per_round(epsilon = 0.5, lambda = 0.1, eta = 0,
                                      tau = 12, beta = 2, v_bar = 1, n = 40)
  expect_equal(pi_val, -0.1256, tolerance = 1e-3)
})

test_that("expected_profit_per_round at tau->infty reduces to (1-eta)*lambda*epsilon", {
  pi_val <- expected_profit_per_round(epsilon = 0.5, lambda = 0.2, eta = 0.3,
                                      tau = Inf, beta = 2, v_bar = 1, n = 40)
  expect_equal(pi_val, (1 - 0.3) * 0.2 * 0.5, tolerance = 1e-9)
})

test_that("bb_adversary_amplitude returns eps_max when Pi(eps_max) > 0 (high lambda, large tau)", {
  ## lambda=0.5, eta=0, tau=100, beta=2, v_bar=1, n=40, eps_max=0.5:
  ## gain = 0.5*0.5 = 0.25; hazard*v_bar*n/tau^2 = 0.6321*40/10000 ~= 0.00253
  ## Pi = 0.25 - 0.00253 ~= 0.2475 > 0 -> play eps_max=0.5.
  amp <- bb_adversary_amplitude(lambda = 0.5, eta = 0, tau = 100,
                                beta = 2, v_bar = 1, n = 40, eps_max = 0.5)
  expect_equal(amp, 0.5, tolerance = 1e-9)
})

test_that("bb_adversary_amplitude returns 0 when Pi(eps_max) < 0 (low tau, loss dominates)", {
  ## lambda=0.5, eta=0, tau=2, beta=2, v_bar=1, n=40, eps_max=0.5:
  ## gain = 0.25; loss = 0.6321 * 40 / 4 = 6.321
  ## Pi = 0.25 - 6.321 < 0 -> play 0.
  amp <- bb_adversary_amplitude(lambda = 0.5, eta = 0, tau = 2,
                                beta = 2, v_bar = 1, n = 40, eps_max = 0.5)
  expect_equal(amp, 0, tolerance = 1e-9)
})

test_that("bb_adversary_amplitude returns eps_max at tau -> infty with positive lambda", {
  ## tau -> infty: penalty vanishes -> Pi = (1-eta)*lambda*eps_max > 0.
  amp <- bb_adversary_amplitude(lambda = 0.1, eta = 0.5, tau = 1e6,
                                beta = 2, v_bar = 1, n = 40, eps_max = 0.5)
  expect_equal(amp, 0.5, tolerance = 1e-9)
})

test_that("bb_adversary_amplitude returns 0 at lambda -> 0", {
  ## lambda -> 0: gain vanishes, only penalty remains, Pi < 0 -> 0.
  amp <- bb_adversary_amplitude(lambda = 0, eta = 0, tau = 10,
                                beta = 2, v_bar = 1, n = 40, eps_max = 0.5)
  expect_equal(amp, 0, tolerance = 1e-9)
})

test_that("bb_adversary_amplitude returns 0 at eta=1 (knife-edge: no operator share)", {
  ## eta=1: gain coefficient (1-eta)*lambda = 0 -> Pi = -loss <= 0 -> 0.
  amp <- bb_adversary_amplitude(lambda = 0.5, eta = 1, tau = 100,
                                beta = 2, v_bar = 1, n = 40, eps_max = 0.5)
  expect_equal(amp, 0, tolerance = 1e-9)
})

test_that("bb_adversary_amplitude defaults eps_max = 1/beta when not supplied", {
  ## With beta=2, default eps_max = 0.5. Verify default path matches explicit.
  amp_default <- bb_adversary_amplitude(lambda = 0.5, eta = 0, tau = 100,
                                        beta = 2, v_bar = 1, n = 40)
  amp_explicit <- bb_adversary_amplitude(lambda = 0.5, eta = 0, tau = 100,
                                         beta = 2, v_bar = 1, n = 40,
                                         eps_max = 1.0 / 2)
  expect_equal(amp_default, amp_explicit, tolerance = 1e-9)
})

test_that("bb_adversary_amplitude is strict at threshold Pi(eps_max) = 0 -> plays 0", {
  ## Construct (lambda, eta, tau) such that Pi(eps_max) is essentially zero.
  ## At eps_max = 1/beta = 0.5, hazard = 1 - exp(-1) ~= 0.6321.
  ## Pi(0.5) = (1-eta)*lambda*0.5 - 0.6321*v_bar*n/tau^2 = 0
  ##   => lambda = 0.6321*v_bar*n/(tau^2 * 0.5 * (1-eta))
  ## with eta=0, v_bar=1, n=40, tau=12: lambda = 0.6321*40/(144*0.5) ~= 0.3512.
  ## Test: at lambda just below threshold => 0; just above => eps_max.
  lambda_thr <- 0.6321 * 40 / (144 * 0.5)
  amp_below <- bb_adversary_amplitude(lambda = lambda_thr - 1e-4, eta = 0,
                                      tau = 12, beta = 2, v_bar = 1, n = 40,
                                      eps_max = 0.5)
  amp_above <- bb_adversary_amplitude(lambda = lambda_thr + 1e-4, eta = 0,
                                      tau = 12, beta = 2, v_bar = 1, n = 40,
                                      eps_max = 0.5)
  expect_equal(amp_below, 0, tolerance = 1e-9)
  expect_equal(amp_above, 0.5, tolerance = 1e-9)
})

test_that("bb_adversary_amplitude required-by-spec call returns 0.5 (positive surplus)", {
  ## Plan spec: bb_adversary_amplitude(0.5, 0, 100, 2, 1, 40, 0.5) -> 0.5.
  expect_equal(
    bb_adversary_amplitude(0.5, 0, 100, 2, 1, 40, 0.5),
    0.5, tolerance = 1e-9
  )
})

test_that("bb_adversary_amplitude required-by-spec call returns 0 (loss dominates)", {
  ## Plan spec: bb_adversary_amplitude(0.5, 0, 2, 2, 1, 40, 0.5) -> 0.
  expect_equal(
    bb_adversary_amplitude(0.5, 0, 2, 2, 1, 40, 0.5),
    0, tolerance = 1e-9
  )
})
