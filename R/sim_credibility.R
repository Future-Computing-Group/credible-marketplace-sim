suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Credibility enforcement mechanisms ──────────────────────────────
##
## Each mechanism determines how/when operator deviations are detected
## and what penalties are applied.
##
## Mechanism types:
##   - none:       no enforcement
##   - broadcast:  ascending clinching auction with public price history;
##                 agents independently verify prices; any deviation
##                 produces inconsistent public record
##   - blockchain: deposit-commit-reveal (DRA); operator commits rule
##                 on-chain before bids; post-round audit verifies
##                 consistency; deviation produces slashable evidence
##   - exchange:   neutral operator with fixed fee; structural removal
##                 of deviation incentive (domain separation)
##   - regulatory: probabilistic audit with penalty; models external
##                 governance authority with limited resources

make_credibility_mechanism <- function(
  type = c("none", "broadcast", "blockchain", "exchange", "regulatory"),
  tau_audit  = 5,      # audit delay in rounds (blockchain)
  p_detect   = 0.6,    # detection probability (regulatory)
  penalty    = 10.0,   # penalty multiplier for detected deviation
  fee        = 0.01,   # per-transaction fee (exchange)
  deposit    = 5.0,    # operator deposit amount (blockchain)
  slash_rate = 0.5     # fraction of deposit slashed on detection (blockchain)
) {
  type <- match.arg(type)

  list(
    type       = type,
    tau_audit  = tau_audit,
    p_detect   = p_detect,
    penalty    = penalty,
    fee        = fee,
    deposit    = deposit,
    slash_rate = slash_rate,
    # Accumulator for blockchain DRA: stores committed rules and outcomes
    committed_rule  = NULL,
    audit_buffer    = list()
  )
}


## Apply credibility enforcement to an operator's round outcome
enforce_credibility <- function(mechanism, operator_outcome, round,
                                broadcast_prices = NULL, history = NULL) {
  detected   <- FALSE
  penalty_applied <- 0
  latency_overhead <- 0

  switch(mechanism$type,

    none = {
      # No enforcement: deviation goes undetected
    },

    broadcast = {
      # Ascending clinching auction: prices are publicly broadcast at each
      # clock round.  If the operator announced different prices to
      # different agents or computed inconsistent allocations, any agent
      # comparing its observed price trajectory to the public record
      # detects the discrepancy.
      #
      # Detection model: if operator extracted surplus, the price history
      # must be inconsistent with the allocation.  Detection probability
      # depends on whether ascending auction was actually used.
      if (operator_outcome$surplus > 0) {
        if (!is.null(broadcast_prices) && length(broadcast_prices) > 0) {
          # Ascending auction was used: prices are public, any manipulation
          # is immediately detectable by comparing allocation to prices
          detected <- TRUE
        } else {
          # Fallback: sealed-bid but with broadcast commitment
          detected <- TRUE
        }
        penalty_applied <- operator_outcome$surplus * mechanism$penalty
      }
      latency_overhead <- 0  # broadcast adds negligible latency
    },

    blockchain = {
      # Deposit-Commit-Reveal (DRA) protocol:
      # 1. DEPOSIT: operator posts collateral (deposit) before market opens
      # 2. COMMIT:  operator commits allocation rule hash on-chain
      # 3. REVEAL:  after allocation, bids and outcomes posted on-chain
      # 4. AUDIT:   any party can verify consistency within tau_audit rounds
      #
      # If operator deviated, the committed rule and revealed outcome
      # are inconsistent, producing slashable evidence.

      if (operator_outcome$surplus > 0) {
        # Deviation creates inconsistency between committed rule and outcome.
        # Detection occurs at next audit checkpoint.
        if (round %% mechanism$tau_audit == 0) {
          detected <- TRUE
          # Penalty = slash deposit + surplus clawback
          slash_amount <- mechanism$deposit * mechanism$slash_rate
          penalty_applied <- slash_amount + operator_outcome$surplus * mechanism$penalty
        }
      }
      # DRA consensus latency: block confirmation time
      latency_overhead <- 5  # ms
    },

    exchange = {
      # Neutral exchange (domain separation, Proposition 1):
      # Operator earns only fixed per-transaction fee.
      # Any deviation that reduces allocation reduces fee income,
      # so deviation is structurally unprofitable.
      #
      # Model: operator surplus forced to fee income only.
      latency_overhead <- 0
      # Exchange has no deviation incentive by construction
    },

    regulatory = {
      # Probabilistic audit by external governance authority.
      # Each round, audit occurs with probability p_detect.
      # If audit occurs and surplus was extracted, penalty is applied.
      if (operator_outcome$surplus > 0 && runif(1) < mechanism$p_detect) {
        detected <- TRUE
        penalty_applied <- operator_outcome$surplus * mechanism$penalty
      }
      latency_overhead <- 0
    }
  )

  net_surplus <- operator_outcome$surplus - penalty_applied
  # Under exchange, operator surplus is replaced by fee income
  if (mechanism$type == "exchange") {
    net_surplus <- 0  # no surplus extraction possible
  }

  list(
    detected            = detected,
    penalty_applied     = penalty_applied,
    latency_overhead    = latency_overhead,
    net_operator_surplus = net_surplus
  )
}
