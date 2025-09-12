;; title: Loan Portfolio Analytics
;; A comprehensive portfolio management system for lenders with advanced analytics and risk management

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-PORTFOLIO-NOT-FOUND (err u201))
(define-constant ERR-INVALID-ALLOCATION (err u202))
(define-constant ERR-INSUFFICIENT-DIVERSIFICATION (err u203))
(define-constant ERR-RISK-LIMIT-EXCEEDED (err u204))
(define-constant ERR-PORTFOLIO-LOCKED (err u205))
(define-constant ERR-INVALID-STRATEGY (err u206))
(define-constant ERR-ANALYSIS-EXPIRED (err u207))
(define-constant ERR-INVALID-PERFORMANCE-PERIOD (err u208))

;; Portfolio management constants
(define-constant MAX-PORTFOLIO-LOANS u50)
(define-constant MIN-DIVERSIFICATION-SCORE u60)
(define-constant ANALYSIS-VALIDITY-PERIOD u144) ;; ~24 hours
(define-constant PERFORMANCE-CALCULATION-FEE u100000) ;; 0.1 STX

;; Data variables for portfolio system
(define-data-var portfolio-counter uint u0)
(define-data-var analytics-session-counter uint u0)
(define-data-var rebalancing-fee uint u200000) ;; 0.2 STX
(define-data-var max-concentration-limit uint u25) ;; 25% max per loan type

;; Core portfolio structure
(define-map loan-portfolios
    { portfolio-id: uint }
    {
        owner: principal,
        name: (string-ascii 50),
        strategy: (string-ascii 30),
        risk-tolerance: uint,
        target-yield: uint,
        max-exposure: uint,
        creation-height: uint,
        last-rebalanced: uint,
        is-active: bool,
        auto-rebalance: bool
    }
)

;; Portfolio loan allocations
(define-map portfolio-allocations
    { portfolio-id: uint, loan-id: uint }
    {
        allocation-amount: uint,
        weight-percentage: uint,
        entry-rate: uint,
        allocation-date: uint,
        status: (string-ascii 20),
        expected-return: uint
    }
)

;; Portfolio performance metrics
(define-map portfolio-performance
    { portfolio-id: uint, period: uint }
    {
        total-invested: uint,
        total-returned: uint,
        active-loans: uint,
        completed-loans: uint,
        defaulted-loans: uint,
        yield-to-date: uint,
        risk-adjusted-return: uint,
        sharpe-ratio: uint,
        volatility-score: uint,
        diversification-score: uint
    }
)

;; Advanced analytics sessions
(define-map analytics-sessions
    { session-id: uint }
    {
        portfolio-id: uint,
        lender: principal,
        analysis-type: (string-ascii 30),
        start-height: uint,
        completion-height: uint,
        results: (tuple 
            (roi uint)
            (risk-score uint)
            (efficiency-score uint)
            (recommendation (string-ascii 100))
        ),
        is-completed: bool
    }
)

;; Risk assessment framework
(define-map portfolio-risk-metrics
    { portfolio-id: uint }
    {
        var-95: uint, ;; Value at Risk (95% confidence)
        max-drawdown: uint,
        concentration-risk: uint,
        liquidity-risk: uint,
        default-correlation: uint,
        stress-test-score: uint,
        overall-risk-rating: (string-ascii 10),
        last-assessment: uint
    }
)

;; Strategic allocation templates
(define-map allocation-strategies
    { strategy-id: uint }
    {
        name: (string-ascii 30),
        description: (string-ascii 200),
        risk-profile: (string-ascii 20),
        min-diversification: uint,
        max-single-allocation: uint,
        target-sectors: (list 5 (string-ascii 20)),
        rebalance-threshold: uint,
        created-by: principal
    }
)

;; Portfolio rebalancing history
(define-map rebalancing-history
    { portfolio-id: uint, rebalance-id: uint }
    {
        trigger-reason: (string-ascii 50),
        previous-allocation: (list 10 uint),
        new-allocation: (list 10 uint),
        rebalance-cost: uint,
        performance-impact: int,
        timestamp: uint,
        executed-by: principal
    }
)

(define-data-var strategy-counter uint u0)
(define-data-var rebalance-counter uint u0)

;; Create a new loan portfolio
(define-public (create-portfolio 
    (name (string-ascii 50))
    (strategy (string-ascii 30))
    (risk-tolerance uint)
    (target-yield uint)
    (max-exposure uint)
    (auto-rebalance bool))
    (let
        ((portfolio-id (+ (var-get portfolio-counter) u1)))
        
        (asserts! (<= risk-tolerance u100) ERR-INVALID-ALLOCATION)
        (asserts! (> max-exposure u0) ERR-INVALID-ALLOCATION)
        
        (map-set loan-portfolios
            { portfolio-id: portfolio-id }
            {
                owner: tx-sender,
                name: name,
                strategy: strategy,
                risk-tolerance: risk-tolerance,
                target-yield: target-yield,
                max-exposure: max-exposure,
                creation-height: stacks-block-height,
                last-rebalanced: stacks-block-height,
                is-active: true,
                auto-rebalance: auto-rebalance
            }
        )
        
        (var-set portfolio-counter portfolio-id)
        (ok portfolio-id)
    )
)

;; Add loan to portfolio with strategic allocation
(define-public (allocate-loan-to-portfolio 
    (portfolio-id uint)
    (loan-id uint)
    (allocation-amount uint)
    (expected-return uint))
    (let
        ((portfolio (unwrap! (map-get? loan-portfolios { portfolio-id: portfolio-id }) ERR-PORTFOLIO-NOT-FOUND))
         (current-allocations (get-portfolio-loan-count portfolio-id)))
        
        (asserts! (is-eq (get owner portfolio) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active portfolio) ERR-PORTFOLIO-LOCKED)
        (asserts! (< current-allocations MAX-PORTFOLIO-LOANS) ERR-RISK-LIMIT-EXCEEDED)
        (asserts! (<= allocation-amount (get max-exposure portfolio)) ERR-INVALID-ALLOCATION)
        
        (let
            ((portfolio-total (get-portfolio-total-allocation portfolio-id))
             (weight-percentage (if (> portfolio-total u0)
                                  (/ (* allocation-amount u100) portfolio-total)
                                  u100)))
            
            ;; Ensure diversification requirements
            (asserts! (<= weight-percentage (var-get max-concentration-limit)) ERR-INSUFFICIENT-DIVERSIFICATION)
            
            (map-set portfolio-allocations
                { portfolio-id: portfolio-id, loan-id: loan-id }
                {
                    allocation-amount: allocation-amount,
                    weight-percentage: weight-percentage,
                    entry-rate: expected-return,
                    allocation-date: stacks-block-height,
                    status: "ALLOCATED",
                    expected-return: expected-return
                }
            )
            
            (unwrap-panic (update-portfolio-metrics portfolio-id))
            (ok true)
        )
    )
)

;; Generate comprehensive portfolio analytics
(define-public (generate-portfolio-analytics (portfolio-id uint) (analysis-type (string-ascii 30)))
    (let
        ((portfolio (unwrap! (map-get? loan-portfolios { portfolio-id: portfolio-id }) ERR-PORTFOLIO-NOT-FOUND))
         (session-id (+ (var-get analytics-session-counter) u1)))
        
        (asserts! (is-eq (get owner portfolio) tx-sender) ERR-NOT-AUTHORIZED)
        (try! (stx-transfer? PERFORMANCE-CALCULATION-FEE tx-sender (as-contract tx-sender)))
        
        (map-set analytics-sessions
            { session-id: session-id }
            {
                portfolio-id: portfolio-id,
                lender: tx-sender,
                analysis-type: analysis-type,
                start-height: stacks-block-height,
                completion-height: u0,
                results: {
                    roi: u0,
                    risk-score: u0,
                    efficiency-score: u0,
                    recommendation: "ANALYZING"
                },
                is-completed: false
            }
        )
        
        (var-set analytics-session-counter session-id)
        (try! (compute-portfolio-analytics session-id))
        (ok session-id)
    )
)

;; Advanced risk assessment and stress testing
(define-public (conduct-portfolio-risk-assessment (portfolio-id uint))
    (let
        ((portfolio (unwrap! (map-get? loan-portfolios { portfolio-id: portfolio-id }) ERR-PORTFOLIO-NOT-FOUND))
         (performance-data (get-latest-performance-data portfolio-id)))
        
        (asserts! (is-eq (get owner portfolio) tx-sender) ERR-NOT-AUTHORIZED)
        
        (let
            ((concentration-risk (calculate-concentration-risk portfolio-id))
             (liquidity-risk (assess-liquidity-risk portfolio-id))
             (var-95 (calculate-value-at-risk portfolio-id))
             (stress-score (perform-stress-test portfolio-id))
             (overall-rating (determine-risk-rating concentration-risk liquidity-risk stress-score)))
            
            (map-set portfolio-risk-metrics
                { portfolio-id: portfolio-id }
                {
                    var-95: var-95,
                    max-drawdown: (calculate-max-drawdown performance-data),
                    concentration-risk: concentration-risk,
                    liquidity-risk: liquidity-risk,
                    default-correlation: (calculate-default-correlation portfolio-id),
                    stress-test-score: stress-score,
                    overall-risk-rating: overall-rating,
                    last-assessment: stacks-block-height
                }
            )
            (ok true)
        )
    )
)

;; Intelligent portfolio rebalancing
(define-public (rebalance-portfolio (portfolio-id uint) (trigger-reason (string-ascii 50)))
    (let
        ((portfolio (unwrap! (map-get? loan-portfolios { portfolio-id: portfolio-id }) ERR-PORTFOLIO-NOT-FOUND))
         (rebalance-id (+ (var-get rebalance-counter) u1)))
        
        (asserts! (is-eq (get owner portfolio) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active portfolio) ERR-PORTFOLIO-LOCKED)
        
        (try! (stx-transfer? (var-get rebalancing-fee) tx-sender (as-contract tx-sender)))
        
        (let
            ((current-allocation (get-current-allocation-weights portfolio-id))
             (optimal-allocation (calculate-optimal-allocation portfolio-id))
             (rebalance-cost (var-get rebalancing-fee)))
            
            (map-set rebalancing-history
                { portfolio-id: portfolio-id, rebalance-id: rebalance-id }
                {
                    trigger-reason: trigger-reason,
                    previous-allocation: current-allocation,
                    new-allocation: optimal-allocation,
                    rebalance-cost: rebalance-cost,
                    performance-impact: 0, ;; Will be calculated post-rebalancing
                    timestamp: stacks-block-height,
                    executed-by: tx-sender
                }
            )
            
            (map-set loan-portfolios
                { portfolio-id: portfolio-id }
                (merge portfolio { last-rebalanced: stacks-block-height })
            )
            
            (var-set rebalance-counter rebalance-id)
            (ok rebalance-id)
        )
    )
)

;; Create custom allocation strategy
(define-public (create-allocation-strategy
    (name (string-ascii 30))
    (description (string-ascii 200))
    (risk-profile (string-ascii 20))
    (min-diversification uint)
    (max-single-allocation uint)
    (rebalance-threshold uint))
    (let
        ((strategy-id (+ (var-get strategy-counter) u1)))
        
        (asserts! (<= max-single-allocation u50) ERR-INVALID-STRATEGY)
        (asserts! (>= min-diversification u30) ERR-INSUFFICIENT-DIVERSIFICATION)
        
        (map-set allocation-strategies
            { strategy-id: strategy-id }
            {
                name: name,
                description: description,
                risk-profile: risk-profile,
                min-diversification: min-diversification,
                max-single-allocation: max-single-allocation,
                target-sectors: (list "PERSONAL" "BUSINESS" "EDUCATION" "REAL_ESTATE" "STARTUP"),
                rebalance-threshold: rebalance-threshold,
                created-by: tx-sender
            }
        )
        
        (var-set strategy-counter strategy-id)
        (ok strategy-id)
    )
)

;; Private helper functions for calculations

(define-private (get-portfolio-loan-count (portfolio-id uint))
    ;; Simplified implementation - would iterate through allocations
    u0
)

(define-private (get-portfolio-total-allocation (portfolio-id uint))
    ;; Would sum all allocation amounts for the portfolio
    u1000000 ;; Placeholder
)

(define-private (update-portfolio-metrics (portfolio-id uint))
    ;; Update performance metrics based on current allocations
    (ok true)
)

(define-private (compute-portfolio-analytics (session-id uint))
    (let
        ((session (unwrap! (map-get? analytics-sessions { session-id: session-id }) ERR-ANALYSIS-EXPIRED))
         (roi (calculate-portfolio-roi (get portfolio-id session)))
         (risk-score (assess-portfolio-risk (get portfolio-id session)))
         (efficiency (calculate-efficiency-ratio (get portfolio-id session))))
        
        (map-set analytics-sessions
            { session-id: session-id }
            (merge session {
                completion-height: stacks-block-height,
                results: {
                    roi: roi,
                    risk-score: risk-score,
                    efficiency-score: efficiency,
                    recommendation: (generate-recommendation roi risk-score efficiency)
                },
                is-completed: true
            })
        )
        (ok true)
    )
)

(define-private (calculate-portfolio-roi (portfolio-id uint))
    ;; Calculate return on investment
    u750 ;; 7.5% placeholder
)

(define-private (assess-portfolio-risk (portfolio-id uint))
    ;; Assess overall risk score
    u65 ;; Medium risk placeholder
)

(define-private (calculate-efficiency-ratio (portfolio-id uint))
    ;; Calculate risk-adjusted efficiency
    u80 ;; Good efficiency placeholder
)

(define-private (generate-recommendation (roi uint) (risk uint) (efficiency uint))
    (if (and (> roi u600) (< risk u70))
        "MAINTAIN_STRATEGY"
        (if (> risk u80)
            "REDUCE_RISK"
            "INCREASE_YIELD"
        )
    )
)

(define-private (calculate-concentration-risk (portfolio-id uint))
    ;; Calculate concentration risk based on allocation distribution
    u45 ;; Medium concentration risk
)

(define-private (assess-liquidity-risk (portfolio-id uint))
    ;; Assess how quickly portfolio can be liquidated
    u30 ;; Low liquidity risk
)

(define-private (calculate-value-at-risk (portfolio-id uint))
    ;; Calculate 95% VaR
    u120000 ;; 12% of portfolio value
)

(define-private (perform-stress-test (portfolio-id uint))
    ;; Simulate portfolio performance under stress scenarios
    u75 ;; Good stress test score
)

(define-private (determine-risk-rating (concentration uint) (liquidity uint) (stress uint))
    (if (and (< concentration u50) (< liquidity u40) (> stress u70))
        "LOW"
        (if (and (< concentration u70) (< liquidity u60) (> stress u50))
            "MEDIUM"
            "HIGH"
        )
    )
)

(define-private (calculate-max-drawdown (performance-data (tuple (roi uint))))
    ;; Calculate maximum peak-to-trough decline
    u8 ;; 8% max drawdown
)

(define-private (get-latest-performance-data (portfolio-id uint))
    { roi: u750 } ;; Simplified performance data
)

(define-private (calculate-default-correlation (portfolio-id uint))
    ;; Calculate correlation between defaults in portfolio
    u15 ;; Low correlation
)

(define-private (get-current-allocation-weights (portfolio-id uint))
    (list u20 u25 u15 u20 u20) ;; Current weights
)

(define-private (calculate-optimal-allocation (portfolio-id uint))
    (list u22 u23 u18 u19 u18) ;; Optimal weights
)

;; Read-only functions

(define-read-only (get-portfolio (portfolio-id uint))
    (map-get? loan-portfolios { portfolio-id: portfolio-id })
)

(define-read-only (get-portfolio-allocation (portfolio-id uint) (loan-id uint))
    (map-get? portfolio-allocations { portfolio-id: portfolio-id, loan-id: loan-id })
)

(define-read-only (get-portfolio-performance (portfolio-id uint) (period uint))
    (map-get? portfolio-performance { portfolio-id: portfolio-id, period: period })
)

(define-read-only (get-analytics-session (session-id uint))
    (map-get? analytics-sessions { session-id: session-id })
)

(define-read-only (get-portfolio-risk-metrics (portfolio-id uint))
    (map-get? portfolio-risk-metrics { portfolio-id: portfolio-id })
)

(define-read-only (get-allocation-strategy (strategy-id uint))
    (map-get? allocation-strategies { strategy-id: strategy-id })
)

(define-read-only (get-rebalancing-history (portfolio-id uint) (rebalance-id uint))
    (map-get? rebalancing-history { portfolio-id: portfolio-id, rebalance-id: rebalance-id })
)

(define-read-only (get-portfolio-summary (portfolio-id uint))
    (let
        ((portfolio (map-get? loan-portfolios { portfolio-id: portfolio-id }))
         (risk-metrics (map-get? portfolio-risk-metrics { portfolio-id: portfolio-id })))
        {
            portfolio: portfolio,
            risk-assessment: risk-metrics,
            total-loans: (get-portfolio-loan-count portfolio-id),
            last-analysis: u0
        }
    )
)

;; Admin functions
(define-public (set-portfolio-parameters (max-loans uint) (rebalance-fee-new uint) (max-concentration uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set rebalancing-fee rebalance-fee-new)
        (var-set max-concentration-limit max-concentration)
        (ok true)
    )
)

(define-constant CONTRACT-OWNER tx-sender)
