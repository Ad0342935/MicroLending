;; SocialImpactScoring - ESG and community impact tracking for microlending
;; Tracks social, environmental, and governance impact of loans with rewards and scoring

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u300))
(define-constant ERR_IMPACT_NOT_FOUND (err u301))
(define-constant ERR_INVALID_SCORE (err u302))
(define-constant ERR_MILESTONE_NOT_FOUND (err u303))
(define-constant ERR_ALREADY_VERIFIED (err u304))
(define-constant ERR_INSUFFICIENT_IMPACT (err u305))
(define-constant ERR_VERIFICATION_EXPIRED (err u306))
(define-constant ERR_INVALID_METRIC (err u307))

;; Contract constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant MAX_IMPACT_SCORE u100)
(define-constant VERIFICATION_WINDOW u1440) ;; ~10 days
(define-constant MIN_IMPACT_FOR_REWARDS u60)

;; Data variables
(define-data-var next-impact-id uint u1)
(define-data-var next-milestone-id uint u1)
(define-data-var impact-reward-pool uint u0)
(define-data-var verification-fee uint u100000) ;; 0.1 STX

;; Social impact categories and metrics
(define-map loan-impact-profiles
  { loan-id: uint }
  {
    social-score: uint,
    environmental-score: uint,
    governance-score: uint,
    overall-esg-score: uint,
    community-benefit-level: (string-ascii 20),
    impact-category: (string-ascii 30),
    verified-by: (optional principal),
    verification-date: uint,
    impact-description: (string-ascii 300),
    measurable-outcomes: (string-ascii 200)
  })

;; Impact milestones and goals
(define-map impact-milestones
  { milestone-id: uint }
  {
    loan-id: uint,
    milestone-type: (string-ascii 30),
    description: (string-ascii 200),
    target-metric: uint,
    current-progress: uint,
    deadline: uint,
    reward-amount: uint,
    completed: bool,
    verified: bool,
    created-by: principal
  })

;; Community impact verifications
(define-map impact-verifications
  { loan-id: uint, verifier: principal }
  {
    social-impact-confirmed: bool,
    environmental-impact-confirmed: bool,
    governance-impact-confirmed: bool,
    verification-notes: (string-ascii 300),
    verification-timestamp: uint,
    verification-score: uint
  })

;; Impact-based loan rewards
(define-map impact-rewards
  { loan-id: uint }
  {
    base-reward: uint,
    milestone-rewards: uint,
    esg-bonus: uint,
    community-multiplier: uint,
    total-earned: uint,
    last-distribution: uint,
    eligible-for-rates: bool
  })

;; Authorized impact verifiers
(define-map authorized-verifiers
  { verifier: principal }
  {
    specialization: (string-ascii 50),
    verifications-completed: uint,
    accuracy-score: uint,
    active: bool,
    authorized-at: uint
  })

;; Public Functions

;; Register loan for impact tracking
(define-public (register-loan-for-impact
  (loan-id uint)
  (impact-category (string-ascii 30))
  (impact-description (string-ascii 300))
  (measurable-outcomes (string-ascii 200)))
  (let
    ((impact-id (var-get next-impact-id))
     (current-block stacks-block-height))
    
    ;; Verify loan exists in main contract
    (asserts! (is-some (contract-call? .MicroLending get-loan loan-id)) ERR_IMPACT_NOT_FOUND)
    
    (map-set loan-impact-profiles
      { loan-id: loan-id }
      {
        social-score: u0,
        environmental-score: u0,
        governance-score: u0,
        overall-esg-score: u0,
        community-benefit-level: "PENDING",
        impact-category: impact-category,
        verified-by: none,
        verification-date: u0,
        impact-description: impact-description,
        measurable-outcomes: measurable-outcomes
      })
    
    ;; Initialize impact rewards
    (map-set impact-rewards
      { loan-id: loan-id }
      {
        base-reward: u0,
        milestone-rewards: u0,
        esg-bonus: u0,
        community-multiplier: u100,
        total-earned: u0,
        last-distribution: current-block,
        eligible-for-rates: false
      })
    
    (var-set next-impact-id (+ impact-id u1))
    (ok impact-id)))

;; Submit impact verification (by authorized verifiers)
(define-public (verify-loan-impact
  (loan-id uint)
  (social-score uint)
  (environmental-score uint)
  (governance-score uint)
  (verification-notes (string-ascii 300)))
  (let
    ((impact-profile (unwrap! (map-get? loan-impact-profiles { loan-id: loan-id }) ERR_IMPACT_NOT_FOUND))
     (verifier-data (unwrap! (map-get? authorized-verifiers { verifier: tx-sender }) ERR_NOT_AUTHORIZED))
     (current-block stacks-block-height)
     (overall-esg (/ (+ social-score environmental-score governance-score) u3)))
    
    (asserts! (get active verifier-data) ERR_NOT_AUTHORIZED)
    (asserts! (and (<= social-score MAX_IMPACT_SCORE) (<= environmental-score MAX_IMPACT_SCORE) (<= governance-score MAX_IMPACT_SCORE)) ERR_INVALID_SCORE)
    (asserts! (is-none (get verified-by impact-profile)) ERR_ALREADY_VERIFIED)
    
    ;; Pay verification fee
    (try! (stx-transfer? (var-get verification-fee) tx-sender (as-contract tx-sender)))
    
    ;; Update impact profile
    (map-set loan-impact-profiles
      { loan-id: loan-id }
      (merge impact-profile {
        social-score: social-score,
        environmental-score: environmental-score,
        governance-score: governance-score,
        overall-esg-score: overall-esg,
        community-benefit-level: (determine-benefit-level overall-esg),
        verified-by: (some tx-sender),
        verification-date: current-block
      }))
    
    ;; Record verification details
    (map-set impact-verifications
      { loan-id: loan-id, verifier: tx-sender }
      {
        social-impact-confirmed: (>= social-score u60),
        environmental-impact-confirmed: (>= environmental-score u60),
        governance-impact-confirmed: (>= governance-score u60),
        verification-notes: verification-notes,
        verification-timestamp: current-block,
        verification-score: overall-esg
      })
    
    ;; Update verifier stats
    (update-verifier-stats tx-sender)
    
    ;; Calculate and assign impact rewards
    (calculate-impact-rewards loan-id overall-esg)
    (ok overall-esg)))

;; Create impact milestone for a loan
(define-public (create-impact-milestone
  (loan-id uint)
  (milestone-type (string-ascii 30))
  (description (string-ascii 200))
  (target-metric uint)
  (deadline-blocks uint)
  (reward-amount uint))
  (let
    ((milestone-id (var-get next-milestone-id))
     (impact-profile (unwrap! (map-get? loan-impact-profiles { loan-id: loan-id }) ERR_IMPACT_NOT_FOUND))
     (current-block stacks-block-height))
    
    ;; Contribute reward to impact pool
    (try! (stx-transfer? reward-amount tx-sender (as-contract tx-sender)))
    
    (map-set impact-milestones
      { milestone-id: milestone-id }
      {
        loan-id: loan-id,
        milestone-type: milestone-type,
        description: description,
        target-metric: target-metric,
        current-progress: u0,
        deadline: (+ current-block deadline-blocks),
        reward-amount: reward-amount,
        completed: false,
        verified: false,
        created-by: tx-sender
      })
    
    (var-set impact-reward-pool (+ (var-get impact-reward-pool) reward-amount))
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)))

;; Update milestone progress
(define-public (update-milestone-progress (milestone-id uint) (new-progress uint))
  (let
    ((milestone-data (unwrap! (map-get? impact-milestones { milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND))
     (verifier-data (unwrap! (map-get? authorized-verifiers { verifier: tx-sender }) ERR_NOT_AUTHORIZED)))
    
    (asserts! (get active verifier-data) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-progress (get target-metric milestone-data)) ERR_INVALID_METRIC)
    
    (map-set impact-milestones
      { milestone-id: milestone-id }
      (merge milestone-data {
        current-progress: new-progress,
        completed: (>= new-progress (get target-metric milestone-data)),
        verified: true
      }))
    
    ;; Distribute rewards if milestone completed
    (if (>= new-progress (get target-metric milestone-data))
      (distribute-milestone-reward milestone-id)
      (ok true))))

;; Calculate impact-based interest rate discount
(define-read-only (calculate-impact-rate-discount (loan-id uint))
  (match (map-get? loan-impact-profiles { loan-id: loan-id })
    impact-profile
      (let
        ((esg-score (get overall-esg-score impact-profile))
         (community-level (get community-benefit-level impact-profile)))
        (if (>= esg-score u80)
          u50  ;; 0.5% discount for high impact
          (if (>= esg-score u60)
            u25  ;; 0.25% discount for medium impact
            u0))) ;; No discount
    u0))

;; Get comprehensive impact report
(define-read-only (get-impact-report (loan-id uint))
  (let
    ((impact-profile (map-get? loan-impact-profiles { loan-id: loan-id }))
     (impact-rewards-data (map-get? impact-rewards { loan-id: loan-id })))
    {
      impact-profile: impact-profile,
      rewards-earned: impact-rewards-data,
      rate-discount: (calculate-impact-rate-discount loan-id),
      impact-rank: (calculate-impact-rank loan-id),
      community-contributions: (get-community-contribution-score loan-id)
    }))

;; Read-only functions

(define-read-only (get-loan-impact-profile (loan-id uint))
  (map-get? loan-impact-profiles { loan-id: loan-id }))

(define-read-only (get-impact-milestone (milestone-id uint))
  (map-get? impact-milestones { milestone-id: milestone-id }))

(define-read-only (get-impact-verification (loan-id uint) (verifier principal))
  (map-get? impact-verifications { loan-id: loan-id, verifier: verifier }))

(define-read-only (get-impact-rewards (loan-id uint))
  (map-get? impact-rewards { loan-id: loan-id }))

(define-read-only (is-authorized-verifier (verifier principal))
  (is-some (map-get? authorized-verifiers { verifier: verifier })))

;; Private helper functions

;; Determine community benefit level based on ESG score
(define-private (determine-benefit-level (esg-score uint))
  (if (>= esg-score u85)
    "HIGH_IMPACT"
    (if (>= esg-score u70)
      "MEDIUM_IMPACT"
      (if (>= esg-score u50)
        "LOW_IMPACT"
        "MINIMAL_IMPACT"))))

;; Calculate impact rewards based on ESG scores
(define-private (calculate-impact-rewards (loan-id uint) (esg-score uint))
  (let
    ((base-reward (if (>= esg-score MIN_IMPACT_FOR_REWARDS) u50000 u0))
     (esg-bonus (/ (* esg-score u1000) u100))
     (community-multiplier (if (>= esg-score u80) u150 u100)))
    
    (map-set impact-rewards
      { loan-id: loan-id }
      {
        base-reward: base-reward,
        milestone-rewards: u0,
        esg-bonus: esg-bonus,
        community-multiplier: community-multiplier,
        total-earned: (+ base-reward esg-bonus),
        last-distribution: stacks-block-height,
        eligible-for-rates: (>= esg-score MIN_IMPACT_FOR_REWARDS)
      })
    true))

;; Update verifier statistics
(define-private (update-verifier-stats (verifier principal))
  (let
    ((current-stats (unwrap-panic (map-get? authorized-verifiers { verifier: verifier }))))
    (map-set authorized-verifiers
      { verifier: verifier }
      (merge current-stats {
        verifications-completed: (+ (get verifications-completed current-stats) u1),
        accuracy-score: (+ (get accuracy-score current-stats) u5)
      }))
    true))

;; Distribute milestone reward
(define-private (distribute-milestone-reward (milestone-id uint))
  (let
    ((milestone-data (unwrap! (map-get? impact-milestones { milestone-id: milestone-id }) ERR_MILESTONE_NOT_FOUND))
     (reward-amount (get reward-amount milestone-data))
     (loan-id (get loan-id milestone-data)))
    
    ;; Add to milestone rewards
    (match (map-get? impact-rewards { loan-id: loan-id })
      rewards-data
        (map-set impact-rewards
          { loan-id: loan-id }
          (merge rewards-data {
            milestone-rewards: (+ (get milestone-rewards rewards-data) reward-amount),
            total-earned: (+ (get total-earned rewards-data) reward-amount)
          }))
      false)
    
    (ok true)))

;; Calculate impact rank compared to other loans
(define-private (calculate-impact-rank (loan-id uint))
  (match (map-get? loan-impact-profiles { loan-id: loan-id })
    impact-profile
      (let
        ((esg-score (get overall-esg-score impact-profile)))
        (if (>= esg-score u90)
          "TOP_TIER"
          (if (>= esg-score u75)
            "HIGH_IMPACT"
            (if (>= esg-score u50)
              "MODERATE_IMPACT"
              "DEVELOPING_IMPACT"))))
    "UNRANKED"))

;; Get community contribution score
(define-private (get-community-contribution-score (loan-id uint))
  (match (map-get? loan-impact-profiles { loan-id: loan-id })
    impact-profile
      (/ (+ (get social-score impact-profile) (get governance-score impact-profile)) u2)
    u0))

;; Admin functions

;; Authorize impact verifier
(define-public (authorize-verifier (verifier principal) (specialization (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set authorized-verifiers
      { verifier: verifier }
      {
        specialization: specialization,
        verifications-completed: u0,
        accuracy-score: u100,
        active: true,
        authorized-at: stacks-block-height
      })
    (ok true)))

;; Fund impact reward pool
(define-public (fund-impact-rewards (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set impact-reward-pool (+ (var-get impact-reward-pool) amount))
    (ok true)))

;; Set verification fee
(define-public (set-verification-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set verification-fee new-fee)
    (ok true)))

;; Get system statistics
(define-read-only (get-impact-system-stats)
  {
    total-impact-loans: (- (var-get next-impact-id) u1),
    total-milestones: (- (var-get next-milestone-id) u1),
    reward-pool-balance: (var-get impact-reward-pool),
    verification-fee: (var-get verification-fee),
    min-impact-threshold: MIN_IMPACT_FOR_REWARDS
  })
