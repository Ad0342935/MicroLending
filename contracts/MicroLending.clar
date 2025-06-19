
;; title: MicroLending

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-LOAN-NOT-FOUND (err u103))
(define-constant ERR-ALREADY-ACTIVE-LOAN (err u104))
(define-constant ERR-LOAN-NOT-ACTIVE (err u105))
(define-constant ERR-COLLATERAL-REQUIRED (err u106))

;; Data Variables
(define-data-var platform-fee uint u5) ;; 5% platform fee
(define-data-var minimum-collateral uint u1000000) ;; in micro STX

;; Data Maps
(define-map loans
    { loan-id: uint }
    {
        borrower: principal,
        lender: (optional principal),
        amount: uint,
        collateral: uint,
        interest-rate: uint,
        term-length: uint,
        status: (string-ascii 20),
        start-height: uint,
        repaid-amount: uint
    }
)

(define-map user-credit-scores
    { user: principal }
    { 
        score: uint,
        loans-taken: uint,
        loans-repaid: uint
    }
)

(define-map user-balances
    { user: principal }
    { balance: uint }
)

;; Counter for loan IDs
(define-data-var loan-counter uint u0)

;; Public Functions

;; Deposit STX to platform
(define-public (deposit-stx (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set user-balances 
            { user: tx-sender }
            { balance: (+ (get-user-balance tx-sender) amount) }
        )
        (ok true)
    )
)

;; Request a loan
(define-public (request-loan (amount uint) (collateral uint) (interest-rate uint) (term-length uint))
    (let
        (
            (loan-id (+ (var-get loan-counter) u1))
            (user-credit (default-to 
                { score: u0, loans-taken: u0, loans-repaid: u0 }
                (map-get? user-credit-scores { user: tx-sender })))
        )
        (asserts! (>= collateral (var-get minimum-collateral)) ERR-COLLATERAL-REQUIRED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        
        ;; Transfer collateral
        (try! (stx-transfer? collateral tx-sender (as-contract tx-sender)))
        
        ;; Create loan
        (map-set loans
            { loan-id: loan-id }
            {
                borrower: tx-sender,
                lender: none,
                amount: amount,
                collateral: collateral,
                interest-rate: interest-rate,
                term-length: term-length,
                status: "REQUESTED",
                start-height: u0,
                repaid-amount: u0
            }
        )
        
        ;; Update loan counter
        (var-set loan-counter loan-id)
        (ok loan-id)
    )
)

;; Fund a loan
(define-public (fund-loan (loan-id uint))
    (let
        ((loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
         (lender-balance (get-user-balance tx-sender)))
        
        (asserts! (is-eq (get status loan) "REQUESTED") ERR-LOAN-NOT-ACTIVE)
        (asserts! (>= lender-balance (get amount loan)) ERR-INSUFFICIENT-BALANCE)
        
        ;; Update loan status
        (map-set loans
            { loan-id: loan-id }
            (merge loan {
                lender: (some tx-sender),
                status: "ACTIVE",
                start-height: stacks-block-height
            })
        )
        
        ;; Transfer funds to borrower
        (try! (as-contract (stx-transfer? (get amount loan) tx-sender (get borrower loan))))
        (ok true)
    )
)

;; Repay loan
(define-public (repay-loan (loan-id uint) (payment uint))
    (let
        ((loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
         (remaining (- (get amount loan) (get repaid-amount loan))))
        
        (asserts! (is-eq (get status loan) "ACTIVE") ERR-LOAN-NOT-ACTIVE)
        (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
        
        ;; Process payment
        (try! (stx-transfer? payment tx-sender (as-contract tx-sender)))
        
        ;; Update loan
        (map-set loans
            { loan-id: loan-id }
            (merge loan {
                repaid-amount: (+ (get repaid-amount loan) payment),
                status: (if (>= payment remaining) "COMPLETED" (get status loan))
            })
        )
        
        ;; Update credit score if loan completed
        (if (>= payment remaining)
            (update-credit-score tx-sender true)
            true
        )
        (ok true)
    )
)

;; Private Functions

;; Get user balance
(define-private (get-user-balance (user principal))
    (default-to u0 (get balance (map-get? user-balances { user: user })))
)

;; Update credit score
(define-private (update-credit-score (user principal) (success bool))
    (let
        ((current-credit (default-to
            { score: u0, loans-taken: u0, loans-repaid: u0 }
            (map-get? user-credit-scores { user: user }))))
        (map-set user-credit-scores
            { user: user }
            {
                score: (if success (+ (get score current-credit) u10) (get score current-credit)),
                loans-taken: (+ (get loans-taken current-credit) u1),
                loans-repaid: (if success (+ (get loans-repaid current-credit) u1) (get loans-repaid current-credit))
            }
        )
        true
    )
)

;; Read-only Functions

;; Get loan details
(define-read-only (get-loan (loan-id uint))
    (map-get? loans { loan-id: loan-id })
)

;; Get user credit score
(define-read-only (get-credit-score (user principal))
    (map-get? user-credit-scores { user: user })
)




(define-constant ERR-CANNOT-CANCEL (err u107))

(define-public (cancel-loan (loan-id uint))
    (let
        ((loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND)))
        
        (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status loan) "REQUESTED") ERR-CANNOT-CANCEL)
        
        (try! (as-contract (stx-transfer? (get collateral loan) (as-contract tx-sender) tx-sender)))
        
        (map-set loans
            { loan-id: loan-id }
            (merge loan { status: "CANCELLED" })
        )
        (ok true)
    )
)



(define-private (calculate-early-repayment-bonus (start-height uint) (term-length uint))
    (let
        ((current-height stacks-block-height)
         (expected-end (+ start-height term-length)))
        (if (< current-height expected-end)
            u20
            u10)
    )
)

(define-public (early-repay-loan (loan-id uint))
    (let
        ((loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
         (remaining (- (get amount loan) (get repaid-amount loan)))
         (bonus-points (calculate-early-repayment-bonus (get start-height loan) (get term-length loan))))
        
        (asserts! (is-eq (get status loan) "ACTIVE") ERR-LOAN-NOT-ACTIVE)
        (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
        
        (try! (stx-transfer? remaining tx-sender (as-contract tx-sender)))
        
        (map-set loans
            { loan-id: loan-id }
            (merge loan {
                repaid-amount: (get amount loan),
                status: "COMPLETED"
            })
        )
        
        (update-credit-score-with-bonus tx-sender bonus-points)
        (ok true)
    )
)

(define-private (update-credit-score-with-bonus (user principal) (bonus uint))
    (let
        ((current-credit (default-to
            { score: u0, loans-taken: u0, loans-repaid: u0 }
            (map-get? user-credit-scores { user: user }))))
        (map-set user-credit-scores
            { user: user }
            {
                score: (+ (get score current-credit) bonus),
                loans-taken: (get loans-taken current-credit),
                loans-repaid: (+ (get loans-repaid current-credit) u1)
            }
        )
        true
    )
)


(define-map loan-types
    { type-id: uint }
    {
        name: (string-ascii 20),
        min-collateral: uint,
        max-amount: uint,
        min-credit-score: uint,
        interest-rate-range: (tuple (min uint) (max uint))
    }
)

(define-public (add-loan-type (type-id uint) (name (string-ascii 20)) (min-collateral uint) 
               (max-amount uint) (min-credit-score uint) (min-rate uint) (max-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set loan-types
            { type-id: type-id }
            {
                name: name,
                min-collateral: min-collateral,
                max-amount: max-amount,
                min-credit-score: min-credit-score,
                interest-rate-range: { min: min-rate, max: max-rate }
            }
        )
        (ok true)
    )
)

(define-read-only (get-loan-type (type-id uint))
    (map-get? loan-types { type-id: type-id })
)


(define-map lender-ratings
    { lender: principal }
    {
        loans-funded: uint,
        total-amount-lent: uint,
        active-loans: uint,
        rating: uint
    }
)

(define-public (update-lender-rating (lender principal) (amount uint))
    (let
        ((current-rating (default-to
            { loans-funded: u0, total-amount-lent: u0, active-loans: u0, rating: u0 }
            (map-get? lender-ratings { lender: lender }))))
        (map-set lender-ratings
            { lender: lender }
            {
                loans-funded: (+ (get loans-funded current-rating) u1),
                total-amount-lent: (+ (get total-amount-lent current-rating) amount),
                active-loans: (+ (get active-loans current-rating) u1),
                rating: (+ (get rating current-rating) u1)
            }
        )
        (ok true)
    )
)

(define-read-only (get-lender-rating (lender principal))
    (map-get? lender-ratings { lender: lender })
)


(define-constant ERR-EXTENSION-NOT-ALLOWED (err u108))

(define-map loan-extensions
    { loan-id: uint }
    {
        requested-blocks: uint,
        status: (string-ascii 20)
    }
)

(define-public (request-loan-extension (loan-id uint) (additional-blocks uint))
    (let
        ((loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND)))
        
        (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status loan) "ACTIVE") ERR-LOAN-NOT-ACTIVE)
        
        (map-set loan-extensions
            { loan-id: loan-id }
            {
                requested-blocks: additional-blocks,
                status: "PENDING"
            }
        )
        (ok true)
    )
)


(define-map referrals
    { referrer: principal }
    {
        total-referrals: uint,
        active-referrals: uint,
        rewards-earned: uint
    }
)

(define-constant REFERRAL-REWARD u100000) ;; in micro STX

(define-public (register-referral (referrer principal))
    (let
        ((current-stats (default-to
            { total-referrals: u0, active-referrals: u0, rewards-earned: u0 }
            (map-get? referrals { referrer: referrer }))))
        
        (map-set referrals
            { referrer: referrer }
            {
                total-referrals: (+ (get total-referrals current-stats) u1),
                active-referrals: (+ (get active-referrals current-stats) u1),
                rewards-earned: (+ (get rewards-earned current-stats) REFERRAL-REWARD)
            }
        )
        
        (try! (as-contract (stx-transfer? REFERRAL-REWARD (as-contract tx-sender) referrer)))
        (ok true)
    )
)

(define-read-only (get-referral-stats (referrer principal))
    (map-get? referrals { referrer: referrer })
)






(define-map insurance-pool
    { pool-id: uint }
    {
        total-amount: uint,
        coverage-ratio: uint,
        active-policies: uint,
        claims-paid: uint
    }
)

(define-map loan-insurance
    { loan-id: uint }
    {
        insured-amount: uint,
        premium-paid: uint,
        is-active: bool
    }
)

(define-public (contribute-to-insurance-pool (amount uint))
    (let
        ((current-pool (default-to
            { total-amount: u0, coverage-ratio: u50, active-policies: u0, claims-paid: u0 }
            (map-get? insurance-pool { pool-id: u1 }))))
        
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        (map-set insurance-pool
            { pool-id: u1 }
            {
                total-amount: (+ (get total-amount current-pool) amount),
                coverage-ratio: (get coverage-ratio current-pool),
                active-policies: (get active-policies current-pool),
                claims-paid: (get claims-paid current-pool)
            }
        )
        (ok true)
    )
)

(define-public (insure-loan (loan-id uint) (coverage-amount uint))
    (let
        ((loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
         (premium (* coverage-amount u01)))
        
        (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
        
        (map-set loan-insurance
            { loan-id: loan-id }
            {
                insured-amount: coverage-amount,
                premium-paid: premium,
                is-active: true
            }
        )
        (ok true)
    )
)


(define-map loan-auctions
    { auction-id: uint }
    {
        loan-id: uint,
        min-rate: uint,
        max-rate: uint,
        best-bid: uint,
        best-bidder: (optional principal),
        end-height: uint,
        status: (string-ascii 20)
    }
)

(define-data-var auction-counter uint u0)

(define-public (create-loan-auction (loan-id uint) (min-rate uint) (max-rate uint) (duration uint))
    (let
        ((auction-id (+ (var-get auction-counter) u1))
         (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND)))
        
        (asserts! (is-eq (get status loan) "REQUESTED") ERR-LOAN-NOT-ACTIVE)
        (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
        
        (map-set loan-auctions
            { auction-id: auction-id }
            {
                loan-id: loan-id,
                min-rate: min-rate,
                max-rate: max-rate,
                best-bid: max-rate,
                best-bidder: none,
                end-height: (+ stacks-block-height duration),
                status: "ACTIVE"
            }
        )
        
        (var-set auction-counter auction-id)
        (ok auction-id)
    )
)

(define-public (place-bid (auction-id uint) (bid-rate uint))
    (let
        ((auction (unwrap! (map-get? loan-auctions { auction-id: auction-id }) ERR-LOAN-NOT-FOUND)))
        
        (asserts! (< bid-rate (get best-bid auction)) ERR-INVALID-AMOUNT)
        (asserts! (>= bid-rate (get min-rate auction)) ERR-INVALID-AMOUNT)
        (asserts! (< stacks-block-height (get end-height auction)) ERR-LOAN-NOT-ACTIVE)
        
        (map-set loan-auctions
            { auction-id: auction-id }
            (merge auction {
                best-bid: bid-rate,
                best-bidder: (some tx-sender)
            })
        )
        (ok true)
    )
)


(define-map payment-schedules
    { loan-id: uint }
    {
        total-payments: uint,
        payment-amount: uint,
        payment-interval: uint,
        payments-made: uint,
        next-payment-height: uint
    }
)

(define-public (create-payment-schedule (loan-id uint) (num-payments uint))
    (let
        ((loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
         (payment-amount (/ (get amount loan) num-payments))
         (payment-interval (/ (get term-length loan) num-payments)))
        
        (asserts! (is-eq (get status loan) "ACTIVE") ERR-LOAN-NOT-ACTIVE)
        (asserts! (> num-payments u0) ERR-INVALID-AMOUNT)
        
        (map-set payment-schedules
            { loan-id: loan-id }
            {
                total-payments: num-payments,
                payment-amount: payment-amount,
                payment-interval: payment-interval,
                payments-made: u0,
                next-payment-height: (+ stacks-block-height payment-interval)
            }
        )
        (ok true)
    )
)

(define-public (make-scheduled-payment (loan-id uint))
    (let
        ((schedule (unwrap! (map-get? payment-schedules { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
         (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND)))
        
        (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (<= stacks-block-height (get next-payment-height schedule)) ERR-LOAN-NOT-ACTIVE)
        
        (try! (stx-transfer? (get payment-amount schedule) tx-sender (as-contract tx-sender)))
        
        (map-set payment-schedules
            { loan-id: loan-id }
            (merge schedule {
                payments-made: (+ (get payments-made schedule) u1),
                next-payment-height: (+ (get next-payment-height schedule) (get payment-interval schedule))
            })
        )
        (ok true)
    )
)

(define-constant ERR-LOAN-DEFAULTED (err u109))
(define-constant ERR-NOT-DEFAULTED (err u110))
(define-constant ERR-ALREADY-LIQUIDATED (err u111))

(define-data-var default-grace-period uint u1440)
(define-data-var liquidation-penalty uint u10)

(define-map loan-defaults
    { loan-id: uint }
    {
        default-height: uint,
        liquidated: bool,
        liquidation-amount: uint,
        penalty-applied: uint
    }
)

(define-map platform-treasury
    { treasury-id: uint }
    { balance: uint }
)

(define-public (check-loan-default (loan-id uint))
    (let
        ((loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
         (schedule (map-get? payment-schedules { loan-id: loan-id })))
        
        (asserts! (is-eq (get status loan) "ACTIVE") ERR-LOAN-NOT-ACTIVE)
        
        (if (is-some schedule)
            (let ((sched (unwrap-panic schedule)))
                (if (and 
                    (> stacks-block-height (+ (get next-payment-height sched) (var-get default-grace-period)))
                    (< (get payments-made sched) (get total-payments sched)))
                    (begin
                        (try! (mark-loan-default loan-id))
                        (ok true))
                    (ok false)))
            (if (> stacks-block-height (+ (+ (get start-height loan) (get term-length loan)) (var-get default-grace-period)))
                (begin
                    (try! (mark-loan-default loan-id))
                    (ok true))
                (ok false)))
    )
)

(define-private (mark-loan-default (loan-id uint))
    (let
        ((loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND)))
        
        (map-set loans
            { loan-id: loan-id }
            (merge loan { status: "DEFAULTED" })
        )
        
        (map-set loan-defaults
            { loan-id: loan-id }
            {
                default-height: stacks-block-height,
                liquidated: false,
                liquidation-amount: u0,
                penalty-applied: (/ (* (get collateral loan) (var-get liquidation-penalty)) u100)
            }
        )
        
        (update-credit-score-default (get borrower loan))
        (ok true)
    )
)

;; Helper function for min
(define-private (min (a uint) (b uint))
    (if (< a b) a b)
)

(define-public (liquidate-collateral (loan-id uint))
    (let
        ((loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
         (default-info (unwrap! (map-get? loan-defaults { loan-id: loan-id }) ERR-NOT-DEFAULTED))
         (lender (unwrap! (get lender loan) ERR-LOAN-NOT-FOUND))
         (remaining-debt (- (get amount loan) (get repaid-amount loan)))
         (liquidation-amount (min (get collateral loan) remaining-debt))
         (treasury-amount (- (get collateral loan) liquidation-amount)))
        
        (asserts! (is-eq (get status loan) "DEFAULTED") ERR-NOT-DEFAULTED)
        (asserts! (not (get liquidated default-info)) ERR-ALREADY-LIQUIDATED)
        
        (if (> liquidation-amount u0)
            (try! (as-contract (stx-transfer? liquidation-amount (as-contract tx-sender) lender)))
            true)
        
        (if (> treasury-amount u0)
            (begin
                (unwrap! (add-to-treasury treasury-amount) (err u102))
                true)
            true)
        
        (map-set loan-defaults
            { loan-id: loan-id }
            (merge default-info {
                liquidated: true,
                liquidation-amount: liquidation-amount
            })
        )
        
        (map-set loans
            { loan-id: loan-id }
            (merge loan { status: "LIQUIDATED" })
        )
        
        (ok liquidation-amount)
    )
)

(define-public (recover-partial-default (loan-id uint) (recovery-amount uint))
    (let
        ((loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
         (default-info (unwrap! (map-get? loan-defaults { loan-id: loan-id }) ERR-NOT-DEFAULTED))
         (remaining-debt (- (get amount loan) (get repaid-amount loan))))
        
        (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status loan) "DEFAULTED") ERR-NOT-DEFAULTED)
        (asserts! (not (get liquidated default-info)) ERR-ALREADY-LIQUIDATED)
        (asserts! (<= recovery-amount remaining-debt) ERR-INVALID-AMOUNT)
        
        (try! (stx-transfer? recovery-amount tx-sender (as-contract tx-sender)))
        
        (let ((new-repaid (+ (get repaid-amount loan) recovery-amount)))
            (map-set loans
                { loan-id: loan-id }
                (merge loan {
                    repaid-amount: new-repaid,
                    status: (if (>= new-repaid (get amount loan)) "COMPLETED" "DEFAULTED")
                })
            )
        )
        
        (if (>= (+ (get repaid-amount loan) recovery-amount) (get amount loan))
            (begin
                (try! (as-contract (stx-transfer? (get collateral loan) (as-contract tx-sender) (get borrower loan))))
                (update-credit-score-recovery tx-sender)
                (ok true))
            (ok true))
    )
)

(define-private (add-to-treasury (amount uint))
    (let
        ((current-balance (default-to u0 (get balance (map-get? platform-treasury { treasury-id: u1 })))))
        (map-set platform-treasury
            { treasury-id: u1 }
            { balance: (+ current-balance amount) }
        )
        (ok true)
    )
)

(define-private (update-credit-score-default (user principal))
    (let
        ((current-credit (default-to
            { score: u0, loans-taken: u0, loans-repaid: u0 }
            (map-get? user-credit-scores { user: user }))))
        (map-set user-credit-scores
            { user: user }
            {
                score: (if (>= (get score current-credit) u50) (- (get score current-credit) u50) u0),
                loans-taken: (get loans-taken current-credit),
                loans-repaid: (get loans-repaid current-credit)
            }
        )
        true
    )
)

(define-private (update-credit-score-recovery (user principal))
    (let
        ((current-credit (default-to
            { score: u0, loans-taken: u0, loans-repaid: u0 }
            (map-get? user-credit-scores { user: user }))))
        (map-set user-credit-scores
            { user: user }
            {
                score: (+ (get score current-credit) u25),
                loans-taken: (get loans-taken current-credit),
                loans-repaid: (+ (get loans-repaid current-credit) u1)
            }
        )
        true
    )
)

(define-public (set-default-parameters (grace-period uint) (penalty uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set default-grace-period grace-period)
        (var-set liquidation-penalty penalty)
        (ok true)
    )
)

(define-read-only (get-loan-default-info (loan-id uint))
    (map-get? loan-defaults { loan-id: loan-id })
)

(define-read-only (is-loan-overdue (loan-id uint))
    (let
        ((loan (map-get? loans { loan-id: loan-id }))
         (schedule (map-get? payment-schedules { loan-id: loan-id })))
        (if (and (is-some loan) (is-some schedule))
            (let 
                ((l (unwrap-panic loan))
                 (s (unwrap-panic schedule)))
                (and 
                    (is-eq (get status l) "ACTIVE")
                    (> stacks-block-height (get next-payment-height s))
                    (< (get payments-made s) (get total-payments s))))
            (if (is-some loan)
                (let ((l (unwrap-panic loan)))
                    (and
                        (is-eq (get status l) "ACTIVE")
                        (> stacks-block-height (+ (get start-height l) (get term-length l)))))
                false))
    )
)

(define-read-only (get-platform-treasury-balance)
    (default-to u0 (get balance (map-get? platform-treasury { treasury-id: u1 })))
)

(define-read-only (get-default-parameters)
    {
        grace-period: (var-get default-grace-period),
        liquidation-penalty: (var-get liquidation-penalty)
    }
)