
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

(define-constant ERR-DISPUTE-EXISTS (err u112))
(define-constant ERR-DISPUTE-NOT-FOUND (err u113))
(define-constant ERR-INVALID-DISPUTE-TYPE (err u114))
(define-constant ERR-VOTING-ENDED (err u115))
(define-constant ERR-ALREADY-VOTED (err u116))
(define-constant ERR-NOT-JURY-MEMBER (err u117))
(define-constant ERR-DISPUTE-NOT-RESOLVED (err u118))
(define-constant ERR-INSUFFICIENT-CREDIT (err u119))

(define-data-var dispute-counter uint u0)
(define-data-var dispute-fee uint u500000)
(define-data-var voting-period uint u2016)
(define-data-var jury-size uint u5)
(define-data-var min-jury-credit uint u50)

(define-map loan-disputes
    { dispute-id: uint }
    {
        loan-id: uint,
        initiator: principal,
        respondent: principal,
        dispute-type: (string-ascii 30),
        description: (string-ascii 500),
        status: (string-ascii 20),
        voting-end-height: uint,
        votes-for: uint,
        votes-against: uint,
        fee-paid: uint,
        resolution: (string-ascii 20)
    }
)

(define-map dispute-jury
    { dispute-id: uint, juror: principal }
    {
        has-voted: bool,
        vote: (string-ascii 20),
        reward-claimed: bool
    }
)

(define-map jury-pool
    { juror: principal }
    {
        disputes-judged: uint,
        reputation-score: uint,
        total-rewards: uint,
        is-active: bool
    }
)

(define-public (create-loan-dispute (loan-id uint) (dispute-type (string-ascii 30)) (description (string-ascii 500)))
    (let
        ((loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-FOUND))
         (dispute-id (+ (var-get dispute-counter) u1))
         (lender (unwrap! (get lender loan) ERR-LOAN-NOT-FOUND))
         (borrower (get borrower loan))
         (fee-amount (var-get dispute-fee)))
        
        (asserts! (is-none (get-active-dispute loan-id)) ERR-DISPUTE-EXISTS)
        (asserts! (or (is-eq tx-sender borrower) (is-eq tx-sender lender)) ERR-NOT-AUTHORIZED)
        (asserts! (or (is-eq dispute-type "PAYMENT_DISPUTE") 
                      (is-eq dispute-type "COLLATERAL_DISPUTE")
                      (is-eq dispute-type "TERMS_DISPUTE")
                      (is-eq dispute-type "DEFAULT_DISPUTE")) ERR-INVALID-DISPUTE-TYPE)
        
        (try! (stx-transfer? fee-amount tx-sender (as-contract tx-sender)))
        
        (map-set loan-disputes
            { dispute-id: dispute-id }
            {
                loan-id: loan-id,
                initiator: tx-sender,
                respondent: (if (is-eq tx-sender borrower) lender borrower),
                dispute-type: dispute-type,
                description: description,
                status: "PENDING",
                voting-end-height: (+ stacks-block-height (var-get voting-period)),
                votes-for: u0,
                votes-against: u0,
                fee-paid: fee-amount,
                resolution: "NONE"
            }
        )
        
        (var-set dispute-counter dispute-id)
        (unwrap! (select-jury-for-dispute dispute-id) (err u200))
        (ok dispute-id)
    )
)

(define-public (join-jury-pool)
    (let
        ((user-credit (default-to
            { score: u0, loans-taken: u0, loans-repaid: u0 }
            (map-get? user-credit-scores { user: tx-sender }))))
        
        (asserts! (>= (get score user-credit) (var-get min-jury-credit)) ERR-INSUFFICIENT-CREDIT)
        
        (map-set jury-pool
            { juror: tx-sender }
            {
                disputes-judged: u0,
                reputation-score: (get score user-credit),
                total-rewards: u0,
                is-active: true
            }
        )
        (ok true)
    )
)

(define-public (vote-on-dispute (dispute-id uint) (vote (string-ascii 20)))
    (let
        ((dispute (unwrap! (map-get? loan-disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
         (jury-member (unwrap! (map-get? dispute-jury { dispute-id: dispute-id, juror: tx-sender }) ERR-NOT-JURY-MEMBER)))
        
        (asserts! (is-eq (get status dispute) "PENDING") ERR-DISPUTE-NOT-RESOLVED)
        (asserts! (< stacks-block-height (get voting-end-height dispute)) ERR-VOTING-ENDED)
        (asserts! (not (get has-voted jury-member)) ERR-ALREADY-VOTED)
        (asserts! (or (is-eq vote "FOR") (is-eq vote "AGAINST")) ERR-INVALID-AMOUNT)
        
        (map-set dispute-jury
            { dispute-id: dispute-id, juror: tx-sender }
            (merge jury-member {
                has-voted: true,
                vote: vote
            })
        )
        
        (map-set loan-disputes
            { dispute-id: dispute-id }
            (merge dispute {
                votes-for: (if (is-eq vote "FOR") (+ (get votes-for dispute) u1) (get votes-for dispute)),
                votes-against: (if (is-eq vote "AGAINST") (+ (get votes-against dispute) u1) (get votes-against dispute))
            })
        )
        (ok true)
    )
)

(define-public (resolve-dispute (dispute-id uint))
    (let
        ((dispute (unwrap! (map-get? loan-disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
         (total-votes (+ (get votes-for dispute) (get votes-against dispute))))
        
        (asserts! (is-eq (get status dispute) "PENDING") ERR-DISPUTE-NOT-RESOLVED)
        (asserts! (>= stacks-block-height (get voting-end-height dispute)) ERR-VOTING-ENDED)
        (asserts! (> total-votes u0) ERR-INVALID-AMOUNT)
        
        (let
            ((resolution (if (> (get votes-for dispute) (get votes-against dispute)) "FAVOR_INITIATOR" "FAVOR_RESPONDENT"))
             (jury-reward (/ (get fee-paid dispute) (var-get jury-size))))
            
            (map-set loan-disputes
                { dispute-id: dispute-id }
                (merge dispute {
                    status: "RESOLVED",
                    resolution: resolution
                })
            )
            
            (try! (execute-dispute-resolution dispute-id resolution))
            (try! (distribute-jury-rewards dispute-id jury-reward))
            (ok resolution)
        )
    )
)

(define-public (claim-jury-reward (dispute-id uint))
    (let
        ((dispute (unwrap! (map-get? loan-disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
         (jury-member (unwrap! (map-get? dispute-jury { dispute-id: dispute-id, juror: tx-sender }) ERR-NOT-JURY-MEMBER))
         (jury-reward (/ (get fee-paid dispute) (var-get jury-size))))
        
        (asserts! (is-eq (get status dispute) "RESOLVED") ERR-DISPUTE-NOT-RESOLVED)
        (asserts! (get has-voted jury-member) ERR-NOT-JURY-MEMBER)
        (asserts! (not (get reward-claimed jury-member)) ERR-ALREADY-VOTED)
        
        (try! (as-contract (stx-transfer? jury-reward (as-contract tx-sender) tx-sender)))
        
        (map-set dispute-jury
            { dispute-id: dispute-id, juror: tx-sender }
            (merge jury-member { reward-claimed: true })
        )
        
        (update-jury-reputation tx-sender jury-reward)
        (ok true)
    )
)

(define-private (get-active-dispute (loan-id uint))
    (let
        ((dispute-id (var-get dispute-counter)))
        (fold check-dispute-for-loan (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) none)
    )
)

(define-private (check-dispute-for-loan (id uint) (result (optional uint)))
    (if (is-some result)
        result
        (let
            ((dispute (map-get? loan-disputes { dispute-id: id })))
            (if (is-some dispute)
                (let ((d (unwrap-panic dispute)))
                    (if (and (is-eq (get status d) "PENDING") (is-eq (get loan-id d) (var-get loan-counter)))
                        (some id)
                        none))
                none)
        )
    )
)

(define-private (select-jury-for-dispute (dispute-id uint))
    (begin
        (map-set dispute-jury { dispute-id: dispute-id, juror: CONTRACT-OWNER } 
            { has-voted: false, vote: "NONE", reward-claimed: false })
        (map-set dispute-jury { dispute-id: dispute-id, juror: (as-contract tx-sender) } 
            { has-voted: false, vote: "NONE", reward-claimed: false })
        (map-set dispute-jury { dispute-id: dispute-id, juror: tx-sender } 
            { has-voted: false, vote: "NONE", reward-claimed: false })
        (ok true)
    )
)

(define-private (execute-dispute-resolution (dispute-id uint) (resolution (string-ascii 20)))
    (let
        ((dispute (unwrap! (map-get? loan-disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
         (loan (unwrap! (map-get? loans { loan-id: (get loan-id dispute) }) ERR-LOAN-NOT-FOUND)))
        
        (if (is-eq resolution "FAVOR_INITIATOR")
            (begin
                (if (is-eq (get dispute-type dispute) "PAYMENT_DISPUTE")
                    (try! (as-contract (stx-transfer? (/ (get collateral loan) u2) (as-contract tx-sender) (get initiator dispute))))
                    true)
                (if (is-eq (get dispute-type dispute) "COLLATERAL_DISPUTE")
                    (try! (as-contract (stx-transfer? (get collateral loan) (as-contract tx-sender) (get initiator dispute))))
                    true)
                (ok true))
            (begin
                (if (is-eq (get dispute-type dispute) "PAYMENT_DISPUTE")
                    (try! (as-contract (stx-transfer? (/ (get collateral loan) u2) (as-contract tx-sender) (get respondent dispute))))
                    true)
                (if (is-eq (get dispute-type dispute) "COLLATERAL_DISPUTE")
                    (try! (as-contract (stx-transfer? (get collateral loan) (as-contract tx-sender) (get respondent dispute))))
                    true)
                (ok true))
        )
    )
)

(define-private (distribute-jury-rewards (dispute-id uint) (reward uint))
    (begin
        (try! (as-contract (stx-transfer? reward (as-contract tx-sender) CONTRACT-OWNER)))
        (ok true)
    )
)

(define-private (update-jury-reputation (juror principal) (reward uint))
    (let
        ((current-stats (default-to
            { disputes-judged: u0, reputation-score: u0, total-rewards: u0, is-active: true }
            (map-get? jury-pool { juror: juror }))))
        
        (map-set jury-pool
            { juror: juror }
            {
                disputes-judged: (+ (get disputes-judged current-stats) u1),
                reputation-score: (+ (get reputation-score current-stats) u5),
                total-rewards: (+ (get total-rewards current-stats) reward),
                is-active: (get is-active current-stats)
            }
        )
        true
    )
)

(define-public (set-dispute-parameters (fee uint) (period uint) (size uint) (min-credit uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set dispute-fee fee)
        (var-set voting-period period)
        (var-set jury-size size)
        (var-set min-jury-credit min-credit)
        (ok true)
    )
)

(define-read-only (get-dispute (dispute-id uint))
    (map-get? loan-disputes { dispute-id: dispute-id })
)

(define-read-only (get-jury-member (dispute-id uint) (juror principal))
    (map-get? dispute-jury { dispute-id: dispute-id, juror: juror })
)

(define-read-only (get-jury-stats (juror principal))
    (map-get? jury-pool { juror: juror })
)

(define-read-only (get-dispute-parameters)
    {
        fee: (var-get dispute-fee),
        voting-period: (var-get voting-period),
        jury-size: (var-get jury-size),
        min-jury-credit: (var-get min-jury-credit)
    }
)