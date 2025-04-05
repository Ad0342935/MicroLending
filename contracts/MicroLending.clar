
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


