
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

