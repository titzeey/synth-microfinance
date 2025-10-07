;; Synth Microfinance - Decentralized Microfinance Platform
;; A community-driven lending platform with reputation-based risk assessment

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-loan-exists (err u104))
(define-constant err-loan-not-active (err u105))
(define-constant err-already-vouched (err u106))
(define-constant err-invalid-amount (err u107))
(define-constant err-repayment-failed (err u108))

;; Data Variables
(define-data-var loan-counter uint u0)
(define-data-var base-interest-rate uint u1000) ;; 10% (basis points)
(define-data-var min-vouch-count uint u3)

;; Data Maps
(define-map loans
    uint
    {
        borrower: principal,
        amount: uint,
        interest-rate: uint,
        repaid-amount: uint,
        total-due: uint,
        status: (string-ascii 20),
        vouch-count: uint,
        created-at: uint,
        due-at: uint
    }
)

(define-map user-profiles
    principal
    {
        trust-score: uint,
        total-loans: uint,
        successful-repayments: uint,
        failed-repayments: uint,
        total-vouches: uint,
        impact-points: uint
    }
)

(define-map loan-vouches
    {loan-id: uint, voucher: principal}
    {vouched: bool, vouch-amount: uint}
)

(define-map trust-tokens
    principal
    uint
)

(define-map impact-tokens
    principal
    uint
)

;; Private Functions
(define-private (calculate-interest-rate (trust-score uint))
    (let
        (
            (base-rate (var-get base-interest-rate))
            (discount (/ (* trust-score u10) u100))
        )
        (if (>= discount base-rate)
            u100
            (- base-rate discount)
        )
    )
)

(define-private (calculate-total-due (amount uint) (interest-rate uint))
    (let
        (
            (interest (/ (* amount interest-rate) u10000))
        )
        (+ amount interest)
    )
)

;; Public Functions

;; Initialize user profile
(define-public (initialize-profile)
    (ok (map-set user-profiles tx-sender
        {
            trust-score: u500,
            total-loans: u0,
            successful-repayments: u0,
            failed-repayments: u0,
            total-vouches: u0,
            impact-points: u0
        }
    ))
)

;; Request a loan
(define-public (request-loan (amount uint) (duration uint))
    (let
        (
            (loan-id (+ (var-get loan-counter) u1))
            (profile (default-to 
                {trust-score: u500, total-loans: u0, successful-repayments: u0, 
                 failed-repayments: u0, total-vouches: u0, impact-points: u0}
                (map-get? user-profiles tx-sender)))
            (interest-rate (calculate-interest-rate (get trust-score profile)))
            (total-due (calculate-total-due amount interest-rate))
        )
        (asserts! (> amount u0) err-invalid-amount)
        (map-set loans loan-id
            {
                borrower: tx-sender,
                amount: amount,
                interest-rate: interest-rate,
                repaid-amount: u0,
                total-due: total-due,
                status: "pending",
                vouch-count: u0,
                created-at: block-height,
                due-at: (+ block-height duration)
            }
        )
        (var-set loan-counter loan-id)
        (map-set user-profiles tx-sender
            (merge profile {total-loans: (+ (get total-loans profile) u1)})
        )
        (ok loan-id)
    )
)

;; Vouch for a loan
(define-public (vouch-for-loan (loan-id uint) (vouch-amount uint))
    (let
        (
            (loan (unwrap! (map-get? loans loan-id) err-not-found))
            (existing-vouch (map-get? loan-vouches {loan-id: loan-id, voucher: tx-sender}))
            (voucher-profile (default-to 
                {trust-score: u500, total-loans: u0, successful-repayments: u0, 
                 failed-repayments: u0, total-vouches: u0, impact-points: u0}
                (map-get? user-profiles tx-sender)))
        )
        (asserts! (is-none existing-vouch) err-already-vouched)
        (asserts! (is-eq (get status loan) "pending") err-loan-not-active)
        (asserts! (> vouch-amount u0) err-invalid-amount)
        
        (map-set loan-vouches {loan-id: loan-id, voucher: tx-sender}
            {vouched: true, vouch-amount: vouch-amount}
        )
        (map-set loans loan-id
            (merge loan {vouch-count: (+ (get vouch-count loan) u1)})
        )
        (map-set user-profiles tx-sender
            (merge voucher-profile {total-vouches: (+ (get total-vouches voucher-profile) u1)})
        )
        
        ;; Auto-activate loan if minimum vouches reached
        (if (>= (+ (get vouch-count loan) u1) (var-get min-vouch-count))
            (begin
                (map-set loans loan-id (merge loan {status: "active"}))
                (ok true)
            )
            (ok true)
        )
    )
)

;; Repay loan
(define-public (repay-loan (loan-id uint) (payment-amount uint))
    (let
        (
            (loan (unwrap! (map-get? loans loan-id) err-not-found))
            (borrower-profile (unwrap! (map-get? user-profiles tx-sender) err-not-found))
            (new-repaid (+ (get repaid-amount loan) payment-amount))
            (is-fully-repaid (>= new-repaid (get total-due loan)))
        )
        (asserts! (is-eq tx-sender (get borrower loan)) err-unauthorized)
        (asserts! (is-eq (get status loan) "active") err-loan-not-active)
        
        (map-set loans loan-id
            (merge loan {
                repaid-amount: new-repaid,
                status: (if is-fully-repaid "completed" "active")
            })
        )
        
        (if is-fully-repaid
            (begin
                ;; Update trust score and profile
                (map-set user-profiles tx-sender
                    (merge borrower-profile {
                        trust-score: (+ (get trust-score borrower-profile) u50),
                        successful-repayments: (+ (get successful-repayments borrower-profile) u1)
                    })
                )
                ;; Mint TRUST tokens
                (map-set trust-tokens tx-sender
                    (+ (default-to u0 (map-get? trust-tokens tx-sender)) u100)
                )
            )
            true
        )
        (ok is-fully-repaid)
    )
)

;; Award impact tokens
(define-public (award-impact-tokens (recipient principal) (amount uint) (reason (string-ascii 100)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set impact-tokens recipient
            (+ (default-to u0 (map-get? impact-tokens recipient)) amount)
        )
        (let
            (
                (profile (unwrap! (map-get? user-profiles recipient) err-not-found))
            )
            (map-set user-profiles recipient
                (merge profile {impact-points: (+ (get impact-points profile) amount)})
            )
        )
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-loan (loan-id uint))
    (map-get? loans loan-id)
)

(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles user)
)

(define-read-only (get-trust-balance (user principal))
    (default-to u0 (map-get? trust-tokens user))
)

(define-read-only (get-impact-balance (user principal))
    (default-to u0 (map-get? impact-tokens user))
)

(define-read-only (get-loan-vouch (loan-id uint) (voucher principal))
    (map-get? loan-vouches {loan-id: loan-id, voucher: voucher})
)

(define-read-only (get-base-interest-rate)
    (var-get base-interest-rate)
)

;; Admin functions
(define-public (set-base-interest-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set base-interest-rate new-rate)
        (ok true)
    )
)

(define-public (set-min-vouch-count (new-count uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set min-vouch-count new-count)
        (ok true)
    )
)