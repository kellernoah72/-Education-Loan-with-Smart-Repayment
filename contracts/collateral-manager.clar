(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-COLLATERAL-EXISTS (err u201))
(define-constant ERR-NO-COLLATERAL (err u202))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u203))
(define-constant ERR-LOAN-NOT-ACTIVE (err u204))
(define-constant ERR-LIQUIDATION-THRESHOLD-NOT-MET (err u205))
(define-constant ERR-COLLATERAL-LOCKED (err u206))

(define-constant LIQUIDATION-THRESHOLD u150)
(define-constant LIQUIDATION-PENALTY u10)

(define-data-var contract-owner principal tx-sender)

(define-map collateral-deposits
    principal
    {
        stx-amount: uint,
        deposited-at: uint,
        loan-amount: uint,
        is-active: bool
    }
)

(define-public (deposit-stx-collateral (loan-amount uint))
    (let (
        (borrower tx-sender)
        (required-collateral (/ (* loan-amount LIQUIDATION-THRESHOLD) u100))
    )
        (asserts! (is-none (map-get? collateral-deposits borrower)) ERR-COLLATERAL-EXISTS)
        (asserts! (>= (stx-get-balance borrower) required-collateral) ERR-INSUFFICIENT-COLLATERAL)
        
        (try! (stx-transfer? required-collateral borrower (as-contract tx-sender)))
        
        (map-set collateral-deposits borrower {
            stx-amount: required-collateral,
            deposited-at: stacks-block-height,
            loan-amount: loan-amount,
            is-active: true
        })
        (ok true)))

(define-public (liquidate-collateral (borrower principal))
    (let (
        (collateral-data (unwrap! (map-get? collateral-deposits borrower) ERR-NO-COLLATERAL))
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active collateral-data) ERR-COLLATERAL-LOCKED)
        
        (let (
            (penalty-amount (/ (* (get stx-amount collateral-data) LIQUIDATION-PENALTY) u100))
            (remaining-amount (- (get stx-amount collateral-data) penalty-amount))
        )
            (try! (as-contract (stx-transfer? penalty-amount tx-sender (var-get contract-owner))))
            (try! (as-contract (stx-transfer? remaining-amount tx-sender borrower)))
        )
        
        (map-set collateral-deposits borrower {
            stx-amount: u0,
            deposited-at: (get deposited-at collateral-data),
            loan-amount: (get loan-amount collateral-data),
            is-active: false
        })
        (ok true)))

(define-public (release-collateral (borrower principal))
    (let (
        (collateral-data (unwrap! (map-get? collateral-deposits borrower) ERR-NO-COLLATERAL))
    )
        (asserts! (or (is-eq tx-sender (var-get contract-owner)) (is-eq tx-sender borrower)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active collateral-data) ERR-COLLATERAL-LOCKED)
        
        (try! (as-contract (stx-transfer? (get stx-amount collateral-data) tx-sender borrower)))
        
        (map-set collateral-deposits borrower {
            stx-amount: u0,
            deposited-at: (get deposited-at collateral-data),
            loan-amount: (get loan-amount collateral-data),
            is-active: false
        })
        (ok true)))

(define-read-only (get-collateral-details (borrower principal))
    (ok (map-get? collateral-deposits borrower)))

(define-read-only (calculate-collateral-ratio (borrower principal))
    (let (
        (collateral-data (unwrap! (map-get? collateral-deposits borrower) ERR-NO-COLLATERAL))
        (loan-amount (get loan-amount collateral-data))
    )
        (ok (/ (* (get stx-amount collateral-data) u100) loan-amount))))

(define-read-only (is-collateral-sufficient (borrower principal))
    (match (calculate-collateral-ratio borrower)
        ok-ratio (ok (>= ok-ratio LIQUIDATION-THRESHOLD))
        err-error (err err-error)))

(define-public (check-liquidation-eligibility (borrower principal))
    (let (
        (collateral-data (unwrap! (map-get? collateral-deposits borrower) ERR-NO-COLLATERAL))
        (is-sufficient (unwrap! (is-collateral-sufficient borrower) ERR-NO-COLLATERAL))
    )
        (ok (and 
            (not is-sufficient)
            (get is-active collateral-data)
        ))))
