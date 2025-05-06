(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LOAN-EXISTS (err u101))
(define-constant ERR-NO-LOAN-EXISTS (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-LOAN-NOT-ACTIVE (err u104))
(define-constant ERR-INVALID-AMOUNT (err u105))

(define-data-var min-income-percentage uint u10)
(define-data-var max-loan-amount uint u100000)
(define-data-var contract-owner principal tx-sender)

(define-map loans
    principal
    {
        amount: uint,
        remaining: uint,
        income: uint,
        status: (string-ascii 20),
        start-block: uint,
        last-payment: uint
    }
)

(define-map borrower-stats
    principal
    {
        total-borrowed: uint,
        total-repaid: uint,
        loans-taken: uint
    }
)

(define-public (initialize-contract (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set contract-owner new-owner))))

(define-public (set-min-income-percentage (new-percentage uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set min-income-percentage new-percentage))))

(define-public (apply-for-loan (amount uint) (annual-income uint))
    (let ((borrower tx-sender))
        (asserts! (<= amount (var-get max-loan-amount)) ERR-INVALID-AMOUNT)
        (asserts! (is-none (map-get? loans borrower)) ERR-LOAN-EXISTS)
        (try! (stx-transfer? amount (var-get contract-owner) borrower))
        (map-set loans borrower {
            amount: amount,
            remaining: amount,
            income: annual-income,
            status: "active",
            start-block: stacks-block-height,
            last-payment: stacks-block-height
        })
        (match (map-get? borrower-stats borrower)
            prev-stats (map-set borrower-stats borrower {
                total-borrowed: (+ amount (get total-borrowed prev-stats)),
                total-repaid: (get total-repaid prev-stats),
                loans-taken: (+ u1 (get loans-taken prev-stats))
            })
            (map-set borrower-stats borrower {
                total-borrowed: amount,
                total-repaid: u0,
                loans-taken: u1
            })
        )
        (ok true)))

(define-public (make-payment (payment-amount uint))
    (let (
        (borrower tx-sender)
        (loan-data (unwrap! (map-get? loans borrower) ERR-NO-LOAN-EXISTS))
        (remaining (get remaining loan-data))
    )
        (asserts! (is-eq (get status loan-data) "active") ERR-LOAN-NOT-ACTIVE)
        (asserts! (<= payment-amount remaining) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? payment-amount borrower (var-get contract-owner)))
        
        (if (is-eq payment-amount remaining)
            (map-set loans borrower {
                amount: (get amount loan-data),
                remaining: u0,
                income: (get income loan-data),
                status: "completed",
                start-block: (get start-block loan-data),
                last-payment: stacks-block-height
            })
            (map-set loans borrower {
                amount: (get amount loan-data),
                remaining: (- remaining payment-amount),
                income: (get income loan-data),
                status: "active",
                start-block: (get start-block loan-data),
                last-payment: stacks-block-height
            })
        )
        
        (match (map-get? borrower-stats borrower)
            prev-stats (map-set borrower-stats borrower {
                total-borrowed: (get total-borrowed prev-stats),
                total-repaid: (+ payment-amount (get total-repaid prev-stats)),
                loans-taken: (get loans-taken prev-stats)
            })
            (map-set borrower-stats borrower {
                total-borrowed: u0,
                total-repaid: payment-amount,
                loans-taken: u0
            })
        )
        (ok true)))

(define-read-only (get-loan-details (borrower principal))
    (ok (map-get? loans borrower)))

(define-read-only (get-borrower-statistics (borrower principal))
    (ok (map-get? borrower-stats borrower)))

(define-read-only (calculate-min-payment (borrower principal))
    (match (map-get? loans borrower)
        loan-data (ok (/ (* (get income loan-data) (var-get min-income-percentage)) u1000))
        ERR-NO-LOAN-EXISTS))


(define-constant PAYMENT-PERIODS u12)

(define-read-only (generate-payment-schedule (borrower principal))
    (let (
        (loan-data (unwrap! (map-get? loans borrower) ERR-NO-LOAN-EXISTS))
        (min-payment (unwrap! (calculate-min-payment borrower) ERR-NO-LOAN-EXISTS))
        (remaining (get remaining loan-data))
    )
    (ok {
        monthly-payment: (/ remaining PAYMENT-PERIODS),
        min-payment: min-payment,
        total-remaining: remaining,
        payment-periods: PAYMENT-PERIODS
    })))


(define-constant EARLY-REPAYMENT-BLOCKS u1000)
(define-constant EARLY-REPAYMENT-DISCOUNT-PERCENT u5)

(define-public (make-early-repayment)
    (let (
        (borrower tx-sender)
        (loan-data (unwrap! (map-get? loans borrower) ERR-NO-LOAN-EXISTS))
        (blocks-elapsed (- stacks-block-height (get start-block loan-data)))
        (remaining (get remaining loan-data))
    )
        (asserts! (is-eq (get status loan-data) "active") ERR-LOAN-NOT-ACTIVE)
        (asserts! (<= blocks-elapsed EARLY-REPAYMENT-BLOCKS) ERR-INVALID-AMOUNT)
        
        (let (
            (discount (/ (* remaining EARLY-REPAYMENT-DISCOUNT-PERCENT) u100))
            (final-payment (- remaining discount))
        )
            (try! (stx-transfer? final-payment borrower (var-get contract-owner)))
            
            (map-set loans borrower {
                amount: (get amount loan-data),
                remaining: u0,
                income: (get income loan-data),
                status: "completed",
                start-block: (get start-block loan-data),
                last-payment: stacks-block-height
            })
            
            (match (map-get? borrower-stats borrower)
                prev-stats (map-set borrower-stats borrower {
                    total-borrowed: (get total-borrowed prev-stats),
                    total-repaid: (+ final-payment (get total-repaid prev-stats)),
                    loans-taken: (get loans-taken prev-stats)
                })
                (map-set borrower-stats borrower {
                    total-borrowed: u0,
                    total-repaid: final-payment,
                    loans-taken: u0
                })
            )
            (ok true))))