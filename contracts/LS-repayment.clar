(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-LOAN-EXISTS (err u101))
(define-constant ERR-NO-LOAN-EXISTS (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-LOAN-NOT-ACTIVE (err u104))
(define-constant ERR-INVALID-AMOUNT (err u105))
(define-constant ERR-LOAN-ALREADY-DEFAULTED (err u106))
(define-constant ERR-LOAN-NOT-OVERDUE (err u107))

(define-constant OVERDUE-BLOCKS u1440)
(define-constant DEFAULT-GRACE-BLOCKS u4320)
(define-constant LATE-PENALTY-PERCENT u2)

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

(define-map loan-defaults
    principal
    {
        is-overdue: bool,
        overdue-since: uint,
        penalty-amount: uint,
        is-defaulted: bool,
        default-date: uint
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
        (map-set loan-defaults borrower {
            is-overdue: false,
            overdue-since: u0,
            penalty-amount: u0,
            is-defaulted: false,
            default-date: u0
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
        (default-data (unwrap! (map-get? loan-defaults borrower) ERR-NO-LOAN-EXISTS))
        (remaining (get remaining loan-data))
        (penalty (get penalty-amount default-data))
        (total-due (+ remaining penalty))
    )
        (asserts! (is-eq (get status loan-data) "active") ERR-LOAN-NOT-ACTIVE)
        (asserts! (<= payment-amount total-due) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? payment-amount borrower (var-get contract-owner)))
        
        (let (
            (penalty-payment (if (<= payment-amount penalty) payment-amount penalty))
            (principal-payment (- payment-amount penalty-payment))
            (new-remaining (- remaining principal-payment))
            (new-penalty (- penalty penalty-payment))
        )
            (if (and (is-eq new-remaining u0) (is-eq new-penalty u0))
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
                    remaining: new-remaining,
                    income: (get income loan-data),
                    status: "active",
                    start-block: (get start-block loan-data),
                    last-payment: stacks-block-height
                })
            )
            
            (map-set loan-defaults borrower {
                is-overdue: false,
                overdue-since: u0,
                penalty-amount: new-penalty,
                is-defaulted: (get is-defaulted default-data),
                default-date: (get default-date default-data)
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

(define-public (mark-loan-overdue (borrower principal))
    (let (
        (loan-data (unwrap! (map-get? loans borrower) ERR-NO-LOAN-EXISTS))
        (default-data (unwrap! (map-get? loan-defaults borrower) ERR-NO-LOAN-EXISTS))
        (blocks-since-payment (- stacks-block-height (get last-payment loan-data)))
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status loan-data) "active") ERR-LOAN-NOT-ACTIVE)
        (asserts! (>= blocks-since-payment OVERDUE-BLOCKS) ERR-LOAN-NOT-OVERDUE)
        (asserts! (not (get is-overdue default-data)) ERR-LOAN-ALREADY-DEFAULTED)
        
        (let (
            (penalty (/ (* (get remaining loan-data) LATE-PENALTY-PERCENT) u100))
        )
            (map-set loan-defaults borrower {
                is-overdue: true,
                overdue-since: stacks-block-height,
                penalty-amount: (+ (get penalty-amount default-data) penalty),
                is-defaulted: (get is-defaulted default-data),
                default-date: (get default-date default-data)
            })
        )
        (ok true)))

(define-public (mark-loan-defaulted (borrower principal))
    (let (
        (loan-data (unwrap! (map-get? loans borrower) ERR-NO-LOAN-EXISTS))
        (default-data (unwrap! (map-get? loan-defaults borrower) ERR-NO-LOAN-EXISTS))
        (blocks-overdue (- stacks-block-height (get overdue-since default-data)))
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status loan-data) "active") ERR-LOAN-NOT-ACTIVE)
        (asserts! (get is-overdue default-data) ERR-LOAN-NOT-OVERDUE)
        (asserts! (>= blocks-overdue DEFAULT-GRACE-BLOCKS) ERR-LOAN-NOT-OVERDUE)
        (asserts! (not (get is-defaulted default-data)) ERR-LOAN-ALREADY-DEFAULTED)
        
        (map-set loans borrower {
            amount: (get amount loan-data),
            remaining: (get remaining loan-data),
            income: (get income loan-data),
            status: "defaulted",
            start-block: (get start-block loan-data),
            last-payment: (get last-payment loan-data)
        })
        
        (map-set loan-defaults borrower {
            is-overdue: (get is-overdue default-data),
            overdue-since: (get overdue-since default-data),
            penalty-amount: (get penalty-amount default-data),
            is-defaulted: true,
            default-date: stacks-block-height
        })
        (ok true)))

(define-read-only (get-loan-details (borrower principal))
    (ok (map-get? loans borrower)))

(define-read-only (get-borrower-statistics (borrower principal))
    (ok (map-get? borrower-stats borrower)))

(define-read-only (get-default-status (borrower principal))
    (ok (map-get? loan-defaults borrower)))

(define-read-only (is-loan-overdue (borrower principal))
    (let (
        (loan-data (unwrap! (map-get? loans borrower) ERR-NO-LOAN-EXISTS))
        (blocks-since-payment (- stacks-block-height (get last-payment loan-data)))
    )
        (ok (>= blocks-since-payment OVERDUE-BLOCKS))))

(define-read-only (calculate-total-due (borrower principal))
    (let (
        (loan-data (unwrap! (map-get? loans borrower) ERR-NO-LOAN-EXISTS))
        (default-data (unwrap! (map-get? loan-defaults borrower) ERR-NO-LOAN-EXISTS))
    )
        (ok (+ (get remaining loan-data) (get penalty-amount default-data)))))

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