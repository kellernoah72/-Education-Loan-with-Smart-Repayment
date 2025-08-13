;; Loan Refinancing System for Education Loans
;; Enables borrowers to refinance existing loans with improved terms

(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-LOAN-NOT-FOUND (err u301))
(define-constant ERR-APPLICATION-EXISTS (err u302))
(define-constant ERR-APPLICATION-NOT-FOUND (err u303))
(define-constant ERR-LOAN-NOT-ELIGIBLE (err u304))
(define-constant ERR-INSUFFICIENT-HISTORY (err u305))
(define-constant ERR-REFINANCING-IN-PROGRESS (err u306))
(define-constant ERR-INVALID-TERMS (err u307))
(define-constant ERR-CONSOLIDATION-FAILED (err u308))

;; Refinancing constants
(define-constant MIN-PAYMENT-HISTORY u6) ;; Minimum 6 payments for eligibility
(define-constant EXCELLENT-CREDIT-THRESHOLD u95) ;; 95% on-time payment rate
(define-constant GOOD-CREDIT-THRESHOLD u85) ;; 85% on-time payment rate
(define-constant MAX-RATE-REDUCTION u30) ;; Maximum 30% rate reduction

(define-data-var contract-owner principal tx-sender)
(define-data-var base-interest-rate uint u8) ;; Base rate as percentage
(define-data-var refinancing-fee uint u50) ;; Refinancing processing fee

;; Track refinancing applications
(define-map refinancing-applications
    principal
    {
        original-amount: uint,
        new-amount: uint,
        current-rate: uint,
        proposed-rate: uint,
        new-term-blocks: uint,
        application-date: uint,
        status: (string-ascii 20), ;; "pending", "approved", "rejected", "completed"
        credit-score: uint,
        consolidating: bool
    }
)

;; Track borrower credit history
(define-map borrower-credit
    principal
    {
        total-payments: uint,
        on-time-payments: uint,
        late-payments: uint,
        total-refinancings: uint,
        last-refinancing: uint,
        credit-score: uint,
        payment-streak: uint
    }
)

;; Track refinancing history
(define-map refinancing-history
    { borrower: principal, refinancing-id: uint }
    {
        original-loan-amount: uint,
        new-loan-amount: uint,
        rate-reduction: uint,
        completion-date: uint,
        savings-amount: uint
    }
)

(define-data-var refinancing-counter uint u0)

;; Calculate credit score based on payment behavior
(define-private (calculate-credit-score (on-time-rate uint) (total-payments uint))
    (let (
        (base-score u50)
        (payment-bonus (if (>= total-payments MIN-PAYMENT-HISTORY) u20 u0))
        (rate-bonus (if (>= on-time-rate EXCELLENT-CREDIT-THRESHOLD) 
            u30 
            (if (>= on-time-rate GOOD-CREDIT-THRESHOLD) u15 u0)))
    )
        (+ base-score payment-bonus rate-bonus)))

;; Calculate new interest rate based on credit score
(define-private (calculate-new-rate (credit-score uint) (current-rate uint))
    (let (
        (rate-reduction (if (>= credit-score EXCELLENT-CREDIT-THRESHOLD) 
            (/ (* current-rate u30) u100)
            (if (>= credit-score GOOD-CREDIT-THRESHOLD) 
                (/ (* current-rate u20) u100)
                (if (>= credit-score u70) (/ (* current-rate u10) u100) u0))))
    )
        (- current-rate rate-reduction)))

;; Update borrower credit score (simplified version)
(define-public (update-credit-score (borrower principal) (on-time-payments uint) (total-payments uint))
    (let (
        (current-credit (default-to 
            { total-payments: u0, on-time-payments: u0, late-payments: u0, 
              total-refinancings: u0, last-refinancing: u0, credit-score: u0, payment-streak: u0 }
            (map-get? borrower-credit borrower)))
        (on-time-rate (if (> total-payments u0) 
            (/ (* on-time-payments u100) total-payments) 
            u0))
        (new-score (calculate-credit-score on-time-rate total-payments))
    )
        (map-set borrower-credit borrower {
            total-payments: total-payments,
            on-time-payments: on-time-payments,
            late-payments: (- total-payments on-time-payments),
            total-refinancings: (get total-refinancings current-credit),
            last-refinancing: (get last-refinancing current-credit),
            credit-score: new-score,
            payment-streak: (get payment-streak current-credit)
        })
        (ok new-score)))

;; Apply for loan refinancing
(define-public (apply-for-refinancing (loan-amount uint) (credit-score uint) (new-term-blocks uint))
    (let (
        (borrower tx-sender)
        (current-rate (var-get base-interest-rate))
        (proposed-rate (calculate-new-rate credit-score current-rate))
    )
        (asserts! (is-none (map-get? refinancing-applications borrower)) ERR-APPLICATION-EXISTS)
        (asserts! (>= credit-score u70) ERR-LOAN-NOT-ELIGIBLE) ;; Minimum credit score
        (asserts! (> new-term-blocks u0) ERR-INVALID-TERMS)
        (asserts! (> loan-amount u0) ERR-INVALID-TERMS)
        
        (map-set refinancing-applications borrower {
            original-amount: loan-amount,
            new-amount: loan-amount,
            current-rate: current-rate,
            proposed-rate: proposed-rate,
            new-term-blocks: new-term-blocks,
            application-date: stacks-block-height,
            status: "pending",
            credit-score: credit-score,
            consolidating: false
        })
        (ok true)))

;; Approve or reject refinancing application
(define-public (process-refinancing-application (borrower principal) (approved bool))
    (let (
        (application (unwrap! (map-get? refinancing-applications borrower) ERR-APPLICATION-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status application) "pending") ERR-INVALID-TERMS)
        
        (map-set refinancing-applications borrower {
            original-amount: (get original-amount application),
            new-amount: (get new-amount application),
            current-rate: (get current-rate application),
            proposed-rate: (get proposed-rate application),
            new-term-blocks: (get new-term-blocks application),
            application-date: (get application-date application),
            status: (if approved "approved" "rejected"),
            credit-score: (get credit-score application),
            consolidating: (get consolidating application)
        })
        (ok approved)))

;; Execute approved refinancing
(define-public (execute-refinancing)
    (let (
        (borrower tx-sender)
        (application (unwrap! (map-get? refinancing-applications borrower) ERR-APPLICATION-NOT-FOUND))
        (refinancing-id (+ (var-get refinancing-counter) u1))
    )
        (asserts! (is-eq (get status application) "approved") ERR-LOAN-NOT-ELIGIBLE)
        
        ;; Pay refinancing fee
        (try! (stx-transfer? (var-get refinancing-fee) borrower (var-get contract-owner)))
        
        ;; Record refinancing history
        (map-set refinancing-history 
            { borrower: borrower, refinancing-id: refinancing-id }
            {
                original-loan-amount: (get original-amount application),
                new-loan-amount: (get new-amount application),
                rate-reduction: (- (get current-rate application) (get proposed-rate application)),
                completion-date: stacks-block-height,
                savings-amount: (calculate-savings application)
            })
        
        ;; Update borrower credit history
        (match (map-get? borrower-credit borrower)
            prev-credit (map-set borrower-credit borrower {
                total-payments: (get total-payments prev-credit),
                on-time-payments: (get on-time-payments prev-credit),
                late-payments: (get late-payments prev-credit),
                total-refinancings: (+ (get total-refinancings prev-credit) u1),
                last-refinancing: stacks-block-height,
                credit-score: (get credit-score prev-credit),
                payment-streak: (get payment-streak prev-credit)
            })
            (map-set borrower-credit borrower {
                total-payments: u0,
                on-time-payments: u0,
                late-payments: u0,
                total-refinancings: u1,
                last-refinancing: stacks-block-height,
                credit-score: (get credit-score application),
                payment-streak: u0
            })
        )
        
        ;; Complete application
        (map-set refinancing-applications borrower {
            original-amount: (get original-amount application),
            new-amount: (get new-amount application),
            current-rate: (get current-rate application),
            proposed-rate: (get proposed-rate application),
            new-term-blocks: (get new-term-blocks application),
            application-date: (get application-date application),
            status: "completed",
            credit-score: (get credit-score application),
            consolidating: (get consolidating application)
        })
        
        (var-set refinancing-counter refinancing-id)
        (ok refinancing-id)))

;; Calculate potential savings from refinancing
(define-private (calculate-savings (application (tuple (original-amount uint) (new-amount uint) (current-rate uint) (proposed-rate uint) (new-term-blocks uint) (application-date uint) (status (string-ascii 20)) (credit-score uint) (consolidating bool))))
    (let (
        (rate-difference (- (get current-rate application) (get proposed-rate application)))
        (loan-amount (get original-amount application))
        (estimated-savings (/ (* loan-amount rate-difference) u100))
    )
        estimated-savings))

;; Consolidate multiple loans (simplified version)
(define-public (apply-for-consolidation (total-amount uint) (credit-score uint) (new-term-blocks uint))
    (let (
        (borrower tx-sender)
        (current-rate (var-get base-interest-rate))
        (proposed-rate (calculate-new-rate credit-score current-rate))
        (consolidation-fee u100)
    )
        (asserts! (is-none (map-get? refinancing-applications borrower)) ERR-APPLICATION-EXISTS)
        (asserts! (>= credit-score u70) ERR-LOAN-NOT-ELIGIBLE)
        (asserts! (> new-term-blocks u0) ERR-INVALID-TERMS)
        (asserts! (> total-amount u0) ERR-INVALID-TERMS)
        
        (map-set refinancing-applications borrower {
            original-amount: total-amount,
            new-amount: (+ total-amount consolidation-fee),
            current-rate: current-rate,
            proposed-rate: proposed-rate,
            new-term-blocks: new-term-blocks,
            application-date: stacks-block-height,
            status: "pending",
            credit-score: credit-score,
            consolidating: true
        })
        (ok true)))

;; Check if borrower is eligible for refinancing
(define-read-only (check-refinancing-eligibility (borrower principal))
    (let (
        (credit-data (map-get? borrower-credit borrower))
        (credit-score (match credit-data 
            credit (get credit-score credit)
            u0))
    )
        (ok {
            eligible: (and 
                (>= credit-score u70)
                (is-none (map-get? refinancing-applications borrower))),
            credit-score: credit-score,
            potential-rate: (calculate-new-rate credit-score (var-get base-interest-rate))
        })))

;; Get refinancing application status
(define-read-only (get-refinancing-application (borrower principal))
    (ok (map-get? refinancing-applications borrower)))

;; Get borrower credit information
(define-read-only (get-borrower-credit (borrower principal))
    (ok (map-get? borrower-credit borrower)))

;; Get refinancing history
(define-read-only (get-refinancing-history (borrower principal) (refinancing-id uint))
    (ok (map-get? refinancing-history { borrower: borrower, refinancing-id: refinancing-id })))

;; Calculate potential monthly payment for new terms
(define-read-only (calculate-new-payment (borrower principal) (new-term-blocks uint))
    (let (
        (application (map-get? refinancing-applications borrower))
    )
        (match application
            app (ok (/ (get new-amount app) (/ new-term-blocks u144))) ;; Assuming 144 blocks per day
            ERR-APPLICATION-NOT-FOUND
        )))

;; Calculate total interest savings
(define-read-only (calculate-total-savings (original-amount uint) (current-rate uint) (new-rate uint) (term-blocks uint))
    (let (
        (current-total-interest (/ (* original-amount current-rate term-blocks) (* u100 u52560))) ;; Approx blocks per year
        (new-total-interest (/ (* original-amount new-rate term-blocks) (* u100 u52560)))
        (total-savings (- current-total-interest new-total-interest))
    )
        (ok total-savings)))

;; Set base interest rate (owner only)
(define-public (set-base-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set base-interest-rate new-rate)
        (ok true)))

;; Set refinancing fee (owner only)
(define-public (set-refinancing-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set refinancing-fee new-fee)
        (ok true)))

;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)))

;; Emergency cancel application (borrower only)
(define-public (cancel-refinancing-application)
    (let (
        (borrower tx-sender)
        (application (unwrap! (map-get? refinancing-applications borrower) ERR-APPLICATION-NOT-FOUND))
    )
        (asserts! (is-eq (get status application) "pending") ERR-INVALID-TERMS)
        
        (map-delete refinancing-applications borrower)
        (ok true)))
