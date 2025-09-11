;; Student Performance Rewards System
;; Incentivizes academic achievement through loan benefits and rewards

(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-STUDENT-NOT-FOUND (err u401))
(define-constant ERR-PERFORMANCE-EXISTS (err u402))
(define-constant ERR-INVALID-GPA (err u403))
(define-constant ERR-INSUFFICIENT-BALANCE (err u404))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u405))
(define-constant ERR-MILESTONE-NOT-ACHIEVED (err u406))

;; Performance thresholds for rewards
(define-constant GPA-EXCELLENT u375) ;; 3.75 GPA (scaled by 100)
(define-constant GPA-GOOD u325) ;; 3.25 GPA (scaled by 100)
(define-constant GRADUATION-BONUS u1000) ;; Graduation bonus amount

(define-data-var contract-owner principal tx-sender)
(define-data-var total-rewards-pool uint u50000)
(define-data-var rewards-distributed uint u0)

;; Track student academic performance
(define-map student-performance
    principal
    {
        current-gpa: uint, ;; GPA scaled by 100 (375 = 3.75)
        completed-semesters: uint,
        total-semesters: uint,
        graduation-status: (string-ascii 20),
        last-update: uint,
        performance-level: (string-ascii 15),
        total-earned-rewards: uint
    }
)

;; Register student for performance tracking
(define-public (register-student-performance (total-semesters uint))
    (let ((student tx-sender))
        (asserts! (is-none (map-get? student-performance student)) ERR-PERFORMANCE-EXISTS)
        (asserts! (> total-semesters u0) ERR-INVALID-GPA)
        
        (map-set student-performance student {
            current-gpa: u0,
            completed-semesters: u0,
            total-semesters: total-semesters,
            graduation-status: "enrolled",
            last-update: stacks-block-height,
            performance-level: "poor",
            total-earned-rewards: u0
        })
        (ok true)))

;; Update student GPA by contract owner
(define-public (update-student-performance (student principal) (gpa uint) (completed-semesters uint))
    (let (
        (performance-data (unwrap! (map-get? student-performance student) ERR-STUDENT-NOT-FOUND))
        (performance-level (if (>= gpa GPA-EXCELLENT) "excellent"
            (if (>= gpa GPA-GOOD) "good" "satisfactory")))
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= gpa u400) ERR-INVALID-GPA) ;; Max GPA 4.0 (scaled)
        (asserts! (<= completed-semesters (get total-semesters performance-data)) ERR-INVALID-GPA)
        
        ;; Update performance data
        (map-set student-performance student {
            current-gpa: gpa,
            completed-semesters: completed-semesters,
            total-semesters: (get total-semesters performance-data),
            graduation-status: (if (is-eq completed-semesters (get total-semesters performance-data))
                "graduated" "enrolled"),
            last-update: stacks-block-height,
            performance-level: performance-level,
            total-earned-rewards: (get total-earned-rewards performance-data)
        })
        
        (ok performance-level)))

;; Claim graduation bonus
(define-public (claim-graduation-bonus)
    (let (
        (student tx-sender)
        (performance-data (unwrap! (map-get? student-performance student) ERR-STUDENT-NOT-FOUND))
    )
        (asserts! (is-eq (get graduation-status performance-data) "graduated") ERR-MILESTONE-NOT-ACHIEVED)
        (asserts! (<= (+ (var-get rewards-distributed) GRADUATION-BONUS) 
            (var-get total-rewards-pool)) ERR-INSUFFICIENT-BALANCE)
        
        ;; Transfer reward to student
        (try! (as-contract (stx-transfer? GRADUATION-BONUS tx-sender student)))
        
        (var-set rewards-distributed (+ (var-get rewards-distributed) GRADUATION-BONUS))
        (ok GRADUATION-BONUS)))

;; Read-only functions
(define-read-only (get-student-performance (student principal))
    (ok (map-get? student-performance student)))

(define-read-only (get-rewards-pool-status)
    (ok {
        total-pool: (var-get total-rewards-pool),
        distributed: (var-get rewards-distributed),
        remaining: (- (var-get total-rewards-pool) (var-get rewards-distributed))
    }))

;; Admin functions
(define-public (add-rewards-to-pool (amount uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-rewards-pool (+ (var-get total-rewards-pool) amount))
        (ok true)))

(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)))


