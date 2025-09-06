;; Commission Feedback System
;; Structured feedback collection for completed commissions

;; Constants
(define-constant contract-owner tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-COMMISSION-NOT-FOUND (err u201))
(define-constant ERR-COMMISSION-NOT-COMPLETED (err u202))
(define-constant ERR-FEEDBACK-ALREADY-EXISTS (err u203))
(define-constant ERR-INVALID-RATING (err u204))
(define-constant ERR-CANNOT-FEEDBACK-SELF (err u205))

;; Data variables
(define-data-var total-feedback uint u0)

;; Feedback structure with multiple criteria
(define-map CommissionFeedback
    { commission-id: uint, reviewer: principal }
    {
        communication-rating: uint,
        quality-rating: uint,
        timeliness-rating: uint,
        professionalism-rating: uint,
        overall-rating: uint,
        written-feedback: (string-ascii 500),
        would-work-again: bool,
        timestamp: uint,
        reviewee: principal
    }
)

;; Aggregated feedback statistics per user
(define-map UserFeedbackStats
    { user: principal }
    {
        total-feedback-received: uint,
        avg-communication: uint,
        avg-quality: uint,
        avg-timeliness: uint,
        avg-professionalism: uint,
        avg-overall: uint,
        positive-recommendations: uint,
        last-updated: uint
    }
)

;; Commission completion status tracking
(define-map CommissionCompletionStatus
    { commission-id: uint }
    {
        client: principal,
        artist: principal,
        status: uint,
        completed-at: uint
    }
)

;; Register commission completion (called by main contract)
(define-public (register-commission-completion
    (commission-id uint)
    (client principal)
    (artist principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
        
        (map-set CommissionCompletionStatus
            { commission-id: commission-id }
            {
                client: client,
                artist: artist,
                status: u2,
                completed-at: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Submit detailed feedback for completed commission
(define-public (submit-feedback
    (commission-id uint)
    (communication-rating uint)
    (quality-rating uint)
    (timeliness-rating uint)
    (professionalism-rating uint)
    (written-feedback (string-ascii 500))
    (would-work-again bool))
    (let (
        (commission-data (unwrap! (map-get? CommissionCompletionStatus { commission-id: commission-id }) ERR-COMMISSION-NOT-FOUND))
        (reviewer tx-sender)
        (reviewee (if (is-eq reviewer (get client commission-data))
                    (get artist commission-data)
                    (get client commission-data)))
    )
        ;; Validate ratings are between 1-5
        (asserts! (and (>= communication-rating u1) (<= communication-rating u5)) ERR-INVALID-RATING)
        (asserts! (and (>= quality-rating u1) (<= quality-rating u5)) ERR-INVALID-RATING)
        (asserts! (and (>= timeliness-rating u1) (<= timeliness-rating u5)) ERR-INVALID-RATING)
        (asserts! (and (>= professionalism-rating u1) (<= professionalism-rating u5)) ERR-INVALID-RATING)
        
        ;; Ensure commission is completed
        (asserts! (is-eq (get status commission-data) u2) ERR-COMMISSION-NOT-COMPLETED)
        
        ;; Ensure reviewer is part of this commission
        (asserts! (or (is-eq reviewer (get client commission-data)) 
                     (is-eq reviewer (get artist commission-data))) ERR-NOT-AUTHORIZED)
        
        ;; Ensure no self-feedback
        (asserts! (not (is-eq reviewer reviewee)) ERR-CANNOT-FEEDBACK-SELF)
        
        ;; Check feedback doesn't already exist
        (asserts! (is-none (map-get? CommissionFeedback { commission-id: commission-id, reviewer: reviewer })) 
                 ERR-FEEDBACK-ALREADY-EXISTS)
        
        (let (
            (overall-rating (/ (+ communication-rating quality-rating timeliness-rating professionalism-rating) u4))
        )
            ;; Store feedback
            (map-set CommissionFeedback
                { commission-id: commission-id, reviewer: reviewer }
                {
                    communication-rating: communication-rating,
                    quality-rating: quality-rating,
                    timeliness-rating: timeliness-rating,
                    professionalism-rating: professionalism-rating,
                    overall-rating: overall-rating,
                    written-feedback: written-feedback,
                    would-work-again: would-work-again,
                    timestamp: stacks-block-height,
                    reviewee: reviewee
                }
            )
            
            ;; Update aggregate statistics
            (try! (update-user-feedback-stats reviewee communication-rating quality-rating 
                                             timeliness-rating professionalism-rating overall-rating would-work-again))
            
            (var-set total-feedback (+ (var-get total-feedback) u1))
            (ok overall-rating)
        )
    )
)

;; Private function to update user feedback statistics
(define-private (update-user-feedback-stats
    (user principal)
    (communication uint)
    (quality uint)
    (timeliness uint)
    (professionalism uint)
    (overall uint)
    (recommend bool))
    (let (
        (current-stats (default-to 
            {
                total-feedback-received: u0,
                avg-communication: u0,
                avg-quality: u0,
                avg-timeliness: u0,
                avg-professionalism: u0,
                avg-overall: u0,
                positive-recommendations: u0,
                last-updated: u0
            }
            (map-get? UserFeedbackStats { user: user })))
        (new-total (+ (get total-feedback-received current-stats) u1))
        (new-recommendations (if recommend 
                            (+ (get positive-recommendations current-stats) u1)
                            (get positive-recommendations current-stats)))
    )
        (map-set UserFeedbackStats
            { user: user }
            {
                total-feedback-received: new-total,
                avg-communication: (/ (+ (* (get avg-communication current-stats) (get total-feedback-received current-stats)) communication) new-total),
                avg-quality: (/ (+ (* (get avg-quality current-stats) (get total-feedback-received current-stats)) quality) new-total),
                avg-timeliness: (/ (+ (* (get avg-timeliness current-stats) (get total-feedback-received current-stats)) timeliness) new-total),
                avg-professionalism: (/ (+ (* (get avg-professionalism current-stats) (get total-feedback-received current-stats)) professionalism) new-total),
                avg-overall: (/ (+ (* (get avg-overall current-stats) (get total-feedback-received current-stats)) overall) new-total),
                positive-recommendations: new-recommendations,
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Read-only functions

(define-read-only (get-commission-feedback (commission-id uint) (reviewer principal))
    (map-get? CommissionFeedback { commission-id: commission-id, reviewer: reviewer })
)

(define-read-only (get-user-feedback-stats (user principal))
    (map-get? UserFeedbackStats { user: user })
)

(define-read-only (get-commission-status (commission-id uint))
    (map-get? CommissionCompletionStatus { commission-id: commission-id })
)

(define-read-only (get-recommendation-percentage (user principal))
    (match (map-get? UserFeedbackStats { user: user })
        stats (if (> (get total-feedback-received stats) u0)
                 (ok (/ (* (get positive-recommendations stats) u100) (get total-feedback-received stats)))
                 (ok u0))
        (ok u0)
    )
)

(define-read-only (get-system-stats)
    (ok {
        total-feedback: (var-get total-feedback)
    })
)