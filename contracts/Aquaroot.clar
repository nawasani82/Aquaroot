(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-source (err u101))
(define-constant err-already-reported (err u102))
(define-constant err-invalid-quality (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-already-subscribed (err u105))
(define-constant err-not-subscribed (err u106))
(define-constant err-invalid-threshold (err u107))
(define-constant err-subscription-limit-reached (err u108))

(define-data-var total-sources uint u0)
(define-data-var total-reports uint u0)
(define-data-var reward-amount uint u10)
(define-data-var total-subscriptions uint u0)
(define-data-var max-subscriptions-per-user uint u50)
(define-data-var subscription-fee uint u5)

(define-map water-sources
    uint 
    {
        name: (string-ascii 50),
        latitude: int,
        longitude: int,
        status: (string-ascii 20),
        total-reports: uint,
        created-by: principal
    }
)

(define-map source-reports
    { source-id: uint, reporter: principal }
    {
        quality-score: uint,
        timestamp: uint,
        notes: (string-ascii 100)
    }
)

(define-map reporter-stats
    principal
    {
        total-reports: uint,
        reputation-score: uint,
        rewards-earned: uint
    }
)

(define-map user-subscriptions
    { subscriber: principal, source-id: uint }
    {
        quality-threshold: uint,
        subscription-date: uint,
        is-active: bool,
        alert-count: uint
    }
)

(define-map subscriber-count
    principal
    { count: uint }
)

(define-map quality-alerts
    uint
    {
        source-id: uint,
        triggered-by-report: uint,
        quality-score: uint,
        alert-timestamp: uint,
        subscriber-count: uint
    }
)

(define-map alert-recipients
    { alert-id: uint, recipient: principal }
    {
        delivered: bool,
        delivery-timestamp: uint
    }
)

(define-public (add-water-source (name (string-ascii 50)) (latitude int) (longitude int))
    (let
        ((new-id (var-get total-sources)))
        (map-set water-sources new-id
            {
                name: name,
                latitude: latitude,
                longitude: longitude,
                status: "active",
                total-reports: u0,
                created-by: tx-sender
            }
        )
        (var-set total-sources (+ new-id u1))
        (ok new-id)
    )
)

(define-public (submit-report 
    (source-id uint) 
    (quality-score uint)
    (notes (string-ascii 100)))
    (let
        ((source (unwrap! (map-get? water-sources source-id) (err err-invalid-source)))
         (report-key {source-id: source-id, reporter: tx-sender}))
        
        (asserts! (< quality-score u11) (err err-invalid-quality))
        (asserts! (is-none (map-get? source-reports report-key)) (err err-already-reported))
        
        (map-set source-reports report-key
            {
                quality-score: quality-score,
                timestamp: stacks-block-height,
                notes: notes
            }
        )
        
        (update-reporter-stats tx-sender)
        (update-source-stats source-id)
        (unwrap! (pay-reporter tx-sender) (err err-insufficient-funds))
        (unwrap! (check-and-trigger-alerts source-id quality-score) (err err-insufficient-funds))
        
        (var-set total-reports (+ (var-get total-reports) u1))
        (ok true)
    )
)

(define-public (update-reward-amount (new-amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (var-set reward-amount new-amount)
        (ok true)
    )
)

(define-public (subscribe-to-source (source-id uint) (quality-threshold uint))
    (let
        ((subscription-key {subscriber: tx-sender, source-id: source-id})
         (current-count (default-to {count: u0} (map-get? subscriber-count tx-sender)))
         (fee (var-get subscription-fee)))
        
        (asserts! (is-some (map-get? water-sources source-id)) (err err-invalid-source))
        (asserts! (< quality-threshold u11) (err err-invalid-threshold))
        (asserts! (is-none (map-get? user-subscriptions subscription-key)) (err err-already-subscribed))
        (asserts! (< (get count current-count) (var-get max-subscriptions-per-user)) (err err-subscription-limit-reached))
        
        (unwrap! (stx-transfer? fee tx-sender contract-owner) (err err-insufficient-funds))
        
        (map-set user-subscriptions subscription-key
            {
                quality-threshold: quality-threshold,
                subscription-date: stacks-block-height,
                is-active: true,
                alert-count: u0
            }
        )
        
        (map-set subscriber-count tx-sender
            {count: (+ (get count current-count) u1)}
        )
        
        (var-set total-subscriptions (+ (var-get total-subscriptions) u1))
        (ok true)
    )
)

(define-public (unsubscribe-from-source (source-id uint))
    (let
        ((subscription-key {subscriber: tx-sender, source-id: source-id})
         (current-count (default-to {count: u0} (map-get? subscriber-count tx-sender))))
        
        (asserts! (is-some (map-get? user-subscriptions subscription-key)) (err err-not-subscribed))
        
        (map-delete user-subscriptions subscription-key)
        
        (map-set subscriber-count tx-sender
            {count: (- (get count current-count) u1)}
        )
        
        (var-set total-subscriptions (- (var-get total-subscriptions) u1))
        (ok true)
    )
)

(define-public (update-subscription-threshold (source-id uint) (new-threshold uint))
    (let
        ((subscription-key {subscriber: tx-sender, source-id: source-id})
         (subscription (unwrap! (map-get? user-subscriptions subscription-key) (err err-not-subscribed))))
        
        (asserts! (< new-threshold u11) (err err-invalid-threshold))
        
        (map-set user-subscriptions subscription-key
            (merge subscription {quality-threshold: new-threshold})
        )
        
        (ok true)
    )
)

(define-public (pause-subscription (source-id uint))
    (let
        ((subscription-key {subscriber: tx-sender, source-id: source-id})
         (subscription (unwrap! (map-get? user-subscriptions subscription-key) (err err-not-subscribed))))
        
        (map-set user-subscriptions subscription-key
            (merge subscription {is-active: false})
        )
        
        (ok true)
    )
)

(define-public (resume-subscription (source-id uint))
    (let
        ((subscription-key {subscriber: tx-sender, source-id: source-id})
         (subscription (unwrap! (map-get? user-subscriptions subscription-key) (err err-not-subscribed))))
        
        (map-set user-subscriptions subscription-key
            (merge subscription {is-active: true})
        )
        
        (ok true)
    )
)

(define-public (update-subscription-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (var-set subscription-fee new-fee)
        (ok true)
    )
)

(define-public (update-max-subscriptions (new-max uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (var-set max-subscriptions-per-user new-max)
        (ok true)
    )
)

(define-private (update-reporter-stats (reporter principal))
    (let
        ((current-stats (default-to 
            {total-reports: u0, reputation-score: u0, rewards-earned: u0}
            (map-get? reporter-stats reporter))))
        (map-set reporter-stats reporter
            {
                total-reports: (+ (get total-reports current-stats) u1),
                reputation-score: (+ (get reputation-score current-stats) u1),
                rewards-earned: (+ (get rewards-earned current-stats) (var-get reward-amount))
            }
        )
    )
)

(define-private (update-source-stats (source-id uint))
    (let
        ((source (unwrap-panic (map-get? water-sources source-id))))
        (map-set water-sources source-id
            (merge source {total-reports: (+ (get total-reports source) u1)})
        )
    )
)

(define-private (pay-reporter (reporter principal))
    (let
        ((amount (var-get reward-amount)))
        (stx-transfer? amount contract-owner reporter)
    )
)

(define-private (check-and-trigger-alerts (source-id uint) (quality-score uint))
    (let
        ((alert-triggered false))
        (begin
            (fold check-subscription-and-alert 
                  (list source-id source-id source-id source-id source-id source-id source-id source-id source-id source-id)
                  {source-id: source-id, quality-score: quality-score, alert-triggered: false})
            (ok true))
    )
)

(define-private (check-subscription-and-alert 
    (iteration uint) 
    (context {source-id: uint, quality-score: uint, alert-triggered: bool}))
    (let
        ((source-id (get source-id context))
         (quality-score (get quality-score context))
         (subscription-keys (get-source-subscriptions source-id))
         (alert-id (var-get total-subscriptions)))
        
        (if (and (not (get alert-triggered context)) 
                 (has-subscriptions-below-threshold source-id quality-score))
            (begin
                (create-quality-alert source-id quality-score alert-id)
                (distribute-alerts-to-subscribers source-id quality-score alert-id)
                (merge context {alert-triggered: true}))
            context)
    )
)

(define-private (has-subscriptions-below-threshold (source-id uint) (quality-score uint))
    (let
        ((test-subscription-1 (get-user-subscription-if-triggered contract-owner source-id quality-score))
         (test-subscription-2 (get-user-subscription-if-triggered contract-owner source-id quality-score))
         (test-subscription-3 (get-user-subscription-if-triggered contract-owner source-id quality-score)))
        (or (is-some test-subscription-1)
            (or (is-some test-subscription-2) (is-some test-subscription-3)))
    )
)

(define-private (get-user-subscription-if-triggered (user principal) (source-id uint) (quality-score uint))
    (let
        ((subscription (map-get? user-subscriptions {subscriber: user, source-id: source-id})))
        (if (is-some subscription)
            (let
                ((sub-data (unwrap-panic subscription)))
                (if (and (get is-active sub-data)
                         (< quality-score (get quality-threshold sub-data)))
                    subscription
                    none))
            none)
    )
)

(define-private (create-quality-alert (source-id uint) (quality-score uint) (alert-id uint))
    (map-set quality-alerts alert-id
        {
            source-id: source-id,
            triggered-by-report: (var-get total-reports),
            quality-score: quality-score,
            alert-timestamp: stacks-block-height,
            subscriber-count: (count-active-subscribers source-id quality-score)
        }
    )
)

(define-private (distribute-alerts-to-subscribers (source-id uint) (quality-score uint) (alert-id uint))
    (begin
        (deliver-alert-to-user contract-owner source-id quality-score alert-id)
        true
    )
)

(define-private (deliver-alert-to-user (user principal) (source-id uint) (quality-score uint) (alert-id uint))
    (let
        ((subscription (map-get? user-subscriptions {subscriber: user, source-id: source-id})))
        (if (is-some subscription)
            (let
                ((sub-data (unwrap-panic subscription)))
                (if (and (get is-active sub-data)
                         (< quality-score (get quality-threshold sub-data)))
                    (begin
                        (map-set alert-recipients {alert-id: alert-id, recipient: user}
                            {
                                delivered: true,
                                delivery-timestamp: stacks-block-height
                            }
                        )
                        (update-subscription-alert-count user source-id)
                        true)
                    false))
            false)
    )
)

(define-private (update-subscription-alert-count (user principal) (source-id uint))
    (let
        ((subscription-key {subscriber: user, source-id: source-id})
         (subscription (unwrap-panic (map-get? user-subscriptions subscription-key))))
        (map-set user-subscriptions subscription-key
            (merge subscription {alert-count: (+ (get alert-count subscription) u1)})
        )
    )
)

(define-private (count-active-subscribers (source-id uint) (quality-score uint))
    (if (is-subscription-triggered contract-owner source-id quality-score)
        u1
        u0
    )
)

(define-private (is-subscription-triggered (user principal) (source-id uint) (quality-score uint))
    (let
        ((subscription (map-get? user-subscriptions {subscriber: user, source-id: source-id})))
        (if (is-some subscription)
            (let
                ((sub-data (unwrap-panic subscription)))
                (and (get is-active sub-data)
                     (< quality-score (get quality-threshold sub-data))))
            false)
    )
)

(define-private (get-source-subscriptions (source-id uint))
    (list 
        {subscriber: contract-owner, source-id: source-id}
        {subscriber: contract-owner, source-id: source-id}
        {subscriber: contract-owner, source-id: source-id}
    )
)

(define-read-only (get-water-source (source-id uint))
    (map-get? water-sources source-id)
)

(define-read-only (get-source-report (source-id uint) (reporter principal))
    (map-get? source-reports {source-id: source-id, reporter: reporter})
)

(define-read-only (get-reporter-stats (reporter principal))
    (map-get? reporter-stats reporter)
)

(define-read-only (get-total-sources)
    (var-get total-sources)
)

(define-read-only (get-total-reports)
    (var-get total-reports)
)

(define-read-only (get-user-subscription (subscriber principal) (source-id uint))
    (map-get? user-subscriptions {subscriber: subscriber, source-id: source-id})
)

(define-read-only (get-subscriber-count (subscriber principal))
    (map-get? subscriber-count subscriber)
)

(define-read-only (get-quality-alert (alert-id uint))
    (map-get? quality-alerts alert-id)
)

(define-read-only (get-alert-recipient (alert-id uint) (recipient principal))
    (map-get? alert-recipients {alert-id: alert-id, recipient: recipient})
)

(define-read-only (get-total-subscriptions)
    (var-get total-subscriptions)
)

(define-read-only (get-subscription-fee)
    (var-get subscription-fee)
)

(define-read-only (get-max-subscriptions-per-user)
    (var-get max-subscriptions-per-user)
)