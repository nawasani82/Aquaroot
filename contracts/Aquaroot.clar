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
(define-constant err-not-qualified-validator (err u109))
(define-constant err-already-validated (err u110))
(define-constant err-validation-not-found (err u111))
(define-constant err-source-not-pending (err u112))
(define-constant err-insufficient-validations (err u113))

(define-data-var total-sources uint u0)
(define-data-var total-reports uint u0)
(define-data-var reward-amount uint u10)
(define-data-var total-subscriptions uint u0)
(define-data-var max-subscriptions-per-user uint u50)
(define-data-var subscription-fee uint u5)
(define-data-var min-validator-reports uint u5)
(define-data-var min-validator-reputation uint u10)
(define-data-var required-validations uint u3)
(define-data-var validator-reward uint u20)

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
                status: "pending",
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
        (asserts! (is-eq (get status (unwrap! (map-get? water-sources source-id) (err err-invalid-source))) "active") (err err-source-not-pending))
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

(define-map source-validations
    { source-id: uint, validator: principal }
    {
        validation-type: (string-ascii 20),
        timestamp: uint,
        notes: (string-ascii 200),
        verified: bool
    }
)

(define-map validation-summary
    uint
    {
        validation-count: uint,
        approval-count: uint,
        rejection-count: uint,
        status: (string-ascii 20)
    }
)

(define-map qualified-validators
    principal
    {
        total-validations: uint,
        successful-validations: uint,
        validation-score: uint,
        is-active: bool
    }
)

(define-public (validate-water-source 
    (source-id uint) 
    (validation-type (string-ascii 20))
    (notes (string-ascii 200)))
    (let
        ((source (unwrap! (map-get? water-sources source-id) (err err-invalid-source)))
         (validator-stats (unwrap! (map-get? reporter-stats tx-sender) (err err-not-qualified-validator)))
         (validation-key {source-id: source-id, validator: tx-sender}))
        
        (asserts! (is-eq (get status source) "pending") (err err-source-not-pending))
        (asserts! (>= (get total-reports validator-stats) (var-get min-validator-reports)) (err err-not-qualified-validator))
        (asserts! (>= (get reputation-score validator-stats) (var-get min-validator-reputation)) (err err-not-qualified-validator))
        (asserts! (is-none (map-get? source-validations validation-key)) (err err-already-validated))
        (asserts! (or (is-eq validation-type "approve") (is-eq validation-type "reject")) (err err-invalid-quality))
        
        (map-set source-validations validation-key
            {
                validation-type: validation-type,
                timestamp: stacks-block-height,
                notes: notes,
                verified: true
            }
        )
        
        (update-validator-stats tx-sender)
        (update-validation-summary source-id validation-type)
        (unwrap! (check-validation-completion source-id) (err err-insufficient-validations))
        
        (ok true)
    )
)

(define-public (register-as-validator)
    (let
        ((reporter-stats-data (unwrap! (map-get? reporter-stats tx-sender) (err err-not-qualified-validator))))
        
        (asserts! (>= (get total-reports reporter-stats-data) (var-get min-validator-reports)) (err err-not-qualified-validator))
        (asserts! (>= (get reputation-score reporter-stats-data) (var-get min-validator-reputation)) (err err-not-qualified-validator))
        
        (map-set qualified-validators tx-sender
            {
                total-validations: u0,
                successful-validations: u0,
                validation-score: u0,
                is-active: true
            }
        )
        
        (ok true)
    )
)

(define-public (deactivate-validator (validator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        
        (let
            ((validator-data (unwrap! (map-get? qualified-validators validator) (err err-not-qualified-validator))))
            (map-set qualified-validators validator
                (merge validator-data {is-active: false})
            )
        )
        
        (ok true)
    )
)

(define-public (update-validation-requirements 
    (min-reports uint) 
    (min-reputation uint) 
    (required-vals uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (var-set min-validator-reports min-reports)
        (var-set min-validator-reputation min-reputation)
        (var-set required-validations required-vals)
        (ok true)
    )
)

(define-public (update-validator-reward (new-reward uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (var-set validator-reward new-reward)
        (ok true)
    )
)

(define-public (force-activate-source (source-id uint))
    (let
        ((source (unwrap! (map-get? water-sources source-id) (err err-invalid-source))))
        
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (asserts! (is-eq (get status source) "pending") (err err-source-not-pending))
        
        (map-set water-sources source-id
            (merge source {status: "active"})
        )
        
        (ok true)
    )
)

(define-private (update-validator-stats (validator principal))
    (let
        ((current-validator-stats (default-to 
            {total-validations: u0, successful-validations: u0, validation-score: u0, is-active: true}
            (map-get? qualified-validators validator))))
        (map-set qualified-validators validator
            {
                total-validations: (+ (get total-validations current-validator-stats) u1),
                successful-validations: (+ (get successful-validations current-validator-stats) u1),
                validation-score: (+ (get validation-score current-validator-stats) u2),
                is-active: (get is-active current-validator-stats)
            }
        )
    )
)

(define-private (update-validation-summary (source-id uint) (validation-type (string-ascii 20)))
    (let
        ((current-summary (default-to 
            {validation-count: u0, approval-count: u0, rejection-count: u0, status: "pending"}
            (map-get? validation-summary source-id)))
         (is-approval (is-eq validation-type "approve"))
         (new-validation-count (+ (get validation-count current-summary) u1))
         (new-approval-count (if is-approval 
                               (+ (get approval-count current-summary) u1)
                               (get approval-count current-summary)))
         (new-rejection-count (if (not is-approval) 
                                (+ (get rejection-count current-summary) u1)
                                (get rejection-count current-summary))))
        
        (map-set validation-summary source-id
            {
                validation-count: new-validation-count,
                approval-count: new-approval-count,
                rejection-count: new-rejection-count,
                status: "pending"
            }
        )
    )
)

(define-private (check-validation-completion (source-id uint))
    (let
        ((summary (unwrap! (map-get? validation-summary source-id) (err err-validation-not-found)))
         (source (unwrap! (map-get? water-sources source-id) (err err-invalid-source)))
         (required-vals (var-get required-validations)))
        
        (if (>= (get validation-count summary) required-vals)
            (let
                ((approval-ratio (* (get approval-count summary) u100))
                 (total-validations (get validation-count summary))
                 (approval-percentage (/ approval-ratio total-validations)))
                
                (if (>= approval-percentage u60)
                    (begin
                        (map-set water-sources source-id
                            (merge source {status: "active"})
                        )
                        (map-set validation-summary source-id
                            (merge summary {status: "approved"})
                        )
                        (unwrap! (reward-validators source-id) (err err-insufficient-funds))
                        (ok true))
                    (begin
                        (map-set water-sources source-id
                            (merge source {status: "rejected"})
                        )
                        (map-set validation-summary source-id
                            (merge summary {status: "rejected"})
                        )
                        (ok false))))
            (ok false))
    )
)

(define-private (reward-validators (source-id uint))
    (let
        ((validation-reward (var-get validator-reward)))
        (pay-validator-reward contract-owner validation-reward)
    )
)

(define-private (pay-validator-reward (validator principal) (amount uint))
    (stx-transfer? amount contract-owner validator)
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

(define-read-only (get-source-validation (source-id uint) (validator principal))
    (map-get? source-validations {source-id: source-id, validator: validator})
)

(define-read-only (get-validation-summary (source-id uint))
    (map-get? validation-summary source-id)
)

(define-read-only (get-qualified-validator (validator principal))
    (map-get? qualified-validators validator)
)

(define-read-only (get-validation-requirements)
    {
        min-validator-reports: (var-get min-validator-reports),
        min-validator-reputation: (var-get min-validator-reputation),
        required-validations: (var-get required-validations),
        validator-reward: (var-get validator-reward)
    }
)





