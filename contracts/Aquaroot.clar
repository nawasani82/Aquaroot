(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-source (err u101))
(define-constant err-already-reported (err u102))
(define-constant err-invalid-quality (err u103))
(define-constant err-insufficient-funds (err u104))

(define-data-var total-sources uint u0)
(define-data-var total-reports uint u0)
(define-data-var reward-amount uint u10)

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