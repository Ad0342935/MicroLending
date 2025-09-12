;; Music Tip Jar - self-contained STX tipping with on-chain stats and memos
;; - Fans can tip artists (any principal) in STX
;; - Stats: total received, tip count, last tip block-height
;; - Optional artist profile (name/url) self-managed by the artist
;; - Emits events via print for indexers (tip + profile-set)
;; - No custody risk: STX transfers go directly from tipper (tx-sender) to artist

(define-constant ERR-AMOUNT-ZERO u100)

(define-constant EVT-TIP "tip")
(define-constant EVT-PROFILE "profile-set")

;; Per-artist cumulative stats
(define-map artist-stats principal
  {
    total: uint,
    count: uint,
    last-tip-height: uint
  }
)

;; Optional artist profile (self-managed)
(define-map artist-profiles principal
  {
    name: (string-ascii 32),
    url: (optional (string-utf8 128))
  }
)

;; Read a given artist's cumulative stats; returns zeroed defaults if absent
(define-read-only (get-artist-stats (artist principal))
  (match (map-get? artist-stats artist)
    stats stats
    { total: u0, count: u0, last-tip-height: u0 }
  )
)

;; Read a given artist's profile; returns (none) if not set
(define-read-only (get-artist-profile (artist principal))
  (map-get? artist-profiles artist)
)

;; Artists can set or update their profile (name and optional URL)
(define-public (set-profile (name (string-ascii 32)) (maybe-url (optional (string-utf8 128))))
  (begin
    (map-set artist-profiles tx-sender { name: name, url: maybe-url })
    (print {
      event: EVT-PROFILE,
      artist: tx-sender,
      name: name,
      has-url: (is-some maybe-url)
    })
    (ok true)
  )
)

;; Tip an artist mount STX with optional utf8 memo
;; Transfers directly from tx-sender to artist; records stats; emits an event.
(define-public (tip (artist principal) (amount uint) (maybe-memo (optional (string-utf8 200))))
  (begin
    (if (is-eq amount u0)
      (err ERR-AMOUNT-ZERO)
      (begin
        ;; transfer from the tipper to the artist
        (try! (stx-transfer? amount tx-sender artist))

        ;; update stats (initialize with zeroes if missing)
        (let (
          (prev (default-to
                  { total: u0, count: u0, last-tip-height: u0 }
                  (map-get? artist-stats artist)
                ))
        )
          (map-set artist-stats artist {
            total: (+ (get total prev) amount),
            count: (+ (get count prev) u1),
            last-tip-height: block-height
          })
        )

        ;; emit a simple event for indexers / off-chain UX
        (print {
          event: EVT-TIP,
          artist: artist,
          from: tx-sender,
          amount: amount,
          memo: maybe-memo,
          height: block-height
        })

        (ok true)
      )
    )
  )
)