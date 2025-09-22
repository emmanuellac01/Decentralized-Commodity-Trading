;; Decentralized Commodity Trading Platform
;; A smart contract for direct peer-to-peer trading of agricultural and industrial commodities

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-COMMODITY-NOT-FOUND (err u101))
(define-constant ERR-ORDER-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-INVALID-QUANTITY (err u104))
(define-constant ERR-INVALID-PRICE (err u105))
(define-constant ERR-ORDER-ALREADY-FILLED (err u106))
(define-constant ERR-CANNOT-TRADE-OWN-ORDER (err u107))
(define-constant ERR-TRADE-NOT-FOUND (err u108))
(define-constant ERR-TRADE-ALREADY-CONFIRMED (err u109))
(define-constant ERR-INVALID-RATING (err u110))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Platform fee (0.5% = 50 basis points)
(define-constant PLATFORM-FEE u50)
(define-constant BASIS-POINTS u10000)

;; Data structures
(define-map commodities 
  { commodity-id: uint }
  {
    name: (string-ascii 50),
    category: (string-ascii 20),
    unit: (string-ascii 10),
    seller: principal,
    quantity-available: uint,
    price-per-unit: uint,
    quality-grade: (string-ascii 10),
    location: (string-ascii 100),
    harvest-date: uint,
    is-active: bool
  }
)

(define-map orders
  { order-id: uint }
  {
    commodity-id: uint,
    buyer: principal,
    quantity: uint,
    price-per-unit: uint,
    total-amount: uint,
    order-type: (string-ascii 10), ;; "buy" or "sell"
    status: (string-ascii 20), ;; "open", "filled", "cancelled"
    created-at: uint
  }
)

(define-map trades
  { trade-id: uint }
  {
    commodity-id: uint,
    seller: principal,
    buyer: principal,
    quantity: uint,
    price-per-unit: uint,
    total-amount: uint,
    seller-confirmed: bool,
    buyer-confirmed: bool,
    completed: bool,
    created-at: uint
  }
)

(define-map user-balances
  { user: principal }
  { balance: uint }
)

(define-map user-ratings
  { user: principal }
  {
    total-score: uint,
    rating-count: uint,
    average-rating: uint
  }
)

;; Counters
(define-data-var commodity-counter uint u0)
(define-data-var order-counter uint u0)
(define-data-var trade-counter uint u0)

;; Platform revenue
(define-data-var platform-revenue uint u0)

;; Helper functions
(define-private (increment-commodity-counter)
  (let ((current (var-get commodity-counter)))
    (var-set commodity-counter (+ current u1))
    (+ current u1)
  )
)

(define-private (increment-order-counter)
  (let ((current (var-get order-counter)))
    (var-set order-counter (+ current u1))
    (+ current u1)
  )
)

(define-private (increment-trade-counter)
  (let ((current (var-get trade-counter)))
    (var-set trade-counter (+ current u1))
    (+ current u1)
  )
)

(define-private (calculate-fee (amount uint))
  (/ (* amount PLATFORM-FEE) BASIS-POINTS)
)

;; Public functions

;; List a new commodity for sale
(define-public (list-commodity 
    (name (string-ascii 50))
    (category (string-ascii 20))
    (unit (string-ascii 10))
    (quantity uint)
    (price-per-unit uint)
    (quality-grade (string-ascii 10))
    (location (string-ascii 100))
    (harvest-date uint)
  )
  (let ((commodity-id (increment-commodity-counter)))
    (if (and (> quantity u0) (> price-per-unit u0))
      (begin
        (map-set commodities
          { commodity-id: commodity-id }
          {
            name: name,
            category: category,
            unit: unit,
            seller: tx-sender,
            quantity-available: quantity,
            price-per-unit: price-per-unit,
            quality-grade: quality-grade,
            location: location,
            harvest-date: harvest-date,
            is-active: true
          }
        )
        (ok commodity-id)
      )
      ERR-INVALID-QUANTITY
    )
  )
)

;; Place a buy order
(define-public (place-buy-order (commodity-id uint) (quantity uint) (max-price-per-unit uint))
  (let 
    (
      (commodity (unwrap! (map-get? commodities { commodity-id: commodity-id }) ERR-COMMODITY-NOT-FOUND))
      (total-amount (* quantity max-price-per-unit))
      (user-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender }))))
      (order-id (increment-order-counter))
    )
    (if (and 
          (> quantity u0)
          (> max-price-per-unit u0)
          (>= user-balance total-amount)
          (get is-active commodity)
        )
      (begin
        ;; Escrow the funds
        (map-set user-balances
          { user: tx-sender }
          { balance: (- user-balance total-amount) }
        )
        ;; Create the order
        (map-set orders
          { order-id: order-id }
          {
            commodity-id: commodity-id,
            buyer: tx-sender,
            quantity: quantity,
            price-per-unit: max-price-per-unit,
            total-amount: total-amount,
            order-type: "buy",
            status: "open",
            created-at: block-height
          }
        )
        (ok order-id)
      )
      ERR-INSUFFICIENT-BALANCE
    )
  )
)

;; Accept a buy order (seller initiates trade)
(define-public (accept-buy-order (order-id uint))
  (let 
    (
      (order (unwrap! (map-get? orders { order-id: order-id }) ERR-ORDER-NOT-FOUND))
      (commodity (unwrap! (map-get? commodities { commodity-id: (get commodity-id order) }) ERR-COMMODITY-NOT-FOUND))
      (trade-id (increment-trade-counter))
    )
    (if (and
          (is-eq tx-sender (get seller commodity))
          (is-eq (get status order) "open")
          (>= (get quantity-available commodity) (get quantity order))
        )
      (begin
        ;; Update order status
        (map-set orders
          { order-id: order-id }
          (merge order { status: "filled" })
        )
        ;; Update commodity availability
        (map-set commodities
          { commodity-id: (get commodity-id order) }
          (merge commodity { 
            quantity-available: (- (get quantity-available commodity) (get quantity order))
          })
        )
        ;; Create trade
        (map-set trades
          { trade-id: trade-id }
          {
            commodity-id: (get commodity-id order),
            seller: tx-sender,
            buyer: (get buyer order),
            quantity: (get quantity order),
            price-per-unit: (get price-per-unit order),
            total-amount: (get total-amount order),
            seller-confirmed: true,
            buyer-confirmed: false,
            completed: false,
            created-at: block-height
          }
        )
        (ok trade-id)
      )
      ERR-NOT-AUTHORIZED
    )
  )
)

;; Confirm trade delivery (buyer confirms receipt)
(define-public (confirm-delivery (trade-id uint))
  (let 
    (
      (trade (unwrap! (map-get? trades { trade-id: trade-id }) ERR-TRADE-NOT-FOUND))
      (fee (calculate-fee (get total-amount trade)))
      (seller-amount (- (get total-amount trade) fee))
      (seller-balance (default-to u0 (get balance (map-get? user-balances { user: (get seller trade) }))))
    )
    (if (and
          (is-eq tx-sender (get buyer trade))
          (not (get completed trade))
          (get seller-confirmed trade)
        )
      (begin
        ;; Update trade status
        (map-set trades
          { trade-id: trade-id }
          (merge trade { 
            buyer-confirmed: true,
            completed: true
          })
        )
        ;; Transfer funds to seller (minus fee)
        (map-set user-balances
          { user: (get seller trade) }
          { balance: (+ seller-balance seller-amount) }
        )
        ;; Add fee to platform revenue
        (var-set platform-revenue (+ (var-get platform-revenue) fee))
        (ok true)
      )
      ERR-NOT-AUTHORIZED
    )
  )
)

;; Deposit funds to user balance
(define-public (deposit-funds (amount uint))
  (let ((current-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender })))))
    (if (> amount u0)
      (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set user-balances
          { user: tx-sender }
          { balance: (+ current-balance amount) }
        )
        (ok true)
      )
      ERR-INVALID-QUANTITY
    )
  )
)

;; Withdraw funds from user balance
(define-public (withdraw-funds (amount uint))
  (let ((current-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender })))))
    (if (and (> amount u0) (>= current-balance amount))
      (begin
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (map-set user-balances
          { user: tx-sender }
          { balance: (- current-balance amount) }
        )
        (ok true)
      )
      ERR-INSUFFICIENT-BALANCE
    )
  )
)

;; Rate a user after a completed trade
(define-public (rate-user (user principal) (rating uint))
  (let 
    (
      (current-rating (default-to 
        { total-score: u0, rating-count: u0, average-rating: u0 }
        (map-get? user-ratings { user: user })
      ))
    )
    (if (and (>= rating u1) (<= rating u5))
      (let 
        (
          (new-total-score (+ (get total-score current-rating) rating))
          (new-count (+ (get rating-count current-rating) u1))
          (new-average (/ new-total-score new-count))
        )
        (map-set user-ratings
          { user: user }
          {
            total-score: new-total-score,
            rating-count: new-count,
            average-rating: new-average
          }
        )
        (ok true)
      )
      ERR-INVALID-RATING
    )
  )
)

;; Cancel an open order
(define-public (cancel-order (order-id uint))
  (let ((order (unwrap! (map-get? orders { order-id: order-id }) ERR-ORDER-NOT-FOUND)))
    (if (and
          (is-eq tx-sender (get buyer order))
          (is-eq (get status order) "open")
        )
      (begin
        ;; Refund escrowed funds
        (let ((user-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender })))))
          (map-set user-balances
            { user: tx-sender }
            { balance: (+ user-balance (get total-amount order)) }
          )
        )
        ;; Update order status
        (map-set orders
          { order-id: order-id }
          (merge order { status: "cancelled" })
        )
        (ok true)
      )
      ERR-NOT-AUTHORIZED
    )
  )
)

;; Read-only functions

;; Get commodity details
(define-read-only (get-commodity (commodity-id uint))
  (map-get? commodities { commodity-id: commodity-id })
)

;; Get order details
(define-read-only (get-order (order-id uint))
  (map-get? orders { order-id: order-id })
)

;; Get trade details
(define-read-only (get-trade (trade-id uint))
  (map-get? trades { trade-id: trade-id })
)

;; Get user balance
(define-read-only (get-user-balance (user principal))
  (default-to u0 (get balance (map-get? user-balances { user: user })))
)

;; Get user rating
(define-read-only (get-user-rating (user principal))
  (map-get? user-ratings { user: user })
)

;; Get platform revenue (only owner)
(define-read-only (get-platform-revenue)
  (if (is-eq tx-sender CONTRACT-OWNER)
    (ok (var-get platform-revenue))
    ERR-NOT-AUTHORIZED
  )
)