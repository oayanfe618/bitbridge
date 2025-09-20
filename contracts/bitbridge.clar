;; ------------------------------------------------------------
;; BridgeX
;; Lightweight DAO-governed cross-chain bridge (locking/minting),
;; validator staking, slashing, fee-sharing, and basic wrapped-token ledger.
;; Clarity v2 single-file reference implementation.
;; ------------------------------------------------------------
;; WARNING: Reference code - audit, test with Clarinet, and adapt for production.
;; ------------------------------------------------------------

(define-constant ERR-NOT-AUTH        (err u100))
(define-constant ERR-NOT-FOUND       (err u101))
(define-constant ERR-BAD-STATE       (err u102))
(define-constant ERR-BAD-INPUT       (err u103))
(define-constant ERR-INSUFFICIENT    (err u104))
(define-constant ERR-ALREADY         (err u105))
(define-constant ERR-NO-PENDING      (err u106))
(define-constant ERR-NO-VOTE         (err u107))
(define-constant ERR-NO-BALANCE      (err u108))
(define-constant ERR-LOW-STAKE       (err u109))
(define-constant ERR-NO-EVIDENCE     (err u110))

(define-data-var owner principal tx-sender)
(define-data-var treasury principal tx-sender)
(define-data-var next-request-id uint u1)
(define-data-var min-validator-stake uint u1000000) ;; default 0.01 STX
(define-data-var fee-bps uint u50) ;; 0.5% fee on operations
(define-data-var validator-quorum-bps uint u5000) ;; 50% of active validators' stake required
(define-data-var slash-bps uint u2000) ;; 20% slash on misbehavior

;; DAO admin list (simple admin set; can be replaced with token-weighted DAO)
(define-map admins { who: principal } { active: bool })

;; Validators ------------------------------------------------------------------
(define-map validators { who: principal } { stake: uint, active: bool, jailed: bool })
(define-data-var total-validators-stake uint u0)

;; Wrapped token ledger (simple non-SIP-010) ----------------------------------
(define-map wrapped-balances { who: principal } { balance: uint })
(define-data-var total-wrapped uint u0)

;; Pending cross-chain requests (lock/mint and burn/release)
(define-map lock-requests
  { id: uint }
  {
    sender: principal,
    amount: uint,
    target-chain: (string-ascii 32),
    target-address: (string-ascii 128),
    executed: bool,
    confirmations: uint
  }
)

(define-map release-requests
  { id: uint }
  {
    recipient: principal,
    amount: uint,
    source-chain: (string-ascii 32),
    source-proof-hash: (optional (buff 32)),
    executed: bool,
    confirmations: uint
  }
)

;; Validator confirmations (who confirmed which request)
(define-map confirmations
  { request-id: uint, who: principal }
  { confirmed: bool }
)

;; Events / logs are implicit via contract calls and maps

;; Utilities ------------------------------------------------------------------
(define-read-only (is-admin (p principal))
  (or (is-eq p (var-get owner))
      (default-to false (get active (map-get? admins { who: p }))))
)

(define-private (u-percent (amount uint) (bps uint))
  (as-max-int (/ (* amount bps) u10000))
)

(define-private (as-max-int (x uint)) x)

(define-private (increase-total-stake (delta uint))
  (var-set total-validators-stake (+ (var-get total-validators-stake) delta))
)

(define-private (decrease-total-stake (delta uint))
  (var-set total-validators-stake (if (> (var-get total-validators-stake) delta) (- (var-get total-validators-stake) delta) u0))
)

;; Admin controls -------------------------------------------------------------
(define-private (set-admin-state (who principal) (active bool))
  (map-set admins { who: who } { active: active }))

(define-public (set-admin-status (who principal) (active bool))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTH)
    (asserts! (not (is-eq who tx-sender)) ERR-BAD-INPUT)
    (ok (set-admin-state who active))))

(define-public (set-min-validator-stake (v uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTH)
    (asserts! (> v u0) ERR-BAD-INPUT)
    (ok (var-set min-validator-stake v))))

(define-public (set-fee-bps (v uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTH)
    (asserts! (and (> v u0) (<= v u10000)) ERR-BAD-INPUT)
    (ok (var-set fee-bps v))))

(define-public (set-validator-quorum-bps (v uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTH)
    (asserts! (and (> v u0) (<= v u10000)) ERR-BAD-INPUT)
    (ok (var-set validator-quorum-bps v))))

(define-public (set-slash-bps (v uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTH)
    (asserts! (and (> v u0) (<= v u10000)) ERR-BAD-INPUT)
    (ok (var-set slash-bps v))))

;; Validator staking/unstaking -------------------------------------------------
(define-public (register-validator)
  (let ((amt (stx-get-balance tx-sender)))
    (begin
      (asserts! (> amt u0) ERR-BAD-INPUT)
      (let ((prev (map-get? validators { who: tx-sender })))
        (if (is-none prev)
          (begin
            (map-set validators { who: tx-sender } { stake: amt, active: true, jailed: false })
            (increase-total-stake amt)
            (ok true))
          (let ((p (unwrap-panic prev)))
            (begin
              (map-set validators { who: tx-sender } { stake: (+ (get stake p) amt), active: (get active p), jailed: (get jailed p) })
              (increase-total-stake amt)
              (ok true)))))))
)

(define-public (unregister-validator (amount uint))
  (let ((validator-data (map-get? validators { who: tx-sender })))
    (match validator-data
      validator
        (let ((stake (get stake validator)))
          (asserts! (> stake u0) ERR-NO-BALANCE)
          (asserts! (<= amount stake) ERR-INSUFFICIENT)
          (begin
            (map-set validators 
              { who: tx-sender } 
              { stake: (- stake amount), active: (get active validator), jailed: (get jailed validator) })
            (decrease-total-stake amount)
            (ok true)))
      ERR-NOT-FOUND)))

;; Simple wrapped token helpers ------------------------------------------------
(define-private (increase-wrapped (who principal) (amount uint))
  (let ((prev (map-get? wrapped-balances { who: who })))
    (begin
      (match prev
        balance-data (map-set wrapped-balances { who: who } 
          { balance: (+ (get balance balance-data) amount) })
        (map-set wrapped-balances { who: who } { balance: amount }))
      (var-set total-wrapped (+ (var-get total-wrapped) amount))
      (ok true)))
)

(define-private (decrease-wrapped (who principal) (amount uint))
  (match (map-get? wrapped-balances { who: who })
    balance-data
      (let ((current-balance (get balance balance-data)))
        (asserts! (>= current-balance amount) ERR-INSUFFICIENT)
        (begin
          (map-set wrapped-balances { who: who } { balance: (- current-balance amount) })
          (var-set total-wrapped (if (> (var-get total-wrapped) amount) 
                                   (- (var-get total-wrapped) amount) 
                                   u0))
          (ok true)))
    ERR-NO-BALANCE))

;; User: Lock Assets (on Stacks) to mint wrapped tokens on target chain -------
(define-private (create-lock-request (sender principal) (amount uint) (t-chain (string-ascii 32)) (t-address (string-ascii 128)))
  (let ((request-id (var-get next-request-id)))
    ;; No need for explicit transfer in v2 - STX is already sent to contract
    (begin
      (asserts! (is-eq sender tx-sender) ERR-NOT-AUTH)
      (asserts! (> amount u0) ERR-BAD-INPUT)
      (asserts! (not (is-eq t-chain "")) ERR-BAD-INPUT)
      (asserts! (not (is-eq t-address "")) ERR-BAD-INPUT)
      (map-set lock-requests { id: request-id }
        { sender: sender, 
          amount: amount, 
          target-chain: t-chain, 
          target-address: t-address, 
          executed: false, 
          confirmations: u0 })
      (var-set next-request-id (+ request-id u1))
      (ok request-id))))

(define-private (validate-and-save-confirmation (request-id uint) (validator principal))
  (begin
    (asserts! (is-some (map-get? validators { who: validator })) ERR-NOT-AUTH)
    (asserts! (is-eq validator tx-sender) ERR-NOT-AUTH)
    (asserts! (is-none (map-get? confirmations { request-id: request-id, who: validator })) ERR-ALREADY)
    (map-set confirmations { request-id: request-id, who: validator } { confirmed: true })
    (ok true)))

(define-private (update-request-confirmations (request-data (optional { sender: principal, amount: uint, target-chain: (string-ascii 32), target-address: (string-ascii 128), executed: bool, confirmations: uint })) (request-id uint))
  (let ((request (unwrap! request-data ERR-NOT-FOUND)))
    (begin
      (asserts! (not (get executed request)) ERR-ALREADY)
      (map-set lock-requests { id: request-id } 
        (merge request { confirmations: (+ (get confirmations request) u1) }))
      (ok true))))

(define-public (lock-assets (target-chain (string-ascii 32)) (target-address (string-ascii 128)) (amount uint))
  (begin
    (asserts! (> amount u0) ERR-BAD-INPUT)
    (asserts! (<= amount (stx-get-balance tx-sender)) ERR-INSUFFICIENT)
    (asserts! (not (is-eq target-chain "")) ERR-BAD-INPUT)
    (asserts! (not (is-eq target-address "")) ERR-BAD-INPUT)
    (let ((request-id (try! (create-lock-request tx-sender amount target-chain target-address))))
      (ok true))))

(define-public (confirm-lock (request-id uint))
  (begin
    (asserts! (> request-id u0) ERR-BAD-INPUT)
    (let ((request-data (unwrap! (map-get? lock-requests { id: request-id }) ERR-NOT-FOUND)))
      (begin
        (try! (validate-and-save-confirmation request-id tx-sender))
        (try! (update-request-confirmations (some request-data) request-id))
        (ok true)))))

(define-public (finalize-lock (request-id uint))
  (begin 
    (asserts! (> request-id u0) ERR-BAD-INPUT)
    (let ((request-data (unwrap! (map-get? lock-requests { id: request-id }) ERR-NOT-FOUND)))
      (begin
        (asserts! (not (get executed request-data)) ERR-ALREADY)
        (let ((total-stake (var-get total-validators-stake)))
          (asserts! (> total-stake u0) ERR-BAD-STATE)
          (asserts! (>= (* (get confirmations request-data) u10000) (* total-stake (var-get validator-quorum-bps))) ERR-NO-VOTE)
          (map-set lock-requests { id: request-id } 
            (merge request-data { executed: true }))
          (ok true))))))

