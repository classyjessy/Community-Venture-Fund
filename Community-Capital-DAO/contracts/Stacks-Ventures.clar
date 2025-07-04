;; Project Funding DAO Smart Contract
;; A decentralized autonomous organization for funding projects through community voting

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-proposal-exists (err u104))
(define-constant err-voting-period-ended (err u105))
(define-constant err-voting-period-active (err u106))
(define-constant err-already-voted (err u107))
(define-constant err-insufficient-funds (err u108))
(define-constant err-proposal-not-approved (err u109))
(define-constant err-funds-already-claimed (err u110))
(define-constant err-invalid-voting-period (err u111))
(define-constant err-invalid-quorum (err u112))

;; Data Variables
(define-data-var next-proposal-id uint u1)
(define-data-var dao-treasury uint u0)
(define-data-var min-voting-period uint u144) ;; ~1 day in blocks
(define-data-var max-voting-period uint u1440) ;; ~10 days in blocks
(define-data-var quorum-percentage uint u10) ;; 10% quorum required
(define-data-var approval-threshold uint u50) ;; 50% approval threshold

;; Data Maps
(define-map proposals
  uint
  {
    id: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    amount: uint,
    recipient: principal,
    votes-for: uint,
    votes-against: uint,
    voting-end-block: uint,
    executed: bool,
    created-at: uint
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool, amount: uint }
)

(define-map member-voting-power
  principal
  uint
)

(define-map member-registry
  principal
  { joined-at: uint, active: bool }
)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-member-voting-power (member principal))
  (default-to u0 (map-get? member-voting-power member))
)

(define-read-only (get-member-info (member principal))
  (map-get? member-registry member)
)

(define-read-only (get-dao-treasury)
  (var-get dao-treasury)
)

(define-read-only (get-next-proposal-id)
  (var-get next-proposal-id)
)

(define-read-only (get-voting-parameters)
  {
    min-voting-period: (var-get min-voting-period),
    max-voting-period: (var-get max-voting-period),
    quorum-percentage: (var-get quorum-percentage),
    approval-threshold: (var-get approval-threshold)
  }
)

(define-read-only (calculate-quorum (total-voting-power uint))
  (/ (* total-voting-power (var-get quorum-percentage)) u100)
)

(define-read-only (is-proposal-approved (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (let
      (
        (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
        (approval-rate (if (> total-votes u0)
                        (/ (* (get votes-for proposal) u100) total-votes)
                        u0))
      )
      (>= approval-rate (var-get approval-threshold))
    )
    false
  )
)

(define-read-only (has-voting-ended (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (>= stacks-block-height (get voting-end-block proposal))
    false
  )
)

;; Private functions
(define-private (is-member (user principal))
  (match (map-get? member-registry user)
    member (get active member)
    false
  )
)

(define-private (get-total-voting-power)
  ;; Simplified implementation - returns a placeholder value
  ;; In a real implementation, this would iterate through all members
  ;; and sum their voting power
  u0
)

;; Public functions

;; Join DAO as a member
(define-public (join-dao)
  (let
    (
      (caller tx-sender)
      (current-block stacks-block-height)
    )
    (asserts! (is-none (map-get? member-registry caller)) err-proposal-exists)
    (map-set member-registry caller { joined-at: current-block, active: true })
    (map-set member-voting-power caller u1) ;; Initial voting power
    (ok true)
  )
)

;; Deposit funds to DAO treasury
(define-public (deposit-to-treasury (amount uint))
  (let
    (
      (caller tx-sender)
    )
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    (var-set dao-treasury (+ (var-get dao-treasury) amount))
    (ok true)
  )
)

;; Helper function to validate string input
(define-private (is-valid-string (input (string-ascii 500)))
  (and (> (len input) u0) (<= (len input) u500))
)

;; Helper function to validate title
(define-private (is-valid-title (input (string-ascii 100)))
  (and (> (len input) u0) (<= (len input) u100))
)

;; Create a new proposal
(define-public (create-proposal 
  (title (string-ascii 100)) 
  (description (string-ascii 500))
  (amount uint)
  (recipient principal)
  (voting-period uint))
  (let
    (
      (caller tx-sender)
      (proposal-id (var-get next-proposal-id))
      (current-block stacks-block-height)
      (end-block (+ current-block voting-period))
      (validated-title (unwrap! (if (is-valid-title title) (some title) none) err-invalid-amount))
      (validated-description (unwrap! (if (is-valid-string description) (some description) none) err-invalid-amount))
      (validated-recipient (unwrap! (if (is-standard recipient) (some recipient) none) err-invalid-amount))
    )
    (asserts! (is-member caller) err-unauthorized)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (and (>= voting-period (var-get min-voting-period)) 
                  (<= voting-period (var-get max-voting-period))) err-invalid-voting-period)
    (asserts! (<= amount (var-get dao-treasury)) err-insufficient-funds)
    
    (map-set proposals proposal-id
      {
        id: proposal-id,
        title: validated-title,
        description: validated-description,
        proposer: caller,
        amount: amount,
        recipient: validated-recipient,
        votes-for: u0,
        votes-against: u0,
        voting-end-block: end-block,
        executed: false,
        created-at: current-block
      }
    )
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (caller tx-sender)
      (voter-power (get-member-voting-power caller))
      (existing-vote (map-get? votes { proposal-id: proposal-id, voter: caller }))
      (proposal (unwrap! (map-get? proposals proposal-id) err-not-found))
    )
    (asserts! (is-member caller) err-unauthorized)
    (asserts! (> voter-power u0) err-unauthorized)
    (asserts! (< stacks-block-height (get voting-end-block proposal)) err-voting-period-ended)
    (asserts! (is-none existing-vote) err-already-voted)
    
    ;; Record the vote
    (map-set votes { proposal-id: proposal-id, voter: caller } 
      { vote: vote-for, amount: voter-power })
    
    ;; Update proposal vote counts
    (let
      (
        (updated-votes-for (if vote-for 
                            (+ (get votes-for proposal) voter-power)
                            (get votes-for proposal)))
        (updated-votes-against (if vote-for 
                                (get votes-against proposal)
                                (+ (get votes-against proposal) voter-power)))
      )
      (map-set proposals proposal-id
        (merge proposal
          { 
            votes-for: updated-votes-for,
            votes-against: updated-votes-against
          }
        )
      )
    )
    
    (ok true)
  )
)

;; Execute an approved proposal
(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (caller tx-sender)
      (proposal (unwrap! (map-get? proposals proposal-id) err-not-found))
    )
    (asserts! (has-voting-ended proposal-id) err-voting-period-active)
    (asserts! (not (get executed proposal)) err-funds-already-claimed)
    (asserts! (is-proposal-approved proposal-id) err-proposal-not-approved)
    (asserts! (>= (var-get dao-treasury) (get amount proposal)) err-insufficient-funds)
    
    ;; Transfer funds to recipient
    (try! (as-contract (stx-transfer? (get amount proposal) tx-sender (get recipient proposal))))
    
    ;; Update treasury and proposal status
    (var-set dao-treasury (- (var-get dao-treasury) (get amount proposal)))
    (map-set proposals proposal-id (merge proposal { executed: true }))
    
    (ok true)
  )
)

;; Admin functions (only contract owner)

;; Update voting parameters
(define-public (update-voting-parameters 
  (new-min-period uint) 
  (new-max-period uint) 
  (new-quorum uint) 
  (new-threshold uint))
  (let
    (
      (caller tx-sender)
    )
    (asserts! (is-eq caller contract-owner) err-owner-only)
    (asserts! (< new-min-period new-max-period) err-invalid-voting-period)
    (asserts! (and (> new-quorum u0) (<= new-quorum u100)) err-invalid-quorum)
    (asserts! (and (> new-threshold u0) (<= new-threshold u100)) err-invalid-quorum)
    
    (var-set min-voting-period new-min-period)
    (var-set max-voting-period new-max-period)
    (var-set quorum-percentage new-quorum)
    (var-set approval-threshold new-threshold)
    
    (ok true)
  )
)

;; Update member voting power (admin only)
(define-public (update-member-voting-power (member principal) (new-power uint))
  (let
    (
      (caller tx-sender)
    )
    (asserts! (is-eq caller contract-owner) err-owner-only)
    (asserts! (is-member member) err-not-found)
    (asserts! (> new-power u0) err-invalid-amount)
    
    (map-set member-voting-power member new-power)
    (ok true)
  )
)

;; Deactivate member (admin only)
(define-public (deactivate-member (member principal))
  (let
    (
      (caller tx-sender)
      (member-info (unwrap! (map-get? member-registry member) err-not-found))
    )
    (asserts! (is-eq caller contract-owner) err-owner-only)
    (asserts! (get active member-info) err-not-found)
    
    (map-set member-registry member (merge member-info { active: false }))
    (ok true)
  )
)

;; Emergency withdraw (admin only)
(define-public (emergency-withdraw (amount uint))
  (let
    (
      (caller tx-sender)
    )
    (asserts! (is-eq caller contract-owner) err-owner-only)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (var-get dao-treasury) amount) err-insufficient-funds)
    
    (try! (as-contract (stx-transfer? amount tx-sender caller)))
    (var-set dao-treasury (- (var-get dao-treasury) amount))
    
    (ok true)
  )
)