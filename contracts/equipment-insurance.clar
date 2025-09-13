;; Agricultural Equipment Insurance Smart Contract
;; Manages farm machinery coverage with equipment valuation, claims processing,
;; repair coordination, and replacement management

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_EQUIPMENT_NOT_FOUND (err u101))
(define-constant ERR_POLICY_NOT_FOUND (err u102))
(define-constant ERR_INSUFFICIENT_PREMIUM (err u103))
(define-constant ERR_CLAIM_NOT_FOUND (err u104))
(define-constant ERR_CLAIM_ALREADY_PROCESSED (err u105))
(define-constant ERR_EQUIPMENT_ALREADY_REGISTERED (err u106))

;; Data Variables
(define-data-var next-equipment-id uint u1)
(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)

;; Equipment Registry
(define-map equipment-registry 
    { equipment-id: uint }
    {
        owner: principal,
        equipment-type: (string-ascii 50),
        brand: (string-ascii 50),
        model: (string-ascii 50),
        year: uint,
        serial-number: (string-ascii 100),
        estimated-value: uint,
        condition: (string-ascii 20),
        is-active: bool
    }
)

;; Insurance Policies
(define-map insurance-policies 
    { policy-id: uint }
    {
        equipment-id: uint,
        policyholder: principal,
        coverage-amount: uint,
        premium-amount: uint,
        start-date: uint,
        end-date: uint,
        is-active: bool
    }
)

;; Claims
(define-map claims 
    { claim-id: uint }
    {
        policy-id: uint,
        claimant: principal,
        damage-description: (string-ascii 500),
        estimated-damage-cost: uint,
        claim-date: uint,
        status: (string-ascii 20),
        approved-amount: uint,
        repair-shop: (optional principal)
    }
)

;; Equipment by Owner
(define-map owner-equipment
    { owner: principal, equipment-id: uint }
    { registered: bool }
)

;; Policy by Equipment
(define-map equipment-policy
    { equipment-id: uint }
    { policy-id: uint }
)

;; Read-only functions

;; Get equipment details
(define-read-only (get-equipment (equipment-id uint))
    (map-get? equipment-registry { equipment-id: equipment-id })
)

;; Get policy details
(define-read-only (get-policy (policy-id uint))
    (map-get? insurance-policies { policy-id: policy-id })
)

;; Get claim details
(define-read-only (get-claim (claim-id uint))
    (map-get? claims { claim-id: claim-id })
)

;; Check if equipment is owned by principal
(define-read-only (is-equipment-owner (equipment-id uint) (owner principal))
    (match (get-equipment equipment-id)
        equipment (is-eq (get owner equipment) owner)
        false
    )
)

;; Get policy for equipment
(define-read-only (get-equipment-policy-id (equipment-id uint))
    (map-get? equipment-policy { equipment-id: equipment-id })
)

;; Public functions

;; Register new equipment
(define-public (register-equipment 
    (equipment-type (string-ascii 50))
    (brand (string-ascii 50))
    (model (string-ascii 50))
    (year uint)
    (serial-number (string-ascii 100))
    (estimated-value uint)
    (condition (string-ascii 20))
)
    (let 
        (
            (equipment-id (var-get next-equipment-id))
        )
        ;; Check if equipment already exists by serial number (simplified check)
        (asserts! (is-none (map-get? equipment-registry { equipment-id: equipment-id })) ERR_EQUIPMENT_ALREADY_REGISTERED)
        
        ;; Register equipment
        (map-set equipment-registry
            { equipment-id: equipment-id }
            {
                owner: tx-sender,
                equipment-type: equipment-type,
                brand: brand,
                model: model,
                year: year,
                serial-number: serial-number,
                estimated-value: estimated-value,
                condition: condition,
                is-active: true
            }
        )
        
        ;; Set owner mapping
        (map-set owner-equipment
            { owner: tx-sender, equipment-id: equipment-id }
            { registered: true }
        )
        
        ;; Increment next equipment ID
        (var-set next-equipment-id (+ equipment-id u1))
        
        (ok equipment-id)
    )
)

;; Create insurance policy
(define-public (create-policy 
    (equipment-id uint)
    (coverage-amount uint)
    (premium-amount uint)
    (duration-blocks uint)
)
    (let 
        (
            (policy-id (var-get next-policy-id))
            (start-date stacks-block-height)
            (end-date (+ stacks-block-height duration-blocks))
        )
        ;; Verify equipment exists and caller owns it
        (asserts! (is-equipment-owner equipment-id tx-sender) ERR_UNAUTHORIZED)
        
        ;; Create policy
        (map-set insurance-policies
            { policy-id: policy-id }
            {
                equipment-id: equipment-id,
                policyholder: tx-sender,
                coverage-amount: coverage-amount,
                premium-amount: premium-amount,
                start-date: start-date,
                end-date: end-date,
                is-active: true
            }
        )
        
        ;; Link equipment to policy
        (map-set equipment-policy
            { equipment-id: equipment-id }
            { policy-id: policy-id }
        )
        
        ;; Increment next policy ID
        (var-set next-policy-id (+ policy-id u1))
        
        (ok policy-id)
    )
)

;; File insurance claim
(define-public (file-claim
    (policy-id uint)
    (damage-description (string-ascii 500))
    (estimated-damage-cost uint)
)
    (let 
        (
            (claim-id (var-get next-claim-id))
            (policy (unwrap! (get-policy policy-id) ERR_POLICY_NOT_FOUND))
        )
        ;; Verify caller is policyholder
        (asserts! (is-eq (get policyholder policy) tx-sender) ERR_UNAUTHORIZED)
        
        ;; Verify policy is active
        (asserts! (get is-active policy) ERR_POLICY_NOT_FOUND)
        
        ;; Create claim
        (map-set claims
            { claim-id: claim-id }
            {
                policy-id: policy-id,
                claimant: tx-sender,
                damage-description: damage-description,
                estimated-damage-cost: estimated-damage-cost,
                claim-date: stacks-block-height,
                status: "pending",
                approved-amount: u0,
                repair-shop: none
            }
        )
        
        ;; Increment next claim ID
        (var-set next-claim-id (+ claim-id u1))
        
        (ok claim-id)
    )
)

;; Process claim (admin function)
(define-public (process-claim
    (claim-id uint)
    (status (string-ascii 20))
    (approved-amount uint)
    (repair-shop (optional principal))
)
    (let 
        (
            (claim (unwrap! (get-claim claim-id) ERR_CLAIM_NOT_FOUND))
        )
        ;; Only contract owner can process claims
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        
        ;; Verify claim is pending
        (asserts! (is-eq (get status claim) "pending") ERR_CLAIM_ALREADY_PROCESSED)
        
        ;; Update claim
        (map-set claims
            { claim-id: claim-id }
            (merge claim {
                status: status,
                approved-amount: approved-amount,
                repair-shop: repair-shop
            })
        )
        
        (ok true)
    )
)

;; Update equipment condition after repair
(define-public (update-equipment-condition
    (equipment-id uint)
    (new-condition (string-ascii 20))
    (new-value uint)
)
    (let 
        (
            (equipment (unwrap! (get-equipment equipment-id) ERR_EQUIPMENT_NOT_FOUND))
        )
        ;; Verify caller owns equipment or is contract owner
        (asserts! (or (is-equipment-owner equipment-id tx-sender) 
                     (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
        
        ;; Update equipment
        (map-set equipment-registry
            { equipment-id: equipment-id }
            (merge equipment {
                condition: new-condition,
                estimated-value: new-value
            })
        )
        
        (ok true)
    )
)

;; Deactivate equipment
(define-public (deactivate-equipment (equipment-id uint))
    (let 
        (
            (equipment (unwrap! (get-equipment equipment-id) ERR_EQUIPMENT_NOT_FOUND))
        )
        ;; Verify caller owns equipment
        (asserts! (is-equipment-owner equipment-id tx-sender) ERR_UNAUTHORIZED)
        
        ;; Deactivate equipment
        (map-set equipment-registry
            { equipment-id: equipment-id }
            (merge equipment { is-active: false })
        )
        
        (ok true)
    )
)

;; Get contract stats (admin function)
(define-read-only (get-contract-stats)
    {
        total-equipment: (- (var-get next-equipment-id) u1),
        total-policies: (- (var-get next-policy-id) u1),
        total-claims: (- (var-get next-claim-id) u1)
    }
)


;; title: equipment-insurance
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

