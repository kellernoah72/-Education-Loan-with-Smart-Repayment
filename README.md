# Education Loan Smart Contract

A Clarity smart contract for managing education loans with income-based repayment terms.

## Features

- Loan application with income verification
- Income-based repayment calculation
- Flexible payment system
- Loan completion tracking
- Borrower statistics

## Contract Functions

### Administrative Functions
- `initialize-contract`: Set contract owner
- `set-min-income-percentage`: Update minimum income percentage for payments

### Borrower Functions
- `apply-for-loan`: Apply for a new education loan
- `make-payment`: Make a loan repayment
- `get-loan-details`: View current loan status
- `get-borrower-statistics`: View borrower history
- `calculate-min-payment`: Calculate minimum payment based on income

## Usage

1. Deploy the contract
2. Initialize with contract owner
3. Borrowers can apply for loans by calling `apply-for-loan`
4. Make payments using `make-payment`
5. Track loan status with `get-loan-details`

## Requirements

- Clarinet
- STX tokens for loan funding and repayment
