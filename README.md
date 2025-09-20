# BridgeBit - Cross-Chain Bridge Smart Contract

A lightweight DAO-governed cross-chain bridge implementation in Clarity v2.

## Features

- **Cross-Chain Bridging**: Lock/mint and burn/release functionality
- **Validator System**: Staking, slashing, and fee-sharing mechanics
- **DAO Governance**: Simple admin-based governance (extensible to token-weighted)
- **Basic Wrapped Token**: Non-SIP-010 wrapped token implementation

## Architecture

- **Validators**: Stake STX to participate in bridge validation
- **Quorum**: Configurable validator stake threshold for confirmations
- **Fee Structure**: Adjustable fee basis points for operations
- **Slashing**: Penalty mechanism for validator misbehavior

## Getting Started

1. Clone the repository
2. Install dependencies:
```bash
npm install
```
3. Run tests:
```bash
clarinet test
```

## Configuration

Key parameters (adjustable by admins):
- Minimum validator stake: 0.01 STX (default)
- Fee: 0.5% (50 basis points)
- Validator quorum: 50% (5000 basis points)
- Slash penalty: 20% (2000 basis points)

## License

MIT License - See LICENSE file for details
