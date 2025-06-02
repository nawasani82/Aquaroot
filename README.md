# 🌊 Aquaroot - Water Source Monitoring Smart Contract

## 💧 Overview
Aquaroot is a decentralized water source monitoring system that enables community-driven reporting and rewards for water quality tracking.

## 🎯 Features
- Add new water sources with location data
- Submit water quality reports
- Earn rewards for active participation
- Track reporter reputation
- Monitor water source statistics

## 📝 Contract Functions

### Public Functions
- `add-water-source`: Register a new water source
- `submit-report`: Submit a water quality report
- `update-reward-amount`: Update the reward amount (owner only)

### Read-Only Functions
- `get-water-source`: Retrieve water source details
- `get-source-report`: Get specific report details
- `get-reporter-stats`: View reporter statistics
- `get-total-sources`: Get total number of registered sources
- `get-total-reports`: Get total number of submitted reports

## 🚀 Usage

1. Deploy the contract
2. Add water sources using `add-water-source`
3. Submit reports using `submit-report`
4. Check statistics using read-only functions

## 💎 Rewards
Reporters earn STX tokens for each valid submission. The reward amount can be adjusted by the contract owner.

## 🔒 Security
- One report per source per reporter
- Quality scores must be between 0-10
- Only contract owner can modify reward amounts
```


