#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail
set -o xtrace

# Check required environment variables
: "${CARDANO_NODE_NETWORK_ID:?CARDANO_NODE_NETWORK_ID must be set}"
: "${CARDANO_NODE_SOCKET_PATH:?CARDANO_NODE_SOCKET_PATH must be set}"

# Constants
readonly STATE_DIR="$PWD/.run"
readonly CL_DIR="$STATE_DIR/cl"
readonly WALLET_VKEY_FILE="$CL_DIR/wallet.vkey"
readonly WALLET_SKEY_FILE="$CL_DIR/wallet.skey"
readonly PUBLISH_BODY_FILE="$CL_DIR/publish.body"
readonly PUBLISH_TX_FILE="$CL_DIR/publish.tx"

mkdir -p "$CL_DIR"

# Setup wallet keys
cp .run/testnet/utxo-keys/utxo1.vkey "$WALLET_VKEY_FILE"
cp .run/testnet/utxo-keys/utxo1.skey "$WALLET_SKEY_FILE"
# and store the wallet address
WALLET_ADDR=$(cardano-cli address build --payment-verification-key-file "$WALLET_VKEY_FILE" --testnet-magic "$CARDANO_NODE_NETWORK_ID")
echo "$WALLET_ADDR" > "$CL_DIR/wallet.addr"

# Find UTxO with the highest lovelace value to fund the transaction
UTXO=$(cardano-cli conway query utxo --address "$WALLET_ADDR" --output-json | \
    jq -r 'to_entries | map(select(.value.value.lovelace != null)) | sort_by(.value.value.lovelace) | reverse | .[0].key')

# Function to build a publish transaction with given min ADA amount at the reference script output
build_tx() {
    local publish_min_ada=$1
    local allow_failure=$2
    cardano-cli conway transaction build \
        --testnet-magic "$CARDANO_NODE_NETWORK_ID" \
        --tx-in "$UTXO" \
        --tx-out "$WALLET_ADDR+$publish_min_ada" \
        --tx-out-reference-script-file plutus/cl.json \
        --change-address "$WALLET_ADDR" \
        --out-file "$PUBLISH_BODY_FILE" 2>&1 || $allow_failure
}

# First attempt with 0 lovelace to get minimum UTxO requirement
ERROR_MSG=$(build_tx 0 true)
MIN_UTXO=$(echo "$ERROR_MSG" | grep "Minimum required UTxO:" | awk '{print $4}')

if [ -z "$MIN_UTXO" ]; then
    echo "Failed to get minimum UTxO requirement"
    exit 1
fi
# Second attempt with the correct amount
build_tx "$MIN_UTXO" false

cardano-cli transaction sign \
  --tx-body-file "$PUBLISH_BODY_FILE" \
  --signing-key-file "$WALLET_SKEY_FILE" \
  --testnet-magic "$CARDANO_NODE_NETWORK_ID" \
  --out-file "$PUBLISH_TX_FILE"

cardano-cli transaction submit \
  --testnet-magic "$CARDANO_NODE_NETWORK_ID" \
  --tx-file "$PUBLISH_TX_FILE"

echo "$(cardano-cli transaction txid --tx-file "$PUBLISH_TX_FILE")#0" > "$CL_DIR/ref.txin"

retry --delay 1 -- check-tx "$(cat "$CL_DIR/ref.txin")"

