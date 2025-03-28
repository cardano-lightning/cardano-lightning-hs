#!/usr/bin/env bash

FAUCET_ADDRESS=$(cat .run/cl/wallet.addr)
FAUCET_SKEY_FILE=.run/cl/wallet.skey

OPENER_WALLET_DIR=./opener-wallet
if [ ! -d $OPENER_WALLET_DIR ]; then
  new=$(mk-wallet --funding-amount 1000000 --funding-skey-file .run/cl/wallet.skey --verbose)
  ln -s $new $OPENER_WALLET_DIR
fi

NON_OPENER_WALLET_DIR=./non-opener-wallet
if [ ! -d $NON_OPENER_WALLET_DIR ]; then
  new=$(mk-wallet --funding-amount 1000000 --funding-skey-file .run/cl/wallet.skey --verbose)
  ln -s $new $NON_OPENER_WALLET_DIR
fi

cabal run cardano-lightning -- \
  build \
  --fee-input "$(cardano-cli conway query utxo --address $FAUCET_ADDRESS --output-json | jq -r "keys[0]")" \
  --testnet-magic "${CARDANO_NODE_NETWORK_ID}" \
  --node-socket-file "${CARDANO_NODE_SOCKET_PATH}" \
  --cl-script-utxo "$(cat .run/cl/ref.txin)" \
  --opener-key "$(cat ${OPENER_WALLET_DIR}/payment.pub | bech32)" \
  --non-opener-key "$(cat ${NON_OPENER_WALLET_DIR}/payment.pub | bech32)" \
  --contestation-period 10 \
  --amount 1000000 \
  --out-dir .


