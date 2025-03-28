#!/usr/bin/env python3

import argparse
import json
import os
import sys
from pathlib import Path
from typing import Optional
import subprocess
import tempfile
from collections import namedtuple
import time

TxIn = namedtuple('TxIn', ['tx_hash', 'tx_ix'])
AUTxO = namedtuple('AUTxO', ['tx_in', 'address', 'lovelace', 'tokens'])

def submit_tx(tx_file, socket_path=None, await_timeout=60):
    run(f"cardano-cli conway transaction submit --tx-file {tx_file}")
    tx_id = run(f"cardano-cli conway transaction txid --tx-file {tx_file}").strip()
    tx_in = TxIn(tx_id, 0)
    await_utxo(tx_in, await_timeout, socket_path)
    return tx_id

def await_utxo(tx_in, timeout, socket_path=None):
    start_time = time.time()
    while time.time() - start_time < timeout:
        try:
            utxos = get_utxos(tx_in, socket_path)
            if any(u.tx_in == tx_in for u in utxos):
                return
            time.sleep(1)
        except Exception as err:
            print(f"Error getting UTxO: {err}")
            time.sleep(1)
    raise Exception(f"UTxO not found after {timeout} seconds")

verbose = False
def run(command):
    if verbose:
        sys.stderr.write(f"$ {command}\n")
    result = subprocess.run(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if result.returncode != 0:
        print(f"Command failed: {result.returncode}")
        sys.stderr.buffer.write(result.stderr)
        raise Exception(f"Command failed: {result.returncode}")
    return result.stdout.decode('utf-8').strip()

def get_utxos(query_input, socket_path) -> list[AUTxO]:
    args = []
    if isinstance(query_input, str):
        args.append(("address", query_input))
    elif isinstance(query_input, TxIn):
        args.append(("tx-in", f"{query_input.tx_hash}#{query_input.tx_ix}"))

    if socket_path:
        args.append(("socket-path", socket_path))
    cmd = f"cardano-cli query utxo --cardano-mode {args_str(args)} --out-file /dev/stdout"
    utxos = []
    utxos_json = json.loads(run(cmd))
    for tx_in_str, tx_out in utxos_json.items():
        tx_hash, tx_ix = tx_in_str.split("#")
        tx_in = TxIn(tx_hash, int(tx_ix))
        address = tx_out["address"]
        lovelace = tx_out["value"]["lovelace"]
        tokens = []  # We could parse other tokens here if needed
        utxos.append(AUTxO(tx_in, address, lovelace, tokens))
    return utxos

def args_str(args):
    return " ".join([f"--{arg} '{value}'" for arg, value in args])

def fund_wallet(funding_addr: str, funding_skey: str, target_addr: str,
                amount: int, socket_path: str, network_id: str) -> str:
    # Get UTxOs from funding wallet
    utxos = get_utxos(funding_addr, socket_path)
    if not utxos:
        raise Exception("No UTxOs found in funding wallet")

    # Find a suitable UTxO that has enough funds
    funding_utxo = next((u for u in utxos if u.lovelace >= amount + 2000000), None)  # 2 ADA for fees
    if not funding_utxo:
        raise Exception(f"No UTxO with sufficient funds (need {amount + 2000000} lovelace)")

    # Build the transaction
    tx_body_file = "tx.body"
    tx_file = "tx.signed"

    args = [
        ("tx-in", f"{funding_utxo.tx_in.tx_hash}#{funding_utxo.tx_in.tx_ix}"),
        ("tx-out", f"{target_addr}+{amount}"),
        ("change-address", funding_addr),
        ("out-file", tx_body_file)
    ]

    command = f"cardano-cli conway transaction build {args_str(args)}"
    run(command)

    # Sign the transaction
    run(f"cardano-cli conway transaction sign "
        f"--tx-body-file {tx_body_file} "
        f"--signing-key-file {funding_skey} "
        f"--out-file {tx_file}")

    # Submit and wait for confirmation
    return submit_tx(tx_file, socket_path)

def main():
    orig_dir = os.getcwd()

    parser = argparse.ArgumentParser(description='Create and optionally fund a Cardano wallet')
    parser.add_argument('--mnemonic-length', type=int, default=24,
                      help='Number of words in mnemonic (default: 24)')
    parser.add_argument('--funding-skey-file', type=str,
                      help='Path to signing key file used for funding')
    parser.add_argument('--funding-amount', type=int,
                      help='Amount in lovelace to fund the new wallet')
    parser.add_argument('--cardano-node-socket-path', type=str,
                      default=os.environ.get('CARDANO_NODE_SOCKET_PATH'),
                      help='Path to cardano node socket')
    parser.add_argument('--cardano-node-network-id', type=str,
                      default=os.environ.get('CARDANO_NODE_NETWORK_ID'),
                      help='Network ID')
    parser.add_argument('--verbose', action='store_true',
                      help='Print all commands on stderr', default=False)

    args = parser.parse_args()

    global verbose
    verbose = args.verbose

    # Validate funding arguments
    if bool(args.funding_skey_file) != bool(args.funding_amount):
        parser.error("Both --funding-skey-file and --funding-amount must be provided for funding")

    if args.funding_amount and not all([args.cardano_node_socket_path, args.cardano_node_network_id]):
        parser.error("Funding requires both socket path and network ID to be set")

    # Create and move to temporary directory
    temp_dir = tempfile.mkdtemp()
    os.chdir(temp_dir)

    # Generate wallet files
    run(f"cardano-address recovery-phrase generate --size {args.mnemonic_length} > mnemonic")
    run("cat mnemonic | cardano-address key from-recovery-phrase Shelley > root.prv")
    run("cat root.prv | cardano-address key child 1852H/1815H/0H/0/0 > payment.prv")
    run("cat root.prv | cardano-address key child 1852H/1815H/0H/2/0 > stake.prv")

    # Generate public keys
    run("cat stake.prv | cardano-address key public --without-chain-code > stake.pub")
    run("cat payment.prv | cardano-address key public --without-chain-code > payment.pub")

    # Generate payment address
    run("cat payment.pub | cardano-address address payment --network-tag 0 | "
        "cardano-address address delegation $(cat stake.pub) > payment.addr")

    # Convert keys
    run("cardano-cli key convert-cardano-address-key "
        "--shelley-payment-key --signing-key-file payment.prv --out-file payment.skey")
    run("cardano-cli key verification-key "
        "--signing-key-file payment.skey --verification-key-file payment.vkey")

    if args.funding_amount:
        # Get funding wallet info - use orig_dir to access the funding key file
        full_funding_skey_file = os.path.join(orig_dir, args.funding_skey_file)
        with open(full_funding_skey_file) as f:
            # we should generate the address from the skey file
            funding_vkey_file = "funding.vkey"
            run(f"cardano-cli conway key verification-key --signing-key-file {full_funding_skey_file} --verification-key-file {funding_vkey_file}")
            funding_addr = run(f"cardano-cli conway address build --payment-verification-key-file {funding_vkey_file} --testnet-magic 42")

        # Get new wallet address
        with open("payment.addr") as f:
            new_addr = f.read().strip()

        # Fund the new wallet
        tx_id = fund_wallet(
            funding_addr=funding_addr,
            funding_skey=full_funding_skey_file,
            target_addr=new_addr,
            amount=args.funding_amount,
            socket_path=args.cardano_node_socket_path,
            network_id=args.cardano_node_network_id
        )
        if verbose:
            sys.stderr.write(f"Funded new wallet with transaction: {tx_id}\n")

    sys.stdout.write(temp_dir)
if __name__ == "__main__":
    main()
