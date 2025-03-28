# A lot of this is copied from cardano-parts, see https://github.com/input-output-hk/cardano-parts/issues/42

{ repoRoot, inputs, pkgs, lib, system }:
let
  inherit (inputs) self;
  inherit (lib) mkForce;
  inherit (pkgs) coreutils;
  stateDir = "./.run";

  cardano-node-ready = pkgs.writeShellApplication {
    name = "cardano-node-ready";
    text = ''
      # Check if socket exists
      [ -S "$CARDANO_NODE_SOCKET_PATH" ] || exit 1

      # Try to ping the node
      if ! cardano-cli ping -c 1 -u "$CARDANO_NODE_SOCKET_PATH" -m "$CARDANO_NODE_NETWORK_ID"; then
        exit 1
      fi

      # Query tip to ensure node is synced
      if ! cardano-cli query tip --testnet-magic "$CARDANO_NODE_NETWORK_ID" > /dev/null 2>&1; then
        exit 1
      fi
    '';
    runtimeInputs = [ inputs.cardano-node.packages.cardano-cli ];
  };

  cardano-node-query = pkgs.writeShellApplication {
    name = "cardano-node-query";
    text = ''
      while true; do
        date -u --rfc-3339=seconds
        cardano-cli query tip
        echo
        sleep 10
      done
    '';
    runtimeInputs = [ inputs.cardano-node.packages.cardano-cli ];
  };

  check-tx = pkgs.writeShellApplication {
    name = "check-tx";
    text = ''
      set -euo pipefail
      cardano-cli conway query utxo --tx-in "$1" --output-json | jq -e 'to_entries | .[0]' > /dev/null
    '';
    runtimeInputs = [ pkgs.retry inputs.cardano-node.packages.cardano-cli pkgs.jq ];
  };

  initialize = pkgs.writeShellApplication {
    name = "initialize";
    text = ''
      # exec ./scripts/initialize.sh
      echo "This is a placeholder for the initialize script"
    '';
    runtimeInputs = [ pkgs.retry ];
  };

  publish-script = pkgs.writeShellApplication {
    name = "publish-script";
    runtimeInputs = [ inputs.cardano-node.packages.cardano-cli pkgs.jq check-tx pkgs.retry ];
    text = builtins.readFile ./scripts/publish-script.sh;
  };

in (pkgs.formats.yaml { }).generate "process-compose.yaml" {
  version = "0.5";
  log_level = "info";
  log_length = 1200000;
  log_location = "${stateDir}/process-compose.log";
  log_configuration = {
    rotation = {
      max_size_mb = 8;
      max_age_days = 3;
      max_backups = 3;
      compress = true;
    };
    fields_order = ["time" "level" "message"];
    disable_json = true;
    timestamp_format = "06-01-02 15:04:05.000";
    no_metadata = false;
    add_timestamp = true;
  };

  processes = {
    clear-state = {
      command = "rm -fR ${stateDir}";
    };

    cardano-node = {
      namespace = "cardano-node";
      command = "deploy-local-testnet ${stateDir}/testnet/";
      readiness_probe = {
        exec.command = "${cardano-node-ready}/bin/cardano-node-ready";
        period_seconds = 5;
        timeout_seconds = 10;
        failure_threshold = 3;
        success_threshold = 1;
        initial_delay_seconds = 5;
      };
      environment = [
        "CARDANO_NODE_NETWORK_ID=42"
        "CARDANO_NODE_SOCKET_PATH=${stateDir}/testnet/node-spo1/node.sock"
      ];
      depends_on."clear-state".condition = "process_completed_successfully";
    };

    cardano-node-query = {
      namespace = "cardano-node";
      command = "${cardano-node-query}/bin/cardano-node-query";
      depends_on."cardano-node".condition = "process_healthy";
      environment = [
        "CARDANO_NODE_NETWORK_ID=42"
        "CARDANO_NODE_SOCKET_PATH=${stateDir}/testnet/node-spo1/node.sock"
      ];
    };

    initialize = {
      namespace = "mgdoc";
      command = "${initialize}/bin/initialize";
      depends_on."cardano-node".condition = "process_healthy";
      environment = [
        "CARDANO_NODE_NETWORK_ID=42"
        "CARDANO_NODE_SOCKET_PATH=${stateDir}/testnet/node-spo1/node.sock"
      ];
    };

    publish-script = {
      namespace = "mgdoc";
      command = "${publish-script}/bin/publish-script";
      depends_on."cardano-node".condition = "process_healthy";
      environment = [
        "CARDANO_NODE_NETWORK_ID=42"
        "CARDANO_NODE_SOCKET_PATH=${stateDir}/testnet/node-spo1/node.sock"
      ];
    };
  };
}

