#!/bin/bash

# List of servers to try
SERVERS=("150.164.26.216" "150.164.26.217" "150.164.26.185")
USER="heron"
REMOTE_PORT=8722
LOCAL_PORT=22
LOG_FILE="/home/heron/prjcts/bioinfo/utils/tunnel_log.txt"

# Function to log messages with timestamps
log_message() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

# Function to check if the tunnel is open
check_tunnel() {
    netstat -an | grep -q ":$REMOTE_PORT"
}

# Function to establish an SSH tunnel
create_tunnel() {
    local server=$1
    log_message "Trying to create a tunnel through $server..."
    ssh -N -R $REMOTE_PORT:127.0.0.1:$LOCAL_PORT -p 22 "$USER@$server" &
    sleep 5
}

# Log script execution
log_message "Checking if tunnel is open..."

# Check if tunnel is already open
if check_tunnel; then
    log_message "Tunnel is already open."
    exit 0
fi

# Try each server until a tunnel is established
for server in "${SERVERS[@]}"; do
    create_tunnel "$server"
    if check_tunnel; then
        log_message "Tunnel successfully established through $server."
        exit 0
    fi
done

log_message "Failed to establish a tunnel through any server."
exit 1
