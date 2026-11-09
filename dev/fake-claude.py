#!/usr/bin/env python3
"""Stands in for `claude -p ... stream-json` in tests: replays a recording
from dev/record.py. Lines claude wrote are written back; where the recording
has a line the client sent, this waits for the client to send one. Request
ids in responses are swapped for the ones the client really used.
The recording is named by $CLAUDE_NATIVE_REPLAY; arguments are ignored."""
import json, os, sys, time

ids = {}  # recorded request_id -> the client's

def answer_settings(request_id):
    sys.stdout.write(json.dumps({"type": "control_response", "response": {
        "subtype": "success", "request_id": request_id,
        "response": {"applied": {"model": "claude-haiku-5-5", "effort": "medium"}}}}) + "\n")
    sys.stdout.flush()

for line in open(os.environ["CLAUDE_NATIVE_REPLAY"]):
    record = json.loads(line)
    msg = record["msg"]
    if record["dir"] == "in":
        # get_settings is not in recordings: answer it and read on.
        while True:
            got = sys.stdin.readline()
            if not got:
                sys.exit(0)
            sent = json.loads(got)
            if sent.get("type") == "control_request" and sent["request"].get("subtype") == "get_settings":
                answer_settings(sent["request_id"])
                continue
            break
        # Like --replay-user-messages: echo a prompt as it is taken up.
        if sent.get("type") == "user":
            sys.stdout.write(json.dumps({"type": "user", "message": sent["message"], "isReplay": True}) + "\n")
            sys.stdout.flush()
        if msg.get("type") == "control_request":
            ids[msg["request_id"]] = sent.get("request_id")
        continue
    if msg.get("type") == "control_response":
        response = msg["response"]
        response["request_id"] = ids.get(response["request_id"], response["request_id"])
    sys.stdout.write(json.dumps(msg) + "\n")
    sys.stdout.flush()
    time.sleep(0.002)
# Like claude: stay until stdin closes, still answering get_settings.
for got in sys.stdin:
    sent = json.loads(got)
    if sent.get("type") == "control_request" and sent["request"].get("subtype") == "get_settings":
        answer_settings(sent["request_id"])
