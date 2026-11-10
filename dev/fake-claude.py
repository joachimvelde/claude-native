#!/usr/bin/env python3
"""Stands in for `claude -p ... stream-json` in tests: replays a recording
from dev/record.py. Lines claude wrote are written back; where the recording
has a line the client sent, this waits for the client to send one. Request
ids in responses are swapped for the ones the client really used.
The recording is named by $CLAUDE_NATIVE_REPLAY; arguments are ignored."""
import json, os, sys, time

ids = {}  # recorded request_id -> the client's

# Control requests that are not in recordings, and what to answer them.
ANSWERS = {
    "get_settings": {"applied": {"model": "claude-haiku-5-5", "effort": "medium"}},
    "get_usage": {
        "session": {"total_cost_usd": 0.42, "total_api_duration_ms": 75000,
                    "total_lines_added": 12, "total_lines_removed": 3},
        "subscription_type": "max", "rate_limits_available": True,
        "rate_limits": {"limits": [
            {"kind": "session", "group": "session", "percent": 95,
             "resets_at": "2026-11-10T14:00:00.137965+00:00", "scope": None},
            {"kind": "weekly_all", "group": "weekly", "percent": 21,
             "resets_at": "2026-11-13T17:00:00.137965+00:00", "scope": None},
            {"kind": "weekly_scoped", "group": "weekly", "percent": 11,
             "resets_at": "2026-11-13T17:00:00.138123+00:00",
             "scope": {"model": {"id": None, "display_name": "Fable"}, "surface": None}}]},
        "behaviors": {"day": {
            "request_count": 803, "session_count": 18,
            "recommendations": [{"key": "long_context", "pct": 87,
                                 "title": "Start fresh when you switch tasks", "detail": "run /clear"}],
            "skills": [{"name": "dataviz", "pct": 2}], "agents": [], "plugins": [], "mcp_servers": []}}},
    "get_context_usage": {"totalTokens": 23404, "maxTokens": 1000000, "percentage": 2},
}

def answer(sent):
    """Answer SENT if it is a control request not in recordings."""
    if sent.get("type") != "control_request" or sent["request"].get("subtype") not in ANSWERS:
        return False
    sys.stdout.write(json.dumps({"type": "control_response", "response": {
        "subtype": "success", "request_id": sent["request_id"],
        "response": ANSWERS[sent["request"]["subtype"]]}}) + "\n")
    sys.stdout.flush()
    return True

for line in open(os.environ["CLAUDE_NATIVE_REPLAY"]):
    record = json.loads(line)
    msg = record["msg"]
    if record["dir"] == "in":
        # Some requests are not in recordings: answer them and read on.
        while True:
            got = sys.stdin.readline()
            if not got:
                sys.exit(0)
            sent = json.loads(got)
            if not answer(sent):
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
# Like claude: stay until stdin closes, still answering those requests.
for got in sys.stdin:
    answer(json.loads(got))
