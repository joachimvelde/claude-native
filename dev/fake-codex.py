#!/usr/bin/env python3
"""Stands in for `codex app-server` in tests: replays a recording from
dev/record-codex.py. The recording is a list of steps, each something the
client sent and what Codex wrote after it. Each message the client sends
plays the first unplayed step that began with a message like it (a request
with the same method, a notification, or an answer to Codex's request), so
the client may send its requests in another order than the recorder did.
Answers carry the ids the client used. A request the recording lacks gets
an empty result. The recording is named by $AGENT_NATIVE_REPLAY; arguments
are ignored."""
import json, os, sys, time

steps = []  # [kind, sent, outputs, played]
for line in open(os.environ["AGENT_NATIVE_REPLAY"]):
    record = json.loads(line)
    if record["dir"] == "in":
        steps.append([None, record["msg"], [], False])
    elif steps:
        steps[-1][2].append(record["msg"])


def kind(msg):
    """What a message is, for matching it to a recorded one."""
    if "method" in msg:
        return msg["method"]
    return "answer"


ids = {}  # recorded request id -> the client's


def write(msg):
    sys.stdout.write(json.dumps(msg) + "\n")
    sys.stdout.flush()


for got in sys.stdin:
    if not got.strip():
        continue
    sent = json.loads(got)
    step = next((s for s in steps if not s[3] and kind(s[1]) == kind(sent)), None)
    if step is None:
        if "id" in sent and "method" in sent:
            write({"id": sent["id"], "result": {}})
        continue
    step[3] = True
    if "id" in step[1] and "method" in step[1]:
        ids[step[1]["id"]] = sent["id"]
    for msg in step[2]:
        # Answers to the client's requests carry its ids; Codex's own
        # requests keep theirs.
        if "method" not in msg and msg.get("id") in ids:
            msg = dict(msg, id=ids[msg["id"]])
        write(msg)
        time.sleep(0.002)
