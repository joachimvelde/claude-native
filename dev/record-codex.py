#!/usr/bin/env python3
"""Records a scripted `codex app-server` session, for building and testing
the Emacs client against real messages. Usage: record-codex.py OUT.jsonl SANDBOX_DIR

Each line of OUT is {"dir": "in"|"out", "msg": ...}: what was sent to Codex
and what it wrote back, in order. Prompts must match those in
test/agent-native-test.el, which sends them again to dev/fake-codex.py."""
import json, os, select, subprocess, sys, time

out_path, sandbox = sys.argv[1], os.path.realpath(sys.argv[2])
p = subprocess.Popen(["codex", "app-server"], cwd=sandbox, stdin=subprocess.PIPE,
                     stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
log = open(out_path, "w")
buf, lines, next_id = b"", [], [0]


def send(msg):
    log.write(json.dumps({"dir": "in", "msg": msg}) + "\n"); log.flush()
    p.stdin.write((json.dumps(msg) + "\n").encode()); p.stdin.flush()


def request(method, params=None):
    next_id[0] += 1
    msg = {"id": next_id[0], "method": method}
    if params is not None:
        msg["params"] = params
    send(msg)
    return next_id[0]


def pump(until=None, seconds=120, approve=True):
    """Log what Codex writes until UNTIL(msg) holds; accept its approvals."""
    global buf, lines
    end = time.time() + seconds
    while time.time() < end:
        if not lines:
            ready, _, _ = select.select([p.stdout], [], [], 0.3)
            if not ready:
                continue
            chunk = os.read(p.stdout.fileno(), 1 << 16)
            if not chunk:
                return None
            buf += chunk
            *full, buf = buf.split(b"\n")
            lines += [l for l in full if l.strip()]
            continue
        msg = json.loads(lines.pop(0))
        log.write(json.dumps({"dir": "out", "msg": msg}) + "\n"); log.flush()
        if approve and "id" in msg and msg.get("method", "").endswith("requestApproval"):
            send({"id": msg["id"], "result": {"decision": "accept"}})
        if until and until(msg):
            return msg
    return None


def call(method, params=None, seconds=60):
    i = request(method, params)
    return pump(lambda m: m.get("id") == i and "method" not in m, seconds)


def turn_done(msg):
    return msg.get("method") == "turn/completed" and msg["params"]["threadId"] == thread


call("initialize", {"clientInfo": {"name": "agent-native", "title": "agent-native", "version": "0.2.0"},
                    "capabilities": {"experimentalApi": True}})
send({"method": "initialized"})
thread = call("thread/start", {"cwd": sandbox, "model": "gpt-6-luna", "approvalPolicy": "untrusted",
                               "sandbox": "workspace-write"})["result"]["thread"]["id"]
call("model/list", {"limit": 100})
call("skills/list", {"cwds": [sandbox]})

request("turn/start", {"threadId": thread, "effort": "low", "input": [{"type": "text", "text":
    "Run `echo hello` in the shell. Then use the apply_patch tool (not the shell) to create notes.txt "
    "containing the line hi. Then use apply_patch to change hi to hello. Then reply with one short sentence."}]})
done = pump(turn_done, 240)
call("thread/prediction/request", {"threadId": thread, "sourceTurnId": done["params"]["turn"]["id"]}, 30)
pump(lambda m: m.get("method") == "thread/prediction/updated", 20)

request("turn/start", {"threadId": thread, "input": [{"type": "text", "text":
    "Run `sleep 30` in the shell, then say done."}]})
started = pump(lambda m: m.get("method") == "item/started"
               and m["params"]["item"].get("type") == "commandExecution", 120)
pump(seconds=2)
call("turn/interrupt", {"threadId": thread, "turnId": started["params"]["turnId"]})
pump(turn_done, 60)

call("account/rateLimits/read")
call("account/usage/read")
p.stdin.close()
p.wait(timeout=10)
