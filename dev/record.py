#!/usr/bin/env python3
"""Records a scripted Claude Code stream-json session, for building and testing
the Emacs client against real messages. Usage: record.py OUT.jsonl SANDBOX_DIR

Each line of OUT is {"dir": "in"|"out", "msg": ...}: what was sent to claude
and what it wrote back, in order."""
import json, subprocess, sys, time, select, itertools

out_path, sandbox = sys.argv[1], sys.argv[2]
cmd = ["claude", "-p", "--input-format", "stream-json", "--output-format", "stream-json",
       "--verbose", "--include-partial-messages", "--permission-prompt-tool", "stdio",
       "--model", "haiku", "--permission-mode", "default"]
p = subprocess.Popen(cmd, cwd=sandbox, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                     stderr=subprocess.PIPE, text=True, bufsize=1)
log = open(out_path, "w")
ids = itertools.count(1)

def send(o):
    log.write(json.dumps({"dir": "in", "msg": o}) + "\n"); log.flush()
    p.stdin.write(json.dumps(o) + "\n"); p.stdin.flush()

def control(subtype, **kw):
    send({"type": "control_request", "request_id": f"req_{next(ids)}", "request": {"subtype": subtype, **kw}})

def user(text):
    send({"type": "user", "message": {"role": "user", "content": text}})

denied = {"Bash": 1}  # deny the first Bash call, allow the rest

def pump(until_result=True, seconds=90, on_line=None):
    end = time.time() + seconds
    while time.time() < end:
        r, _, _ = select.select([p.stdout], [], [], 0.5)
        if not r:
            continue
        line = p.stdout.readline()
        if not line:
            return
        o = json.loads(line)
        log.write(json.dumps({"dir": "out", "msg": o}) + "\n"); log.flush()
        if o.get("type") == "control_request" and o["request"].get("subtype") == "can_use_tool":
            tool = o["request"].get("tool_name")
            if denied.get(tool, 0) > 0:
                denied[tool] -= 1
                resp = {"behavior": "deny", "message": "The user denied this; try again and it will be allowed."}
            else:
                resp = {"behavior": "allow", "updatedInput": o["request"].get("input", {})}
            send({"type": "control_response", "response": {"subtype": "success", "request_id": o["request_id"], "response": resp}})
        if on_line:
            on_line(o)
        if until_result and o.get("type") == "result":
            return

control("initialize")
pump(until_result=False, seconds=4)
user("Use the Write tool to create notes.txt containing 'hi'. Then use Edit to change 'hi' to 'hello'. "
     "Then run `cat notes.txt` with Bash (if it is denied, try once more). Then reply with one short sentence.")
pump()
control("set_permission_mode", mode="acceptEdits")
pump(until_result=False, seconds=2)
user("Think step by step, then write a 300 word story about a lighthouse.")
started = time.time()
def interrupt_once(o, state={"sent": False}):
    if not state["sent"] and o.get("type") == "stream_event" and time.time() - started > 4:
        state["sent"] = True
        control("interrupt")
pump(on_line=interrupt_once)
user("/cost")
pump(seconds=30)
p.stdin.close(); p.wait(timeout=20)
print("stderr:", p.stderr.read()[:400])
