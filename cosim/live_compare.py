#!/usr/bin/env python3
import argparse
# We do not import Spike as a Python package. We launch the spike executable
# as a child process and read its commit-log output as text.
import subprocess
import sys

# Reuse the same Spike commit-log parser pattern as the offline comparator.
from compare_traces import SPIKE_RE, fmt64, parse_u64


def parse_dino_line(line):
    # DINO prints one complete trace line per simulated instruction:
    # step=... pc=... instr=... x0=... x1=... through x31=...
    fields = {}
    for part in line.strip().split():
        if "=" not in part:
            raise ValueError(f"bad DINO token {part!r}")
        key, value = part.split("=", 1)
        fields[key] = value

    regs = [0] * 32
    for i in range(32):
        regs[i] = parse_u64(fields[f"x{i}"])
    return {
        "step": int(fields["step"], 0),
        "pc": parse_u64(fields["pc"]),
        "instr": int(fields["instr"], 16) & 0xFFFFFFFF,
        "regs": regs,
    }


def make_spike_parser():
    # Spike's commit log usually reports only the register written by the
    # current instruction, so we reconstruct the full x0..x31 register file
    # by carrying previous values forward.
    regs = [0] * 32

    def parse(line):
        # Example Spike line:
        # core 0: 3 0x80000000 (0x01400293) x5 0x0000000000000014
        match = SPIKE_RE.search(line)
        if not match:
            raise ValueError(f"unrecognized Spike commit line: {line.rstrip()}")
        reg = match.group("reg")
        if reg is not None:
            reg_idx = int(reg)
            if reg_idx != 0:
                regs[reg_idx] = parse_u64(match.group("value"))
        regs[0] = 0
        return {
            "pc": parse_u64(match.group("pc")),
            "instr": int(match.group("instr"), 16) & 0xFFFFFFFF,
            "regs": list(regs),
        }

    return parse


def mismatch_message(step, dino, spike):
    # Compare architectural state after one instruction: PC, instruction word,
    # and all 32 integer registers.
    problems = []
    if dino["pc"] != spike["pc"]:
        problems.append(f"pc DINO={fmt64(dino['pc'])} Spike={fmt64(spike['pc'])}")
    if dino["instr"] != spike["instr"]:
        problems.append(f"instr DINO=0x{dino['instr']:08x} Spike=0x{spike['instr']:08x}")

    reg_problems = []
    for reg_idx, (dv, sv) in enumerate(zip(dino["regs"], spike["regs"])):
        if dv != sv:
            reg_problems.append(f"x{reg_idx}: DINO={fmt64(dv)} Spike={fmt64(sv)}")
    if reg_problems:
        problems.append("regs " + ", ".join(reg_problems[:8]))
        if len(reg_problems) > 8:
            problems.append(f"... {len(reg_problems) - 8} more register differences")

    if not problems:
        return None
    return f"MISMATCH at step {step}: " + "; ".join(problems)


def terminate(proc):
    # Stop the still-running child simulator when the live comparator finishes
    # early, especially after the first mismatch.
    if proc.poll() is None:
        proc.terminate()
        try:
            proc.wait(timeout=2)
        except subprocess.TimeoutExpired:
            proc.kill()
            proc.wait()


def main():
    parser = argparse.ArgumentParser(
        description="Streaming Spike-vs-DINO trace comparator, not paused single-step lockstep"
    )
    parser.add_argument("--dino-exe", required=True)
    parser.add_argument("--imem", required=True)
    parser.add_argument("--elf", required=True)
    parser.add_argument("--steps", required=True, type=int)
    parser.add_argument("--base", required=True)
    parser.add_argument("--spike-mem", required=True)
    parser.add_argument("--spike-stderr", required=True)
    parser.add_argument("--dino-stderr", required=True)
    args = parser.parse_args()

    # DINO is our Verilator-built executable. It prints one trace line per step.
    dino_cmd = [args.dino_exe, args.imem, str(args.steps), args.base]

    # Spike is not imported. This command starts the external spike program.
    # --log-commits enables per-instruction commit logging.
    # --log=/dev/stdout sends that log to stdout so Python can read it live.
    spike_cmd = [
        "spike",
        "--isa=rv64i",
        f"-m{args.spike_mem}",
        f"--pc={args.base}",
        "--log-commits",
        f"--instructions={args.steps}",
        "--log=/dev/stdout",
        args.elf,
    ]

    with open(args.dino_stderr, "w", encoding="utf-8") as dino_err, open(
        args.spike_stderr, "w", encoding="utf-8"
    ) as spike_err:
        # Start both simulators at the same time. stdout=subprocess.PIPE gives
        # this Python script a live stream from each process.
        dino = subprocess.Popen(
            dino_cmd,
            stdout=subprocess.PIPE,
            stderr=dino_err,
            text=True,
            bufsize=1,
        )
        spike = subprocess.Popen(
            spike_cmd,
            stdout=subprocess.PIPE,
            stderr=spike_err,
            text=True,
            bufsize=1,
        )

        parse_spike = make_spike_parser()
        try:
            for step in range(args.steps):
                # Read exactly one committed instruction from each side, then
                # compare immediately before moving to the next instruction.
                dino_line = dino.stdout.readline()
                spike_line = spike.stdout.readline()
                if not dino_line:
                    print(f"MISMATCH at step {step}: DINO ended before producing a trace line")
                    return 1
                if not spike_line:
                    print(f"MISMATCH at step {step}: Spike ended before producing a commit line")
                    return 1

                dino_entry = parse_dino_line(dino_line)
                spike_entry = parse_spike(spike_line)
                message = mismatch_message(step, dino_entry, spike_entry)
                if message:
                    print(message)
                    return 1
                print(
                    f"STREAM PASS step {step}: pc={fmt64(dino_entry['pc'])} "
                    f"instr=0x{dino_entry['instr']:08x}"
                )
        finally:
            terminate(dino)
            terminate(spike)

    dino_rc = dino.wait()
    spike_rc = spike.wait()
    if dino_rc != 0:
        print(f"DINO exited with status {dino_rc}")
        return dino_rc
    if spike_rc != 0:
        print(f"Spike exited with status {spike_rc}")
        return spike_rc

    print(f"PASS {args.steps} streaming-compared instructions")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
