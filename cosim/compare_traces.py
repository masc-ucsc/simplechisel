#!/usr/bin/env python3
import argparse
from datetime import datetime, timezone
import json
import re
import sys


SPIKE_RE = re.compile(
    r"core\s+\d+:\s+\d+\s+"
    r"(?P<pc>0x[0-9a-fA-F]+)\s+"
    r"\((?P<instr>0x[0-9a-fA-F]+)\)"
    r"(?:\s+x(?P<reg>\d+)\s+(?P<value>0x[0-9a-fA-F]+))?"
)


def parse_u64(value):
    return int(value, 16) & ((1 << 64) - 1)


def parse_dino_trace(path):
    entries = []
    with open(path, "r", encoding="utf-8") as f:
        for line_no, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
            fields = {}
            for part in line.split():
                if "=" not in part:
                    raise ValueError(f"{path}:{line_no}: bad token {part!r}")
                key, value = part.split("=", 1)
                fields[key] = value
            regs = [0] * 32
            for i in range(32):
                regs[i] = parse_u64(fields[f"x{i}"])
            entries.append(
                {
                    "step": int(fields["step"], 0),
                    "pc": parse_u64(fields["pc"]),
                    "instr": int(fields["instr"], 16) & 0xffffffff,
                    "regs": regs,
                }
            )
    return entries


def parse_spike_log(path):
    entries = []
    regs = [0] * 32
    with open(path, "r", encoding="utf-8") as f:
        for line_no, line in enumerate(f, 1):
            match = SPIKE_RE.search(line)
            if not match:
                if line.strip():
                    raise ValueError(f"{path}:{line_no}: unrecognized Spike log line: {line.rstrip()}")
                continue
            reg = match.group("reg")
            if reg is not None:
                reg_idx = int(reg)
                if reg_idx != 0:
                    regs[reg_idx] = parse_u64(match.group("value"))
            regs[0] = 0
            entries.append(
                {
                    "step": len(entries),
                    "pc": parse_u64(match.group("pc")),
                    "instr": int(match.group("instr"), 16) & 0xffffffff,
                    "regs": list(regs),
                }
            )
    return entries


def fmt64(value):
    return f"0x{value & ((1 << 64) - 1):016x}"


def signed32(value):
    value &= 0xFFFFFFFF
    return value - 0x100000000 if value & 0x80000000 else value


def hex32(value):
    return f"0x{value & 0xFFFFFFFF:08X}"


def sign_extend(value, bits):
    sign = 1 << (bits - 1)
    return (value & (sign - 1)) - (value & sign)


def reg_name(idx):
    return None if idx is None else f"x{idx}"


def decode_instruction(instr):
    opcode = instr & 0x7F
    rd = (instr >> 7) & 0x1F
    funct3 = (instr >> 12) & 0x7
    rs1 = (instr >> 15) & 0x1F
    rs2 = (instr >> 20) & 0x1F
    funct7 = (instr >> 25) & 0x7F

    r_names = {
        (0x00, 0x0): "ADD", (0x20, 0x0): "SUB", (0x00, 0x1): "SLL",
        (0x00, 0x2): "SLT", (0x00, 0x3): "SLTU", (0x00, 0x4): "XOR",
        (0x00, 0x5): "SRL", (0x20, 0x5): "SRA", (0x00, 0x6): "OR",
        (0x00, 0x7): "AND",
    }
    i_names = {
        0x0: "ADDI", 0x2: "SLTI", 0x3: "SLTIU", 0x4: "XORI",
        0x6: "ORI", 0x7: "ANDI",
    }
    load_names = {0x0: "LB", 0x1: "LH", 0x2: "LW", 0x3: "LD", 0x4: "LBU", 0x5: "LHU", 0x6: "LWU"}
    store_names = {0x0: "SB", 0x1: "SH", 0x2: "SW", 0x3: "SD"}
    branch_names = {0x0: "BEQ", 0x1: "BNE", 0x4: "BLT", 0x5: "BGE", 0x6: "BLTU", 0x7: "BGEU"}

    decoded = {"mnemonic": "UNKNOWN", "rd": None, "rs1": None, "rs2": None, "imm": None, "rd_idx": None}

    if opcode in (0x33, 0x3B):
        decoded.update({
            "mnemonic": r_names.get((funct7, funct3), "UNKNOWN"),
            "rd": reg_name(rd),
            "rs1": reg_name(rs1),
            "rs2": reg_name(rs2),
            "rd_idx": rd,
        })
        if opcode == 0x3B and decoded["mnemonic"] != "UNKNOWN":
            decoded["mnemonic"] += "W"
    elif opcode in (0x13, 0x1B):
        if funct3 == 0x1:
            mnemonic = "SLLI"
            imm = (instr >> 20) & 0x3F
        elif funct3 == 0x5:
            mnemonic = "SRAI" if funct7 == 0x20 else "SRLI"
            imm = (instr >> 20) & 0x3F
        else:
            mnemonic = i_names.get(funct3, "UNKNOWN")
            imm = sign_extend((instr >> 20) & 0xFFF, 12)
        if opcode == 0x1B and mnemonic != "UNKNOWN":
            mnemonic += "W"
        decoded.update({"mnemonic": mnemonic, "rd": reg_name(rd), "rs1": reg_name(rs1), "imm": imm, "rd_idx": rd})
    elif opcode == 0x03:
        imm = sign_extend((instr >> 20) & 0xFFF, 12)
        decoded.update({
            "mnemonic": load_names.get(funct3, "UNKNOWN"),
            "rd": reg_name(rd),
            "rs1": reg_name(rs1),
            "imm": imm,
            "rd_idx": rd,
        })
    elif opcode == 0x23:
        imm = ((instr >> 7) & 0x1F) | (((instr >> 25) & 0x7F) << 5)
        decoded.update({
            "mnemonic": store_names.get(funct3, "UNKNOWN"),
            "rs1": reg_name(rs1),
            "rs2": reg_name(rs2),
            "imm": sign_extend(imm, 12),
        })
    elif opcode == 0x63:
        imm = (((instr >> 31) & 0x1) << 12) | (((instr >> 7) & 0x1) << 11) | (((instr >> 25) & 0x3F) << 5) | (((instr >> 8) & 0xF) << 1)
        decoded.update({
            "mnemonic": branch_names.get(funct3, "UNKNOWN"),
            "rs1": reg_name(rs1),
            "rs2": reg_name(rs2),
            "imm": sign_extend(imm, 13),
        })
    elif opcode == 0x37:
        decoded.update({"mnemonic": "LUI", "rd": reg_name(rd), "imm": signed32(instr & 0xFFFFF000), "rd_idx": rd})
    elif opcode == 0x17:
        decoded.update({"mnemonic": "AUIPC", "rd": reg_name(rd), "imm": signed32(instr & 0xFFFFF000), "rd_idx": rd})
    elif opcode == 0x6F:
        imm = (((instr >> 31) & 0x1) << 20) | (((instr >> 12) & 0xFF) << 12) | (((instr >> 20) & 0x1) << 11) | (((instr >> 21) & 0x3FF) << 1)
        decoded.update({"mnemonic": "JAL", "rd": reg_name(rd), "imm": sign_extend(imm, 21), "rd_idx": rd})
    elif opcode == 0x67:
        imm = sign_extend((instr >> 20) & 0xFFF, 12)
        decoded.update({"mnemonic": "JALR", "rd": reg_name(rd), "rs1": reg_name(rs1), "imm": imm, "rd_idx": rd})

    return decoded


def write_value(entry, decoded):
    rd_idx = decoded.get("rd_idx")
    if rd_idx is None or rd_idx == 0:
        return None
    return signed32(entry["regs"][rd_idx])


def compare(dino, spike):
    if len(dino) != len(spike):
        return (
            False,
            f"trace length mismatch: DINO has {len(dino)} entries, Spike has {len(spike)} entries",
        )

    for idx, (d, s) in enumerate(zip(dino, spike)):
        problems = []
        if d["pc"] != s["pc"]:
            problems.append(f"pc DINO={fmt64(d['pc'])} Spike={fmt64(s['pc'])}")
        if d["instr"] != s["instr"]:
            problems.append(f"instr DINO=0x{d['instr']:08x} Spike=0x{s['instr']:08x}")
        reg_problems = []
        for reg_idx, (dv, sv) in enumerate(zip(d["regs"], s["regs"])):
            if dv != sv:
                reg_problems.append(f"x{reg_idx}: DINO={fmt64(dv)} Spike={fmt64(sv)}")
        if reg_problems:
            problems.append("regs " + ", ".join(reg_problems[:8]))
            if len(reg_problems) > 8:
                problems.append(f"... {len(reg_problems) - 8} more register differences")
        if problems:
            return False, f"MISMATCH at step {idx}: " + "; ".join(problems)

    return True, f"PASS {len(dino)} lockstep instructions"


def make_instruction_row(idx, dino_entry, spike_entry):
    decoded = decode_instruction(dino_entry["instr"])
    registers = {"rd": decoded["rd"], "rs1": decoded["rs1"], "rs2": decoded["rs2"]}
    mismatch = None

    if dino_entry["pc"] != spike_entry["pc"]:
        mismatch = {
            "register": "PC",
            "expected": signed32(spike_entry["pc"]),
            "got": signed32(dino_entry["pc"]),
            "hexExpected": hex32(spike_entry["pc"]),
            "hexGot": hex32(dino_entry["pc"]),
        }
    elif dino_entry["instr"] != spike_entry["instr"]:
        mismatch = {
            "register": "instruction",
            "expected": signed32(spike_entry["instr"]),
            "got": signed32(dino_entry["instr"]),
            "hexExpected": hex32(spike_entry["instr"]),
            "hexGot": hex32(dino_entry["instr"]),
        }
    else:
        for reg_idx, (dv, sv) in enumerate(zip(dino_entry["regs"], spike_entry["regs"])):
            if dv != sv:
                mismatch = {
                    "register": f"x{reg_idx}",
                    "expected": signed32(sv),
                    "got": signed32(dv),
                    "hexExpected": hex32(sv),
                    "hexGot": hex32(dv),
                }
                break

    passed = mismatch is None
    if passed:
        golden_value = write_value(spike_entry, decoded)
        dut_value = write_value(dino_entry, decoded)
    elif mismatch["register"] == "PC":
        golden_value = None
        dut_value = None
    else:
        golden_value = mismatch["expected"]
        dut_value = mismatch["got"]

    return {
        "instrNumber": idx + 1,
        "mnemonic": decoded["mnemonic"],
        "passed": passed,
        "registers": registers,
        "imm": decoded["imm"],
        "goldenValue": golden_value,
        "dutValue": dut_value,
        "mismatch": mismatch,
    }


def make_test_result(test_name, dino, spike):
    instructions = []
    first_mismatch = None

    for idx, (dino_entry, spike_entry) in enumerate(zip(dino, spike)):
        row = make_instruction_row(idx, dino_entry, spike_entry)
        instructions.append(row)
        if not row["passed"]:
            first_mismatch = row["instrNumber"]
            break

    if first_mismatch is None and len(dino) != len(spike):
        first_mismatch = len(instructions) + 1
        instructions.append({
            "instrNumber": first_mismatch,
            "mnemonic": "TRACE_LENGTH",
            "passed": False,
            "registers": {"rd": None, "rs1": None, "rs2": None},
            "imm": None,
            "goldenValue": len(spike),
            "dutValue": len(dino),
            "mismatch": {
                "register": "trace_length",
                "expected": len(spike),
                "got": len(dino),
                "hexExpected": hex32(len(spike)),
                "hexGot": hex32(len(dino)),
            },
        })

    return {
        "testName": test_name,
        "passed": first_mismatch is None,
        "totalInstructions": len(instructions),
        "firstMismatchInstr": first_mismatch,
        "instructions": instructions,
    }


def make_run_result(test_name, dino, spike, run_id, timestamp, rtl_dir, mutation_label):
    test = make_test_result(test_name, dino, spike)
    passed = 1 if test["passed"] else 0
    return {
        "runId": run_id,
        "timestamp": timestamp,
        "rtlDir": rtl_dir,
        "mutationLabel": mutation_label,
        "totalTests": 1,
        "passed": passed,
        "failed": 1 - passed,
        "tests": [test],
    }


def main(argv):
    parser = argparse.ArgumentParser(description="Compare DINO and Spike traces")
    parser.add_argument("dino_trace")
    parser.add_argument("spike_log")
    parser.add_argument("--test-name", default="unknown")
    parser.add_argument("--json-out")
    parser.add_argument("--run-id")
    parser.add_argument("--rtl-dir", default="unknown")
    parser.add_argument("--mutation-label")
    args = parser.parse_args(argv[1:])

    dino = parse_dino_trace(args.dino_trace)
    spike = parse_spike_log(args.spike_log)
    ok, message = compare(dino, spike)
    if args.json_out:
        timestamp = datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")
        run_id = args.run_id or f"run_{args.test_name}_{timestamp.replace('-', '').replace(':', '')}"
        result = make_run_result(args.test_name, dino, spike, run_id, timestamp, args.rtl_dir, args.mutation_label)
        with open(args.json_out, "w", encoding="utf-8") as f:
            json.dump(result, f, indent=2)
            f.write("\n")
    print(message)
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
