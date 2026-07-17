#!/usr/bin/env python3
import argparse
from datetime import datetime, timezone
import json


def main():
    parser = argparse.ArgumentParser(description="Merge per-test cosim JSON files into one campaign JSON")
    parser.add_argument("--out", required=True)
    parser.add_argument("--run-id", required=True)
    parser.add_argument("--rtl-dir", required=True)
    parser.add_argument("--mutation-label")
    parser.add_argument("result_jsons", nargs="+")
    args = parser.parse_args()

    tests = []
    for path in args.result_jsons:
        with open(path, "r", encoding="utf-8") as f:
            data = json.load(f)
        tests.extend(data["tests"])

    passed = sum(1 for test in tests if test["passed"])
    failed = len(tests) - passed
    timestamp = datetime.now(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")
    merged = {
        "runId": args.run_id,
        "timestamp": timestamp,
        "rtlDir": args.rtl_dir,
        "mutationLabel": args.mutation_label,
        "totalTests": len(tests),
        "passed": passed,
        "failed": failed,
        "tests": tests,
    }

    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(merged, f, indent=2)
        f.write("\n")


if __name__ == "__main__":
    raise SystemExit(main())
