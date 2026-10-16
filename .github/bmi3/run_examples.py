"""Run the MODFLOW 6 example simulations through modflowapi with BMI 3.

modflowapi negotiates the 'xmi' and 'bmi:pointers' extensions, so libmf6
terminates a run if a stable pointer is reallocated after initialize. Fails
if any run hits that guard or if a simulation fails that is not in KNOWN.

Usage: python run_examples.py EXAMPLES_DIR LIBMF6
"""

import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

# failures that do not come from BMI 3; they fail the same way with the
# upstream libmf6, xmipy 1.5, and modflowapi develop
KNOWN = {
    # modflowapi datamodel.py lists SFR "hk", which IDM removed from SFR
    "ex-gwf-drn-p01a",
    "ex-gwf-drn-p01b",
    "ex-gwf-lak-p02",
    "ex-gwf-lgr",
    "ex-gwf-sagehen",
    "ex-gwf-sfr-p01",
    "ex-gwf-sfr-p01b",
    "ex-gwf-sfr-pindersauera",
    "ex-gwf-sfr-pindersauerb",
    "ex-gwt-prudic2004t2/mf6gwf",
    "ex-gwt-prudic2004t2/mf6gwt",
    "ex-gwt-synthetic-valley/mf6gwf",
    "ex-gwt-synthetic-valley/mf6gwt",
    # need heads that the example script writes
    "ex-prt-mp7-p02/prt",
    "ex-prt-mp7-p04/prt",
    # inconsistent ANGLDEGX in the DISU input
    "ex-gwf-rad-disu",
    # does not converge
    "ex-gwt-adv-schemes/gwt_voronoi_square-wave_central",
}

CHILD = """
import sys
from modflowapi import run_simulation
run_simulation(sys.argv[1], sys.argv[2], lambda sim, step: None)
"""


def run(sim_dir, libmf6):
    """Run one simulation in its own process; libmf6 holds one at a time."""
    try:
        proc = subprocess.run(
            [sys.executable, "-I", "-c", CHILD, libmf6, str(sim_dir)],
            cwd=sim_dir,
            capture_output=True,
            text=True,
            timeout=1800,
        )
        out = proc.stdout + proc.stderr
        ok = proc.returncode == 0 and "Normal termination" in out
    except subprocess.TimeoutExpired:
        ok, out = False, "timeout"
    lst = sim_dir / "mfsim.lst"
    guard = lst.exists() and "stable BMI pointer" in lst.read_text(errors="ignore")
    return sim_dir, ok, guard, out.strip().splitlines()[-3:]


def main():
    root, libmf6 = Path(sys.argv[1]).resolve(), str(Path(sys.argv[2]).resolve())
    sims = sorted({p.parent for p in root.rglob("mfsim.nam")})

    # an example's transport and tracking models read its flow model's
    # output, so run each example's simulations in order, flow first
    def order(sim):
        name = sim.name.lower()
        return (0 if "gwf" in name or "flow" in name else 1, name)

    examples = {}
    for sim in sims:
        examples.setdefault(sim.relative_to(root).parts[0], []).append(sim)

    def run_example(group):
        return [run(d, libmf6) for d in sorted(group, key=order)]

    with ThreadPoolExecutor(max_workers=4) as pool:
        results = [r for rs in pool.map(run_example, examples.values()) for r in rs]

    bad = []
    for sim_dir, ok, guard, tail in results:
        name = sim_dir.relative_to(root).as_posix()
        if guard:
            bad.append(f"stable pointer reallocated: {name}")
        elif not ok and name not in KNOWN:
            bad.append(f"failed: {name}: {' | '.join(tail)}")
    nok = sum(ok for _, ok, _, _ in results)
    print(f"{nok}/{len(results)} ran to normal termination")
    for line in bad:
        print(line)
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    main()
