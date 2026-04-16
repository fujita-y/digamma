#!/usr/bin/env python3
"""
Scheme Implementation Benchmark Comparator

Runs `make nanos`, `make guile`, and `make ypsilon` multiple times,
parses user time from console output, averages results, and generates
an HTML report with bar charts for each benchmark test.
"""

import subprocess
import re
import os
import sys
import time
from collections import defaultdict

# --- Configuration ---
IMPLEMENTATIONS = {
    "nanos":   {"command": ["make", "nanos"],   "label": "Nanos"},
    "guile":   {"command": ["make", "guile"],   "label": "Guile"},
    "ypsilon": {"command": ["make", "ypsilon"], "label": "Ypsilon"},
}

NUM_RUNS = 1
BENCH_DIR = os.path.dirname(os.path.abspath(__file__))
OUTPUT_FILE = os.path.join(BENCH_DIR, "benchmark_report.html")

# Benchmark categories
CATEGORIES = {
    "GABRIEL": [
        "browse", "cpstak", "dderiv", "deriv", "destruc",
        "diviter", "divrec", "tak", "takl", "triangl",
    ],
    "ARITHMETIC": [
        "fft", "fib", "fibfp", "mbrot", "nucleic",
        "pnpoly", "sum", "sumfp",
    ],
    "MISCELLANEOUS": [
        "ack", "boyer", "nboyer", "conform", "earley",
        "graphs", "mazefun", "nqueens", "paraffins", "peval",
        "ray", "scheme",
    ],
}


def parse_output(text):
    """
    Parse console output to extract benchmark names and their real/user times.

    Expected output format per benchmark:
        ;;  browse  (x600)
        ;;  0.123456 real    0.234567 user    0.012345 sys
        ;;  ----------------------------------------------------------------

    Returns dict: {bench_name: {"real": float, "user": float}}
    """
    results = {}
    current_bench = None

    for line in text.splitlines():
        # Match benchmark header: ;;  browse  (x600)
        m = re.match(r'^;;\s+(\w+)\s+\(x\d+\)', line)
        if m:
            current_bench = m.group(1)
            continue

        # Match timing line: ;;  0.123 real    0.234 user    0.012 sys
        m = re.match(r'^;;\s*([\d.]+)\s+real\s+([\d.]+)\s+user\s+([\d.]+)\s+sys', line)
        if m and current_bench:
            real_time = float(m.group(1))
            user_time = float(m.group(2))
            results[current_bench] = {"real": real_time, "user": user_time}
            current_bench = None

    return results


def run_benchmark(impl_key):
    """Run a single benchmark implementation and return parsed results."""
    impl = IMPLEMENTATIONS[impl_key]
    cmd = impl["command"]
    label = impl["label"]

    print(f"  Running: {' '.join(cmd)} ...", end=" ", flush=True)
    try:
        proc = subprocess.run(
            cmd,
            cwd=BENCH_DIR,
            capture_output=True,
            text=True,
            timeout=600,
        )
        output = proc.stdout + proc.stderr
        results = parse_output(output)
        print(f"OK ({len(results)} benchmarks)")
        return results
    except subprocess.TimeoutExpired:
        print("TIMEOUT")
        return {}
    except Exception as e:
        print(f"ERROR: {e}")
        return {}


def collect_data():
    """Run all implementations NUM_RUNS times and compute averages."""
    # all_runs[impl][bench][metric] = [list of times]
    all_runs = {k: defaultdict(lambda: defaultdict(list)) for k in IMPLEMENTATIONS}

    for run_idx in range(1, NUM_RUNS + 1):
        print(f"\n{'='*60}")
        print(f"  Run {run_idx} of {NUM_RUNS}")
        print(f"{'='*60}")
        for impl_key in IMPLEMENTATIONS:
            results = run_benchmark(impl_key)
            for bench_name, timing in results.items():
                all_runs[impl_key][bench_name]["real"].append(timing["real"])
                all_runs[impl_key][bench_name]["user"].append(timing["user"])

    # Compute averages: averages[impl][bench] = {"real": avg, "user": avg}
    averages = {}
    for impl_key in IMPLEMENTATIONS:
        averages[impl_key] = {}
        for bench_name, metrics in all_runs[impl_key].items():
            averages[impl_key][bench_name] = {
                "real": sum(metrics["real"]) / len(metrics["real"]),
                "user": sum(metrics["user"]) / len(metrics["user"]),
            }

    return averages


def get_all_benchmarks(averages):
    """Get the union of all benchmark names across implementations."""
    all_benchmarks = set()
    for impl_data in averages.values():
        all_benchmarks.update(impl_data.keys())
    return sorted(all_benchmarks)


def generate_html(averages):
    """Generate an HTML report with bar charts for each benchmark test."""

    impl_labels = [IMPLEMENTATIONS[k]["label"] for k in IMPLEMENTATIONS]
    impl_keys = list(IMPLEMENTATIONS.keys())

    # Colors for each implementation (vibrant, distinguishable)
    colors = {
        impl_keys[0]: {"bg": "rgba(99, 102, 241, 0.85)",  "border": "rgba(99, 102, 241, 1)"},   # Indigo
        impl_keys[1]: {"bg": "rgba(16, 185, 129, 0.85)",  "border": "rgba(16, 185, 129, 1)"},   # Emerald
        impl_keys[2]: {"bg": "rgba(245, 158, 11, 0.85)",  "border": "rgba(245, 158, 11, 1)"},   # Amber
    }

    all_benchmarks = get_all_benchmarks(averages)

    # Build per-benchmark chart data
    chart_blocks = []
    chart_id = 0
    for cat_name, cat_benchmarks in CATEGORIES.items():
        # Filter to benchmarks that actually have data
        active = [b for b in cat_benchmarks if b in all_benchmarks]
        if not active:
            continue

        cards_html = ""
        for bench in active:
            chart_id += 1
            cid = f"chart{chart_id}"

            labels_js = ", ".join(
                f'"{IMPLEMENTATIONS[k]["label"]}"' for k in impl_keys
            )
            real_values_js = ", ".join(
                str(round(averages.get(k, {}).get(bench, {}).get("real", 0), 6)) for k in impl_keys
            )
            user_values_js = ", ".join(
                str(round(averages.get(k, {}).get(bench, {}).get("user", 0), 6)) for k in impl_keys
            )
            bg_js = ", ".join(f'"{colors[k]["bg"]}"' for k in impl_keys)
            border_js = ", ".join(f'"{colors[k]["border"]}"' for k in impl_keys)

            # Build data table rows
            table_rows = ""
            for k in impl_keys:
                timing = averages.get(k, {}).get(bench, None)
                if timing is not None:
                    real_str = f"{timing['real']:.6f}"
                    user_str = f"{timing['user']:.6f}"
                else:
                    real_str = "N/A"
                    user_str = "N/A"
                table_rows += f"""
                    <tr>
                        <td style="padding:4px 12px;color:#cbd5e1;">{IMPLEMENTATIONS[k]["label"]}</td>
                        <td style="padding:4px 12px;color:#e2e8f0;font-family:'JetBrains Mono',monospace;text-align:right;">{real_str}s</td>
                        <td style="padding:4px 12px;color:#e2e8f0;font-family:'JetBrains Mono',monospace;text-align:right;">{user_str}s</td>
                    </tr>"""

            cards_html += f"""
            <div class="bench-card">
                <h3 class="bench-title">{bench}</h3>
                <div class="chart-wrap">
                    <canvas id="{cid}"></canvas>
                </div>
                <table class="data-table">
                    <tr>
                        <th style="padding:4px 12px;color:#94a3b8;text-align:left;font-weight:500;"></th>
                        <th style="padding:4px 12px;color:#94a3b8;text-align:right;font-weight:500;font-size:0.8rem;">real</th>
                        <th style="padding:4px 12px;color:#94a3b8;text-align:right;font-weight:500;font-size:0.8rem;">user</th>
                    </tr>{table_rows}
                </table>
                <script>
                    createBarChart("{cid}",
                        [{labels_js}],
                        [{real_values_js}],
                        [{user_values_js}],
                        [{bg_js}],
                        [{border_js}]
                    );
                </script>
            </div>"""

        chart_blocks.append(f"""
        <section class="category-section">
            <h2 class="category-title">{cat_name}</h2>
            <div class="bench-grid">{cards_html}
            </div>
        </section>""")

    timestamp = time.strftime("%Y-%m-%d %H:%M:%S")

    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Scheme Benchmark Results</title>
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap" rel="stylesheet">
    <script src="https://cdn.jsdelivr.net/npm/chart.js@4"></script>
    <style>
        *,*::before,*::after {{ box-sizing:border-box; margin:0; padding:0; }}

        body {{
            font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
            background: #0f172a;
            color: #e2e8f0;
            min-height: 100vh;
        }}

        .page-header {{
            background: linear-gradient(135deg, #1e1b4b 0%, #312e81 50%, #1e3a5f 100%);
            padding: 48px 32px 40px;
            text-align: center;
            border-bottom: 1px solid rgba(99, 102, 241, 0.3);
            position: relative;
            overflow: hidden;
        }}
        .page-header::before {{
            content: '';
            position: absolute;
            inset: 0;
            background: radial-gradient(ellipse at 30% 20%, rgba(99,102,241,0.15) 0%, transparent 60%),
                        radial-gradient(ellipse at 70% 80%, rgba(16,185,129,0.1) 0%, transparent 60%);
            pointer-events: none;
        }}
        .page-header h1 {{
            font-size: 2.25rem;
            font-weight: 700;
            letter-spacing: -0.02em;
            background: linear-gradient(135deg, #c7d2fe, #818cf8, #6ee7b7);
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            background-clip: text;
            position: relative;
        }}
        .page-header .subtitle {{
            margin-top: 8px;
            color: #94a3b8;
            font-size: 0.95rem;
            font-weight: 400;
            position: relative;
        }}

        .legend-bar {{
            display: flex;
            justify-content: center;
            gap: 32px;
            padding: 20px 32px;
            background: #1e293b;
            border-bottom: 1px solid #334155;
        }}
        .legend-item {{
            display: flex;
            align-items: center;
            gap: 8px;
            font-size: 0.9rem;
            font-weight: 500;
            color: #cbd5e1;
        }}
        .legend-swatch {{
            width: 14px;
            height: 14px;
            border-radius: 3px;
        }}

        .container {{
            max-width: 1400px;
            margin: 0 auto;
            padding: 32px 24px 64px;
        }}

        .category-section {{
            margin-bottom: 48px;
        }}
        .category-title {{
            font-size: 1.35rem;
            font-weight: 600;
            color: #818cf8;
            padding-bottom: 12px;
            margin-bottom: 24px;
            border-bottom: 2px solid #334155;
            letter-spacing: 0.05em;
            text-transform: uppercase;
        }}

        .bench-grid {{
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(340px, 1fr));
            gap: 20px;
        }}

        .bench-card {{
            background: linear-gradient(145deg, #1e293b, #1a2332);
            border: 1px solid #334155;
            border-radius: 12px;
            padding: 20px;
            transition: transform 0.2s ease, box-shadow 0.2s ease, border-color 0.2s ease;
        }}
        .bench-card:hover {{
            transform: translateY(-3px);
            box-shadow: 0 8px 30px rgba(0,0,0,0.4);
            border-color: #4f46e5;
        }}

        .bench-title {{
            font-size: 1.1rem;
            font-weight: 600;
            color: #f1f5f9;
            margin-bottom: 16px;
            font-family: 'JetBrains Mono', monospace;
        }}

        .chart-wrap {{
            position: relative;
            height: 180px;
            margin-bottom: 12px;
        }}

        .data-table {{
            width: 100%;
            border-collapse: collapse;
            font-size: 0.85rem;
            margin-top: 8px;
            border-top: 1px solid #334155;
            padding-top: 8px;
        }}
        .data-table tr:not(:last-child) {{
            border-bottom: 1px solid rgba(51,65,85,0.5);
        }}

        .footer {{
            text-align: center;
            padding: 24px;
            color: #64748b;
            font-size: 0.8rem;
            border-top: 1px solid #1e293b;
        }}

        @media (max-width: 768px) {{
            .bench-grid {{ grid-template-columns: 1fr; }}
            .page-header h1 {{ font-size: 1.5rem; }}
        }}
    </style>
</head>
<body>
    <header class="page-header">
        <h1>Scheme Implementation Benchmark</h1>
        <p class="subtitle">Gambit Benchmarks &mdash; Real / User Time (seconds, lower is better) &mdash; Average of {NUM_RUNS} runs &mdash; {timestamp}</p>
    </header>

    <div class="legend-bar">
        <div class="legend-item">
            <span class="legend-swatch" style="background:{colors[impl_keys[0]]["bg"]};"></span>
            {impl_labels[0]}
        </div>
        <div class="legend-item">
            <span class="legend-swatch" style="background:{colors[impl_keys[1]]["bg"]};"></span>
            {impl_labels[1]}
        </div>
        <div class="legend-item">
            <span class="legend-swatch" style="background:{colors[impl_keys[2]]["bg"]};"></span>
            {impl_labels[2]}
        </div>
    </div>

    <script>
        function createBarChart(canvasId, labels, realData, userData, bgColors, borderColors) {{
            const ctx = document.getElementById(canvasId).getContext('2d');
            // Create lighter versions of bgColors for the real-time bars
            const realBgColors = bgColors.map(c => c.replace(/[\d.]+\)$/, '0.4)'));
            const realBorderColors = borderColors.map(c => c.replace(/[\d.]+\)$/, '0.6)'));
            new Chart(ctx, {{
                type: 'bar',
                data: {{
                    labels: labels,
                    datasets: [
                        {{
                            label: 'Real',
                            data: realData,
                            backgroundColor: realBgColors,
                            borderColor: realBorderColors,
                            borderWidth: 1.5,
                            borderRadius: 6,
                            borderSkipped: false,
                        }},
                        {{
                            label: 'User',
                            data: userData,
                            backgroundColor: bgColors,
                            borderColor: borderColors,
                            borderWidth: 1.5,
                            borderRadius: 6,
                            borderSkipped: false,
                        }}
                    ]
                }},
                options: {{
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: {{
                        legend: {{ display: false }},
                        tooltip: {{
                            backgroundColor: '#1e293b',
                            titleColor: '#e2e8f0',
                            bodyColor: '#cbd5e1',
                            borderColor: '#475569',
                            borderWidth: 1,
                            cornerRadius: 8,
                            padding: 10,
                            callbacks: {{
                                label: function(ctx) {{
                                    return ctx.dataset.label + ': ' + ctx.parsed.y.toFixed(6) + 's';
                                }}
                            }}
                        }}
                    }},
                    scales: {{
                        x: {{
                            ticks: {{ color: '#94a3b8', font: {{ family: "'Inter', sans-serif", size: 12 }} }},
                            grid: {{ display: false }},
                        }},
                        y: {{
                            beginAtZero: true,
                            ticks: {{
                                color: '#64748b',
                                font: {{ family: "'JetBrains Mono', monospace", size: 11 }},
                                callback: function(v) {{ return v.toFixed(2) + 's'; }}
                            }},
                            grid: {{ color: 'rgba(51,65,85,0.4)', lineWidth: 0.5 }},
                        }}
                    }},
                    animation: {{
                        duration: 800,
                        easing: 'easeOutQuart',
                    }}
                }}
            }});
        }}
    </script>

    <div class="container">
        {"".join(chart_blocks)}
    </div>

    <footer class="footer">
        Generated by benchmark.py &mdash; {NUM_RUNS} runs averaged &mdash; {timestamp}
    </footer>
</body>
</html>"""

    with open(OUTPUT_FILE, "w") as f:
        f.write(html)
    print(f"\nReport written to: {OUTPUT_FILE}")


def main():
    print("=" * 60)
    print("  Scheme Implementation Benchmark Comparator")
    print(f"  Implementations: {', '.join(IMPLEMENTATIONS[k]['label'] for k in IMPLEMENTATIONS)}")
    print(f"  Runs per implementation: {NUM_RUNS}")
    print("=" * 60)

    averages = collect_data()

    # Print summary table
    all_benchmarks = get_all_benchmarks(averages)
    print(f"\n{'='*60}")
    print("  RESULTS (average time in seconds)")
    print(f"{'='*80}")
    header = f"  {'Benchmark':<14}"
    for k in IMPLEMENTATIONS:
        label = IMPLEMENTATIONS[k]['label']
        header += f"  {label+'/real':>12}  {label+'/user':>12}"
    print(header)
    print("  " + "-" * (14 + 28 * len(IMPLEMENTATIONS)))
    for bench in all_benchmarks:
        row = f"  {bench:<14}"
        for k in IMPLEMENTATIONS:
            timing = averages.get(k, {}).get(bench, None)
            if timing is not None:
                row += f"  {timing['real']:12.4f}  {timing['user']:12.4f}"
            else:
                row += f"  {'N/A':>12}  {'N/A':>12}"
        print(row)

    generate_html(averages)


if __name__ == "__main__":
    main()
