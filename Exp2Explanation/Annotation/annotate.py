"""
annotate.py

LLM-based annotation script using the Anthropic API.
Reads the annotation schema from instructions.md in the same folder.
Reads utterances from a CSV, annotates each one using Claude,
and writes results to a new CSV preserving all original columns.

Usage:
    python annotate.py --input your_data.csv --utterance_col utterance --output run1_output.csv

Requirements:
    pip install anthropic pandas
"""

import anthropic
import pandas as pd
import argparse
import time
import os

# ── CONFIGURATION ─────────────────────────────────────────────────────────────

MODEL = "claude-sonnet-4-6"  # Change to "claude-sonnet-4-6" for cheaper test runs

VALID_LABELS = [
    'P=0',
  'P=1',
  'P',
  'K=0',
  'K=1',
  'K',
  'C=0',
  'C=1',
  'C',
  'S=0',
  'S=1',
  'S',
  'Pu=0',
  'Pu=1',
  'Pu',
  'Ku=0',
  'Ku=1',
  'Ku',
  'Cu_p=0',
  'Cu_p=1',
  'Cu_p',
  'Cu_f=0',
  'Cu_f=1',
  'Cu_f',
  'Su_p=0',
  'Su_p=1',
  'Su_p',
  'Su_f=0',
  'Su_f=1',
  'Su_f',
  'br_p=0',
  'br_p=1',
  'br_p',
  'br_f=0',
  'br_f=1',
  'br_f',
  'Unclear'
]

DELAY_BETWEEN_CALLS = 0.5   # seconds between API calls. Increase if rate limited.
MAX_RETRIES = 2              # how many times to retry if model returns invalid label

# ─────────────────────────────────────────────────────────────────────────────


def load_instructions(script_dir):
    """Load instructions.md from the same folder as the script."""
    instructions_path = os.path.join(script_dir, "Instructions.md")
    if not os.path.exists(instructions_path):
        raise FileNotFoundError(
            f"Could not find Instructions.md in {script_dir}\n"
            f"Please place Instructions.md in the same folder as annotate.py"
        )
    with open(instructions_path, "r", encoding="utf-8") as f:
        return f.read()


def build_system_prompt(instructions):
    """Combine instructions with output format requirement."""
    return (
        instructions.strip()
        + "\n\n"
        + "Respond with ONLY the label, nothing else. "
        + "Your response must be exactly one of: "
        + ", ".join(VALID_LABELS)
        + ". Do not include any explanation or punctuation."
    )


def call_api(client, system_prompt, utterance):
    """Make a single API call and return the raw text response."""
    message = client.messages.create(
        model=MODEL,
        max_tokens=20,
        system=system_prompt,
        messages=[
            {"role": "user", "content": utterance}
        ]
    )
    return message.content[0].text.strip()


def annotate_utterance(client, system_prompt, utterance):
    """
    Annotate a single utterance with retry logic.
    Returns (label, note) where note records any retry behaviour.
    """
    # First attempt
    raw = call_api(client, system_prompt, utterance)
    if raw in VALID_LABELS:
        return raw, None

    # Retry loop with explicit correction prompt
    for attempt in range(MAX_RETRIES):
        correction_prompt = (
            f"Your previous response '{raw}' is not a valid label. "
            f"You must respond with exactly one of: {', '.join(VALID_LABELS)}. "
            f"The utterance to annotate was: {utterance}"
        )
        message = client.messages.create(
            model=MODEL,
            max_tokens=20,
            system=system_prompt,
            messages=[
                {"role": "user", "content": utterance},
                {"role": "assistant", "content": raw},
                {"role": "user", "content": correction_prompt}
            ]
        )
        raw_retry = message.content[0].text.strip()
        if raw_retry in VALID_LABELS:
            return raw_retry, f"corrected after {attempt + 1} retry (original: '{raw}')"
        raw = raw_retry
        time.sleep(DELAY_BETWEEN_CALLS)

    # All retries exhausted
    return f"INVALID: {raw}", f"failed after {MAX_RETRIES} retries"


def main():
    parser = argparse.ArgumentParser(description="Annotate utterances using Claude API")
    parser.add_argument("--input", required=True, help="Path to input CSV file")
    parser.add_argument("--utterance_col", required=True, help="Name of the column containing utterances")
    parser.add_argument("--output", required=True, help="Path to output CSV file")
    parser.add_argument("--api_key", default=None, help="Anthropic API key (or set ANTHROPIC_API_KEY env variable)")
    args = parser.parse_args()

    # ── API KEY ───────────────────────────────────────────────────────────────
    api_key = args.api_key or os.environ.get("ANTHROPIC_API_KEY")
    if not api_key:
        raise ValueError(
            "No API key found. Either pass --api_key or set the "
            "ANTHROPIC_API_KEY environment variable."
        )

    client = anthropic.Anthropic(api_key=api_key)

    # ── LOAD INSTRUCTIONS ─────────────────────────────────────────────────────
    script_dir = os.path.dirname(os.path.abspath(__file__))
    print(f"Loading Instructions.md from {script_dir}...")
    instructions = load_instructions(script_dir)
    system_prompt = build_system_prompt(instructions)
    print("Instructions loaded successfully.")

    # ── LOAD DATA ─────────────────────────────────────────────────────────────
    print(f"Loading {args.input}...")
    df = pd.read_csv(args.input)

    if args.utterance_col not in df.columns:
        raise ValueError(
            f"Column '{args.utterance_col}' not found in CSV. "
            f"Available columns: {list(df.columns)}"
        )

    total = len(df)
    print(f"Found {total} utterances to annotate using {MODEL}.")
    print(f"Valid labels: {', '.join(VALID_LABELS)}\n")

    # ── ANNOTATE ──────────────────────────────────────────────────────────────
    labels = []
    notes = []

    for i, utterance in enumerate(df[args.utterance_col]):
        try:
            if pd.isna(utterance) or str(utterance).strip() == "":
                label, note = "Unclear", "empty utterance"
                print(f"  [{i+1}/{total}] (empty) → {label}")
            else:
                label, note = annotate_utterance(client, system_prompt, str(utterance))
                note_str = f" [{note}]" if note else ""
                print(f"  [{i+1}/{total}] {str(utterance)[:60]}... → {label}{note_str}")

            labels.append(label)
            notes.append(note)

        except Exception as e:
            print(f"  [{i+1}/{total}] ERROR: {e}")
            labels.append("ERROR")
            notes.append(str(e))

        if i < total - 1:
            time.sleep(DELAY_BETWEEN_CALLS)

    # ── SAVE OUTPUT ───────────────────────────────────────────────────────────
    df["annotation"] = labels
    df["annotation_note"] = notes

    df.to_csv(args.output, index=False)
    print(f"\nDone. Results saved to {args.output}")

    # ── SUMMARY ───────────────────────────────────────────────────────────────
    n_errors = sum(1 for l in labels if l == "ERROR")
    n_invalid = sum(1 for l in labels if str(l).startswith("INVALID"))
    n_retried = sum(1 for n in notes if n and "corrected" in str(n))
    n_unclear = sum(1 for l in labels if l == "Unclear")
    n_clean = total - n_errors - n_invalid

    print(f"\nSummary:")
    print(f"  Total:         {total}")
    print(f"  Clean labels:  {n_clean}")
    print(f"  Unclear:       {n_unclear}")
    print(f"  Retried+fixed: {n_retried}")
    print(f"  Invalid:       {n_invalid}")
    print(f"  Errors:        {n_errors}")

    if n_invalid > 0:
        print(f"\n  Warning: {n_invalid} utterances could not be resolved to a valid label.")
        print(f"  Check the annotation_note column in {args.output} for details.")


if __name__ == "__main__":
    main()