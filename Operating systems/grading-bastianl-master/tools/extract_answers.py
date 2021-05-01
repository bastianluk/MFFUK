#!/usr/bin/env python3

"""
Extract answers from Markdown file where answers are marked with
**A0**, **A1** etc., answer text is everything until an empty line
(i.e. the whole markdown paragraph).
"""

import re
import sys
import yaml

# Answers look like this:
#   **A1** Answer text.
ANSWER_RE = re.compile(r'[*][*](?P<id>A[0-9]*)[*][*](?P<text>.*)')

# Answer consisting of dots only (e.g. '...') is considered empty.
EMPTY_ANSWER_RE = re.compile(r'^[.]*$')

def main():
    """
    Returns number of missing answers.
    """

    answers = {}
    empty_answers_count = 0
    while True:
        line = sys.stdin.readline()
        if not line:
            break

        match = ANSWER_RE.match(line)
        if match is None:
            continue

        answer_text = match.group('text')
        answer_id = match.group('id')

        # Read remaining lines until empty line or EOF
        while True:
            line = sys.stdin.readline()
            if (not line) or (line.rstrip() == ''):
                break
            answer_text = answer_text + " " + line.rstrip()

        answer_text = answer_text.strip()

        if EMPTY_ANSWER_RE.match(answer_text) is not None:
            print("WARNING: empty answer for {}.".format(answer_id), file=sys.stderr)
            empty_answers_count = empty_answers_count + 1
            answer_text = ''

        answers[answer_id] = answer_text

    print(yaml.dump({
        'answers': answers
    }))

    return empty_answers_count

if __name__ == '__main__':
    sys.exit(main())
