import json
import sys
import argparse
from collections import Counter

def analyze_call_balance(events):
    begin_counts = Counter()
    end_counts = Counter()

    for event in events:
        name = event['name']
        event_type = event['event']
        
        if event_type == 'BeginCall':
            begin_counts[name] += 1
        elif event_type == 'EndCall':
            end_counts[name] += 1

    unbalanced_functions = []
    for func_name in set(begin_counts.keys()) | set(end_counts.keys()):
        if begin_counts[func_name] != end_counts[func_name]:
            unbalanced_functions.append({
                'function': func_name,
                'begin_count': begin_counts[func_name],
                'end_count': end_counts[func_name]
            })

    return unbalanced_functions, begin_counts, end_counts

def print_all_counts(begin_counts, end_counts):
    print("\n📊 ALL FUNCTION CALLS:")
    for func_name in sorted(set(begin_counts.keys()) | set(end_counts.keys())):
        print(f"Function: {func_name}")
        print(f"  Begin Calls: {begin_counts[func_name]}")
        print(f"  End Calls: {end_counts[func_name]}")

def main():
    parser = argparse.ArgumentParser(description='Check function call balance in JSON events')
    parser.add_argument('json_file', help='JSON file containing events')
    parser.add_argument('--all', action='store_true', help='Show all function call counts')
    args = parser.parse_args()

    try:
        with open(args.json_file, 'r') as f:
            events = json.load(f)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"ERROR: {e}")
        sys.exit(1)

    unbalanced, begin_counts, end_counts = analyze_call_balance(events)

    if args.all:
        print_all_counts(begin_counts, end_counts)

    if unbalanced:
        print("\n🚨 UNBALANCED FUNCTION CALLS DETECTED 🚨")
        for func in unbalanced:
            print(f"Function: {func['function']}")
            print(f"  Begin Calls: {func['begin_count']}")
            print(f"  End Calls: {func['end_count']}")
        sys.exit(1)
    else:
        if not args.all:
            print("✅ ALL CALLS BALANCED!")

if __name__ == "__main__":
    main()
