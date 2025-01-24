import json
import sys
from collections import defaultdict

class CallTracker:
    def __init__(self):
        self.call_stack = defaultdict(list)
        self.call_counts = defaultdict(int)

    def process_events(self, events):
        # Track the call hierarchy
        for event in events:
            name = event['name']
            event_type = event['event']

            if event_type == 'BeginCall':
                self.call_stack[name].append(event)
                self.call_counts[name] += 1
            elif event_type == 'EndCall':
                if not self.call_stack[name]:
                    self._unbalanced_error(name, "END CALL WITHOUT MATCHING BEGIN", event)
                else:
                    self.call_stack[name].pop()

        # Check for unbalanced calls after processing all events
        self._check_final_balance()

    def _unbalanced_error(self, name, error_type, event):
        print("\n" + "🚨 UNBALANCED CALL DETECTED 🚨".center(80, '!'))
        print(f"CATASTROPHIC {error_type} FOR: {name}")
        print(f"PROBLEMATIC EVENT: {event}")
        print(f"CURRENT CALL STACK STATE: {dict(self.call_stack)}")
        print(f"CURRENT CALL COUNTS: {dict(self.call_counts)}")
        print("🔥 PANIC MODE ACTIVATED 🔥".center(80, '!'))
        sys.exit(1)

    def _check_final_balance(self):
        for name, stack in self.call_stack.items():
            if stack:
                self._unbalanced_error(name, "UNMATCHED BEGIN CALLS", {
                    "name": name, 
                    "remaining_begins": len(stack)
                })

def main():
    if len(sys.argv) < 2:
        print("USAGE: python check_balance.py <json_file>")
        sys.exit(1)

    try:
        with open(sys.argv[1], 'r') as f:
            events = json.load(f)
    except FileNotFoundError:
        print(f"ERROR: File {sys.argv[1]} not found!")
        sys.exit(1)
    except json.JSONDecodeError:
        print(f"ERROR: Invalid JSON in {sys.argv[1]}")
        sys.exit(1)

    tracker = CallTracker()
    tracker.process_events(events)
    print("✅ ALL CALLS BALANCED! NO ISSUES DETECTED.")

if __name__ == "__main__":
    main()
