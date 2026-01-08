---
name: ralph
description: Autonomous task loop that picks ready tasks, implements them in new threads, commits, and repeats. Use when asked to "ralph", "use a ralph loop", or "ralph this".
---

# Ralph Loop

Autonomous task execution loop for this repository.

## Workflow

1. **Check for ready tasks**
   ```
   task_list action: "list", repoURL: "<repo-url>", ready: true
   ```

2. **If no ready tasks**: Stop and report "Ralph complete - no ready tasks remaining."

3. **If ready tasks exist**:
   - Pick the first ready task
   - Use the `handoff` tool with goal: "Implement and verify task [task-id]: [task-title]. [task-description]. When complete, consider if any learnings from this task should be added to AGENTS.md to improve future performance (e.g., common patterns, gotchas, useful commands). Then commit the changes with the commit-messages skill and invoke the ralph skill to continue the loop."

## Important

- Always pass `ready: true` when listing tasks to only get tasks with satisfied dependencies
- The handoff goal must include instructions to commit and re-invoke ralph
- Each iteration runs in a fresh thread to maintain clean context
