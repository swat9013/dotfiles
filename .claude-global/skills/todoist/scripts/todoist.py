#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "todoist-api-python>=3,<4",
# ]
# ///
"""Todoist project task manager.

Usage:
    todoist.py init
    todoist.py list
    todoist.py add [--priority N] [--due DATE] [--labels L1,L2] [--description DESC] [--parent-id ID] TITLE
    todoist.py update TASK_ID [--content TEXT] [--description TEXT] [--priority N] [--due DATE] [--labels L1,L2]
    todoist.py close TASK_ID
    todoist.py delete TASK_ID
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path

from todoist_api_python.api import TodoistAPI

PARENT_PROJECT_NAME = "projects"
CONFIG_PATH = Path.home() / ".config" / "todoist" / "config.json"


def get_api() -> TodoistAPI:
    if not CONFIG_PATH.exists():
        die(f"Config not found: {CONFIG_PATH}")
    token = json.loads(CONFIG_PATH.read_text())["token"]
    return TodoistAPI(token)


def get_project_name() -> str:
    return os.path.basename(os.getcwd())


def collect_pages(paginator) -> list:
    items = []
    for page in paginator:
        items.extend(page)
    return items


def find_project(projects: list, name: str, *, parent_id: str | None = None):
    for p in projects:
        if p.name == name and p.parent_id == parent_id:
            return p
    return None


def require_project(api: TodoistAPI) -> tuple:
    """Return (project_name, project) or die."""
    name = get_project_name()
    projects = collect_pages(api.get_projects())

    parent = find_project(projects, PARENT_PROJECT_NAME, parent_id=None)
    if not parent:
        die(f"Parent project '{PARENT_PROJECT_NAME}' not found. Run: todoist.py init")

    project = find_project(projects, name, parent_id=parent.id)
    if not project:
        die(f"Project '{name}' not found under {PARENT_PROJECT_NAME}/. Run: todoist.py init")

    return name, project


def die(msg: str) -> None:
    print(f"ERROR: {msg}", file=sys.stderr)
    sys.exit(1)


# --- Subcommands ---


def cmd_init(api: TodoistAPI, _args: argparse.Namespace) -> None:
    name = get_project_name()
    projects = collect_pages(api.get_projects())

    parent = find_project(projects, PARENT_PROJECT_NAME, parent_id=None)
    if not parent:
        parent = api.add_project(name=PARENT_PROJECT_NAME)
        print(f"Created parent: {PARENT_PROJECT_NAME} (id: {parent.id})")

    project = find_project(projects, name, parent_id=parent.id)
    if project:
        print(f"Already initialized: {PARENT_PROJECT_NAME}/{name} (id: {project.id})")
        return

    project = api.add_project(name=name, parent_id=parent.id)
    print(f"Initialized: {PARENT_PROJECT_NAME}/{name} (id: {project.id})")


def cmd_list(api: TodoistAPI, _args: argparse.Namespace) -> None:
    name, project = require_project(api)
    tasks = collect_pages(api.get_tasks(project_id=project.id))

    if not tasks:
        print(f"No tasks in {PARENT_PROJECT_NAME}/{name}")
        return

    def sort_key(t):
        # priority: higher number = higher priority in API (4=p1, 1=p4)
        pri = -(t.priority or 1)
        due = str(t.due.date) if t.due else "9999-99-99"
        return (pri, due)

    tasks.sort(key=sort_key)

    # priority display: API 4=p1, 3=p2, 2=p3, 1=p4
    def fmt_priority(p: int) -> str:
        return f"p{5 - p}" if p else "p4"

    def fmt_due(t) -> str:
        if not t.due:
            return "(none)"
        return str(t.due.date)

    header = f"{'ID':<18} {'P':<3} {'Due':<12} Content"
    print(header)
    for t in tasks:
        line = f"{t.id:<18} {fmt_priority(t.priority):<3} {fmt_due(t):<12} {t.content}"
        print(line)


def cmd_add(api: TodoistAPI, args: argparse.Namespace) -> None:
    name, project = require_project(api)

    kwargs: dict = {
        "content": args.title,
        "project_id": project.id,
    }
    if args.priority is not None:
        # user specifies p1-p4, API uses 4=p1, 1=p4
        kwargs["priority"] = 5 - args.priority
    if args.due:
        kwargs["due_string"] = args.due
    if args.labels:
        kwargs["labels"] = [l.strip() for l in args.labels.split(",")]
    if args.description:
        kwargs["description"] = args.description
    if args.parent_id:
        kwargs["parent_id"] = args.parent_id

    task = api.add_task(**kwargs)
    print(f'Added: {task.id} "{task.content}" in {PARENT_PROJECT_NAME}/{name}')


def cmd_update(api: TodoistAPI, args: argparse.Namespace) -> None:
    kwargs: dict = {}
    if args.content is not None:
        kwargs["content"] = args.content
    if args.description is not None:
        kwargs["description"] = args.description
    if args.priority is not None:
        kwargs["priority"] = 5 - args.priority
    if args.due is not None:
        kwargs["due_string"] = args.due
    if args.labels is not None:
        kwargs["labels"] = [l.strip() for l in args.labels.split(",")]

    if not kwargs:
        die("No fields to update. Specify at least one of: --content, --description, --priority, --due, --labels")

    task = api.update_task(args.task_id, **kwargs)
    fields = ", ".join(kwargs.keys())
    print(f"Updated: {task.id} (fields: {fields})")


def cmd_close(api: TodoistAPI, args: argparse.Namespace) -> None:
    api.complete_task(task_id=args.task_id)
    print(f"Closed: {args.task_id}")


def cmd_delete(api: TodoistAPI, args: argparse.Namespace) -> None:
    api.delete_task(task_id=args.task_id)
    print(f"Deleted: {args.task_id}")


# --- CLI ---


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Todoist project task manager")
    sub = parser.add_subparsers(dest="command", required=True)

    sub.add_parser("init", help="Initialize project")

    sub.add_parser("list", help="List tasks")

    p_add = sub.add_parser("add", help="Add task")
    p_add.add_argument("title", help="Task title")
    p_add.add_argument("--priority", type=int, choices=[1, 2, 3, 4], help="Priority (1=highest, 4=lowest)")
    p_add.add_argument("--due", help="Due date (e.g. 'tomorrow', '2026-03-01')")
    p_add.add_argument("--labels", help="Comma-separated labels")
    p_add.add_argument("--description", help="Task description")
    p_add.add_argument("--parent-id", help="Parent task ID (for subtasks)")

    p_update = sub.add_parser("update", help="Update task")
    p_update.add_argument("task_id", help="Task ID")
    p_update.add_argument("--content", help="New task title")
    p_update.add_argument("--description", help="New description")
    p_update.add_argument("--priority", type=int, choices=[1, 2, 3, 4], help="Priority (1=highest, 4=lowest)")
    p_update.add_argument("--due", help="Due date")
    p_update.add_argument("--labels", help="Comma-separated labels")

    p_close = sub.add_parser("close", help="Complete task")
    p_close.add_argument("task_id", help="Task ID")

    p_delete = sub.add_parser("delete", help="Delete task")
    p_delete.add_argument("task_id", help="Task ID")

    return parser.parse_args()


def main() -> None:
    args = parse_args()
    try:
        api = get_api()
    except Exception as e:
        die(f"Authentication failed. Check {CONFIG_PATH}: {e}")

    dispatch = {
        "init": cmd_init,
        "list": cmd_list,
        "add": cmd_add,
        "update": cmd_update,
        "close": cmd_close,
        "delete": cmd_delete,
    }
    try:
        dispatch[args.command](api, args)
    except Exception as e:
        die(f"API request failed: {e}")


if __name__ == "__main__":
    main()
