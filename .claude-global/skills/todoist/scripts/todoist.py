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
import subprocess
import sys
from pathlib import Path
from urllib.parse import urlparse

from todoist_api_python.api import TodoistAPI

PARENT_PROJECT_NAME = "projects"
CONFIG_PATH = Path.home() / ".config" / "todoist" / "config.json"


def get_api() -> TodoistAPI:
    if not CONFIG_PATH.exists():
        die(f"Config not found: {CONFIG_PATH}")
    token = json.loads(CONFIG_PATH.read_text())["token"]
    return TodoistAPI(token)


def get_project_name() -> str:
    """Return the repository name, resolving to the main worktree if in a linked worktree."""
    try:
        # git rev-parse --git-common-dir returns the shared .git dir
        # For a linked worktree: /path/to/repo/.git/worktrees/<name> -> /path/to/repo/.git
        # For a normal repo: /path/to/repo/.git -> /path/to/repo/.git
        common_dir = subprocess.run(
            ["git", "rev-parse", "--git-common-dir"],
            capture_output=True, text=True, check=True,
        ).stdout.strip()
        # .git dir is <repo-root>/.git, so parent is the repo root
        return Path(common_dir).resolve().parent.name
    except (subprocess.CalledProcessError, OSError):
        return os.path.basename(os.getcwd())


def parse_task_url(url_or_id: str) -> str:
    """Extract task ID from Todoist URL or return as-is."""
    if url_or_id.startswith(("https://", "http://")):
        path = urlparse(url_or_id).path.rstrip("/")
        parts = path.split("/")
        if "task" not in parts:
            die(f"Not a Todoist task URL: {url_or_id}")
        segment = parts[-1]
        # URL format: {slug}-{task_id} or just {task_id}
        return segment.rsplit("-", 1)[-1]
    return url_or_id


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
        pri = -(t.priority or 1)
        due = str(t.due.date) if t.due else "9999-99-99"
        return (pri, due)

    def fmt_priority(p: int) -> str:
        return f"p{5 - p}" if p else "p4"

    # Build parent-child tree
    by_id = {t.id: t for t in tasks}
    children: dict[str, list] = {t.id: [] for t in tasks}
    roots: list = []
    for t in tasks:
        if t.parent_id and t.parent_id in by_id:
            children[t.parent_id].append(t)
        else:
            roots.append(t)

    for group in children.values():
        group.sort(key=sort_key)
    roots.sort(key=sort_key)

    def render_task(t, depth: int = 0) -> None:
        indent = "  " * depth
        meta_parts = [fmt_priority(t.priority)]
        if t.due:
            meta_parts.append(f"due:{t.due.date}")
        if t.labels:
            meta_parts.append(f"labels:{','.join(t.labels)}")
        meta = " ".join(meta_parts)
        print(f"{indent}- [{t.id}] {t.content} ({meta})")
        if t.description:
            for line in t.description.strip().splitlines():
                print(f"{indent}  {line}")
        for child in children.get(t.id, []):
            render_task(child, depth + 1)

    for t in roots:
        render_task(t)


def cmd_get(api: TodoistAPI, args: argparse.Namespace) -> None:
    task_id = parse_task_url(args.task_url)
    task = api.get_task(task_id)

    pri_str = f"p{5 - task.priority}" if task.priority else "p4"
    meta_parts = [pri_str]
    if task.due:
        meta_parts.append(f"due:{task.due.date}")
    if task.labels:
        meta_parts.append(f"labels:{','.join(task.labels)}")

    print(f"# {task.content}")
    print(f"ID: {task.id} | {' | '.join(meta_parts)}")
    print(f"URL: {task.url}")

    if task.description:
        print(f"\n## Description\n{task.description}")

    # Fetch subtasks from the same project
    all_tasks = collect_pages(api.get_tasks(project_id=task.project_id))
    subtasks = [t for t in all_tasks if t.parent_id == task.id]
    if subtasks:
        def sort_key(t):
            pri = -(t.priority or 1)
            due = str(t.due.date) if t.due else "9999-99-99"
            return (pri, due)

        subtasks.sort(key=sort_key)
        print("\n## Subtasks")
        for st in subtasks:
            st_pri = f"p{5 - st.priority}" if st.priority else "p4"
            st_meta = st_pri
            if st.due:
                st_meta += f" due:{st.due.date}"
            print(f"- [{st.id}] {st.content} ({st_meta})")
            if st.description:
                for line in st.description.strip().splitlines():
                    print(f"  {line}")


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

    p_get = sub.add_parser("get", help="Get task details")
    p_get.add_argument("task_url", help="Task URL or ID")

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
        "get": cmd_get,
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
