# dotfiles

## Requirements

- git

## Install

```bash
cd ~ && curl -L raw.github.com/swat9013/dotfiles/master/install.sh | sh
```

## Claude Code Skills

No git clone required. Run from the `skills/` directory where you want to install.

```bash
# All skills
curl -sL https://github.com/swat9013/dotfiles/archive/master.tar.gz | tar xz --strip-components=3 dotfiles-master/.claude-global/skills

# Single skill (replace SKILL_NAME, e.g. drawio, glab, slidev)
curl -sL https://github.com/swat9013/dotfiles/archive/master.tar.gz | tar xz --strip-components=3 dotfiles-master/.claude-global/skills/SKILL_NAME
```

- User scope: run from `~/.claude/skills/`
- Project scope: run from `<project>/.claude/skills/`
