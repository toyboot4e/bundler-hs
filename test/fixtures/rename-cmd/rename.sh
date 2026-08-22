#!/bin/sh
# One TSV line per query: kind, module, old name, default-suffix.
# One line per response: the new name (or qualifier, for extmod).
#
# POSIX sh on purpose: the nix build sandbox has no /usr/bin/env, so a
# `#!/usr/bin/env bash` shebang here is an interpreter that cannot be found,
# and the tab has to be built with printf rather than bash's IFS=$'\t'.
tab=$(printf '\t')
while IFS="$tab" read -r kind mod name suffix; do
  case "$kind" in
    extmod) printf '%s\n' "$mod" | tr -d '.' ;;
    op)
      case "$mod" in
        OpsA) printf '<+.>\n' ;;
        OpsB) printf '<.+>\n' ;;
        *) printf '%s\n' "$name" ;;
      esac
      ;;
    *) printf '%s_%s\n' "$name" "$suffix" ;;
  esac
done
