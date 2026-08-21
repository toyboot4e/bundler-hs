#!/bin/sh
# One TSV line per query: kind, module, old name, default-suffix.
# One line per response: the new name (or qualifier, for extmod).
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
