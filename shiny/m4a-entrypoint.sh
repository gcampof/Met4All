#!/bin/sh
# shiny-server starts R through `su --login`, which drops the container's
# environment. Without this the app never sees M4A_* (so the annotation cache
# baked into the image is ignored and rebuilt) nor the hub cache locations.
# Rebuilt from a pristine copy on every start, so compose changes apply.
set -e
site="$(R RHOME)/etc/Renviron.site"
[ -f "$site.orig" ] || cp "$site" "$site.orig"
cp "$site.orig" "$site"
env | grep -E '^(M4A_|ANNOTATION_HUB_CACHE=|EXPERIMENT_HUB_CACHE=|R_CONFIG_ACTIVE=)' >> "$site" || true
exec /usr/bin/shiny-server "$@"
