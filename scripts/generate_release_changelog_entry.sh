#!/bin/sh
# SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

# lint with $ shellcheck -s sh -S style

: "${GITEA_TOKEN?"Environment variable GITEA_TOKEN must be set."}"

PREVIOUS_TAG_VERSION="$(git describe --abbrev=0)"
GITEA_API_URL="https://git.meli-email.org"
export PREVIOUS_TAG_VERSION GITEA_API_URL

printf "Next release assumed to be for git range %s..HEAD.\n" "${PREVIOUS_TAG_VERSION}"  >&2
printf "Executing friends script to get release contributors.\n" >&2
FRIENDS=$(./scripts/friends.sh "${PREVIOUS_TAG_VERSION}")
printf "Friends script output is:\n%s\n" "${FRIENDS}" >&2
printf "Executing git-cliff to generate new CHANGELOG.md release entry.\n" >&2

if [ "$(printf "%s" "${FRIENDS}" | grep -c '^')" -gt 1 ]; then
  export FRIENDS
fi
git cliff --unreleased
