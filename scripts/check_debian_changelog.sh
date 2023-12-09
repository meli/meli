#!/bin/bash
# SPDX-License-Identifier: EUPL-1.2

usage() { printf "Check if latest version in debian/changelog matches the version in meli/Cargo.toml.\n\nUsage: %s [-q ] [-i]\n\n-q\tQuiet; no output.\n-i\tNon-interactive mode.\n" "${0}" 1>&2; exit 1; }

while getopts "qi" o; do
    case "${o}" in
        q)
            QUIET="true"
            ;;
        i)
            NONINTERACTIVE="true"
            ;;
        *)
            usage
            ;;
    esac
done
shift $((OPTIND-1))

VERSION=$(grep -m1 version meli/Cargo.toml | head -n1 | cut -d'"' -f 2 | head -n1)
DEBIAN_CHANGELOG_VERSION=$(grep -m1 meli debian/changelog | head -n1 | cut -d'(' -f 2 | cut -d')' -f 1 | sed -e 's/\-[0-9]$//')


if [ "${VERSION}" == "${DEBIAN_CHANGELOG_VERSION}" ]; then
  if [ -z "${QUIET}" ]; then
    printf "Versions match: %s\n" "${VERSION}"
  fi
  exit 0;
fi

if [ -z "${QUIET}" ]; then
  printf "Version in meli/Cargo.toml, %s, is not the same as the latest version in debian/changelog file, which is %s\n\n" "${VERSION}" "${DEBIAN_CHANGELOG_VERSION}"
  echo "Please update debian/changelog with the following diff:"
  author=$(grep -m1 authors meli/Cargo.toml | head -n1 | cut -d'"' -f 2 | head -n1)
  now=$(date -u +"%a, %d %b %Y %T +0000")
  prepend_value=$(cat <<EOF
meli (${VERSION}-1) bookworm; urgency=low

-- ${author}  ${now}

EOF
)
  changelog_value=$(cat debian/changelog)
  patch_diff=$(LC_ALL=C TZ=UTC0 diff -Naur debian/changelog <(printf "%s\n\n%s\n" "${prepend_value}" "${changelog_value}"))
  printf "%s\n" "${patch_diff}"
  if [ -n "${NONINTERACTIVE}" ]; then
    echo "Apply the diff automatically?"
    select yn in "Yes" "No"; do
      case $yn in
          Yes ) break;;
          No ) exit 0;;
      esac
    done
    printf "%s\n" "${patch_diff}"|patch debian/changelog
    exit 0
  fi
fi

exit 1
