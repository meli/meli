#!/bin/sh

# Copyright (c) 2024 Manos Pitsidianakis <manos@pitsidianak.is>
# Licensed under the EUPL-1.2-or-later.
#
# You may obtain a copy of the Licence at:
# https://joinup.ec.europa.eu/software/page/eupl
#
# SPDX-License-Identifier: EUPL-1.2

## shellcheck -o all -s sh -S style scripts/mandoc_lint.sh

if test "$#" -ne 1; then
  printf "Usage: %s PATH_TO_MANDOC_FILE\n" "${0}"
  exit 1
fi

case "${1}" in
-h)
  printf "Usage: %s PATH_TO_MANDOC_FILE\n" "${0}"
  exit 0
  ;;
--help)
  printf "Usage: %s PATH_TO_MANDOC_FILE\n" "${0}"
  exit 0
  ;;
*)
  ;;
esac

if ! test -f "${1}"; then
  printf "Argument %s either not exists or is not a regular text file.\n" "${1}"
  exit 1
fi

section=$(echo "${1}" | rev | cut -d "." -f 1)

tmpdir="$(mktemp --directory)"

git show HEAD:"${1}" > "${tmpdir}/MANDOC_IN_HEAD.${section}"

mandoc -T lint "${tmpdir}/MANDOC_IN_HEAD.${section}" 2> /dev/null | cut -d':' -f 5- > "${tmpdir}"/HEAD_LINT

mandoc -T lint "${1}" 2> /dev/null | cut -d':' -f 5- > "${tmpdir}"/CHANGED_LINT

diff -c "${tmpdir}"/HEAD_LINT "${tmpdir}"/CHANGED_LINT

status=$?

rm -rf "${tmpdir}"

if test "${status}" -ne 0; then
  printf "%s introduces new lint messages. Ignore if they are a false positive.\n" "${1}"
fi

# exit "${status}"
