#!/usr/bin/env sh
# SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

# Lint with shellcheck -s sh -S style check_dco.sh

# Notes:
# ======
#
# - We need to make sure git commands do not read from any existing configs to
#   prevent surprises like default trailers being added.
# - we need to pass `--always` to `git-format-patch` to check even empty
#   commits despite them not being something we would merge. This tripped me up
#   when debugging this workflow because I tested it with empty commits. My
#   fault.

export GIT_CONFIG_GLOBAL=""
export GIT_CONFIG_SYSTEM=""
export GIT_CONFIG_NOSYSTEM=1

ensure_env_var() {
  set | grep -q "^${1}=" || (printf "Environment variable %s missing from process environment, exiting.\n" "${1}"; exit "${2}")
}

ensure_env_var "GITHUB_BASE_REF" 1 || exit $?
ensure_env_var "GITHUB_HEAD_REF" 2 || exit $?

# contains_correct_signoff() {
#   author=$(git log --author="$1" --pretty="%an <%ae>" -1)
#   git format-patch --always --stdout "${1}^..${1}" | git interpret-trailers --parse | grep -q "^Signed-off-by: ${author}"
# }
contains_signoff() {
  GIT_CONFIG_GLOBAL="" git format-patch --always -1 --stdout "${1}" | git interpret-trailers  --parse | grep -q "^Signed-off-by: "
}

get_commit_sha() {
  if OUT=$(git rev-parse "${1}"); then
    printf "%s" "${OUT}"
    return
  fi
  printf "Could not git-rev-parse %s, falling back to HEAD...\n" "${1}" 1>&2
  git rev-parse HEAD
}

echo "Debug workflow info:"
echo "Base ref GITHUB_BASE_REF=${GITHUB_BASE_REF}"
echo "Head ref GITHUB_HEAD_REF=${GITHUB_HEAD_REF}"
BASE_REF=$(get_commit_sha "${GITHUB_BASE_REF}")
HEAD_REF=$(get_commit_sha "${GITHUB_HEAD_REF}")
echo "Processed base ref BASE_REF=${BASE_REF}"
echo "Processed head ref HEAD_REF=${HEAD_REF}"

RANGE="${BASE_REF}..${HEAD_REF}"
echo "Range to examine is RANGE=${RANGE}"

if ! SHA_LIST=$(git rev-list "${RANGE}"); then
  printf "Could not get commit range %s with git rev-list, bailing out...\n" "${RANGE}"
  exit 0
fi

echo "SHA list to examine is SHA_LIST="
echo "---------------------------------------------------------------------"
echo "${SHA_LIST}"
echo "---------------------------------------------------------------------"
echo ""
echo "Starting checks..."

output=$(printf "%s\n" "${SHA_LIST}" | while read -r commit_sha; do
  contains_signoff_result=""

  contains_signoff "${commit_sha}"; contains_signoff_result="$?"
  if [ "${contains_signoff_result}" -ne 0 ]; then
    printf "Commit does not contain Signed-off-by git trailer: %s\n\n" "${commit_sha}"
    echo "patch was:"
    echo "---------------------------------------------------------------------"
    GIT_CONFIG_GLOBAL="" git format-patch --always -1 --stdout "${commit_sha}"
    echo "---------------------------------------------------------------------"
    echo "trailers were:"
    echo "---------------------------------------------------------------------"
    GIT_CONFIG_GLOBAL="" git format-patch --always -1 --stdout "${commit_sha}" | git interpret-trailers  --parse
    echo "---------------------------------------------------------------------"
    echo "commit was:"
    echo "---------------------------------------------------------------------"
    git log --no-decorate --pretty=oneline --abbrev-commit -n 1 "${commit_sha}"
    echo "---------------------------------------------------------------------"
  fi
done)

if [ "${output}" = "" ]; then
  exit 0
fi

echo "One or more of your commits in this Pull Request lack the Developer Certificate of Origin "
echo "which is more commonly known as DCO or the \"Signed-off-by: \" trailer line in the "
echo "git commit message."
echo "For information, documentation, help, check: https://wiki.linuxfoundation.org/dco"

echo "The reported errors were:"
printf "%s\n" "${output}" 1>&2

echo ""
echo "Solution:"
echo ""
echo "- end all your commits with a 'Signed-off-by: User <user@localhost>' line, "
echo "  with your own display name and email address."
echo "- Make sure the signoff is separated by the commit message body with an empty line."
echo "- Make sure the signoff is the last line in your commit message."
echo "- Lastly, make sure the signoff matches your git commit author name and email identity."

exit 1
