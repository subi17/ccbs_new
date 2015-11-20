#!/bin/bash

#Original version copied from web - web/shared/script/automerge.sh

die() {
  echo "$@"
  exit 1
}

if [ `hostname` = "nunki" ]; then
  git_dir=$(git rev-parse --git-dir) || die "Not a git repository"
  git config user.email "nunki@jenkins.qvantel.net"
  git config user.name "Jenkins"
  git fetch --prune
  #start_branch=$(git rev-parse --abbrev-ref HEAD)
  start_branch=${GIT_BRANCH#*/}
  git branch -r | ( \
    failure=0
    while read remote_tracking_branch
    do
      branch=${remote_tracking_branch#*/}
      remote=${remote_tracking_branch%%/*}
      [[ "$branch" = "$start_branch" ]] && continue
      [[ "$branch" =~ ^master$ ]] && continue
      git branch --no-track -f tmp "$remote_tracking_branch"
      git checkout -q tmp
      if git merge -q -m "Automerge remote branch $remote/master into $branch" "$remote/master" >/dev/null 2>&1
      then
        git push -q "$remote" "tmp:$branch" 2>/dev/null && \
        echo "        SUCCESS  $remote_tracking_branch" || \
        echo "******** Failed to push $remote tmp:$branch"
      else
        git reset -q --hard
        failure=1
        echo "FAILURE          $remote_tracking_branch"
      fi
      git checkout -q $start_branch >/dev/null
    done
    exit $failure
  )
  failure=$?
  git branch -D tmp
fi
exit $failure
