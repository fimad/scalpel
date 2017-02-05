#!/bin/bash
# Bump the various hard coded scalpel version numbers.

set -e

if [ "$#" != "1" ]
then
  echo "usage: $0 VERSION"
  exit -1
fi

version="$1"

echo "Updating CHANGLOG.md"
sed -i -r "s/^(## HEAD)$/\1\n\n## $version/" CHANGELOG.md

bump-cabal() {
  cabal="$1"
  echo "Updating $cabal"

  # The version number of the package.
  sed -i -r "s/^(version:\s+)[0-9.]+$/\1$version/" "$cabal"

  # The version number in the git tag for the current release.
  sed -i -r "s/^(\s*tag:\s+v)[0-9.]+$/\1$version/" "$cabal"

  # The version of the scalpel-core dependency.
  sed -i -r "s/^([ ,]+scalpel-core\s*==\s*)[0-9.]+$/\1$version/" "$cabal"
}

bump-cabal "scalpel/scalpel.cabal"
bump-cabal "scalpel-core/scalpel-core.cabal"

git diff --color=always | cat -
echo -en "\nDoes this look OK? [N/y] "
read ok && [ "$ok" == "y" ]

git commit -a -m "Bump version to $version"
