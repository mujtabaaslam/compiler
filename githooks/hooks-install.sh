HOOKS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

REPO_DIR="$(git rev-parse --show-toplevel)"
if [[ -z $GIT_DIR ]]; then
  GIT_DIR=".git"
fi
DOT_GIT_DIR="$REPO_DIR/$GIT_DIR"

ln -sf $HOOKS_DIR/pre-commit $DOT_GIT_DIR/hooks/pre-commit
