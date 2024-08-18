# commit.R

# This script commits to github.
require(gert)

# Add paths of files you want to **commit** to Github
gert::git_add(files = ".")
# Commit all those changed files
gert::git_commit_all(message = "...")
# Push those files to Github
gert::git_push()


