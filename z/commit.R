# commit.R

# This script commits to github.
require(gert)

# Add paths of files you want to **commit** to Github
gert::git_add(dir(all.files = TRUE))
# Commit all those changed files
gert::git_commit_all(message = "fixed the output issue in app.R")
# Push those files to Github
gert::git_push()


