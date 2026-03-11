# Branch Protection for `main`

Applying branch protection requires repository admin authentication.  
This was not applied automatically from the current local session because no GitHub API token is available.

## Required checks

Set required status checks to:
- `ubuntu-latest / R-release`
- `ubuntu-latest / R-devel`
- `windows-latest / R-release`
- `macos-latest / R-release`
- `lintr`
- `coverage`

## Apply via GitHub REST API

Use a personal access token with repository admin permissions:

```bash
export GITHUB_TOKEN=YOUR_TOKEN

curl -L \
  -X PUT \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer ${GITHUB_TOKEN}" \
  https://api.github.com/repos/christopherposadasp-ctrl/StatsRPackage/branches/main/protection \
  -d '{
    "required_status_checks": {
      "strict": true,
      "contexts": [
        "ubuntu-latest / R-release",
        "ubuntu-latest / R-devel",
        "windows-latest / R-release",
        "macos-latest / R-release",
        "lintr",
        "coverage"
      ]
    },
    "enforce_admins": true,
    "required_pull_request_reviews": {
      "dismiss_stale_reviews": true,
      "require_code_owner_reviews": false,
      "required_approving_review_count": 1
    },
    "required_conversation_resolution": true,
    "required_linear_history": true,
    "allow_force_pushes": false,
    "allow_deletions": false,
    "block_creations": false,
    "lock_branch": false,
    "allow_fork_syncing": true,
    "restrictions": null
  }'
```
