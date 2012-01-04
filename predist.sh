#!/bin/sh

# Generate CHANGES.darcs
[ -d "$DARCS_REPO" ] && darcs changes --repodir "$DARCS_REPO" > CHANGES.darcs

# Add oasis stuff
oasis setup

# Cleanup
rm -f dist.sh predist.sh boring
