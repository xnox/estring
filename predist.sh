#!/bin/sh

# Generate CHANGES.darcs
[ -d "$DARCS_REPO" ] && darcs changes --repodir "$DARCS_REPO" > CHANGES.darcs

# Let oasis generate a standard Makefile
rm -f Makefile

# Add oasis stuff
oasis setup

# Make the configure script executable
chmod +x configure

# Cleanup
rm -f predist.sh boring
