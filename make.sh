#!/bin/bash

# Execute this script from within the clips-mode project root.
SRCDIR="$PWD"
TMPDIR="/home/$USER/tmp"
RELNAM="clips-mode-0.7"
PKGNAM="$RELNAM".tar

# Check if /home/<username>/tmp exists and create it if it does not.
if [ ! -d "$TMPDIR" ]; then
    mkdir "$TMPDIR"
fi

# Go to the base directory
cd "$TMPDIR"

# Check if a build directory exists, and if it does, delete it, otherwise
# create from scratch.
if [ -d "$RELNAM" ]; then
    rm -rf "$RELNAM"
else
    mkdir "$RELNAM"
fi

# Copy over files
cp "$SRCDIR"/"clips-mode-pkg.el" "$RELNAM"
cp "$SRCDIR"/"clips-mode.el" "$RELNAM"
cp "$SRCDIR"/"inf-clips.el" "$RELNAM"
cp "$SRCDIR"/"COPYING" "$RELNAM"

# Check if the package file already exists and delete it if it does.
if [ -e "$PKGNAM" ]; then
    rm "$PKGNAM"
fi

# Create the TAR package
tar cvf "$PKGNAM" "$RELNAM"
