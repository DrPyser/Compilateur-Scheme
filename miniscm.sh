#! /usr/bin/env sh

SOURCE="$1"
NAME="${SOURCE%.*}"

./bin/miniscm-ir "$SOURCE"
./bin/ir-x86 "${NAME}.ir"
gcc -o "$PWD/$NAME" lib/stdio.s lib/asm.s "${NAME}.s"
chmod +x "$NAME"
