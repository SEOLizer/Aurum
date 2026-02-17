#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking lyxc
OFS=$IFS
IFS="
"
/bin/ld.bfd -b elf64-x86-64 -m elf_x86_64     -s  -L. -o lyxc -T link3285316.res -e _start
if [ $? != 0 ]; then DoExitLink aurumc; fi
IFS=$OFS
