#! /usr/bin/env python

# The backend has a fixed sized array of 100
# to hold the interfaces exported by a unit.
# This test exercise that.

makefile = open("m3makefile", "w")
main = open("Main.m3", "w")

makefile.write("exec(\"../gen.py\")\n")
main.write("MODULE Main EXPORTS ")

for a in range(1,201):
  open("I" + str(a) + ".i3", "w").write("INTERFACE I" + str(a) + ";END I" + str(a) + ".\n")  
  makefile.write("derived_interface(\"I" + str(a) + "\", HIDDEN)\n");
  main.write("I" + str(a) + ",");
  if (a % 10) == 9:
    main.write("\n")

makefile.write("\nimport(\"m3core\")\nimplementation(\"Main\")\nprogram(\"a\")\n""")
main.write("\nMain;BEGIN END Main.\n")
