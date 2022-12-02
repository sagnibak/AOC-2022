#!/bin/python3

import os
import sys


CLEAN_BUILD = "rm build/*"
CLEAN_BIN = "rm bin/*"

def echo_and_run(command: str) -> None:
    print(command)
    os.system(command)

try:
    day = int(sys.argv[1])

    BUILD_COMMAND = f"ghc -O3 -o bin/Day{day} Day{day}.hs -odir build/ -hidir build/"

    echo_and_run(CLEAN_BUILD)
    echo_and_run(BUILD_COMMAND)

    print("Success!")

except ValueError as e:
    if sys.argv[1] == "clean":
        echo_and_run(CLEAN_BUILD)
        echo_and_run(CLEAN_BIN)
    else:
        raise e
