#!/usr/bin/python3
""" Creating a simple shell """


from typing import Tuple


def execute_process(cmd: str) -> Tuple[bytes, bytes, int]:
    from subprocess import PIPE, Popen

    with Popen(cmd, stdout=PIPE, stderr=PIPE, shell=True) as pipe:
        out, err = pipe.communicate()
        return (out, err, pipe.returncode)


if __name__ == "__main__":
    out, err, ret = execute_process("ping -w 3 www.google.com")
    print("Done")
    print(f"STDOUT:  {out.decode()}")
    print(f"STDERR:  {err.decode()}")
    print(f"RETCODE: {str(ret)}")
