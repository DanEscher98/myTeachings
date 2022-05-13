#!/usr/bin/python3


from typing import Tuple


def execute_process(cmd: str) -> Tuple[bytes, bytes, int]:
    from subprocess import PIPE, Popen

    pipe = Popen(cmd, stdout=PIPE, stderr=PIPE, shell=True)
    out, err = pipe.communicate()
    return (out, err, pipe.returncode)


if __name__ == "__main__":
    out, err, ret = execute_process("ping -w 3 www.google.com")
    print("Done")
    print(f"STDOUT:  {out.decode()}")
    print(f"STDERR:  {err.decode()}")
    print(f"RETCODE: {str(ret)}")
