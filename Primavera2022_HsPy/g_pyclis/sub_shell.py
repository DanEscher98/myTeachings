#!/usr/bin/python3


from typing import Tuple


def execute_process(cmd: str) -> Tuple[bytes, bytes, int]:
    from subprocess import PIPE, Popen

    p = Popen(cmd, stdout=PIPE, stderr=PIPE, shell=True)
    out, err = p.communicate()
    return (out, err, p.returncode)


if __name__ == "__main__":
    out, err, ret = execute_process("ping -w 3 www.google.com")
    print("Done")
    print(f"STDOUT:  {out.decode()}")
    print(f"STDERR:  {err.decode()}")
    print(f"RETCODE: {str(ret)}")
