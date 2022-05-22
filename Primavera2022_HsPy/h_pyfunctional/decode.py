#!/usr/bin/python3

import sys


def decode_bin(filename) -> str:
    letters = []
    with open(filename, "r") as file:
        for line in file.readlines():
            try:
                chrs_line = map(lambda b: chr(int(b, 2)), line.split())
                letters.extend(list(chrs_line))
            except ValueError:
                break
    return "".join(letters)


def encode_bin(message, filename, words_perline=6):
    message = list(message.replace("\n", " "))
    space_code = "20"  # "00100000"
    with open(filename, "w") as file:
        while message:
            line = []
            for i in range(words_perline):
                try:
                    bin_code = "{0:02x}".format(ord(message.pop(0)))
                    line.append(bin_code)
                except IndexError:
                    line.extend([space_code] * (words_perline - i))
                    break
            file.write(" ".join(line) + "\n")


if __name__ == "__main__":
    # msg = decode_bin(sys.argv[1])
    # print(msg.replace(". ", ".\n").strip())
    with open(sys.argv[1], "r") as file:
        encode_bin(file.read(), sys.argv[2], 32)
