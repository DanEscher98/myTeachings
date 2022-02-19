#include "lib.h"
#include <stdio.h>
#include <stdlib.h>

typedef struct cmplx {
	int re;
	int im;
} cmplx;

cmplx suma(cmplx a, cmplx b) {
	cmplx answer;
	answer.re = a.re + b.re;
	answer.im = a.im + b.im;
	return answer;
}

void print_cmplx(cmplx a) {
	printf("Real: %d, Im: %d\n", a.re, a.im);
}

int main(int argc, char **argv) {
	cmplx a = { .re = 3, .im = -5 };
	cmplx b = { .re = 2, .im = 4 };
	print_cmplx(suma(a, b));
	return EXIT_SUCCESS;
}
