#include "lib.h"
#include <stdio.h>
#include <stdlib.h>

typedef struct cmplx {
	int re;
	int im;
} cmplx;

cmplx suma(cmplx, cmplx);
void print_cmplx(cmplx);

int main(int argc, char **argv) {
	struct cmplx a = { .re = 3, .im = 2 };
	struct cmplx b = { .re = -2, .im = -1 };
	print_cmplx(suma(a, b));
	return EXIT_SUCCESS;
}

cmplx suma(cmplx a, cmplx b) {
	cmplx answer;
	answer.re = a.re + b.re;
	answer.im = a.im + b.im;
	return answer;
}

void print_cmplx(cmplx n) {
	printf("Re: %d, Im: %d\n", n.re, n.im);
}
