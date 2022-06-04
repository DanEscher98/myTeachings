#include <stdio.h>
#include <stdlib.h>

int counter(int j) {
	static int i = 0;
	i = (i + 1) * j; // Side effect
	return i;
}

int main(void) {
	for (int j=0; j < 5; j++) {
		printf("Val: %d -> %d\n", j, counter(j));
	}
	return EXIT_SUCCESS;
}
