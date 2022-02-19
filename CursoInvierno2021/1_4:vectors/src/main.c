#include "lib.h"
#include <stdio.h>
#include <stdlib.h>
#include <error.h>

typedef struct vector {
	int length;
	int *values;
}vector;

typedef struct matrix {
	int cols;
	int rows;
	int *values;
} matrix;


// init: inicializar vector
vector init_vec(int);
// mul: multiplicar vectores
int dot_vec(vector, vector);
// print_vec: escribir los valores
void print_vec(vector);

matrix init_matrix(int, int);
void print_matrix(matrix);

matrix sum_matrix(matrix, matrix);
matrix dot_matrix(matrix, matrix);

int main(int argc, char **argv) {
	printf("Hello %s!\n", argv[1]);
	matrix a = init_matrix(2, 3);
	printf("\n");
	matrix b = init_matrix(3, 2);
	matrix c = dot_matrix(a, b);
	printf("\n");
	print_matrix(a);
	print_matrix(b);
	print_matrix(c);
	free(a.values);
	return EXIT_SUCCESS;
}

vector init_vec(int n) {
	vector new_vec = { .values = malloc(sizeof(int)*n) };
	int val;
	for (int i=0; i<n; i++) {
		printf("Vec[%d]: ", i);
		scanf("%d", &val);
		new_vec.values[i] = val;
		new_vec.length += 1;
		printf("Length: %d\n", new_vec.length);
	}
	return new_vec;
}

void print_vec(vector vec) {
	for (int i=0; i<vec.length; i++) {
		printf("%d, ", vec.values[i]);
	}
}

int dot_vec(vector a, vector b) {
	int answer = 0;
	if (a.length == b.length) {
		for (int i=0; i<a.length; i++) {
			answer += a.values[i]*b.values[i];
		}
		return answer;
	} else {
		error(1, 1, "Vectors not equal");
		exit(1);
	}
}

matrix sum_matrix(matrix a, matrix b) {
	if (a.cols == b.cols && a.rows == b.rows) {
		matrix ans = {
			.cols = a.cols,
			.rows = a.rows,
			.values = malloc(sizeof(int)*a.cols*b.rows) };
		for (int i=0; i < a.cols*a.rows; i++) {
			ans.values[i] = a.values[i] + b.values[i];
		}
		return ans;
	} else {
		exit(1);
	}
}

matrix dot_matrix(matrix a, matrix b) {
	if (a.cols == b.rows) {
		matrix ans = {
			.rows = a.rows,
			.cols = b.cols,
			.values = malloc(sizeof(int)*b.cols*a.rows)
		};
		for (int i=0; i < ans.rows; i++) {
			for (int j=0; j < ans.cols; j++) {
				int acc = 0;
				for (int pos=0; pos < a.cols; pos++) {
					int a_val = a.values[i*a.cols + pos];
					int b_val = b.values[pos*cols + j];
					printf("acc += a[%d][%d] × b[%d][%d] ->  %d × %d + %d\n",
							i, pos, pos, j, a_val, b_val, acc);
					acc += a_val * b_val;
				}
				ans.values[ans.cols*i+j] = acc;
			}
		}
		return ans;
	} else {
		exit(1);
	}
}

matrix init_matrix(int rows, int cols) {
	matrix new_m = {
		.cols = cols,
		.rows = rows,
		.values = malloc(sizeof(int)*cols*rows)
	};
	for (int i=0; i<rows; i++) {
		for (int j=0; j<cols; j++) {
			printf("Matrix[%d][%d]: ", i, j);
			scanf("%d", new_m.values + cols*i + j);
		}
	}
	return new_m;
}

void print_matrix(matrix m_in) {
	printf("Matrix (%d × %d)\n", m_in.rows, m_in.cols);
	for (int row=0; row < m_in.rows; ++row) {
		row == 0 ? printf("⌈") : (row == m_in.rows - 1 ? printf("⌊") : printf("│"));
		for (int col=0; col < m_in.cols; col++) {
			printf("%3d ", m_in.values[row*m_in.cols + col]);
		}
		row == 0 ? printf("⌉") : (row == m_in.rows - 1 ? printf("⌋") : printf("│"));
		printf("\n");
	}
}
