#include "lib.h"
#include <stdio.h>
#include <stdlib.h>

typedef struct fighter {
    unsigned int health;
    int dmg;
    unsigned int id;
} fighter;

fighter init_fighter(unsigned int, int, unsigned int);
void print_fighter(fighter);
void arena_loop(fighter, fighter);

int main(int argc, char **argv) {
    printf("Hello %s!\n", argv[1]);
    fighter a = init_fighter(10, 6, 1234);
    fighter b = init_fighter(20, 4, 2345);
    arena_loop(a, b);
    return EXIT_SUCCESS;
}

fighter init_fighter(unsigned int health, int dmg, unsigned int id) {
    fighter new_player = {.health = health, .dmg = dmg, .id = id};
    return new_player;
}

void print_fighter(fighter player) {
    printf("ID: %u, health: %u, dmg: %d\n", player.id, player.health,
           player.dmg);
}

void arena_loop(fighter a, fighter b) {
    while (a.health > 0 && b.health > 0) {
		a.health = a.health < b.dmg ? 0 : a.health - b.dmg;
		b.health = b.health < a.dmg ? 0 : b.health - a.dmg;
        print_fighter(a);
        print_fighter(b);
        printf("---------\n");
    }
    if (a.health == b.health) {
        printf("Tie!\n");
    } else if (a.health > b.health) {
        printf("Player: %d wins\n", a.id);
    } else {
        printf("Player: %d wins\n", b.id);
    }
}
