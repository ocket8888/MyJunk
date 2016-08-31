#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]){
	float agi=92.5;
	float time=atoi(argv[1]);
	for(float i;i<time;i+=170/(agi+125)){
		agi+=3;
	}
	printf("Agility: %f\n",agi);
	printf("Damage: %f\n",1.4*(2*agi+75));
	return 0;
}
