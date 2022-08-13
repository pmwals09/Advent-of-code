#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int main(void){
  FILE *fp = fopen("./day-02-data.txt", "r");
  if(fp == NULL){
    fprintf(stderr, "Error opening file.\n");
    exit(1);
  }

  int size = 1000;
  char *input = malloc(size);
  if(input == NULL){
    fprintf(stderr, "Error allocating memory to input\n");
    exit(2);
  }

  int l, w, h;
  int totalPaper = 0;
  int totalRibbon = 0;

  while((fscanf(fp, "%dx%dx%d", &l, &w, &h)) != EOF){
    int dims[] = { l, w, h };

    for(int i = 0; i < 2; i++){
      int idxOfMin = i;
      for(int j = i + 1; j < 3; j++){
        if(dims[j] < dims[idxOfMin]){
          idxOfMin = j;
        }
      }

      int temp = dims[idxOfMin];
      dims[idxOfMin] = dims[i];
      dims[i] = temp;
    }

    totalPaper += ((dims[0] * dims[1]) + (2 * l * w) + (2 * w * h) + (2 * h * l));
    totalRibbon += ((2 * dims[0]) + (2 * dims[1]) + (l * w * h));
  }

  printf("Part one: %d\n", totalPaper);
  printf("Part two: %d\n", totalRibbon);
}
