#include <stdio.h>
#include <stdlib.h>

int main(void){
  // read in the input
  FILE *fptr = fopen("./day-01-data.json", "r");
  if(fptr == NULL){
    fprintf(stderr, "Error opening file\n");
    exit(1);
  }

  int size = 1000;
  char *input = malloc(size);
  if(input == NULL){
    fprintf(stderr, "Error allocating memory to input\n");
    exit(2);
  }
  int charN = 0;
  int c;

  while((c = fgetc(fptr)) != EOF){
    if(charN >= size - 1){
      size += 1000;
      input = realloc(input, size);
      if(input == NULL){
        fprintf(stderr, "Error reallocating memory to input\n");
        exit(3);
      }
    }
    input[charN] = c;
    charN++;
  }
  if(charN == size){
    size++;
    input = realloc(input, size);
    if(input == NULL){
      fprintf(stderr, "Error reallocating memory to input\n");
      exit(3);
    }
  }
  charN++;
  input[charN] = '\0';

  int floor = 0;
  int instructionN = 0;
  char instruction;
  while((instruction = input[instructionN]) != '\0'){
    if(instruction == '('){
      floor++;
    } else if(instruction == ')'){
      floor--;
    }
    instructionN++;
  }

  printf("Part one: %d\n", floor);

  floor = 0;
  instructionN = 0;
  while((instruction = input[instructionN]) != '\0'){
    if(instruction == '('){
      floor++;
    } else if(instruction == ')'){
      floor--;
    }
    if(floor < 0){
      break;
    }
    instructionN++;
  }

  printf("Part two: %d\n", instructionN);

  fclose(fptr);
  free(input);
}
